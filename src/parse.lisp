;;;; parse.lisp — grammar (SPEC §5.3–§5.4). Enforestation-lite (§5.8.6) hooks
;;;; in at macro identifiers from M5 on; everything else is a conventional
;;;; precedence parser.
;;;;
;;;; Node shapes produced here (args layouts; names are keywords, binders are
;;;; .ident nodes):
;;;;   (:ident     [name])
;;;;   binary ops  [lhs rhs]      — :add :sub :mul :div :rem :concat :eq :ne
;;;;                                :lt :le :gt :ge :and :or :pipe
;;;;   unary ops   [operand]      — :not :neg
;;;;   (:call      [callee arg*])
;;;;   (:index     [obj idx])
;;;;   (:field     [obj name])
;;;;   (:block     [stmt* value]) — value is the trailing expr, or scalar nil
;;;;                                when the block ends in `;` (Rust rule §5.4)
;;;;   (:if        [cond then-block else]) — else: nil | block | if
;;;;   (:while     [cond body-block])
;;;;   (:let/:var  [ident type-or-nil value])
;;;;   (:assign    [target value])
;;;;   (:op_assign [op target value]) — op: :add :sub :mul :div :concat
;;;;   (:return    [value])       — value is scalar nil for bare `return;`
;;;;   (:unreachable [])
;;;;   (:fn        [name-or-nil param* ret-or-nil body-block])
;;;;   (:param     [ident type-or-nil])
;;;;   (:type_ident [name])

(in-package #:sputter.impl)

(defstruct (parser (:constructor %make-parser (tokens file)))
  (tokens #() :type simple-vector)
  (file "?" :type string)
  (index 0 :type fixnum))

(defvar *no-brace-expr* nil
  "Bound true while parsing paren-free conditions/scrutinees: a bare `{` there
belongs to the construct's body, never to a block expression (SPEC §5.3).")

;;; --- token cursor -------------------------------------------------------------

(defun p-peek (p &optional (n 0))
  (let ((i (min (+ (parser-index p) n) (1- (length (parser-tokens p))))))
    (aref (parser-tokens p) i)))

(defun p-next (p)
  (let ((tok (p-peek p)))
    (unless (eq (token-type tok) :eof)
      (incf (parser-index p)))
    tok))

(defun p-prev (p)
  (assert (plusp (parser-index p)) () "no previous token")
  (aref (parser-tokens p) (1- (parser-index p))))

(defun p-at (p type &optional (n 0))
  (eq (token-type (p-peek p n)) type))

(defun p-at-kw (p kw &optional (n 0))
  (let ((tok (p-peek p n)))
    (and (eq (token-type tok) :kw) (eq (token-value tok) kw))))

(defun tok-meta (p tok)
  (make-source-meta (parser-file p) (token-line tok) (token-col tok)))

(defun token-describe (tok)
  (case (token-type tok)
    (:eof "end of file")
    (:ident (format nil "identifier `~a`" (symbol-name (token-value tok))))
    (:kw (format nil "keyword `~a`" (string-downcase (token-value tok))))
    (:int (format nil "integer `~a`" (token-text tok)))
    (:float (format nil "float `~a`" (token-text tok)))
    (:string "string literal")
    (:dot-ident (format nil "`~a`" (token-text tok)))
    (t (format nil "`~a`" (token-text tok)))))

(defun parse-error-at (p tok fmt &rest args)
  (apply #'sputter-error-at 'sputter-parse-error
         (parser-file p) (token-line tok) (token-col tok) fmt args))

(defun p-expect (p type what)
  (if (p-at p type)
      (p-next p)
      (parse-error-at p (p-peek p) "expected ~a, got ~a"
                      what (token-describe (p-peek p)))))

(defun p-expect-semi (p what)
  (unless (p-at p :semi)
    (parse-error-at p (p-peek p) "expected `;` after ~a, got ~a"
                    what (token-describe (p-peek p))))
  (p-next p))

;;; --- entry points --------------------------------------------------------------

(defun parse-module (source &key (file "<input>"))
  "Parse a whole .sput file into a list of top-level statement nodes."
  (let ((p (%make-parser (lex source :file file) file))
        (stmts '()))
    (loop until (p-at p :eof)
          do (push (parse-statement p) stmts))
    (nreverse stmts)))

(defun parse-expression (source &key (file "<input>"))
  "Parse a single expression (used by tests and, later, quote bodies)."
  (let* ((p (%make-parser (lex source :file file) file))
         (e (parse-expr p)))
    (unless (p-at p :eof)
      (parse-error-at p (p-peek p) "unexpected ~a after expression"
                      (token-describe (p-peek p))))
    e))

;;; --- statements (SPEC §5.4) ----------------------------------------------------

(defun parse-statement (p)
  "Parse one statement. First value is the node; second is how it was
terminated: :decl (`;` consumed), :braced (}-terminated form), :expr-semi,
or :expr-open (an expression with no `;` — legal only as a block's value)."
  (cond
    ((or (p-at-kw p :let) (p-at-kw p :var)) (parse-binding p))
    ((p-at-kw p :return) (parse-return p))
    ((p-at-kw p :while) (values (parse-while p) (braced-statement-end p)))
    ((p-at-kw p :if) (values (parse-if p) (braced-statement-end p)))
    ((and (p-at-kw p :fn) (p-at p :ident 1))
     (values (parse-fn p :named t) (braced-statement-end p)))
    ((p-at p :lbrace) (values (parse-block p) (braced-statement-end p)))
    (t (parse-expr-statement p))))

(defun braced-statement-end (p)
  "A }-terminated statement takes no `;`; consume a stray one leniently."
  (when (p-at p :semi) (p-next p))
  :braced)

(defun parse-binding (p)
  (let* ((intro (p-next p))              ; let | var
         (head (if (eq (token-value intro) :let) :let :var))
         (name-tok (p-expect p :ident "a name"))
         (name (make-ident (token-value name-tok) :meta (tok-meta p name-tok)))
         (type (when (p-at p :colon)
                 (p-next p)
                 (parse-type p))))
    (p-expect p :assign "`=`")
    (let ((value (parse-expr p)))
      (p-expect-semi p (format nil "`~(~a~)` binding" head))
      (values (make-node head (list name type value) :meta (tok-meta p intro))
              :decl))))

(defun parse-return (p)
  (let ((tok (p-next p)))
    (if (p-at p :semi)
        (progn (p-next p)
               (values (make-node :return (list nil) :meta (tok-meta p tok))
                       :decl))
        (let ((value (parse-expr p)))
          (p-expect-semi p "`return`")
          (values (make-node :return (list value) :meta (tok-meta p tok))
                  :decl)))))

(defun parse-expr-statement (p)
  (let ((e (parse-expr p)))
    (cond
      ;; assignment and compound assignment are statements, never expressions
      ((p-at p :assign)
       (let ((tok (p-next p)))
         (unless (ident-node-p e)
           (parse-error-at p tok
                           "assignment targets must be plain identifiers in v0.1"))
         (let ((value (parse-expr p)))
           (p-expect-semi p "assignment")
           (values (make-node :assign (list e value) :meta (tok-meta p tok))
                   :decl))))
      ((compound-assign-op p)
       (let* ((tok (p-next p))
              (op (compound-assign-op-head (token-type tok))))
         (unless (ident-node-p e)
           (parse-error-at p tok
                           "assignment targets must be plain identifiers in v0.1"))
         (let ((value (parse-expr p)))
           (p-expect-semi p "assignment")
           (values (make-node :op_assign (list op e value) :meta (tok-meta p tok))
                   :decl))))
      ((p-at p :semi)
       (p-next p)
       (values e :expr-semi))
      ;; no `;`: fine for }-terminated expressions used as statements,
      ;; and for a trailing block value (caller checks for `}`)
      ((eq (token-type (p-prev p)) :rbrace)
       (values e (if (p-at p :rbrace) :expr-open :braced)))
      ((p-at p :rbrace)
       (values e :expr-open))
      (t
       (parse-error-at p (p-peek p) "expected `;` after statement, got ~a"
                       (token-describe (p-peek p)))))))

(defun compound-assign-op (p)
  (member (token-type (p-peek p))
          '(:plus-assign :minus-assign :star-assign :slash-assign :concat-assign)))

(defun compound-assign-op-head (type)
  (ecase type
    (:plus-assign :add) (:minus-assign :sub) (:star-assign :mul)
    (:slash-assign :div) (:concat-assign :concat)))

;;; --- blocks and the Rust `;` rule (SPEC §5.4) -----------------------------------

(defun parse-block (p)
  "Parse `{ ... }`. Args are [stmt* value]; the value slot is the trailing
expression when the block ends without `;`, else a synthesized scalar nil."
  (let ((open (p-expect p :lbrace "`{`"))
        (stmts '())
        (value nil))
    (loop
      (when (p-at p :rbrace)
        (p-next p)
        (return))
      (multiple-value-bind (node kind) (parse-statement p)
        (ecase kind
          ((:decl :expr-semi) (push node stmts))
          (:expr-open
           ;; guaranteed by parse-expr-statement: next token is `}`
           (setf value node))
          (:braced
           ;; a trailing }-terminated *expression* is the block's value
           ;; (named fn defs are declarations, never values)
           (if (and (p-at p :rbrace) (value-expr-p node))
               (setf value node)
               (push node stmts))))))
    (make-node :block (append (nreverse stmts) (list value))
               :meta (tok-meta p open))))

(defun value-expr-p (node)
  "Can NODE be a block's trailing value? Expressions only — declarations,
assignments, and named fn defs are not values."
  (and (node-p node)
       (not (member (node-head node)
                    '(:let :var :assign :op_assign :return)))
       (not (and (eq (node-head node) :fn)
                 (first (node-args node))))))

;;; --- control forms ---------------------------------------------------------------

(defun parse-if (p)
  (let ((tok (p-next p))                 ; `if`
        (cond-expr (let ((*no-brace-expr* t)) (parse-expr p)))
        )
    (let ((then (parse-block p))
          (else nil))
      (when (p-at-kw p :else)
        (p-next p)
        (setf else (if (p-at-kw p :if)
                       (parse-if p)
                       (parse-block p))))
      (make-node :if (list cond-expr then else) :meta (tok-meta p tok)))))

(defun parse-while (p)
  (let* ((tok (p-next p))
         (cond-expr (let ((*no-brace-expr* t)) (parse-expr p)))
         (body (parse-block p)))
    (make-node :while (list cond-expr body) :meta (tok-meta p tok))))

;;; --- fn defs and lambdas (SPEC §5.4) ---------------------------------------------

(defun parse-fn (p &key named)
  "`fn name(a: T, b) RetT { body }` — NAMED selects the def form; the
expression form is anonymous (a name there is an error, SPEC §5.3)."
  (let ((tok (p-next p))                 ; `fn`
        (name nil))
    (if named
        (let ((name-tok (p-expect p :ident "a function name")))
          (setf name (make-ident (token-value name-tok)
                                 :meta (tok-meta p name-tok))))
        (when (p-at p :ident)
          (parse-error-at p (p-peek p)
                          "anonymous functions cannot be named (write `let ~a = fn(...) {...};`)"
                          (symbol-name (token-value (p-peek p))))))
    (p-expect p :lparen "`(`")
    (let ((params '()))
      (unless (p-at p :rparen)
        (loop
          (let* ((ptok (p-expect p :ident "a parameter name"))
                 (pname (make-ident (token-value ptok) :meta (tok-meta p ptok)))
                 (ptype (when (p-at p :colon)
                          (p-next p)
                          (parse-type p))))
            (push (make-node :param (list pname ptype) :meta (tok-meta p ptok))
                  params))
          (if (p-at p :comma)
              (progn (p-next p)
                     (when (p-at p :rparen) (return)))  ; trailing comma
              (return))))
      (p-expect p :rparen "`)`")
      ;; TypeExpr split (SPEC §5.4 grammar note): return position parses
      ;; TypeExpr, which never takes a `{` suffix — so `{` here always
      ;; starts the body. Keep this branch explicit; it is load-bearing.
      (let* ((ret (when (p-at p :ident) (parse-type p)))
             (body (parse-block p)))
        (make-node :fn (append (list name) (nreverse params) (list ret body))
                   :meta (tok-meta p tok))))))

(defun parse-type (p)
  "TypeExpr: in v0.1, just a type identifier (SPEC §5.4)."
  (let ((tok (p-expect p :ident "a type name")))
    (make-node :type_ident (list (token-value tok)) :meta (tok-meta p tok))))

;;; --- expressions (SPEC §5.3, fixed table, tightest to loosest) -------------------

(defun parse-expr (p)
  (parse-pipe p))

(defmacro def-left-binop (name next &body token->head)
  "Left-associative binary level: NAME loops over NEXT."
  `(defun ,name (p)
     (let ((e (,next p)))
       (loop
         (let ((head (case (token-type (p-peek p))
                       ,@token->head
                       (t nil))))
           (unless head (return e))
           (let ((tok (p-next p)))
             (setf e (make-node head (list e (,next p))
                                :meta (tok-meta p tok)))))))))

(def-left-binop parse-pipe parse-or
  (:pipe-gt :pipe))

(defun parse-or (p)
  (let ((e (parse-and p)))
    (loop while (p-at-kw p :or)
          do (let ((tok (p-next p)))
               (setf e (make-node :or (list e (parse-and p))
                                  :meta (tok-meta p tok)))))
    e))

(defun parse-and (p)
  (let ((e (parse-cmp p)))
    (loop while (p-at-kw p :and)
          do (let ((tok (p-next p)))
               (setf e (make-node :and (list e (parse-cmp p))
                                  :meta (tok-meta p tok)))))
    e))

(defun cmp-head (tok)
  (case (token-type tok)
    (:eq-eq :eq) (:bang-eq :ne) (:lt :lt) (:le :le) (:gt :gt) (:ge :ge)
    (t nil)))

(defun parse-cmp (p)
  "Comparisons are non-chainable (Zig rule, SPEC §5.3): `a < b < c` is a
parse error."
  (let ((e (parse-add p)))
    (a:when-let ((head (cmp-head (p-peek p))))
      (let ((tok (p-next p)))
        (setf e (make-node head (list e (parse-add p)) :meta (tok-meta p tok))))
      (when (cmp-head (p-peek p))
        (parse-error-at p (p-peek p)
                        "comparison operators are non-chainable; parenthesize and combine with `and`")))
    e))

(def-left-binop parse-add parse-mul
  (:plus :add) (:minus :sub) (:plus-plus :concat))

(def-left-binop parse-mul parse-unary
  (:star :mul) (:slash :div) (:percent :rem))

(defun parse-unary (p)
  (cond
    ((p-at p :bang)
     (let ((tok (p-next p)))
       (make-node :not (list (parse-unary p)) :meta (tok-meta p tok))))
    ((p-at p :minus)
     (let ((tok (p-next p)))
       (make-node :neg (list (parse-unary p)) :meta (tok-meta p tok))))
    (t (parse-postfix p))))

(defun parse-postfix (p)
  (let ((e (parse-primary p)))
    (loop
      (cond
        ((p-at p :lparen)
         (let ((tok (p-next p))
               (args '()))
           (let ((*no-brace-expr* nil))
             (unless (p-at p :rparen)
               (loop
                 (push (parse-expr p) args)
                 (if (p-at p :comma)
                     (progn (p-next p)
                            (when (p-at p :rparen) (return)))
                     (return)))))
           (p-expect p :rparen "`)`")
           (setf e (make-node :call (cons e (nreverse args))
                              :meta (tok-meta p tok)))))
        ((p-at p :lbracket)
         (let ((tok (p-next p)))
           (let ((idx (let ((*no-brace-expr* nil)) (parse-expr p))))
             (p-expect p :rbracket "`]`")
             (setf e (make-node :index (list e idx) :meta (tok-meta p tok))))))
        ((p-at p :dot-ident)
         (let ((tok (p-next p)))
           (setf e (make-node :field (list e (token-value tok))
                              :meta (tok-meta p tok)))))
        (t (return e))))))

(defun parse-primary (p)
  (let ((tok (p-peek p)))
    (case (token-type tok)
      (:int (token-value (p-next p)))
      (:float (token-value (p-next p)))
      (:string (token-value (p-next p)))
      (:ident (let ((tok (p-next p)))
                (make-ident (token-value tok) :meta (tok-meta p tok))))
      (:kw
       (case (token-value tok)
         (:true (p-next p) t)
         (:false (p-next p) +sput-false+)
         (:nil (p-next p) nil)
         (:if (parse-if p))
         (:while (parse-while p))
         (:fn (parse-fn p :named nil))
         (:unreachable
          (let ((tok (p-next p)))
            (make-node :unreachable '() :meta (tok-meta p tok))))
         ((:switch :for)
          (parse-error-at p tok "`~(~a~)` arrives with M3; not supported yet"
                          (token-value tok)))
         (:quote
          (parse-error-at p tok "`quote` arrives with M4; not supported yet"))
         (t (parse-error-at p tok "expected an expression, got ~a"
                            (token-describe tok)))))
      (:lparen
       (p-next p)
       (let ((e (let ((*no-brace-expr* nil)) (parse-expr p))))
         (p-expect p :rparen "`)`")
         e))
      (:lbrace
       (when *no-brace-expr*
         (parse-error-at p tok
                         "a `{` cannot start this expression (the braces here belong to the body); parenthesize the block"))
       (parse-block p))
      ((:lbracket :dot-ident :dot-lbrace)
       (parse-error-at p tok
                       "lists, atoms, and records arrive with M3; not supported yet"))
      (t (parse-error-at p tok "expected an expression, got ~a"
                         (token-describe tok))))))
