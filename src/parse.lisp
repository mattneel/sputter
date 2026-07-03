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

(defvar *in-quote* nil
  "Bound true while parsing quote bodies and macro templates: there (and only
there) raw(x)/inject(x)/insert(e) are the template escapes (SPEC §5.8.3).")

;; The macro registry lives in expand.lisp (loaded later); the parser only
;; asks 'is this name a macro?' and registers signatures at definition sites.
(declaim (ftype (function (keyword) t) macro-name-p)
         (ftype (function (keyword) t) macro-by-example-name-p)
         (ftype (function (keyword list t) t) register-macro-signature)
         (ftype (function (keyword list) t) register-by-example-macro))

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
          do (push (parse-statement-recording-end p) stmts))
    (nreverse stmts)))

(defun parse-statement-recording-end (p)
  "parse-statement, recording the statement's last token line in its meta —
the printer's blank-line preservation needs to see closing braces."
  (multiple-value-bind (node kind) (parse-statement p)
    (when (and (node-p node) (meta-p (node-meta node))
               (not (meta-synthetic (node-meta node))))
      (setf (meta-end-line (node-meta node)) (token-line (p-prev p))))
    (values node kind)))

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
    ((p-at-kw p :macro) (values (parse-macro-def p) (braced-statement-end p)))
    ((p-at-kw p :return) (parse-return p))
    ((p-at-kw p :while) (values (parse-while p) (braced-statement-end p)))
    ((p-at-kw p :for) (values (parse-for p) (braced-statement-end p)))
    ((p-at-kw p :switch) (values (parse-switch p) (braced-statement-end p)))
    ((p-at-kw p :if) (values (parse-if p) (braced-statement-end p)))
    ((and (p-at-kw p :fn) (p-at p :ident 1))
     (values (parse-fn p :named t) (braced-statement-end p)))
    ((p-at p :lbrace) (values (parse-block p) (braced-statement-end p)))
    (t (parse-expr-statement p))))

(defun braced-statement-end (p)
  "A }-terminated statement takes no `;` — but a `;` written after one has
semantic weight (SPEC §5.4): it pins the form as a *statement*, so it can
never be promoted to a block's trailing value."
  (if (p-at p :semi)
      (progn (p-next p) :braced-semi)
      :braced))

(defun parse-binder-name (p)
  "A binding-position name: an identifier — or, inside quote bodies,
inject(x)/raw(x) (anaphoric/literal binders, SPEC §5.8.3)."
  (if (and *in-quote*
           (p-at p :ident)
           (member (token-value (p-peek p)) '(:|inject| :|raw|))
           (p-at p :lparen 1))
      (parse-template-escape p)
      (let ((name-tok (p-expect p :ident "a name")))
        (make-ident (token-value name-tok) :meta (tok-meta p name-tok)))))

(defun parse-template-escape (p)
  "raw(x) / inject(x) / insert(e) inside a quote body."
  (let* ((tok (p-next p))
         (which (token-value tok))
         (head (ecase which (:|raw| :raw) (:|inject| :inject) (:|insert| :insert))))
    (p-expect p :lparen "`(`")
    (let ((arg (if (eq head :insert)
                   (let ((*no-brace-expr* nil)) (parse-expr p))
                   (let ((name-tok (p-expect p :ident "an identifier")))
                     (make-ident (token-value name-tok)
                                 :meta (tok-meta p name-tok))))))
      (p-expect p :rparen "`)`")
      (make-node head (list arg) :meta (tok-meta p tok)))))

(defun parse-binding (p)
  (let* ((intro (p-next p))              ; let | var
         (head (if (eq (token-value intro) :let) :let :var))
         (name (parse-binder-name p))
         (type (when (p-at p :colon)
                 (p-next p)
                 (parse-type p))))
    (p-expect p :assign "`=`")
    (let ((value (parse-expr p)))
      (p-expect-semi p (format nil "`~(~a~)` binding" head))
      (values (make-node head (list name type value) :meta (tok-meta p intro))
              :decl))))

;;; --- macro definitions (SPEC §5.8.1, §5.8.4) -----------------------------------

(defparameter +hole-kinds+
  '(:|expr| :|stmt| :|block| :|ident| :|atom| :|literal| :|type| :|arm|)
  "The kind inventory (SPEC §5.8.2) — the types of macro land.")

(defun parse-kind (p what)
  (let ((tok (p-expect p :ident what)))
    (unless (member (token-value tok) +hole-kinds+)
      (parse-error-at p tok
                      "unknown kind `~a` (expr, stmt, block, ident, atom, literal, type, arm)"
                      (symbol-name (token-value tok))))
    (token-value tok)))

(defun parse-macro-def (p)
  (let ((macro-tok (p-next p)))          ; `macro`
    (cond
      ((p-at-kw p :fn) (parse-macro-fn p macro-tok))
      ((p-at p :ident) (parse-by-example-macro p macro-tok))
      (t (parse-error-at p (p-peek p) "expected `fn` or a macro name after `macro`")))))

(defun parse-by-example-macro (p macro-tok)
  "`macro name { { pattern } => { template }, ... }` (SPEC §5.8.1, M6).
Both sides are raw token groups: LHS is an implicit pattern, RHS an implicit
template. The expander owns matching and template instantiation."
  (let* ((name-tok (p-expect p :ident "a macro name"))
         (name (make-ident (token-value name-tok) :meta (tok-meta p name-tok)))
         (arms '()))
    (p-expect p :lbrace "`{`")
    (unless (p-at p :rbrace)
      (loop
        (let* ((arm-meta (tok-meta p (p-peek p)))
               (pattern (collect-brace-token-group p "a by-example macro pattern"))
               (template (progn
                           (p-expect p :fat-arrow "`=>`")
                           (collect-brace-token-group
                            p "a by-example macro template"))))
          (push (make-node :macro_arm (list pattern template) :meta arm-meta)
                arms))
        (cond ((p-at p :comma)
               (p-next p)
               (when (p-at p :rbrace) (return)))
              ((p-at p :rbrace) (return))
              (t (parse-error-at p (p-peek p)
                                 "expected `,` after by-example macro arm, got ~a"
                                 (token-describe (p-peek p)))))))
    (p-expect p :rbrace "`}`")
    (let ((node (make-node :macro_def (cons name (nreverse arms))
                           :meta (tok-meta p macro-tok))))
      ;; Define-before-use: later forms in this parse can recognize the macro's
      ;; non-parenthesized invocation extent.
      (register-by-example-macro (ident-name name) (rest (node-args node)))
      node)))

(defun collect-brace-token-group (p what)
  "Consume a balanced `{ ... }` group and return its interior tokens."
  (unless (p-at p :lbrace)
    (parse-error-at p (p-peek p) "expected `{` starting ~a, got ~a"
                    what (token-describe (p-peek p))))
  (p-next p)
  (let ((start (parser-index p))
        (depth 1))
    (loop
      (let ((tok (p-peek p)))
        (case (token-type tok)
          (:eof (parse-error-at p tok "unterminated ~a" what))
          ((:lparen :lbracket :lbrace :dot-lbrace) (incf depth))
          ((:rparen :rbracket :rbrace)
           (when (and (eq (token-type tok) :rbrace) (= depth 1))
             (p-next p)
             (return (make-token-group
                      (subseq (parser-tokens p) start (1- (parser-index p))))))
           (decf depth))))
      (p-next p))))

(defun parse-macro-fn (p macro-tok)
  "`macro fn name(a: kind, b: kind) retkind { body }` (SPEC §5.8.4)."
  (p-next p)                             ; `fn`
  (let* ((name-tok (p-expect p :ident "a macro name"))
         (name (make-ident (token-value name-tok) :meta (tok-meta p name-tok)))
         (params '()))
    (p-expect p :lparen "`(`")
    (unless (p-at p :rparen)
      (loop
        (let* ((ptok (p-expect p :ident "a parameter name"))
               (pname (make-ident (token-value ptok) :meta (tok-meta p ptok))))
          (p-expect p :colon "`:` (macro parameters declare kinds)")
          (push (make-node :param (list pname (parse-kind p "a kind"))
                           :meta (tok-meta p ptok))
                params))
        (if (p-at p :comma)
            (progn (p-next p)
                   (when (p-at p :rparen) (return)))
            (return))))
    (p-expect p :rparen "`)`")
    (let* ((ret-kind (parse-kind p "the macro's return kind"))
           (body (let ((*in-quote* nil)) (parse-block p)))
           (node (make-node :macro_fn_def
                            (append (list name) (nreverse params)
                                    (list ret-kind body))
                            :meta (tok-meta p macro-tok))))
      ;; register the *signature* now: later forms in this module parse
      ;; calls to this macro by extent (define-before-use, SPEC §5.2)
      (register-macro-signature
       (ident-name name)
       (mapcar (lambda (param)
                 (cons (ident-name (first (node-args param)))
                       (second (node-args param))))
               (subseq (node-args node) 1 (- (length (node-args node)) 2)))
       ret-kind)
      node)))

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
        ;; a block interior is a fresh statement context: a paren-free
        ;; condition's no-brace rule must not leak into nested bodies
        (*no-brace-expr* nil)
        (stmts '())
        (value nil))
    (loop
      (when (p-at p :rbrace)
        (p-next p)
        (return))
      (multiple-value-bind (node kind) (parse-statement-recording-end p)
        (ecase kind
          ;; :braced-semi: the `;` pinned it as a statement (SPEC §5.4)
          ((:decl :expr-semi :braced-semi) (push node stmts))
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
assignments, macro defs, and named fn defs are not values."
  (and (node-p node)
       (not (member (node-head node)
                    '(:let :var :assign :op_assign :return
                      :macro_fn_def :macro_def)))
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

(defun parse-for (p)
  "`for x in xs { ... }` — iterates a list, value nil (SPEC §5.4)."
  (let* ((tok (p-next p))
         (binder-tok (p-expect p :ident "a binder name"))
         (binder (make-ident (token-value binder-tok)
                             :meta (tok-meta p binder-tok))))
    (unless (p-at-kw p :in)
      (parse-error-at p (p-peek p) "expected `in`, got ~a"
                      (token-describe (p-peek p))))
    (p-next p)
    (let* ((iter (let ((*no-brace-expr* t)) (parse-expr p)))
           (body (parse-block p)))
      (make-node :for_in (list binder iter body) :meta (tok-meta p tok)))))

;;; --- switch and term patterns (SPEC §5.6) ---------------------------------------

(defun parse-switch (p)
  (let* ((tok (p-next p))
         (scrutinee (let ((*no-brace-expr* t)) (parse-expr p))))
    (p-expect p :lbrace "`{`")
    (let ((arms '()))
      (loop until (p-at p :rbrace)
            do (push (parse-arm p) arms)
               ;; Rust comma rule: comma required after expression arms,
               ;; optional after }-terminated arms; trailing comma fine.
               (cond ((p-at p :comma) (p-next p))
                     ((p-at p :rbrace))
                     ((eq (token-type (p-prev p)) :rbrace))
                     (t (parse-error-at p (p-peek p)
                                        "expected `,` after switch arm, got ~a"
                                        (token-describe (p-peek p))))))
      (p-next p)                        ; the closing brace
      (make-node :switch (cons scrutinee (nreverse arms))
                 :meta (tok-meta p tok)))))

(defun parse-arm (p)
  (let ((pattern (if (p-at-kw p :else)
                     ;; `else` is sugar for `_` (SPEC §5.6)
                     (let ((tok (p-next p)))
                       (make-ident "_" :meta (tok-meta p tok)))
                     (parse-pattern p))))
    (let ((arrow (p-expect p :fat-arrow "`=>`")))
      (make-node :arm (list pattern (parse-expr p))
                 :meta (tok-meta p arrow)))))

(defun parse-pattern (p)
  "Term patterns (SPEC §5.6): bare identifiers *bind* (the exact inverse of
macro patterns); literals and atoms match by ==; `_` is the wildcard."
  (let ((tok (p-peek p)))
    (case (token-type tok)
      ((:int :float :string) (token-value (p-next p)))
      (:minus
       (p-next p)
       (let ((num (p-peek p)))
         (case (token-type num)
           ((:int :float) (- (token-value (p-next p))))
           (t (parse-error-at p num
                              "patterns only negate number literals, got ~a"
                              (token-describe num))))))
      (:kw
       (case (token-value tok)
         (:true (p-next p) t)
         (:false (p-next p) +sput-false+)
         (:nil (p-next p) nil)
         (t (parse-error-at p tok "expected a pattern, got ~a"
                            (token-describe tok)))))
      (:ident
       (let ((tok (p-next p)))
         (make-ident (token-value tok) :meta (tok-meta p tok))))
      (:dot-ident
       (let ((tok (p-next p)))
         (if (p-at p :lparen)
             ;; .tag(p1, p2) destructures tagged values, arity-checked
             (progn
               (p-next p)
               (let ((subs '()))
                 (unless (p-at p :rparen)
                   (loop
                     (push (parse-pattern p) subs)
                     (if (p-at p :comma)
                         (progn (p-next p)
                                (when (p-at p :rparen) (return)))
                         (return))))
                 (p-expect p :rparen "`)`")
                 (make-node :tagged_lit (cons (token-value tok) (nreverse subs))
                            :meta (tok-meta p tok))))
             (token-value tok))))      ; a bare atom matches by ==
      (:lbracket
       (let ((tok (p-next p))
             (elems '()))
         (unless (p-at p :rbracket)
           (loop
             (if (p-at p :ellipsis)
                 ;; [p, ...rest] binds the tail; must be last
                 (let ((etok (p-next p))
                       (rest-tok (p-expect p :ident "a name after `...`")))
                   (push (make-node :spread
                                    (list (make-ident (token-value rest-tok)
                                                      :meta (tok-meta p rest-tok)))
                                    :meta (tok-meta p etok))
                         elems)
                   (when (p-at p :comma) (p-next p))
                   (unless (p-at p :rbracket)
                     (parse-error-at p (p-peek p)
                                     "`...rest` must be the last element of a list pattern"))
                   (return))
                 (push (parse-pattern p) elems))
             (if (p-at p :comma)
                 (progn (p-next p)
                        (when (p-at p :rbracket) (return)))
                 (return))))
         (p-expect p :rbracket "`]`")
         (make-node :list_lit (nreverse elems) :meta (tok-meta p tok))))
      (:dot-lbrace
       (parse-record-shape p #'parse-pattern))
      (t (parse-error-at p tok "expected a pattern, got ~a"
                         (token-describe tok))))))

;;; --- quote (SPEC §5.7) -------------------------------------------------------------

(defparameter +quote-kinds+ '(:|expr| :|stmt| :|stmts| :|block| :|type| :|arm|)
  "Fragment specifiers, generalized (SPEC §5.7). Default is expr.")

(defun parse-quote (p)
  "`quote { ... }` / `quote(kind) { ... }` — the body parses with the real
parser (ill-formed fragments are impossible by construction, I3) and the
whole form evaluates to a node (a list of nodes for `stmts`)."
  (let ((tok (p-next p))                ; `quote`
        (kind :|expr|))
    (when (p-at p :lparen)
      (p-next p)
      (let ((kind-tok (p-expect p :ident "a quote kind")))
        (setf kind (token-value kind-tok))
        (unless (member kind +quote-kinds+)
          (parse-error-at p kind-tok
                          "unknown quote kind `~a` (expr, stmt, stmts, block, type, arm)"
                          (symbol-name kind))))
      (p-expect p :rparen "`)`"))
    (let ((*no-brace-expr* nil)
          (*in-quote* t))
      (ecase kind
        (:|block|
         ;; the quote's braces are the block itself
         (make-node :quote (list kind (parse-block p)) :meta (tok-meta p tok)))
        (:|expr|
         (p-expect p :lbrace "`{`")
         (let ((body (parse-expr p)))
           (p-expect p :rbrace "`}`")
           (make-node :quote (list kind body) :meta (tok-meta p tok))))
        (:|stmt|
         (p-expect p :lbrace "`{`")
         (let ((body (parse-statement p)))
           (p-expect p :rbrace "`}`")
           (make-node :quote (list kind body) :meta (tok-meta p tok))))
        (:|stmts|
         (p-expect p :lbrace "`{`")
         (let ((stmts '()))
           (loop until (p-at p :rbrace)
                 do (push (parse-statement p) stmts))
           (p-next p)
           (make-node :quote (cons kind (nreverse stmts))
                      :meta (tok-meta p tok))))
        (:|type|
         (p-expect p :lbrace "`{`")
         (let ((body (parse-type p)))
           (p-expect p :rbrace "`}`")
           (make-node :quote (list kind body) :meta (tok-meta p tok))))
        (:|arm|
         (p-expect p :lbrace "`{`")
         (let ((body (parse-arm p)))
           (when (p-at p :comma) (p-next p)) ; tolerate a trailing comma
           (p-expect p :rbrace "`}`")
           (make-node :quote (list kind body) :meta (tok-meta p tok))))))))

(defun parse-record-shape (p value-parser)
  "`.{ .name = X, ... }` where X comes from VALUE-PARSER — shared by record
literals (expressions) and record patterns."
  (let ((tok (p-expect p :dot-lbrace "`.{`"))
        (inits '()))
    (unless (p-at p :rbrace)
      (loop
        (let ((name-tok (p-peek p)))
          (unless (eq (token-type name-tok) :dot-ident)
            (parse-error-at p name-tok "expected a field (`.name = ...`), got ~a"
                            (token-describe name-tok)))
          (p-next p)
          (p-expect p :assign "`=`")
          (push (make-node :field_init
                           (list (token-value name-tok) (funcall value-parser p))
                           :meta (tok-meta p name-tok))
                inits))
        (if (p-at p :comma)
            (progn (p-next p)
                   (when (p-at p :rbrace) (return)))
            (return))))
    (p-expect p :rbrace "`}`")
    (make-node :record_lit (nreverse inits) :meta (tok-meta p tok))))

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

(defvar *parse-depth* 0)

(defparameter +max-parse-depth+ 500
  "Nesting budget: recursive descent must signal a spanned Sputter error
long before the host control stack would overflow into a raw backtrace (I2).")

(defun parse-expr (p)
  (let ((*parse-depth* (1+ *parse-depth*)))
    (when (> *parse-depth* +max-parse-depth+)
      (parse-error-at p (p-peek p)
                      "this expression is nested more than ~d levels deep"
                      +max-parse-depth+))
    (parse-pipe p)))

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
     (let* ((tok (p-next p))
            (operand (parse-unary p)))
       ;; fold negated number literals into negative scalars: `-1` is the
       ;; literal -1, so parse(print(n)) round-trips trees that carry
       ;; negative scalars (there is no other surface syntax for them)
       (if (and (not (node-p operand)) (numberp operand))
           (- operand)
           (make-node :neg (list operand) :meta (tok-meta p tok)))))
    (t (parse-postfix p))))

(defun collect-paren-group (p)
  "Consume a balanced (...) group; return the interior tokens as a
token-group (the raw macro-call payload, SPEC §5.8.6)."
  (p-expect p :lparen "`(`")
  (let ((start (parser-index p))
        (depth 1))
    (loop
      (let ((tok (p-peek p)))
        (case (token-type tok)
          (:eof (parse-error-at p tok "unterminated macro invocation"))
          ((:lparen :lbracket :lbrace :dot-lbrace) (incf depth))
          ((:rparen :rbracket :rbrace)
           (when (and (eq (token-type tok) :rparen) (= depth 1))
             (p-next p)
             (return (make-token-group
                      (subseq (parser-tokens p) start (1- (parser-index p))))))
           (decf depth))))
      (p-next p))))

(defun collect-balanced-surface-group (p what)
  "Consume one balanced surface group at point (`(...)`, `[...]`, `{...}`, or
`.{...}`). Used by M6's enforestation-lite extent collector."
  (unless (member (token-type (p-peek p))
                  '(:lparen :lbracket :lbrace :dot-lbrace))
    (parse-error-at p (p-peek p) "expected a delimited group in ~a, got ~a"
                    what (token-describe (p-peek p))))
  (let ((depth 0))
    (loop
      (let ((tok (p-peek p)))
        (case (token-type tok)
          (:eof (parse-error-at p tok "unterminated ~a" what))
          ((:lparen :lbracket :lbrace :dot-lbrace) (incf depth))
          ((:rparen :rbracket :rbrace) (decf depth))))
      (p-next p)
      (when (zerop depth) (return)))))

(defun collect-by-example-call (p)
  "Collect a by-example macro invocation's raw extent.
v0.1 by-example invocations are delimited by their first top-level `{...}`
group. A following `else { ... }` belongs to the same extent, which is the
specific longest-match rule needed by `unless` (§10.2)."
  (let* ((start (parser-index p))
         (name-tok (p-next p))
         (name (token-value name-tok))
         (saw-main-group nil))
    (labels ((finish ()
               (make-node :macro_call
                          (list name
                                (make-token-group
                                 (subseq (parser-tokens p) start
                                         (parser-index p)))
                                :by-example)
                          :meta (tok-meta p name-tok)))
             (need-main-group-error ()
               (parse-error-at p (p-peek p)
                               "by-example macro `~a` invocation needs a `{...}` group"
                               (symbol-name name))))
      (loop
        (let ((tok (p-peek p)))
          (cond
            ((eq (token-type tok) :eof)
             (if saw-main-group (return (finish)) (need-main-group-error)))
            ((and saw-main-group (p-at-kw p :else))
             (p-next p)
             (unless (p-at p :lbrace)
               (parse-error-at p (p-peek p)
                               "expected `{` after `else` in by-example macro invocation, got ~a"
                               (token-describe (p-peek p))))
             (collect-balanced-surface-group p "by-example macro `else` arm")
             (return (finish)))
            (saw-main-group
             (return (finish)))
            ((p-at p :lbrace)
             (collect-balanced-surface-group p "by-example macro invocation")
             (setf saw-main-group t))
            ((member (token-type tok) '(:lparen :lbracket :dot-lbrace))
             (collect-balanced-surface-group p "by-example macro invocation"))
            ((member (token-type tok)
                     '(:semi :comma :rparen :rbracket :rbrace :fat-arrow))
             (need-main-group-error))
            (t (p-next p))))))))

(defun parse-postfix (p)
  (let ((e (parse-primary p)))
    ;; enforestation-lite (SPEC §5.8.6): a known macro name in call position
    ;; claims its balanced parens as a raw token group. Inside quotes too —
    ;; a quoted macro call is data, but its extent still needs collecting;
    ;; it expands only when spliced back into a program.
    (when (and (ident-node-p e)
               (p-at p :lparen)
               (macro-name-p (ident-name e)))
      (return-from parse-postfix
        (make-node :macro_call
                   (list (ident-name e) (collect-paren-group p))
                   :meta (node-meta e))))
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
      (:ident
       (cond
         ((and *in-quote*
               (member (token-value tok) '(:|raw| :|inject| :|insert|))
               (p-at p :lparen 1))
          (parse-template-escape p))
         ((macro-by-example-name-p (token-value tok))
          (collect-by-example-call p))
         (t
          (let ((tok (p-next p)))
            (make-ident (token-value tok) :meta (tok-meta p tok))))))
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
         (:switch (parse-switch p))
         (:for (parse-for p))
         (:quote (parse-quote p))
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
      (:lbracket (parse-list-literal p))
      (:dot-lbrace
       (parse-record-shape p (lambda (p)
                               (let ((*no-brace-expr* nil))
                                 (parse-expr p)))))
      (:dot-ident
       ;; prefix position: an atom, or `.tag(v1, v2)` — a tagged literal
       (let ((tok (p-next p)))
         (if (p-at p :lparen)
             (progn
               (p-next p)
               (let ((vals '())
                     (*no-brace-expr* nil))
                 (unless (p-at p :rparen)
                   (loop
                     (push (parse-expr p) vals)
                     (if (p-at p :comma)
                         (progn (p-next p)
                                (when (p-at p :rparen) (return)))
                         (return))))
                 (p-expect p :rparen "`)`")
                 (make-node :tagged_lit (cons (token-value tok) (nreverse vals))
                            :meta (tok-meta p tok))))
             (token-value tok))))
      (t (parse-error-at p tok "expected an expression, got ~a"
                         (token-describe tok))))))

(defun parse-list-literal (p)
  "`[a, b, ...rest]` — elements are expressions; `...expr` is a spread."
  (let ((tok (p-expect p :lbracket "`[`"))
        (elems '())
        (*no-brace-expr* nil))
    (unless (p-at p :rbracket)
      (loop
        (if (p-at p :ellipsis)
            (let ((etok (p-next p)))
              (push (make-node :spread (list (parse-expr p))
                               :meta (tok-meta p etok))
                    elems))
            (push (parse-expr p) elems))
        (if (p-at p :comma)
            (progn (p-next p)
                   (when (p-at p :rbracket) (return)))
            (return))))
    (p-expect p :rbracket "`]`")
    (make-node :list_lit (nreverse elems) :meta (tok-meta p tok))))
