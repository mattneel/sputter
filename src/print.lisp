;;;; print.lisp — print / dump / show (SPEC §5.7).
;;;; `print`: the canonical surface pretty-printer. Contract:
;;;; parse(print(n)) ≡ n modulo meta; minimal parens (I8) — but *sufficient*
;;;; parens: statement-position and condition-position reparse hazards are
;;;; the printer's problem too; canonical layout (4-space indent, one switch
;;;; arm per line, trailing commas on multiline lists).

(in-package #:sputter.impl)

(defparameter *print-width* 100
  "Canonical line-width budget; constructs that fit stay inline.")

(defconstant +indent-step+ 4)

(defvar *force-inline* nil
  "Correctness fallback: when true, must-break constructs (switch, long pipe
chains, multi-item blocks) render inline anyway. Used where the grammar
offers no multiline form — a long line beats a crash or wrong output.")

;;; --- literals ---------------------------------------------------------------

(defun literal-string (x)
  "Render a scalar as a Sputter literal (negative numbers included: the
parser folds `-1` back into the scalar, so this round-trips)."
  (cond ((eq x t) "true")
        ((null x) "nil")
        ((sput-false-p x) "false")
        ((integerp x) (format nil "~d" x))
        ((floatp x) (float-literal-string x))
        ((stringp x) (escape-string-literal x))
        ((keywordp x) (concatenate 'string "." (symbol-name x)))
        (t (assert nil (x) "printer got a non-scalar where a literal belongs: ~s" x))))

(defun float-literal-string (f)
  (let* ((*read-default-float-format* 'double-float)
         (s (prin1-to-string (coerce f 'double-float))))
    ;; Negative space: the result must re-lex as a Sputter float.
    (assert (every (lambda (c) (find c "0123456789.e+-")) s) (s)
            "float printed unlexably: ~s" s)
    s))

(defun escape-string-literal (s)
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for ch across s
          do (case ch
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Tab (write-string "\\t" out))
               (t (if (or (< (char-code ch) 32) (= (char-code ch) 127))
                      (format out "\\x~2,'0X" (char-code ch))
                      (write-char ch out)))))
    (write-char #\" out)))

;;; --- head classification --------------------------------------------------------

(defparameter +binop-info+
  ;; head -> (text tightness), tightest 9 (postfix) … loosest 2 (|>)
  '((:pipe "|>" 2) (:or "or" 3) (:and "and" 4)
    (:eq "==" 5) (:ne "!=" 5) (:lt "<" 5) (:le "<=" 5) (:gt ">" 5) (:ge ">=" 5)
    (:add "+" 6) (:sub "-" 6) (:concat "++" 6)
    (:mul "*" 7) (:div "/" 7) (:rem "%" 7)))

(defparameter +cmp-heads+ '(:eq :ne :lt :le :gt :ge))

(defun binop-info (head) (assoc head +binop-info+))

(defparameter +brace-stmt-heads+
  '(:if :while :block :fn :switch :for_in :macro_fn_def :macro_def)
  "Heads whose statement form is }-terminated and takes no trailing `;`.")

(defparameter +stmt-only-heads+ '(:let :var :assign :op_assign :return))

(defparameter +stmt-stopper-heads+ '(:if :while :switch :for_in :block)
  "Constructs that, at statement start, parse as whole statements: an
expression *beginning* with one of these must print parenthesized or it
re-parses differently (the fmt-round-trip hazard the M1 review caught).")

(defun expr-leading-stopper-p (x)
  "Would X's printed text *begin* with a statement-stopper construct?"
  (and (node-p x)
       (let ((head (node-head x)))
         (cond ((member head +stmt-stopper-heads+) t)
               ((binop-info head) (expr-leading-stopper-p (first (node-args x))))
               ((member head '(:call :index :field))
                (expr-leading-stopper-p (first (node-args x))))
               (t nil)))))

(defun cond-needs-parens-p (x)
  "Paren-free condition/scrutinee positions parse with the no-brace rule:
any bare block primary in there (outside self-delimiting constructs and
bracketed argument positions) makes the text unparseable without parens."
  (and (node-p x)
       (let ((head (node-head x)) (args (node-args x)))
         (cond ((eq head :block) t)
               ;; self-delimiting: their braces belong to their own grammar
               ((member head '(:if :while :switch :for_in :fn :quote
                               :record_lit :list_lit :tagged_lit))
                nil)
               ((member head '(:not :neg)) (cond-needs-parens-p (first args)))
               ((member head '(:call :index :field))
                ;; arguments/indexes sit inside their own delimiters
                (cond-needs-parens-p (first args)))
               ((binop-info head)
                (or (cond-needs-parens-p (first args))
                    (cond-needs-parens-p (second args))))
               (t nil)))))

(defun maybe-paren (s own req)
  (if (< own req) (concatenate 'string "(" s ")") s))

;;; --- expression rendering (inline) -------------------------------------------
;;; render-expr returns the unbroken string, or NIL when the node must break
;;; (switch, multi-pipe chains, multi-item blocks) — unless *force-inline*.

(defun render-expr (x req)
  (cond
    ((not (node-p x)) (literal-string x))
    (t
     (let ((head (node-head x))
           (args (node-args x)))
       (a:if-let ((info (binop-info head)))
         (destructuring-bind (op-text own) (rest info)
           (if (and (eq head :pipe)
                    (pipe-chain-p x)
                    (not *force-inline*))
               nil                      ; ≥2 |> stages: canonical form breaks
               (let* ((cmp (member head +cmp-heads+))
                      (lhs (render-expr (first args) (if cmp (1+ own) own)))
                      (rhs (render-expr (second args) (1+ own))))
                 (when (and lhs rhs)
                   (maybe-paren (format nil "~a ~a ~a" lhs op-text rhs)
                                own req)))))
         (case head
           (:ident (symbol-name (ident-name x)))
           (:not (a:when-let ((s (render-expr (first args) 8)))
                   (maybe-paren (concatenate 'string "!" s) 8 req)))
           (:neg (a:when-let ((s (render-expr (first args) 8)))
                   (maybe-paren (concatenate 'string "-" s) 8 req)))
           (:call
            (let ((callee (render-expr (first args) 9))
                  (call-args (mapcar (lambda (e) (render-expr e 0)) (rest args))))
              (when (and callee (every #'identity call-args))
                (maybe-paren (format nil "~a(~{~a~^, ~})" callee call-args)
                             9 req))))
           (:index
            (let ((obj (render-expr (first args) 9))
                  (idx (render-expr (second args) 0)))
              (when (and obj idx)
                (maybe-paren (format nil "~a[~a]" obj idx) 9 req))))
           (:field
            (a:when-let ((obj (render-expr (first args) 9)))
              (let ((fname (second args)))
                (maybe-paren
                 (cond ((keywordp fname)
                        (format nil "~a.~a" obj (symbol-name fname)))
                       ((ident-node-p fname)  ; spliced computed field
                        (format nil "~a.~a" obj (symbol-name (ident-name fname))))
                       (t                     ; unexpanded template: .insert(e)
                        (format nil "~a.~a" obj (render-expr fname 0))))
                 9 req))))
           (:block (render-block-inline x))
           (:if (render-if-inline x))
           ((:while :for_in)
            (let ((header (render-loop-header x))
                  (body (render-block-inline (a:lastcar args))))
              (when (and header body) (format nil "~a ~a" header body))))
           (:fn (render-fn-inline x))
           (:switch (when *force-inline* (render-switch-inline x)))
           (:tagged_lit
            (let ((vals (mapcar (lambda (e) (render-expr e 0)) (rest args))))
              (when (every #'identity vals)
                (format nil ".~a(~{~a~^, ~})" (symbol-name (first args)) vals))))
           (:record_lit
            (let ((fields (mapcar #'render-field-init-inline args)))
              (when (every #'identity fields)
                (if fields
                    (format nil ".{ ~{~a~^, ~} }" fields)
                    ".{}"))))
           (:field_init (render-field-init-inline x))
           (:list_lit
            (let ((elems (mapcar (lambda (e) (render-expr e 0)) args)))
              (when (every #'identity elems)
                (format nil "[~{~a~^, ~}]" elems))))
           (:spread
            (a:when-let ((s (render-expr (first args) 0)))
              (concatenate 'string "..." s)))
           (:unreachable "unreachable")
           (:quote (render-quote-inline x))
	           (:macro_call
	            (destructuring-bind (name payload &optional style) args
	              (if (eq style :by-example)
	                  (serialize-tokens (token-group-tokens payload))
	                  (format nil "~a(~a)" (symbol-name name)
	                          (serialize-tokens (token-group-tokens payload))))))
           ((:raw :inject :insert)
            (a:when-let ((s (render-expr (first args) 0)))
              (format nil "~(~a~)(~a)" head s)))
           (:param (render-param x))
           (:type_ident (symbol-name (first args)))
           (:arm (render-arm-inline x))
           ((:let :var :assign :op_assign :return)
            (assert nil (x) "statement head ~a reached expression rendering" head))
           (t
            ;; reachable from user space via node() with an invented head —
            ;; panic Sputter-side rather than trip a host assertion
            (rt-panic "print: no rendering for a node with head .~a"
                      (string-downcase (symbol-name head))))))))))

(defun pipe-chain-p (x)
  "≥2 |> stages (the lhs of a pipe is itself a pipe)."
  (and (node-p x) (eq (node-head x) :pipe)
       (node-p (first (node-args x)))
       (eq (node-head (first (node-args x))) :pipe)))

(defun render-field-init-inline (fi)
  (destructuring-bind (name value) (node-args fi)
    (a:when-let ((v (render-expr value 0)))
      (format nil ".~a = ~a" (symbol-name name) v))))

(defun render-loop-header (x)
  (ecase (node-head x)
    (:while (format nil "while ~a" (render-cond (first (node-args x)))))
    (:for_in
     (destructuring-bind (binder iter body) (node-args x)
       (declare (ignore body))
       (format nil "for ~a in ~a"
               (symbol-name (ident-name binder)) (render-cond iter))))))

(defun render-cond (c)
  "Paren-free condition/scrutinee position: never NIL, parenthesized when
the no-brace rule demands it."
  (let ((s (or (render-expr c 0)
               (let ((*force-inline* t)) (render-expr c 0)))))
    (assert s (c) "condition rendered as nothing")
    (if (cond-needs-parens-p c)
        (concatenate 'string "(" s ")")
        s)))

(defun render-if-inline (x)
  (destructuring-bind (c then else) (node-args x)
    (let ((cs (render-cond c))
          (ts (render-block-inline then)))
      (when (and cs ts)
        (cond ((null else) (format nil "if ~a ~a" cs ts))
              ((eq (node-head else) :if)
               (a:when-let ((es (render-if-inline else)))
                 (format nil "if ~a ~a else ~a" cs ts es)))
              (t (a:when-let ((es (render-block-inline else)))
                   (format nil "if ~a ~a else ~a" cs ts es))))))))

(defun render-fn-inline (x)
  (let* ((args (node-args x))
         (name (first args))
         (body (a:lastcar args))
         (ret (a:lastcar (butlast args)))
         (params (subseq args 1 (- (length args) 2))))
    (let ((ps (mapcar #'render-param params))
          (bs (render-block-inline body)))
      (when bs
        (format nil "fn~@[ ~a~](~{~a~^, ~})~@[ ~a~] ~a"
                (and name (symbol-name (ident-name name)))
                ps
                (and ret (render-expr ret 0))
                bs)))))

(defun render-param (param)
  (destructuring-bind (name type) (node-args param)
    ;; the type slot holds a type node — or a bare kind keyword (macro fn)
    (if type
        (format nil "~a: ~a" (render-binder-name name)
                (if (keywordp type) (symbol-name type) (render-expr type 0)))
        (render-binder-name name))))

(defun render-binder-name (name)
  "Binder positions hold ident nodes — or inject(x)/raw(x) escapes in
templates."
  (if (ident-node-p name)
      (symbol-name (ident-name name))
      (render-expr name 0)))

(defun quote-kind-prefix (kind)
  (if (eq kind :|expr|)
      "quote"                           ; the default kind is elided
      (format nil "quote(~a)" (symbol-name kind))))

(defun render-quote-inline (x)
  (destructuring-bind (kind . body) (node-args x)
    (let ((prefix (quote-kind-prefix kind)))
      (case kind
        (:|block| (a:when-let ((b (render-block-inline (first body))))
                    (format nil "~a ~a" prefix b)))
        (:|stmts|
         (let ((parts (mapcar #'render-stmt-inline body)))
           (when (every #'identity parts)
             (format nil "~a {~{ ~a~} }" prefix parts))))
        (:|stmt|
         (a:when-let ((s (render-stmt-inline (first body))))
           (format nil "~a { ~a }" prefix s)))
        (t (a:when-let ((s (render-expr (first body) 0)))
             (format nil "~a { ~a }" prefix s)))))))

(defun render-switch-inline (x)
  "Inline switch — the *force-inline* fallback only; canonical form breaks."
  (let ((scrutinee (render-cond (first (node-args x))))
        (arms (mapcar #'render-arm-inline (rest (node-args x)))))
    (when (every #'identity arms)
      (format nil "switch ~a {~{ ~a,~} }" scrutinee arms))))

(defun render-arm-inline (arm)
  (destructuring-bind (pattern body) (node-args arm)
    (let ((ps (render-expr pattern 0))
          (bs (render-expr body 0)))
      (when (and ps bs) (format nil "~a => ~a" ps bs)))))

(defun value-promotable-brace-p (x)
  "Statement forms the parser would promote to a block value when they sit
last before `}` with no `;`: exactly these need their `;` printed back."
  (and (node-p x)
       (or (member (node-head x) +stmt-stopper-heads+)
           (and (eq (node-head x) :fn) (null (first (node-args x)))))))

(defun render-block-inline (block)
  (let* ((args (node-args block))
         (stmts (butlast args))
         (value (a:lastcar args))
         (parts '()))
    ;; Canonical layout: only zero-or-one-item blocks sit inline —
    ;; `{}`, `{ expr }`, `{ stmt; }` — unless forced.
    (when (and (not *force-inline*)
               (> (+ (length stmts) (if (null value) 0 1)) 1))
      (return-from render-block-inline nil))
    (loop for (s . more) on stmts
          do (a:if-let ((line (render-stmt-inline s)))
               (push (if (and (null more) (null value)
                              (value-promotable-brace-p s))
                         ;; the `;` that pins a trailing brace-form as a
                         ;; statement (SPEC §5.4) must survive printing
                         (concatenate 'string line ";")
                         line)
                     parts)
               (return-from render-block-inline nil)))
    (unless (null value)
      (a:if-let ((line (render-value-expr value)))
        (push line parts)
        (return-from render-block-inline nil)))
    (if (null parts)
        "{}"
        (format nil "{ ~{~a~^ ~} }" (nreverse parts)))))

(defun render-value-expr (value)
  "A block's trailing value as text — parenthesized when its leading token
would otherwise start a statement-stopper."
  (a:when-let ((s (render-expr value 0)))
    (if (and (expr-leading-stopper-p value)
             (not (member (node-head value) +stmt-stopper-heads+)))
        (concatenate 'string "(" s ")")
        s)))

;;; --- statement rendering (inline) ---------------------------------------------

(defun compound-op-text (op)
  (ecase op (:add "+=") (:sub "-=") (:mul "*=") (:div "/=") (:concat "++=")))

(defun render-stmt-inline (node)
  "One statement as a single line (with its `;` where the grammar wants one),
or NIL when it must break (named fn defs are always multiline, canonically)."
  (if (not (node-p node))
      (concatenate 'string (literal-string node) ";")
      (case (node-head node)
        ((:let :var)
         (destructuring-bind (name type value) (node-args node)
           (a:when-let ((vs (render-expr value 0)))
             (format nil "~(~a~) ~a~@[: ~a~] = ~a;"
                     (node-head node) (render-binder-name name)
                     (and type (render-expr type 0)) vs))))
        (:assign
         (destructuring-bind (target value) (node-args node)
           (a:when-let ((vs (render-expr value 0)))
             (format nil "~a = ~a;" (render-expr target 0) vs))))
        (:op_assign
         (destructuring-bind (op target value) (node-args node)
           (a:when-let ((vs (render-expr value 0)))
             (format nil "~a ~a ~a;" (render-expr target 0)
                     (compound-op-text op) vs))))
        (:return
         (let ((value (first (node-args node))))
           (if (null value)
               "return;"
               (a:when-let ((vs (render-expr value 0)))
                 (format nil "return ~a;" vs)))))
        (:fn
         (if (first (node-args node))
             nil                        ; named fn defs always break
             (render-expr node 0)))     ; lambda as expression statement
	        ((:macro_fn_def :macro_def) nil) ; macro defs always break
        ((:if :while :block :for_in :switch) (render-expr node 0))
        (t (a:when-let ((s (render-expr node 0)))
             (concatenate 'string
                          (if (expr-leading-stopper-p node)
                              (concatenate 'string "(" s ")")
                              s)
                          ";"))))))

;;; --- multiline emission ----------------------------------------------------------

(defun out-indent (stream indent)
  (dotimes (i indent) (write-char #\Space stream)))

(defun out-line (stream indent text)
  (out-indent stream indent)
  (write-string text stream)
  (terpri stream))

(defun emit-stmt (stream node indent &optional (suffix ""))
  (if (not (node-p node))
      ;; a bare scalar statement can never break — accept a long line
      (out-line stream indent
                (concatenate 'string (literal-string node) ";" suffix))
      (let ((line (render-stmt-inline node)))
        (if (and line (<= (+ indent (length line) (length suffix)) *print-width*))
            (out-line stream indent (concatenate 'string line suffix))
            (emit-stmt-broken stream node indent suffix)))))

(defun emit-stmt-broken (stream node indent suffix)
  (case (node-head node)
    ((:fn :macro_fn_def) (emit-fn stream node indent "" suffix))
    (:macro_def (emit-by-example-macro stream node indent suffix))
    (:if (emit-if-chain stream node indent "" suffix))
    ((:while :for_in) (emit-value-broken stream "" node suffix indent))
    (:block (emit-value-broken stream "" node suffix indent))
    (:switch (emit-switch stream node indent "" suffix))
    ((:let :var)
     (destructuring-bind (name type value) (node-args node)
       (emit-value-broken stream
                          (format nil "~(~a~) ~a~@[: ~a~] = "
                                  (node-head node)
                                  (symbol-name (ident-name name))
                                  (and type (render-expr type 0)))
                          value (concatenate 'string ";" suffix) indent)))
    (:assign
     (destructuring-bind (target value) (node-args node)
       (emit-value-broken stream (format nil "~a = " (render-expr target 0))
                          value (concatenate 'string ";" suffix) indent)))
    (:op_assign
     (destructuring-bind (op target value) (node-args node)
       (emit-value-broken stream
                          (format nil "~a ~a " (render-expr target 0)
                                  (compound-op-text op))
                          value (concatenate 'string ";" suffix) indent)))
    (:return
     (emit-value-broken stream "return " (first (node-args node))
                        (concatenate 'string ";" suffix) indent))
    (t
     ;; expression statement
     (if (expr-leading-stopper-p node)
         (emit-value-broken stream "(" node
                            (concatenate 'string ");" suffix) indent)
         (emit-value-broken stream "" node
                            (concatenate 'string
                                         (if (member (node-head node)
                                                     +brace-stmt-heads+)
                                             "" ";")
                                         suffix)
                            indent)))))

(defun emit-value-broken (stream prefix value suffix indent)
  "Emit PREFIX + VALUE + SUFFIX, breaking VALUE across lines when it has a
multiline form; otherwise one (possibly long) inline line — never a crash."
  (if (not (node-p value))
      (out-line stream indent
                (concatenate 'string prefix (literal-string value) suffix))
      (case (node-head value)
        (:if (emit-if-chain stream value indent prefix suffix))
        (:fn (emit-fn stream value indent prefix suffix))
        ((:while :for_in)
         (out-line stream indent
                   (format nil "~a~a {" prefix (render-loop-header value)))
         (emit-block-body stream (a:lastcar (node-args value))
                          (+ indent +indent-step+))
         (out-line stream indent (concatenate 'string "}" suffix)))
        (:block
         (out-line stream indent (concatenate 'string prefix "{"))
         (emit-block-body stream value (+ indent +indent-step+))
         (out-line stream indent (concatenate 'string "}" suffix)))
        (:quote
         (destructuring-bind (kind . body) (node-args value)
           (let ((qp (quote-kind-prefix kind)))
             (case kind
               ((:|stmt| :|stmts|)
                (out-line stream indent
                          (concatenate 'string prefix qp " {"))
                (dolist (s body)
                  (emit-stmt stream s (+ indent +indent-step+)))
                (out-line stream indent (concatenate 'string "}" suffix)))
               (:|block|
                (out-line stream indent
                          (concatenate 'string prefix qp " {"))
                (emit-block-body stream (first body) (+ indent +indent-step+))
                (out-line stream indent (concatenate 'string "}" suffix)))
               (t
                (emit-value-broken stream
                                   (concatenate 'string prefix qp " { ")
                                   (first body)
                                   (concatenate 'string " }" suffix)
                                   indent))))))
        (:switch (emit-switch stream value indent prefix suffix))
        (:pipe (if (pipe-chain-p value)
                   (emit-pipe-chain stream value indent prefix suffix)
                   (emit-plain-value stream value indent prefix suffix)))
        (:record_lit
         (if (node-args value)
             (emit-record-broken stream value indent prefix suffix)
             (out-line stream indent
                       (concatenate 'string prefix ".{}" suffix))))
        (:list_lit
         (if (node-args value)
             (emit-list-broken stream value indent prefix suffix)
             (out-line stream indent
                       (concatenate 'string prefix "[]" suffix))))
        (t (emit-plain-value stream value indent prefix suffix)))))

(defun emit-plain-value (stream value indent prefix suffix)
  "No multiline form exists: force an inline rendering, however long."
  (let ((inline (or (render-expr value 0)
                    (let ((*force-inline* t)) (render-expr value 0)))))
    (assert inline (value)
            "no rendering at all for head ~a" (node-head value))
    (out-line stream indent (concatenate 'string prefix inline suffix))))

(defun emit-if-chain (stream node indent prefix suffix)
  "if cond { ... } else if ... { ... } else { ... } — the canonical chain."
  (out-indent stream indent)
  (write-string prefix stream)
  (loop
    (destructuring-bind (c then else) (node-args node)
      (format stream "if ~a {~%" (render-cond c))
      (emit-block-body stream then (+ indent +indent-step+))
      (out-indent stream indent)
      (write-char #\} stream)
      (cond ((null else)
             (write-string suffix stream)
             (terpri stream)
             (return))
            ((eq (node-head else) :if)
             (write-string " else " stream)
             (setf node else))
            (t
             (format stream " else {~%")
             (emit-block-body stream else (+ indent +indent-step+))
             (out-indent stream indent)
             (write-char #\} stream)
             (write-string suffix stream)
             (terpri stream)
             (return))))))

(defun emit-fn (stream node indent prefix suffix)
  "fn defs, lambdas, and macro fn defs share one shape."
  (let* ((args (node-args node))
         (macro-p (eq (node-head node) :macro_fn_def))
         (name (first args))
         (body (a:lastcar args))
         (ret (a:lastcar (butlast args)))
         (params (subseq args 1 (- (length args) 2))))
    (out-line stream indent
              (format nil "~a~:[~;macro ~]fn~@[ ~a~](~{~a~^, ~})~@[ ~a~] {"
                      prefix macro-p
                      (and name (render-binder-name name))
                      (mapcar #'render-param params)
                      (and ret (if (keywordp ret)
                                   (symbol-name ret)
                                   (render-expr ret 0)))))
    (emit-block-body stream body (+ indent +indent-step+))
    (out-line stream indent (concatenate 'string "}" suffix))))

(defun emit-by-example-macro (stream node indent suffix)
  (destructuring-bind (name . arms) (node-args node)
    (out-line stream indent
              (format nil "macro ~a {" (render-binder-name name)))
    (dolist (arm arms)
      (destructuring-bind (pattern template) (node-args arm)
        (out-line stream (+ indent +indent-step+)
                  (format nil "{ ~a } => { ~a },"
                          (serialize-tokens (token-group-tokens pattern))
                          (serialize-tokens (token-group-tokens template))))))
    (out-line stream indent (concatenate 'string "}" suffix))))

(defun emit-switch (stream node indent prefix suffix)
  "Canonical switch: one arm per line; expression arms carry trailing
commas, }-terminated arms none (§5.6 comma rule, §5.7 layout)."
  (out-line stream indent
            (format nil "~aswitch ~a {" prefix
                    (render-cond (first (node-args node)))))
  (dolist (arm (rest (node-args node)))
    (emit-switch-arm stream arm (+ indent +indent-step+)))
  (out-line stream indent (concatenate 'string "}" suffix)))

(defun emit-switch-arm (stream arm indent)
  (destructuring-bind (pattern body) (node-args arm)
    (let* ((ps (or (render-expr pattern 0)
                   (let ((*force-inline* t)) (render-expr pattern 0))))
           (braced (and (node-p body)
                        (member (node-head body) +brace-stmt-heads+)))
           (line (a:when-let ((bs (render-expr body 0)))
                   (format nil "~a => ~a~:[,~;~]" ps bs braced))))
      (if (and line (<= (+ indent (length line)) *print-width*))
          (out-line stream indent line)
          (emit-value-broken stream (format nil "~a => " ps) body
                             (if braced "" ",") indent)))))

(defun emit-pipe-chain (stream node indent prefix suffix)
  "The canonical multi-stage pipeline: head expression on the first line,
one `|> stage` per line, indented one step (§10.1 layout)."
  (let ((stages '())
        (head node))
    (loop while (and (node-p head) (eq (node-head head) :pipe))
          do (push (second (node-args head)) stages)
             (setf head (first (node-args head))))
    (flet ((stage-text (s)
             (or (render-expr s 3)
                 (let ((*force-inline* t)) (render-expr s 3)))))
      (out-line stream indent
                (concatenate 'string prefix (stage-text head)))
      (loop for (s . more) on stages
            do (out-line stream (+ indent +indent-step+)
                         (concatenate 'string "|> " (stage-text s)
                                      (if more "" suffix)))))))

(defun emit-record-broken (stream node indent prefix suffix)
  (out-line stream indent (concatenate 'string prefix ".{"))
  (dolist (fi (node-args node))
    (destructuring-bind (name value) (node-args fi)
      (let ((line (a:when-let ((v (render-expr value 0)))
                    (format nil ".~a = ~a," (symbol-name name) v))))
        (if (and line (<= (+ indent +indent-step+ (length line)) *print-width*))
            (out-line stream (+ indent +indent-step+) line)
            (emit-value-broken stream (format nil ".~a = " (symbol-name name))
                               value "," (+ indent +indent-step+))))))
  (out-line stream indent (concatenate 'string "}" suffix)))

(defun emit-list-broken (stream node indent prefix suffix)
  (out-line stream indent (concatenate 'string prefix "["))
  (dolist (e (node-args node))
    (let ((line (a:when-let ((s (render-expr e 0)))
                  (concatenate 'string s ","))))
      (if (and line (<= (+ indent +indent-step+ (length line)) *print-width*))
          (out-line stream (+ indent +indent-step+) line)
          (emit-value-broken stream "" e "," (+ indent +indent-step+)))))
  (out-line stream indent (concatenate 'string "]" suffix)))

;;; --- blocks, blank-line preservation ----------------------------------------------

(defun tree-max-line (x)
  "Largest source line in the subtree, or NIL when nothing carries a span."
  (when (node-p x)
    (let ((m (node-meta x))
          (best nil))
      (when (and (meta-p m) (meta-line m) (not (meta-synthetic m)))
        (setf best (meta-line m)))
      (dolist (a (node-args x) best)
        (a:when-let ((sub (tree-max-line a)))
          (setf best (if best (max best sub) sub)))))))

(defun node-start-line (x)
  (and (node-p x)
       (let ((m (node-meta x)))
         (and (meta-p m) (not (meta-synthetic m)) (meta-line m)))))

(defun blank-line-between-p (prev cur)
  "Preserve one blank line where the source had one or more."
  (a:when-let ((prev-end (or (and (node-p prev)
                                  (meta-p (node-meta prev))
                                  (meta-end-line (node-meta prev)))
                             (tree-max-line prev)))
               (cur-start (node-start-line cur)))
    (>= (- cur-start prev-end) 2)))

(defun emit-block-body (stream block indent)
  (assert (eq (node-head block) :block) (block))
  (let* ((args (node-args block))
         (stmts (butlast args))
         (value (a:lastcar args))
         (prev nil))
    (loop for (s . more) on stmts
          do (when (and prev (blank-line-between-p prev s))
               (terpri stream))
             (emit-stmt stream s indent
                        ;; print back the `;` that keeps a trailing brace-form
                        ;; a statement rather than the block's value (§5.4)
                        (if (and (null more) (null value)
                                 (value-promotable-brace-p s))
                            ";" ""))
             (setf prev s))
    (unless (null value)
      (when (and prev (blank-line-between-p prev value))
        (terpri stream))
      (emit-value-stmt stream value indent))))

(defun emit-value-stmt (stream value indent)
  "A block's trailing value: an expression line with no `;`."
  (let ((line (and (or (not (node-p value))
                       (not (member (node-head value) +stmt-only-heads+)))
                   (render-value-expr value))))
    (if (and line (<= (+ indent (length line)) *print-width*))
        (out-line stream indent line)
        (if (and (node-p value)
                 (expr-leading-stopper-p value)
                 (not (member (node-head value) +stmt-stopper-heads+)))
            (emit-value-broken stream "(" value ")" indent)
            (emit-value-broken stream "" value "" indent)))))

;;; --- modules -------------------------------------------------------------------

(defun named-def-p (x)
  (and (node-p x)
       (member (node-head x) '(:fn :macro_fn_def :macro_def))
       (first (node-args x))))

(defun print-module (stmts)
  "Render a whole module canonically. `sput fmt` is parse ∘ print-module."
  (with-output-to-string (s)
    (let ((prev nil))
      (dolist (stmt stmts)
        (when prev
          (when (or (blank-line-between-p prev stmt)
                    ;; synthetic output: keep defs visually separated
                    (and (not (and (tree-max-line prev) (node-start-line stmt)))
                         (or (named-def-p prev) (named-def-p stmt))))
            (terpri s)))
        (emit-stmt s stmt 0)
        (setf prev stmt)))))

(defun print-node (x)
  "Render one node (or scalar) as canonical surface syntax (SPEC §5.7 print)."
  (string-right-trim
   '(#\Newline)
   (with-output-to-string (s)
     (if (and (node-p x)
              (or (member (node-head x) +stmt-only-heads+)
                  (named-def-p x)))
         (emit-stmt s x 0)
         (let ((line (render-expr x 0)))
           (if (and line (<= (length line) *print-width*))
               (write-string line s)
               (emit-value-broken s "" x "" 0)))))))

;;; --- dump: nodes as Sputter data literals (SPEC §5.7) ---------------------------
;;; dump(node) renders the node in the language's own record/list syntax —
;;; the output is a valid Sputter expression. Nodes whose args are all
;;; scalars sit on one line; anything deeper breaks its args one per line.

(defun dump-string (x &optional (indent 0))
  (cond
    ;; a macro-call payload has no data-literal form: dump its source text
    ;; (round-trips through the reader as a string, honest about being raw)
    ((token-group-p x)
     (escape-string-literal (serialize-tokens (token-group-tokens x))))
    ((not (node-p x)) (literal-string x))
    ((every (lambda (a) (not (node-p a))) (node-args x))
     (format nil ".{ .head = .~a, .meta = ~a, .args = [~{~a~^, ~}] }"
             (string-downcase (symbol-name (node-head x)))
             (dump-meta-string (node-meta x))
             (mapcar (lambda (a) (dump-string a)) (node-args x))))
    (t
     (let* ((inner (+ indent +indent-step+))
            (pad (make-string inner :initial-element #\Space)))
       (format nil ".{ .head = .~a, .meta = ~a, .args = [~%~{~a~%~}~a]}"
               (string-downcase (symbol-name (node-head x)))
               (dump-meta-string (node-meta x))
               (mapcar (lambda (a)
                         (concatenate 'string pad (dump-string a inner) ","))
                       (node-args x))
               (make-string indent :initial-element #\Space))))))

(defun dump-meta-string (m)
  (format nil ".{ .file = ~a, .line = ~a, .col = ~a, .scopes = [~{~a~^, ~}], .synthetic = ~a }"
          (if (meta-file m) (escape-string-literal (meta-file m)) "nil")
          (or (meta-line m) "nil")
          (or (meta-col m) "nil")
          (mapcar #'literal-string (meta-scopes m))
          (if (meta-synthetic m) "true" "false")))

;;; --- show: runtime values as Sputter literals (SPEC §5.7) ---------------------

(defun show-value (v)
  "Runtime values rendered as Sputter literals; the REPL echoes through this."
  (cond ((eq v t) "true")
        ((null v) "nil")
        ((sput-false-p v) "false")
        ((integerp v) (format nil "~d" v))
        ((floatp v) (float-literal-string v))
        ((stringp v) (escape-string-literal v))
        ((keywordp v) (concatenate 'string "." (symbol-name v)))
        ((consp v) (format nil "[~{~a~^, ~}]" (mapcar #'show-value v)))
        ((tagged-p v)
         (format nil ".~a(~{~a~^, ~})" (symbol-name (tagged-tag v))
                 (map 'list #'show-value (tagged-vals v))))
        ((record-p v)
         (let ((keys (record-keys v)))
           (if keys
               (format nil ".{ ~{~a~^, ~} }"
                       (mapcar (lambda (k)
                                 (format nil ".~a = ~a" (symbol-name k)
                                         (show-value (record-ref v k))))
                               keys))
               ".{}")))
        ((functionp v) (show-function v))
        ((node-p v)
         (format nil "<node ~(~a~)~@[ ~a~]>"
                 (node-head v) (meta-span-string (node-meta v))))
        ((token-group-p v)
         (format nil "<raw tokens ~a>"
                 (serialize-tokens (token-group-tokens v))))
        (t "<host value>")))

(defun show-function (f)
  (multiple-value-bind (name arity) (host-function-info f)
    (format nil "<fn ~a~@[/~d~]>" (or name "anon") arity)))
