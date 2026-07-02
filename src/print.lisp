;;;; print.lisp — print / dump / show (SPEC §5.7). M1 ships `print`:
;;;; the canonical surface pretty-printer. Contract: parse(print(n)) ≡ n
;;;; modulo meta; minimal parens (I8); canonical layout (4-space indent).
;;;; dump/show arrive with M3/M4.

(in-package #:sputter.impl)

(defparameter *print-width* 100
  "Canonical line-width budget; constructs that fit stay inline.")

(defconstant +indent-step+ 4)

;;; --- literals ---------------------------------------------------------------

(defun literal-string (x)
  "Render a scalar as a Sputter literal."
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

;;; --- expression rendering (inline) -------------------------------------------
;;; render-expr returns the unbroken string, or NIL when the node must break
;;; (switch and long pipe chains, from M3 on). REQ is the binding tightness
;;; the context requires; parens appear exactly where the tree demands (I8).

(defparameter +binop-info+
  ;; head -> (text tightness), tightest 9 (postfix) … loosest 2 (|>)
  '((:pipe "|>" 2) (:or "or" 3) (:and "and" 4)
    (:eq "==" 5) (:ne "!=" 5) (:lt "<" 5) (:le "<=" 5) (:gt ">" 5) (:ge ">=" 5)
    (:add "+" 6) (:sub "-" 6) (:concat "++" 6)
    (:mul "*" 7) (:div "/" 7) (:rem "%" 7)))

(defparameter +cmp-heads+ '(:eq :ne :lt :le :gt :ge))

(defun binop-info (head) (assoc head +binop-info+))

(defparameter +brace-stmt-heads+ '(:if :while :block :fn :switch :for_in)
  "Heads whose statement form is }-terminated and takes no trailing `;`.")

(defun maybe-paren (s own req)
  (if (< own req) (concatenate 'string "(" s ")") s))

(defun join-strings (parts sep)
  (when (every #'identity parts)
    (format nil (format nil "~~{~~a~~^~a~~}" sep) parts)))

(defun render-expr (x req)
  (cond
    ((not (node-p x)) (literal-string x))
    (t
     (let ((head (node-head x))
           (args (node-args x)))
       (a:if-let ((info (binop-info head)))
         (destructuring-bind (op-text own) (rest info)
           (declare (ignorable op-text))
           (let* ((cmp (member head +cmp-heads+))
                  (lhs (render-expr (first args) (if cmp (1+ own) own)))
                  (rhs (render-expr (second args) (1+ own))))
             (when (and lhs rhs)
               (maybe-paren (format nil "~a ~a ~a" lhs (second info) rhs)
                            own req))))
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
              (maybe-paren (format nil "~a.~a" obj (symbol-name (second args)))
                           9 req)))
           (:block (render-block-inline x))
           (:if (render-if-inline x))
           (:while
            (let ((c (render-cond (first args)))
                  (b (render-block-inline (second args))))
              (when (and c b) (format nil "while ~a ~a" c b))))
           (:fn (render-fn-inline x))
           (:unreachable "unreachable")
           (:param (render-param x))
           (:type_ident (symbol-name (first args)))
           ((:let :var :assign :op_assign :return)
            (assert nil (x) "statement head ~a reached expression rendering" head))
           (t (assert nil (x) "printer got a head it cannot render: ~a" head))))))))

(defun render-cond (c)
  "Paren-free condition position: guard expressions whose *text* would start
with `{` (the parser gives braces there to the body)."
  (a:when-let ((s (render-expr c 0)))
    (if (starts-with-lbrace-p c)
        (concatenate 'string "(" s ")")
        s)))

(defun starts-with-lbrace-p (x)
  (and (node-p x)
       (let ((head (node-head x)) (args (node-args x)))
         (cond ((eq head :block) t)
               ((binop-info head) (starts-with-lbrace-p (first args)))
               ((member head '(:call :index :field))
                (starts-with-lbrace-p (first args)))
               (t nil)))))

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
    (let ((ps (join-strings (mapcar #'render-param params) ", "))
          (bs (render-block-inline body)))
      (when (and ps bs)
        (format nil "fn~@[ ~a~](~a)~@[ ~a~] ~a"
                (and name (symbol-name (ident-name name)))
                ps
                (and ret (render-expr ret 0))
                bs)))))

(defun render-param (param)
  (destructuring-bind (name type) (node-args param)
    (if type
        (format nil "~a: ~a" (symbol-name (ident-name name)) (render-expr type 0))
        (symbol-name (ident-name name)))))

(defun render-block-inline (block)
  (let* ((args (node-args block))
         (stmts (butlast args))
         (value (a:lastcar args))
         (parts '()))
    ;; Canonical layout: only zero-or-one-item blocks may sit inline —
    ;; `{}`, `{ expr }`, `{ stmt; }`. Anything larger breaks.
    (when (> (+ (length stmts) (if (null value) 0 1)) 1)
      (return-from render-block-inline nil))
    (dolist (s stmts)
      (a:if-let ((line (render-stmt-inline s)))
        (push line parts)
        (return-from render-block-inline nil)))
    (unless (null value)
      (a:if-let ((line (render-expr value 0)))
        (push line parts)
        (return-from render-block-inline nil)))
    (if (null parts)
        "{}"
        (format nil "{ ~{~a~^ ~} }" (nreverse parts)))))

;;; --- statement rendering (inline) ---------------------------------------------

(defun compound-op-text (op)
  (ecase op (:add "+=") (:sub "-=") (:mul "*=") (:div "/=") (:concat "++=")))

(defun render-stmt-inline (node)
  "One statement as a single line (with its `;` where the grammar wants one),
or NIL when it must break (named fn defs are always multiline, canonically)."
  (if (not (node-p node))
      ;; a bare scalar as an expression statement (rare, synthetic)
      (concatenate 'string (literal-string node) ";")
      (case (node-head node)
        ((:let :var)
         (destructuring-bind (name type value) (node-args node)
           (a:when-let ((vs (render-expr value 0)))
             (format nil "~(~a~) ~a~@[: ~a~] = ~a;"
                     (node-head node) (symbol-name (ident-name name))
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
        ((:if :while :block) (render-expr node 0))
        (t (a:when-let ((s (render-expr node 0)))
             (concatenate 'string s ";"))))))

;;; --- multiline emission ----------------------------------------------------------

(defun out-indent (stream indent)
  (dotimes (i indent) (write-char #\Space stream)))

(defun out-line (stream indent text)
  (out-indent stream indent)
  (write-string text stream)
  (terpri stream))

(defun emit-stmt (stream node indent)
  (let ((line (render-stmt-inline node)))
    (if (and line (<= (+ indent (length line)) *print-width*))
        (out-line stream indent line)
        (emit-stmt-broken stream node indent))))

(defun emit-stmt-broken (stream node indent)
  (assert (node-p node) (node) "cannot break a scalar statement")
  (case (node-head node)
    (:fn (emit-fn stream node indent "" ""))
    (:if (emit-if-chain stream node indent "" ""))
    (:while (emit-value-broken stream "" node "" indent))
    (:block (emit-value-broken stream "" node "" indent))
    ((:let :var)
     (destructuring-bind (name type value) (node-args node)
       (emit-value-broken stream
                          (format nil "~(~a~) ~a~@[: ~a~] = "
                                  (node-head node)
                                  (symbol-name (ident-name name))
                                  (and type (render-expr type 0)))
                          value ";" indent)))
    (:assign
     (destructuring-bind (target value) (node-args node)
       (emit-value-broken stream (format nil "~a = " (render-expr target 0))
                          value ";" indent)))
    (:op_assign
     (destructuring-bind (op target value) (node-args node)
       (emit-value-broken stream
                          (format nil "~a ~a " (render-expr target 0)
                                  (compound-op-text op))
                          value ";" indent)))
    (:return
     (emit-value-broken stream "return " (first (node-args node)) ";" indent))
    (t
     (emit-value-broken stream "" node
                        (if (member (node-head node) +brace-stmt-heads+) "" ";")
                        indent))))

(defun emit-value-broken (stream prefix value suffix indent)
  "Emit PREFIX + VALUE + SUFFIX, breaking VALUE across lines when it has a
multiline form; otherwise accept one long line (binary chains do not wrap)."
  (if (not (node-p value))
      (out-line stream indent
                (concatenate 'string prefix (literal-string value) suffix))
      (case (node-head value)
        (:if (emit-if-chain stream value indent prefix suffix))
        (:fn (emit-fn stream value indent prefix suffix))
        (:while
         (destructuring-bind (c body) (node-args value)
           (out-line stream indent
                     (format nil "~awhile ~a {" prefix (render-cond c)))
           (emit-block-body stream body (+ indent +indent-step+))
           (out-line stream indent (concatenate 'string "}" suffix))))
        (:block
         (out-line stream indent (concatenate 'string prefix "{"))
         (emit-block-body stream value (+ indent +indent-step+))
         (out-line stream indent (concatenate 'string "}" suffix)))
        (t
         (let ((inline (render-expr value 0)))
           (assert inline (value)
                   "no multiline form for head ~a" (node-head value))
           (out-line stream indent
                     (concatenate 'string prefix inline suffix)))))))

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
  (let* ((args (node-args node))
         (name (first args))
         (body (a:lastcar args))
         (ret (a:lastcar (butlast args)))
         (params (subseq args 1 (- (length args) 2))))
    (out-line stream indent
              (format nil "~afn~@[ ~a~](~{~a~^, ~})~@[ ~a~] {"
                      prefix
                      (and name (symbol-name (ident-name name)))
                      (mapcar #'render-param params)
                      (and ret (render-expr ret 0))))
    (emit-block-body stream body (+ indent +indent-step+))
    (out-line stream indent (concatenate 'string "}" suffix))))

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
  (a:when-let ((prev-end (tree-max-line prev))
               (cur-start (node-start-line cur)))
    (>= (- cur-start prev-end) 2)))

(defun emit-block-body (stream block indent)
  (assert (eq (node-head block) :block) (block))
  (let* ((args (node-args block))
         (stmts (butlast args))
         (value (a:lastcar args))
         (prev nil))
    (dolist (s stmts)
      (when (and prev (blank-line-between-p prev s))
        (terpri stream))
      (emit-stmt stream s indent)
      (setf prev s))
    (unless (null value)
      (when (and prev (blank-line-between-p prev value))
        (terpri stream))
      (emit-value-stmt stream value indent))))

(defun emit-value-stmt (stream value indent)
  "A block's trailing value: an expression line with no `;`."
  (let ((line (and (or (not (node-p value))
                       (not (member (node-head value) '(:let :var :assign :op_assign :return))))
                   (render-expr value 0))))
    (if (and line (<= (+ indent (length line)) *print-width*))
        (out-line stream indent line)
        (emit-value-broken stream "" value "" indent))))

;;; --- modules -------------------------------------------------------------------

(defun named-def-p (x)
  (and (node-p x)
       (eq (node-head x) :fn)
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

;;; --- show: runtime values as Sputter literals (SPEC §5.7) ---------------------

(defun show-value (v)
  "Runtime values rendered as Sputter literals; the REPL echoes through this.
Records and tagged values complete the picture in M3."
  (cond ((eq v t) "true")
        ((null v) "nil")
        ((sput-false-p v) "false")
        ((integerp v) (format nil "~d" v))
        ((floatp v) (float-literal-string v))
        ((stringp v) (escape-string-literal v))
        ((keywordp v) (concatenate 'string "." (symbol-name v)))
        ((consp v) (format nil "[~{~a~^, ~}]" (mapcar #'show-value v)))
        ((functionp v) (show-function v))
        ((node-p v)
         (format nil "<node ~(~a~)~@[ ~a~]>"
                 (node-head v) (meta-span-string (node-meta v))))
        (t "<host value>")))

(defun show-function (f)
  (multiple-value-bind (name arity) (host-function-info f)
    (format nil "<fn ~a~@[/~d~]>" (or name "anon") arity)))

(defun print-node (x)
  "Render one node (or scalar) as canonical surface syntax (SPEC §5.7 print)."
  (string-right-trim '(#\Newline)
                     (with-output-to-string (s)
                       (if (and (node-p x)
                                (member (node-head x)
                                        '(:let :var :assign :op_assign :return)))
                           (emit-stmt s x 0)
                           (let ((line (render-expr x 0)))
                             (if (and line (<= (length line) *print-width*))
                                 (write-string line s)
                                 (emit-value-broken s "" x "" 0)))))))
