;;;; plasma.lisp — the core IR (SPEC §6): definition, validators, and the
;;;; lowerer from expanded surface nodes to Plasma.
;;;;
;;;; Plasma is the closed set of forms that exists after expansion and
;;;; desugaring. Nothing above the lowerer knows CL exists; nothing below it
;;;; knows macros existed.
;;;;
;;;; Plasma node shapes (args layouts):
;;;;   (:p.lit       [scalar])
;;;;   (:p.ref       [name kind])          kind: :local | :global-fn | :global-var
;;;;   (:p.call      [callee arg*])
;;;;   (:p.host_call [cl-symbol arg*])     the only form that names the host
;;;;   (:p.fn        [name-or-nil p.param* ret-type-kw-or-nil body])
;;;;   (:p.param     [name type-kw-or-nil]) (structural child of p.fn)
;;;;   (:p.if        [cond then else])
;;;;   (:p.let       [name type-kw-or-nil init])  (statement inside p.block;
;;;;                                               at top level: a global def)
;;;;   (:p.assign    [name kind value])            kind: :local | :global
;;;;                                               (globals live in a value
;;;;                                                table, never symbol cells)
;;;;   (:p.block     [stmt* value])
;;;;   (:p.while     [cond body])
;;;;   (:p.return    [value])
;;;;   (:p.and/:p.or [a b])
;;;;   (:p.panic     [message-expr])       span rides in meta
;;;;   (:p.field     [obj name])  (:p.index [obj idx])
;;;;   (:p.match :p.list :p.record :p.tagged arrive with M3)

(in-package #:sputter.impl)

(defparameter +plasma-heads+
  '(:p.lit :p.ref :p.call :p.host_call :p.fn :p.param :p.if :p.let :p.assign
    :p.block :p.match :p.arm :p.list :p.record :p.tagged :p.field :p.index
    :p.while :p.return :p.and :p.or :p.panic)
  "The closed Plasma head set (SPEC §6; p.param and p.arm are structural
additions — children of p.fn/p.match, not expression forms; DECISIONS.md).")

(defun validate-plasma (x)
  "Negative space (SPEC §6): the emitter's input contains Plasma heads only —
no surface heads, no macro-space heads. Exception: p.match keeps structured
*patterns* (SPEC §6 note) — a p.arm's first arg is surface-pattern shaped and
is validated by the emitter's pattern translator instead."
  (cond
    ((not (node-p x)) x)
    ;; a p.lit's payload is a literal *value* — possibly a node-as-data
    ;; (quote lowering embeds quoted subtrees literally); never IR
    ((eq (node-head x) :p.lit) x)
    ((eq (node-head x) :p.match)
     (validate-plasma (first (node-args x)))
     (dolist (arm (rest (node-args x)) x)
       (assert (eq (node-head arm) :p.arm) ()
               "p.match children must be p.arm, got ~a" (node-head arm))
       (validate-plasma (second (node-args arm)))))
    (t
     (assert (member (node-head x) +plasma-heads+) ()
             "non-Plasma head ~a reached the emitter" (node-head x))
     (dolist (a (node-args x) x)
       (validate-plasma a)))))

;;; --- globals (Lisp-1 on Lisp-2, SPEC §7) ---------------------------------------

(defvar *globals* (make-hash-table :test 'eq)
  "name-keyword -> (:fn . cl-symbol) | (:var . mutable-p).
Tracks which idents are known globals so the lowerer can compile direct
calls vs funcalls. Reset per `sput run`/`sput repl` session.")

(defun global-entry (name) (gethash name *globals*))

(defun register-global-fn (name cl-symbol)
  (setf (gethash name *globals*) (cons :fn cl-symbol)))

(defun register-global-var (name mutable)
  (setf (gethash name *globals*) (cons :var mutable)))

;;; --- lowering environment -------------------------------------------------------
;;; Bindings carry the binder's hygiene scope set (SPEC §5.8.5). Resolution
;;; rule (rename-based v0.1): a reference resolves to a local binding only
;;; when the binding's scope set EXACTLY equals the reference's — template
;;; refs skip user binders (falling through to the definition environment's
;;; globals), user refs skip template binders. Marked binders additionally
;;; get fresh emit names so CL's lexical scoping can never conflate them.

(defstruct (lbind (:constructor make-lbind (name kind scopes)))
  (name nil :type keyword)
  (kind nil :type keyword)              ; :let | :var | :param
  (scopes '() :type list))

(defstruct (lenv (:constructor %make-lenv (bindings fn-name)))
  (bindings '() :type list)             ; innermost lbind first
  ;; innermost fn name keyword (or nil at top level) — return target
  (fn-name nil))

(defun scope-set-equal (a b)
  (and (subsetp a b) (subsetp b a)))

(defun binder-scopes (ident-node)
  (let ((m (node-meta ident-node)))
    (and (meta-p m) (meta-scopes m))))

(defun ref-scopes (node)
  (let ((m (and (node-p node) (node-meta node))))
    (and (meta-p m) (meta-scopes m))))

(defun lenv-extend (env name kind &optional scopes)
  (%make-lenv (cons (make-lbind name kind scopes)
                    (lenv-bindings env))
              (lenv-fn-name env)))

(defun lenv-lookup (env name &optional scopes)
  "The innermost binding of NAME whose scope set matches exactly, or NIL.
Template binders are already renamed fresh by the expander; the exact-match
rule handles the remaining direction — marked (template-origin) references
skip unmarked user binders and reach the definition environment's globals."
  (find-if (lambda (b) (and (eq (lbind-name b) name)
                            (scope-set-equal (lbind-scopes b) scopes)))
           (lenv-bindings env)))

(defun top-lenv () (%make-lenv '() nil))

(defun lower-error (node fmt &rest args)
  (let ((m (and (node-p node) (node-meta node))))
    (apply #'sputter-error-at 'sputter-lower-error
           (and m (meta-file m)) (and m (meta-line m)) (and m (meta-col m))
           fmt args)))

(defun sputter-warn (meta fmt &rest args)
  (format *error-output* "warning: ~a~@[~%  at ~a~]~%"
          (apply #'format nil fmt args)
          (and (meta-p meta) (meta-span-string meta))))

;;; --- types (SPEC §7 table) -------------------------------------------------------

(defparameter +known-type-names+
  '(:|i32| :|i64| :|u32| :|u64| :|int| :|f32| :|f64| :|bool| :|str| :|atom|
    :|list| :|record| :|node| :|any|))

(defun lower-type (type-node)
  "type_ident node -> type keyword; unknown names warn and become :|any|
(SPEC §13.16)."
  (when type-node
    (let ((name (first (node-args type-node))))
      (if (member name +known-type-names+)
          name
          (progn
            (sputter-warn (node-meta type-node)
                          "unknown type `~a` (treated as any)" (symbol-name name))
            :|any|)))))

;;; --- operator lowering table (SPEC §7) --------------------------------------------

(defparameter +op-builtins+
  '((:add . sput-add) (:sub . sput-sub) (:mul . sput-mul) (:div . sput-div)
    (:rem . sput-rem) (:concat . sput-concat)
    (:eq . sput-eq) (:ne . sput-ne)
    (:lt . sput-lt) (:le . sput-le) (:gt . sput-gt) (:ge . sput-ge)
    (:not . sput-not) (:neg . sput-neg)))

;;; --- the lowerer ---------------------------------------------------------------------

(defun pl (head node args)
  "Plasma node constructor: preserves the surface node's meta for spans."
  (make-node head args :meta (if (node-p node) (node-meta node) (synthetic-meta))))

(defun lower-top-form (node)
  "Lower one expanded top-level form. Returns a list of Plasma forms
(top-level fn defs and statements are single; kept a list for uniformity)."
  (let ((env (top-lenv)))
    (cond
      ((not (node-p node)) (list (pl :p.lit node (list node))))
      (t
       (case (node-head node)
         (:fn
          (if (first (node-args node))
              (list (lower-named-fn node env))
              (list (lower-expr node env))))
         ((:let :var)
          (destructuring-bind (name type value) (node-args node)
            (let* ((name-kw (ident-name name))
                   (init (lower-expr value env))
                   (type-kw (lower-type type)))
              ;; register after lowering the init: `let x = x;` must not
              ;; resolve to itself
              (register-global-var name-kw (eq (node-head node) :var))
              (list (pl :p.let node (list name-kw type-kw init))))))
         ((:assign :op_assign) (list (lower-stmt node env)))
         (:return (lower-error node "`return` outside a function"))
         (t (list (lower-expr node env))))))))

(defun lower-named-fn (node env)
  "Named fn def: register the global first (self-recursion), then lower."
  (let ((name-kw (ident-name (first (node-args node)))))
    (register-global-fn name-kw nil)    ; cl-symbol filled by the emitter
    (lower-fn node env :name name-kw)))

(defun lower-fn (node env &key name)
  (let* ((args (node-args node))
         (body (a:lastcar args))
         (ret (a:lastcar (butlast args)))
         (params (subseq args 1 (- (length args) 2)))
         (fn-env (%make-lenv (lenv-bindings env) (or name :|%sput-fn|)))
         (p-params '()))
    (dolist (param params)
      (destructuring-bind (pname ptype) (node-args param)
        (let ((pname-kw (ident-name pname)))
          ;; shadowing outer scopes is fine; duplicate params are not
          (when (member pname-kw (mapcar (lambda (p) (first (node-args p)))
                                         p-params))
            (lower-error param "duplicate parameter `~a`" (symbol-name pname-kw)))
          (setf fn-env (lenv-extend fn-env pname-kw :param
                                    (binder-scopes pname)))
          (push (pl :p.param param (list pname-kw (lower-type ptype))) p-params))))
    (pl :p.fn node
        (append (list name)
                (nreverse p-params)
                (list (lower-type ret) (lower-block body fn-env))))))

(defun lower-block (block env)
  (assert (eq (node-head block) :block) (block))
  (let* ((args (node-args block))
         (stmts (butlast args))
         (value (a:lastcar args))
         (p-stmts '()))
    (dolist (s stmts)
      (multiple-value-bind (plasma-stmts new-env) (lower-stmt-seq s env)
        (setf env new-env)
        (dolist (p plasma-stmts) (push p p-stmts))))
    (pl :p.block block
        (append (nreverse p-stmts) (list (lower-expr value env))))))

(defun lower-stmt-seq (node env)
  "Lower one in-block statement. Returns (values plasma-stmt-list new-env) —
bindings extend the environment for the statements that follow."
  (if (not (node-p node))
      (values (list (pl :p.lit node (list node))) env)
      (case (node-head node)
        ((:let :var)
         (destructuring-bind (name type value) (node-args node)
           (let* ((name-kw (ident-name name))
                  (init (lower-expr value env))
                  (kind (if (eq (node-head node) :var) :var :let)))
             (values (list (pl :p.let node (list name-kw (lower-type type) init)))
                     (lenv-extend env name-kw kind (binder-scopes name))))))
        (:fn
         (if (first (node-args node))
             ;; local named fn: bind the name first so the body can recurse,
             ;; then assign (emits as let f = nil; f = fn...)
             (let* ((name-ident (first (node-args node)))
                    (name-kw (ident-name name-ident))
                    (new-env (lenv-extend env name-kw :let
                                          (binder-scopes name-ident)))
                    (p-fn (lower-fn node new-env :name name-kw)))
               (values (list (pl :p.let node (list name-kw nil (pl :p.lit node (list nil))))
                             (pl :p.assign node (list name-kw :local p-fn)))
                       new-env))
             (values (list (lower-expr node env)) env)))
        ((:macro_fn_def :macro_def)
         (lower-error node "macro definitions are top-level forms (SPEC §5.2)"))
        (t (values (list (lower-stmt node env)) env)))))

(defun lower-stmt (node env)
  "Statements that don't extend the environment."
  (case (node-head node)
    (:assign
     (destructuring-bind (target value) (node-args node)
       (lower-assign node target (lower-expr value env) env)))
    (:op_assign
     (destructuring-bind (op target value) (node-args node)
       (lower-assign node target
                     (pl :p.host_call node
                         (list (cdr (assoc op +op-builtins+))
                               (lower-ident-ref target (ident-name target) env)
                               (lower-expr value env)))
                     env)))
    (:return
     (unless (lenv-fn-name env)
       (lower-error node "`return` outside a function"))
     (pl :p.return node (list (lower-expr (first (node-args node)) env))))
    (t (lower-expr node env))))

(defun lower-assign (node target value env)
  (let* ((name-kw (ident-name target))
         (binding (lenv-lookup env name-kw (ref-scopes target)))
         (local (and binding (lbind-kind binding))))
    (case local
      (:var (pl :p.assign node (list name-kw :local value)))
      ((:let :param)
       (lower-error node
                    "cannot reassign `~a` (it is immutable — bind it with `var` to mutate)"
                    (symbol-name name-kw)))
      (t
       (let ((global (global-entry name-kw)))
         (cond
           ((and global (eq (car global) :var) (cdr global))
            (pl :p.assign node (list name-kw :global value)))
           ((and global (eq (car global) :var))
            (lower-error node
                         "cannot reassign `~a` (it is immutable — bind it with `var` to mutate)"
                         (symbol-name name-kw)))
           ((and global (eq (car global) :fn))
            (lower-error node "cannot assign to the function `~a`"
                         (symbol-name name-kw)))
           (t (lower-error node "undefined name `~a`" (symbol-name name-kw)))))))))

(defun lower-ident-ref (node name-kw env &key callee)
  (cond
    ((lenv-lookup env name-kw (ref-scopes node))
     (pl :p.ref node (list name-kw :local)))
    (t (let ((global (global-entry name-kw)))
         (cond
           ((and global (eq (car global) :fn))
            (pl :p.ref node (list name-kw :global-fn)))
           ((and global (eq (car global) :var))
            (pl :p.ref node (list name-kw :global-var)))
           ;; callee position: late-bound global call, CL-style — this is
           ;; what makes mutual recursion work under define-before-use
           (callee (pl :p.ref node (list name-kw :global-fn)))
           (t (lower-error node "undefined name `~a`" (symbol-name name-kw))))))))

(defun lower-expr (node env)
  (cond
    ((not (node-p node)) (pl :p.lit node (list node)))
    (t
     (let ((head (node-head node))
           (args (node-args node)))
       (a:if-let ((builtin (cdr (assoc head +op-builtins+))))
         (pl :p.host_call node
             (cons builtin (mapcar (lambda (e) (lower-expr e env)) args)))
         (case head
           (:ident (lower-ident-ref node (ident-name node) env))
           (:and (pl :p.and node (list (lower-expr (first args) env)
                                       (lower-expr (second args) env))))
           (:or (pl :p.or node (list (lower-expr (first args) env)
                                     (lower-expr (second args) env))))
           (:call
            (let ((callee (first args))
                  (call-args (mapcar (lambda (e) (lower-expr e env)) (rest args))))
              (pl :p.call node
                  (cons (if (ident-node-p callee)
                            (lower-ident-ref callee (ident-name callee) env
                                             :callee t)
                            (lower-expr callee env))
                        call-args))))
           (:field (pl :p.field node (list (lower-expr (first args) env)
                                           (second args))))
           (:index (pl :p.index node (list (lower-expr (first args) env)
                                           (lower-expr (second args) env))))
           (:if
            (destructuring-bind (c then else) args
              (pl :p.if node (list (lower-expr c env)
                                   (lower-block then env)
                                   (if else
                                       (if (eq (node-head else) :block)
                                           (lower-block else env)
                                           (lower-expr else env))
                                       (pl :p.lit node (list nil)))))))
           (:block (lower-block node env))
           (:while
            (destructuring-bind (c body) args
              (pl :p.while node (list (lower-expr c env) (lower-block body env)))))
           (:fn (lower-fn node env :name nil))
           (:unreachable
            (pl :p.panic node (list (pl :p.lit node (list "reached unreachable code")))))
           (:pipe
            ;; Elixir insert-first desugar (SPEC §5.3): x |> f(a) -> f(x, a);
            ;; x |> f -> f(x). Done here, not the parser — macros see .pipe.
            (destructuring-bind (lhs rhs) args
              (lower-expr
               (if (and (node-p rhs) (eq (node-head rhs) :call))
                   (make-node :call
                              (list* (first (node-args rhs)) lhs
                                     (rest (node-args rhs)))
                              :meta (node-meta rhs))
                   (make-node :call (list rhs lhs) :meta (node-meta node)))
               env)))
           (:tagged_lit
            (pl :p.tagged node
                (cons (first args)
                      (mapcar (lambda (e) (lower-expr e env)) (rest args)))))
           (:record_lit (lower-record node env))
           (:list_lit (lower-list node env))
           (:switch (lower-switch node env))
           (:for_in (lower-for node env))
           (:quote (lower-quote node env))
           ((:let :var :assign :op_assign :return)
            (lower-error node "~(~a~) is a statement, not an expression" head))
           ((:macro_fn_def :macro_def)
            (lower-error node "macro definitions are top-level forms (SPEC §5.2)"))
           ((:macro_call :macro_arm :raw :inject :insert :hole :splice_seq)
            (assert nil (node)
                    "macro-space head ~a reached the lowerer outside a quote" head))
           (t (assert nil (node)
                      "lowerer got a head it does not know: ~a" head))))))))

;;; --- data literals ---------------------------------------------------------------

(defun lower-record (node env)
  (let ((kvs '())
        (seen '()))
    (dolist (fi (node-args node))
      (destructuring-bind (k v) (node-args fi)
        (when (member k seen)
          (lower-error fi "duplicate field .~a in record literal" (symbol-name k)))
        (push k seen)
        (push k kvs)
        (push (lower-expr v env) kvs)))
    (pl :p.record node (nreverse kvs))))

(defun lower-list (node env)
  (let ((elems (node-args node)))
    (if (notany (lambda (e) (and (node-p e) (eq (node-head e) :spread))) elems)
        (pl :p.list node (mapcar (lambda (e) (lower-expr e env)) elems))
        ;; spreads: split into literal runs and spread segments, append them
        (let ((segments '())
              (run '()))
          (flet ((flush-run ()
                   (when run
                     (push (pl :p.list node (nreverse run)) segments)
                     (setf run '()))))
            (dolist (e elems)
              (if (and (node-p e) (eq (node-head e) :spread))
                  (progn (flush-run)
                         (push (lower-expr (first (node-args e)) env) segments))
                  (push (lower-expr e env) run)))
            (flush-run))
          (pl :p.host_call node
              (cons 'sput-list-append (nreverse segments)))))))

;;; --- switch (SPEC §5.6) ------------------------------------------------------------

(defun pattern-binders (pattern)
  "The identifier nodes a term pattern binds, in order. Bare identifiers
bind; `_` doesn't; literals/atoms match by ==."
  (cond
    ((not (node-p pattern)) '())
    (t
     (case (node-head pattern)
       (:ident (if (string= (symbol-name (ident-name pattern)) "_")
                   '()
                   (list pattern)))
       (:tagged_lit (a:mappend #'pattern-binders (rest (node-args pattern))))
       (:record_lit
        (a:mappend (lambda (fi) (pattern-binders (second (node-args fi))))
                   (node-args pattern)))
       (:list_lit (a:mappend #'pattern-binders (node-args pattern)))
       (:spread (pattern-binders (first (node-args pattern))))
       (t '())))))

(defun lower-switch (node env)
  (destructuring-bind (scrutinee . arms) (node-args node)
    (pl :p.match node
        (cons (lower-expr scrutinee env)
              (mapcar
               (lambda (arm)
                 (destructuring-bind (pattern body) (node-args arm)
                   (let ((binders (pattern-binders pattern))
                         (arm-env env))
                     (a:when-let ((dup (find-duplicate
                                        (mapcar #'ident-name binders))))
                       (lower-error arm "pattern binds `~a` more than once"
                                    (symbol-name dup)))
                     (dolist (b binders)
                       (setf arm-env (lenv-extend arm-env (ident-name b) :let
                                                  (binder-scopes b))))
                     (pl :p.arm arm (list pattern (lower-expr body arm-env))))))
               arms)))))

(defun find-duplicate (names)
  (loop for (n . rest) on names
        when (member n rest :test #'eq) return n))

;;; --- quote lowering (SPEC §5.7, §5.8.3) -----------------------------------------------
;;; A quote lowers to code that *constructs* its body node at runtime.
;;; Splicing rule (I4, uniform at term level and in macro bodies): an
;;; identifier in the quoted body that names a lexically visible binding
;;; splices that binding's runtime value (nodes splice as nodes, scalars
;;; lift to literals); every other identifier is literal syntax. Nested
;;; quotes stay syntax — depth games are punted to insert() (M5).

(defun lower-quote (node env)
  (destructuring-bind (kind . body) (node-args node)
    (if (eq kind :|stmts|)
        (pl :p.list node (mapcar (lambda (s) (lift-quoted s env)) body))
        (lift-quoted (first body) env))))

(defun lift-quoted (x env)
  (cond
    ((not (node-p x)) (pl :p.lit x (list x)))   ; scalars self-quote
    ((and (ident-node-p x) (lenv-lookup env (ident-name x) (ref-scopes x)))
     ;; in-scope name: splice by bare name (I4); validated at runtime.
     ;; Spliced values are never re-marked — they are not template text.
     (pl :p.host_call x
         (list 'lift-splice (pl :p.ref x (list (ident-name x) :local)))))
    ;; the template escapes (SPEC §5.8.3) — parser produces these heads
    ;; only inside quote bodies
    ((eq (node-head x) :raw)
     (pl :p.host_call x
         (list '%make-marked-ident
               (pl :p.lit x (list (ident-name (first (node-args x)))))
               (pl :p.lit x (list (node-meta x))))))
    ((eq (node-head x) :inject)
     (pl :p.host_call x
         (list '%make-unmarked-ident
               (pl :p.lit x (list (ident-name (first (node-args x)))))
               (pl :p.lit x (list (node-meta x))))))
    ((eq (node-head x) :insert)
     ;; computed splice: the expression evaluates at expand time in the
     ;; macro body's environment
     (pl :p.host_call x
         (list 'lift-splice (lower-expr (first (node-args x)) env))))
    ((not (quoted-splice-p x env))
     ;; nothing to splice below: embed the subtree literally; instantiation
     ;; stamps the expansion mark at runtime (identity outside expansions)
     (pl :p.host_call x
         (list 'template-instantiate (pl :p.lit x (list x)))))
    (t
     ;; rebuild this node around inner splices, preserving its meta
     (pl :p.host_call x
         (list '%rebuild-node
               (pl :p.lit x (list (node-head x)))
               (pl :p.lit x (list (node-meta x)))
               (pl :p.list x
                   (mapcar (lambda (a) (lift-quoted a env)) (node-args x))))))))

(defun quoted-splice-p (x env)
  "Does the quoted subtree X contain any splice point under ENV?"
  (and (node-p x)
       (cond ((eq (node-head x) :quote) nil) ; nested quotes stay syntax
             ((member (node-head x) '(:raw :inject :insert)) t)
             ((and (ident-node-p x)
                   (lenv-lookup env (ident-name x) (ref-scopes x)))
              t)
             (t (some (lambda (a) (quoted-splice-p a env)) (node-args x))))))

;;; --- for..in desugar (SPEC §5.4, §6) -------------------------------------------------

(defvar *fresh-counter* 0)

(defun fresh-name (base)
  "A name no surface program can collide with (`#` never lexes into idents)."
  (name-keyword (format nil "~a#~d" base (incf *fresh-counter*))))

(defun lower-for (node env)
  (destructuring-bind (binder iter body) (node-args node)
    (let* ((cursor (fresh-name "for"))
           (binder-kw (ident-name binder))
           (cursor-env (lenv-extend env cursor :var))
           (body-env (lenv-extend cursor-env binder-kw :let
                                  (binder-scopes binder)))
           (cursor-ref (pl :p.ref node (list cursor :local))))
      (pl :p.block node
          (list (pl :p.let node
                    (list cursor nil
                          (pl :p.host_call node
                              (list 'sput-check-list (lower-expr iter env)))))
                (pl :p.while node
                    (list cursor-ref   ; a non-empty list is truthy, [] is nil
                          (pl :p.block node
                              (list (pl :p.let node
                                        (list binder-kw nil
                                              (pl :p.host_call node
                                                  (list 'car cursor-ref))))
                                    (lower-block body body-env)
                                    (pl :p.assign node
                                        (list cursor :local
                                              (pl :p.host_call node
                                                  (list 'cdr cursor-ref))))
                                    (pl :p.lit node (list nil))))))
                (pl :p.lit node (list nil)))))))
