;;;; emit.lisp — Plasma → CL forms + span table (SPEC §7, §8), and the
;;;; top-level pipeline driver (expand → lower → validate → emit → host eval).

(in-package #:sputter.impl)

;;; --- mangling (SPEC §5.1) -----------------------------------------------------

(defun mangle (name-kw)
  "snake_case -> hyphenated uppercase symbol in package SPUTTER
(`my_func` -> SPUTTER::MY-FUNC)."
  (check-type name-kw keyword)
  (values (intern (substitute #\- #\_ (string-upcase (symbol-name name-kw)))
                  '#:sputter)))

(defun demangle-symbol (sym)
  "SPUTTER::MY-FUNC -> \"my_func\", for user-facing prose."
  (substitute #\_ #\- (string-downcase (symbol-name sym))))

;;; --- types (SPEC §7 table) -------------------------------------------------------

(defparameter +cl-type-map+
  '((:|i32| . (signed-byte 32)) (:|i64| . (signed-byte 64))
    (:|u32| . (unsigned-byte 32)) (:|u64| . (unsigned-byte 64))
    (:|int| . integer)
    (:|f32| . single-float) (:|f64| . double-float)
    (:|bool| . sput-bool) (:|str| . string) (:|atom| . keyword)
    ;; kind ascriptions (macro-body bind-first idiom, SPEC §5.8.3);
    ;; expr/stmt/literal fragments can be scalars, so they stay loose
    (:|expr| . t) (:|stmt| . t) (:|block| . node) (:|ident| . node)
    (:|literal| . t) (:|type| . node) (:|arm| . node)
    (:|list| . list) (:|record| . hash-table) (:|node| . node) (:|any| . t)))

(defun cl-type (type-kw)
  (and type-kw (cdr (assoc type-kw +cl-type-map+))))

(defun sputter-type-name (cl-type-spec)
  "Reverse-map a CL type spec to a Sputter type name, best-effort (for the
type-error rendering at the §8 boundary)."
  (a:if-let ((entry (rassoc cl-type-spec +cl-type-map+ :test #'equal)))
    (symbol-name (car entry))
    (format nil "~(~a~)" cl-type-spec)))

;;; --- emission ----------------------------------------------------------------------

(defvar *current-block* nil
  "Mangled symbol naming the innermost fn block — the target of p.return.")

(defun global-fn-symbol (name-kw)
  "The CL symbol a global-fn ref calls: the builtin's implementation symbol
when registered, else the mangled user symbol (late-bound)."
  (let ((entry (global-entry name-kw)))
    (or (and entry (eq (car entry) :fn) (cdr entry))
        (mangle name-kw))))

(defun emit-plasma (x)
  (assert (node-p x) (x) "emitter expects Plasma nodes, got: ~s" x)
  (let ((args (node-args x)))
    (ecase (node-head x)
      (:p.lit `(quote ,(first args)))
      (:p.ref
       (destructuring-bind (name kind) args
         (ecase kind
           (:local (mangle name))
           ;; Top-level bindings live in the global value table, not in
           ;; symbol value cells: sb-ext:global would forbid lexical
           ;; shadowing, and specials would break closures.
           (:global-var `(sput-global ',name))
           (:global-fn `(function ,(global-fn-symbol name))))))
      (:p.call
       (let ((callee (first args))
             (call-args (mapcar #'emit-plasma (rest args))))
         (if (and (eq (node-head callee) :p.ref)
                  (eq (second (node-args callee)) :global-fn))
             `(,(global-fn-symbol (first (node-args callee))) ,@call-args)
             `(funcall ,(emit-plasma callee) ,@call-args))))
      (:p.host_call
       `(,(first args) ,@(mapcar #'emit-plasma (rest args))))
      (:p.fn (emit-plasma-fn x))
      (:p.if
       (destructuring-bind (c then else) args
         `(if (truthy ,(emit-plasma c))
              ,(emit-plasma then)
              ,(emit-plasma else))))
      (:p.and
       (let ((g (gensym "AND")))
         `(let ((,g ,(emit-plasma (first args))))
            (if (truthy ,g) ,(emit-plasma (second args)) ,g))))
      (:p.or
       (let ((g (gensym "OR")))
         `(let ((,g ,(emit-plasma (first args))))
            (if (truthy ,g) ,g ,(emit-plasma (second args))))))
      (:p.block (emit-stmt-chain (butlast args) (a:lastcar args)))
      (:p.assign
       (destructuring-bind (name kind value) args
         (ecase kind
           (:local `(setq ,(mangle name) ,(emit-plasma value)))
           (:global `(sput-global-set ',name ,(emit-plasma value))))))
      (:p.while
       (destructuring-bind (c body) args
         `(loop while (truthy ,(emit-plasma c))
                do ,(emit-plasma body))))
      (:p.return
       (progn
         (assert *current-block* () "p.return emitted outside a function")
         `(return-from ,*current-block* ,(emit-plasma (first args)))))
      (:p.panic
       (let ((m (node-meta x)))
         `(sput-panic ,(emit-plasma (first args))
                      :file ,(meta-file m) :line ,(meta-line m)
                      :col ,(meta-col m))))
      (:p.field
       (destructuring-bind (obj name) args
         `(sput-field ,(emit-plasma obj) ',name)))
      (:p.index
       (destructuring-bind (obj idx) args
         `(sput-index ,(emit-plasma obj) ,(emit-plasma idx))))
      (:p.list `(list ,@(mapcar #'emit-plasma args)))
      (:p.record
       `(make-record ,@(loop for (k v) on args by #'cddr
                             append (list `',k (emit-plasma v)))))
      (:p.tagged
       `(make-tagged ',(first args) ,@(mapcar #'emit-plasma (rest args))))
      (:p.match (emit-match x))
      (:p.let
       ;; p.let is structural inside p.block (handled by emit-stmt-chain)
       ;; or a global def at top level (handled by emit-top-form)
       (assert nil (x) "p.let reached expression emission")))))

;;; --- p.match → trivia (SPEC §5.6, §7: the ride is an emitter detail) -----------

(defun emit-match (pmatch)
  (let ((g (gensym "SCRUTINEE"))
        (m (node-meta pmatch)))
    `(let ((,g ,(emit-plasma (first (node-args pmatch)))))
       (trivia:match ,g
         ,@(mapcar (lambda (arm)
                     (destructuring-bind (pattern body) (node-args arm)
                       (list (emit-match-pattern pattern)
                             (emit-plasma body))))
                   (rest (node-args pmatch)))
         (_ (rt-no-match ,g ,(meta-file m) ,(meta-line m) ,(meta-col m)))))))

(defun emit-match-pattern (pat)
  "Term pattern -> trivia pattern. Semantics per SPEC §5.6: literals and
atoms by ==, bare identifiers bind, .tag(...) arity-checked, records match
with at-least those fields, lists exactly (or with a ...tail)."
  (cond
    ((not (node-p pat))
     (let ((g (gensym "LIT")))
       `(trivia:guard ,g (sput-equal ,g ',pat))))
    (t
     (ecase (node-head pat)
       (:ident
        (let ((name (ident-name pat)))
          (if (string= (symbol-name name) "_")
              '_
              (mangle name))))
       (:tagged_lit
        (destructuring-bind (tag . subs) (node-args pat)
          (let ((g (gensym "TAGGED")))
            `(trivia:guard1 ,g (and (tagged-p ,g)
                                    (eq (tagged-tag ,g) ',tag)
                                    (= (length (tagged-vals ,g)) ,(length subs)))
                            ,@(loop for s in subs
                                    for i from 0
                                    append (list `(svref (tagged-vals ,g) ,i)
                                                 (emit-match-pattern s)))))))
       (:record_lit
        ;; record patterns also destructure nodes (SPEC §4.4: nodes match
        ;; like tagged data — they expose .head/.meta/.args as fields)
        (let ((g (gensym "RECORD"))
              (fields (node-args pat)))
          `(trivia:guard1 ,g (and (match-fields-p ,g)
                                  ,@(mapcar (lambda (fi)
                                              `(match-field-has-p ,g ',(first (node-args fi))))
                                            fields))
                          ,@(loop for fi in fields
                                  append (list `(sput-field ,g ',(first (node-args fi)))
                                               (emit-match-pattern
                                                (second (node-args fi))))))))
       (:list_lit
        (let* ((elems (node-args pat))
               (tail (and elems
                          (node-p (a:lastcar elems))
                          (eq (node-head (a:lastcar elems)) :spread)
                          (a:lastcar elems))))
          (if tail
              `(list* ,@(mapcar #'emit-match-pattern (butlast elems))
                      ,(emit-match-pattern (first (node-args tail))))
              `(list ,@(mapcar #'emit-match-pattern elems)))))))))

(defun emit-stmt-chain (stmts value)
  "A p.block emits as nested LET* layers around a PROGN spine."
  (cond ((null stmts) (emit-plasma value))
        ((eq (node-head (first stmts)) :p.let)
         (destructuring-bind (name type init) (node-args (first stmts))
           (let ((sym (mangle name))
                 (ct (cl-type type)))
             `(let ((,sym ,(emit-plasma init)))
                ,@(when (and ct (not (eq ct t)))
                    `((declare (type ,ct ,sym))))
                (declare (ignorable ,sym))
                ,(emit-stmt-chain (rest stmts) value)))))
        (t `(progn ,(emit-plasma (first stmts))
                   ,(emit-stmt-chain (rest stmts) value)))))

(defun emit-plasma-fn (pfn &key top-level)
  (let* ((args (node-args pfn))
         (name (first args))
         (body (a:lastcar args))
         (ret (a:lastcar (butlast args)))
         (params (subseq args 1 (- (length args) 2)))
         (param-syms '())
         (declares '()))
    (dolist (p params)
      (destructuring-bind (pname ptype) (node-args p)
        (let ((sym (mangle pname))
              (ct (cl-type ptype)))
          (push sym param-syms)
          (when (and ct (not (eq ct t)))
            (push `(type ,ct ,sym) declares)))))
    (setf param-syms (nreverse param-syms)
          declares (nreverse declares))
    (let* ((block-name (mangle (or name :|%sput-fn|)))
           (body-form (let ((*current-block* block-name))
                        (emit-plasma body)))
           (ret-ct (cl-type ret))
           (typed-body (if (and ret-ct (not (eq ret-ct t)))
                           `(the ,ret-ct ,body-form)
                           body-form)))
      (if (and top-level name)
          `(defun ,block-name (,@param-syms)
             ,@(when declares `((declare ,@declares)))
             ,typed-body)
          `(lambda (,@param-syms)
             ,@(when declares `((declare ,@declares)))
             (block ,(mangle :|%sput-fn|) ,typed-body))))))

(defun emit-top-form (p)
  "Emit one top-level Plasma form as an evaluable CL form."
  (case (node-head p)
    (:p.fn
     (if (first (node-args p))
         (let* ((name-kw (first (node-args p)))
                (sym (mangle name-kw))
                (m (node-meta p)))
           (register-global-fn name-kw sym)
           (when (meta-file m)
             (setf (gethash sym *span-table*)
                   (list (meta-file m) (meta-line m) (meta-col m))))
           (emit-plasma-fn p :top-level t))
         (emit-plasma p)))
    (:p.let
     (destructuring-bind (name type init) (node-args p)
       (declare (ignore type))
       `(sput-global-set ',name ,(emit-plasma init))))
    (t (emit-plasma p))))

;;; --- the pipeline driver (SPEC §3.2) -----------------------------------------------

(declaim (ftype function expand-node install-macro-fn install-by-example-macro
                expand-macro-def-body assert-no-macro-space)) ; expand.lisp loads after this file

(defun eval-top-form (node)
  "One top-level form through the whole pipeline:
EXPAND → LOWER → validate → EMIT → HOST eval. Returns the runtime value.
Macro definitions compile and install at this point (SPEC §3.1) — the live
image is the staging evaluator."
  (cond
    ((and (node-p node) (eq (node-head node) :macro_fn_def))
     (install-macro-fn (expand-macro-def-body node)))
    ((and (node-p node) (eq (node-head node) :macro_def))
     (install-by-example-macro node))
    (t
     (let ((expanded (expand-node node))
           (value nil))
       (assert-no-macro-space expanded "the lowerer")
       (dolist (p (lower-top-form expanded) value)
         (validate-plasma p)
         (setf value (host-eval (emit-top-form p)))
         ;; echo-friendly values for defs
         (when (and (eq (node-head p) :p.fn) (first (node-args p)))
           (setf value (symbol-function (mangle (first (node-args p)))))))))))

(defun run-file (path)
  "Compile + execute one .sput file in the current image state.
Returns the value of the last top-level form."
  (let ((stmts (parse-module (read-source-file path) :file path))
        (value nil))
    (dolist (s stmts value)
      (setf value (eval-top-form s)))))
