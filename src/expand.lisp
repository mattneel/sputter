;;;; expand.lisp — hygienic macro expansion (SPEC §5.8).
;;;;
;;;; A `macro fn` is a comptime function from nodes to nodes: its body is
;;;; compiled through the full pipeline at definition time and installed in
;;;; the image; the expander funcalls it (SPEC §3.1 — the live image is the
;;;; staging evaluator). Expansion is outermost-first and runs to fixpoint
;;;; with a depth limit.
;;;;
;;;; Hygiene (SPEC §5.8.5, rename-based v0.1): every expansion gets a fresh
;;;; integer mark; template-literal nodes are instantiated with that mark in
;;;; meta.scopes (spliced user code is never re-marked). The lowerer resolves
;;;; an identifier to a local binding only when the binding's scope set is
;;;; exactly the reference's — so template refs skip user binders (and reach
;;;; the definition environment's globals), user refs skip template binders,
;;;; and marked binders get fresh emit names so the host never conflates
;;;; them. inject() strips marks (call-site resolution); raw() emits a marked
;;;; identifier even where a hole name would otherwise splice.

(in-package #:sputter.impl)

;; lower/emit are later files; the expander calls them at runtime only
;; (macro bodies compile through the full pipeline at definition time)
(declaim (ftype function lower-top-form emit-top-form))

;;; --- registry --------------------------------------------------------------------

(defstruct (macro-info (:constructor %make-macro-info (name params ret-kind)))
  (name nil :type keyword)
  ;; ((param-name . kind) ...)
  (params '() :type list)
  (ret-kind nil :type keyword)
  ;; the compiled expander function; nil between signature registration
  ;; (parse time) and definition completion (eval time)
  (fn nil))

(defvar *macro-registry* (make-hash-table :test 'eq))

(defun macro-name-p (name)
  (gethash name *macro-registry*))

(defun register-macro-signature (name params ret-kind)
  (setf (gethash name *macro-registry*)
        (%make-macro-info name params ret-kind)))

(defun reset-macros ()
  (clrhash *macro-registry*))

;;; --- errors ---------------------------------------------------------------------

(defun expand-error (node fmt &rest args)
  (let ((m (and (node-p node) (node-meta node))))
    (apply #'sputter-error-at 'sputter-expand-error
           (and m (meta-file m)) (and m (meta-line m)) (and m (meta-col m))
           fmt args)))

;;; --- negative space ----------------------------------------------------------------

(defparameter +expander-owned-heads+
  '(:macro_call :macro_def :macro_fn_def :hole :splice_seq)
  "Heads the expander eliminates (SPEC §4.3). :quote (and raw/insert/inject
inside quote bodies) belong to the lowerer — quoted code is data.")

(defun assert-no-macro-space (x where)
  "Negative space (SPEC §2): no expander-owned head may survive past WHERE.
Quote bodies are exempt — a quoted macro call is data, not a call."
  (labels ((walk (e)
             (when (node-p e)
               (unless (eq (node-head e) :quote)
                 (assert (not (member (node-head e) +expander-owned-heads+)) ()
                         "macro-space head ~a reached ~a" (node-head e) where)
                 (dolist (a (node-args e)) (walk a))))))
    (walk x))
  x)

;;; --- the fixpoint driver (SPEC §5.8.1) -----------------------------------------------

(defvar *expand-depth* 0)

(defparameter +max-expand-depth+ 512
  "SPEC §5.8.1's default expansion depth limit.")

(defun expand-node (node)
  "Outermost-first expansion to fixpoint. Quote bodies pass through untouched."
  (cond
    ((not (node-p node)) node)
    ((eq (node-head node) :quote) node)
    ((eq (node-head node) :macro_call)
     (let ((*expand-depth* (1+ *expand-depth*)))
       (when (> *expand-depth* +max-expand-depth+)
         (expand-error node
                       "macro expansion exceeded ~d levels — a macro is expanding into itself without converging"
                       +max-expand-depth+))
       (expand-macro-call node)))
    (t (make-node (node-head node)
                  (mapcar #'expand-node (node-args node))
                  :meta (node-meta node)))))

(defun expand-macro-call (node)
  (destructuring-bind (name payload) (node-args node)
    (let ((info (gethash name *macro-registry*)))
      (unless info
        (expand-error node "unknown macro `~a`" (symbol-name name)))
      (unless (macro-info-fn info)
        (expand-error node
                      "macro `~a` is used before its definition is complete"
                      (symbol-name name)))
      (let ((args (parse-macro-args node payload (macro-info-params info))))
        (multiple-value-bind (raw mark) (invoke-macro info args)
          (let* ((renamed (rename-template-binders raw mark))
                 (expanded (expand-node renamed)))
            (check-macro-result expanded (macro-info-ret-kind info) node name)
            expanded))))))

(defvar *mark-counter* 0)

(defun invoke-macro (info args)
  "Run the macro fn under a fresh expansion mark (SPEC §5.8.5): templates it
instantiates carry the mark; the spliced ARGS are never re-marked.
Returns (values result mark)."
  (let ((*expansion-mark* (incf *mark-counter*)))
    (values (apply (macro-info-fn info) args) *expansion-mark*)))

(defvar *hygiene-counter* 0)

(defun rename-template-binders (x mark)
  "Rename-based hygiene (SPEC §5.8.5): binder-position identifiers carrying
MARK — template-literal binders — get fresh `name__hN` names, and every
same-marked reference of the same name renames with them. Spliced (unmarked)
code is untouched; nested quote bodies are data and are skipped. The marks
stay on: the lowerer's exact-match rule still routes marked free references
past user binders to the definition environment."
  (let ((renames (make-hash-table :test 'eq)))
    (labels ((marked-p (e)
               (member mark (meta-scopes (node-meta e))))
             (register (ident)
               (when (and (ident-node-p ident) (marked-p ident))
                 (let ((name (ident-name ident)))
                   (unless (gethash name renames)
                     (setf (gethash name renames)
                           (name-keyword
                            (format nil "~a__h~d" (symbol-name name)
                                    (incf *hygiene-counter*))))))))
             (collect (e)
               (when (node-p e)
                 (case (node-head e)
                   (:quote)              ; data
                   ((:let :var :param :fn :for_in)
                    (register (first (node-args e)))
                    (collect-args e))
                   (:arm
                    (mapc #'register (pattern-binders (first (node-args e))))
                    (collect-args e))
                   (t (collect-args e)))))
             (collect-args (e)
               (dolist (a (node-args e)) (collect a)))
             (rewrite (e)
               (cond ((not (node-p e)) e)
                     ((eq (node-head e) :quote) e)
                     ((and (ident-node-p e) (marked-p e)
                           (gethash (ident-name e) renames))
                      (make-ident (gethash (ident-name e) renames)
                                  :meta (node-meta e)))
                     (t (make-node (node-head e)
                                   (mapcar #'rewrite (node-args e))
                                   :meta (node-meta e))))))
      (collect x)
      (if (zerop (hash-table-count renames))
          x
          (rewrite x)))))

;;; --- argument sub-parsing by kind (SPEC §5.8.6) ---------------------------------------

(defun parse-macro-args (call-node payload params)
  (let* ((slices (split-payload (token-group-tokens payload)))
         (slices (if (and (= 1 (length slices))
                          (zerop (length (first slices))))
                     '()                ; m() — no arguments
                     slices)))
    (unless (= (length slices) (length params))
      (expand-error call-node
                    "macro takes ~d argument~:p, got ~d"
                    (length params) (length slices)))
    (loop for slice in slices
          for (param-name . kind) in params
          collect (let ((fragment (parse-fragment slice kind call-node)))
                    (declare (ignorable param-name))
                    (inherit-marks fragment (node-meta call-node))))))

(defun split-payload (tokens)
  "Split a raw token vector at depth-0 commas."
  (let ((slices '())
        (start 0)
        (depth 0))
    (dotimes (i (length tokens))
      (case (token-type (aref tokens i))
        ((:lparen :lbracket :lbrace :dot-lbrace) (incf depth))
        ((:rparen :rbracket :rbrace) (decf depth))
        (:comma (when (zerop depth)
                  (push (subseq tokens start i) slices)
                  (setf start (1+ i))))))
    (push (subseq tokens start) slices)
    (nreverse slices)))

(defun parse-fragment (tokens kind context)
  "Parse a token slice as one fragment of KIND (SPEC §5.8.2's inventory)."
  (when (zerop (length tokens))
    (expand-error context "empty macro argument (expected ~(~a~))" kind))
  (let* ((file (or (and (node-p context) (meta-file (node-meta context)))
                   "<macro>"))
         (last-tok (aref tokens (1- (length tokens))))
         (eof (%make-token :eof nil (token-line last-tok) (token-col last-tok) ""))
         (p (%make-parser (concatenate 'simple-vector tokens (vector eof))
                          file)))
    (flet ((done ()
             (unless (p-at p :eof)
               (parse-error-at p (p-peek p)
                               "unexpected ~a after ~(~a~) fragment"
                               (token-describe (p-peek p)) kind))))
      (let ((result
              (ecase kind
                (:|expr| (parse-expr p))
                (:|stmt| (values (parse-statement p)))
                (:|block| (parse-block p))
                (:|type| (parse-type p))
                (:|arm| (parse-arm p))
                (:|ident|
                 (let ((tok (p-expect p :ident "an identifier")))
                   (make-ident (token-value tok) :meta (tok-meta p tok))))
                (:|atom|
                 (let ((tok (p-peek p)))
                   (unless (eq (token-type tok) :dot-ident)
                     (parse-error-at p tok "expected an atom, got ~a"
                                     (token-describe tok)))
                   (p-next p)
                   (token-value tok)))
                (:|literal|
                 (let ((e (parse-expr p)))
                   (when (or (node-p e) (keywordp e))
                     (expand-error context "expected a literal, got ~a"
                                   (if (node-p e) "an expression" "an atom")))
                   e)))))
        (done)
        result))))

(defun inherit-marks (fragment container-meta)
  "Token payloads carry no marks; when a macro call itself sits inside a
template (its node is marked), the re-parsed fragments inherit those marks —
template text stays template text through the token round-trip."
  (let ((marks (and (meta-p container-meta) (meta-scopes container-meta))))
    (if (null marks)
        fragment
        (labels ((mark (x)
                   (if (node-p x)
                       (make-node (node-head x)
                                  (mapcar #'mark (node-args x))
                                  :meta (mark-meta (node-meta x) marks))
                       x)))
          (mark fragment)))))

(defun mark-meta (m marks)
  (%make-meta :file (meta-file m) :line (meta-line m) :col (meta-col m)
              :scopes (union marks (meta-scopes m))
              :synthetic (meta-synthetic m)))

;;; --- result kind checks (SPEC §5.8.4) ----------------------------------------------

(defparameter +expr-heads+
  '(:ident :add :sub :mul :div :rem :concat :eq :ne :lt :le :gt :ge :and :or
    :not :neg :pipe :call :index :field :if :switch :block :while :for_in
    :unreachable :tagged_lit :record_lit :list_lit :quote :macro_call))

(defun kind-accepts-p (kind x)
  (ecase kind
    (:|expr| (or (scalarp x)
                 (and (node-p x)
                      (or (member (node-head x) +expr-heads+)
                          (and (eq (node-head x) :fn)
                               (null (first (node-args x))))))))
    (:|stmt| (or (kind-accepts-p :|expr| x)
                 (and (node-p x)
                      (member (node-head x)
                              '(:let :var :assign :op_assign :return :fn
                                :macro_fn_def)))))
    (:|block| (and (node-p x) (eq (node-head x) :block)))
    (:|ident| (ident-node-p x))
    (:|atom| (keywordp x))
    (:|literal| (and (scalarp x) (not (keywordp x))))
    (:|type| (and (node-p x) (eq (node-head x) :type_ident)))
    (:|arm| (and (node-p x) (eq (node-head x) :arm)))))

(defun check-macro-result (x kind call-node name)
  (unless (kind-accepts-p kind x)
    (expand-error call-node
                  "macro `~a` promised ~(~a~) but expanded into ~a"
                  (symbol-name name) kind (describe-fragment x))))

(defun describe-fragment (x)
  (cond ((not (node-p x)) (format nil "the literal ~a" (show-value x)))
        (t (format nil "a ~(~a~) node" (node-head x)))))

;;; --- definition-time compile + install (SPEC §3.1, §5.8.4) ---------------------------

(defun install-macro-fn (node)
  "Compile the macro body as an anonymous fn through the full pipeline and
install it. Returns the macro's name keyword."
  (let* ((args (node-args node))
         (name (ident-name (first args)))
         (body (a:lastcar args))
         (ret-kind (a:lastcar (butlast args)))
         (params (subseq args 1 (- (length args) 2)))
         (info (or (gethash name *macro-registry*)
                   ;; parse-time registration can be missing when nodes are
                   ;; synthesized; register from the def itself
                   (register-macro-signature
                    name
                    (mapcar (lambda (param)
                              (cons (ident-name (first (node-args param)))
                                    (second (node-args param))))
                            params)
                    ret-kind)))
         ;; kinds are macro-land types; at runtime params are plain
         (fn-node (make-node :fn
                             (append (list nil)
                                     (mapcar (lambda (param)
                                               (make-node :param
                                                          (list (first (node-args param)) nil)
                                                          :meta (node-meta param)))
                                             params)
                                     (list nil body))
                             :meta (node-meta node))))
    (let ((plasma (first (lower-top-form fn-node))))
      (validate-plasma plasma)
      (setf (macro-info-fn info) (host-eval (emit-top-form plasma))))
    name))

(defun expand-module (stmts)
  "Expand a whole (already parsed) module: macro definitions install in
order (define-before-use) and stay in the list for the caller to render as
consumed; everything else expands to fixpoint."
  (mapcar (lambda (stmt)
            (if (and (node-p stmt) (eq (node-head stmt) :macro_fn_def))
                (progn (install-macro-fn (expand-macro-def-body stmt)) stmt)
                (let ((expanded (expand-node stmt)))
                  (assert-no-macro-space expanded "the expander")
                  expanded)))
          stmts))

(defun expand-macro-def-body (node)
  "Macro calls inside a macro body (outside its quotes) expand at definition
time — macros can use macros."
  (make-node (node-head node)
             (mapcar #'expand-node (node-args node))
             :meta (node-meta node)))