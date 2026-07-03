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
  (ret-kind nil :type (or null keyword))
  ;; the compiled expander function; nil between signature registration
  ;; (parse time) and definition completion (eval time)
  (fn nil)
  ;; M6 by-example macros store raw arms here. NIL means this is a procedural
  ;; `macro fn`.
  (arms nil :type list))

(defvar *macro-registry* (make-hash-table :test 'eq))

(defun macro-name-p (name)
  (gethash name *macro-registry*))

(defun macro-by-example-name-p (name)
  (let ((info (gethash name *macro-registry*)))
    (and info (macro-info-arms info))))

(defun register-macro-signature (name params ret-kind)
  (setf (gethash name *macro-registry*)
        (%make-macro-info name params ret-kind)))

(defun register-by-example-macro (name arms)
  (let ((info (%make-macro-info name '() nil)))
    (setf (macro-info-arms info) arms)
    (setf (gethash name *macro-registry*) info)))

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
  '(:macro_call :macro_def :macro_arm :macro_fn_def :hole :splice_seq)
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
  (destructuring-bind (name payload &optional style) (node-args node)
    (declare (ignore style))
    (let ((info (gethash name *macro-registry*)))
      (unless info
        (expand-error node "unknown macro `~a`" (symbol-name name)))
      (when (macro-info-arms info)
        (return-from expand-macro-call (expand-by-example-call node info payload)))
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

(defun parse-fragment (tokens kind context &key in-quote)
  "Parse a token slice as one fragment of KIND (SPEC §5.8.2's inventory)."
  (when (zerop (length tokens))
    (expand-error context "empty macro argument (expected ~(~a~))" kind))
  (let* ((file (or (and (node-p context) (meta-file (node-meta context)))
                   "<macro>"))
         (last-tok (aref tokens (1- (length tokens))))
         (eof (%make-token :eof nil (token-line last-tok) (token-col last-tok) ""))
         (p (%make-parser (concatenate 'simple-vector tokens (vector eof))
                          file))
         (*in-quote* in-quote))
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

;;; --- by-example macros (SPEC §5.8.1, M6) --------------------------------------------

(defstruct (macro-binding
             (:constructor make-macro-binding (kind sequence-p value tokens)))
  (kind nil :type keyword)
  (sequence-p nil :type boolean)
  ;; VALUE is one node/scalar for ordinary holes, or a list of nodes/scalars for
  ;; `...name: kind` sequence holes.
  value
  ;; The original raw tokens for this binding, used when a template splices a
  ;; sequence into another by-example macro invocation (`cond { ...rest }`).
  (tokens #() :type simple-vector))

(defun expand-by-example-call (node info payload)
  (let ((tokens (token-group-tokens payload))
        (arms (macro-info-arms info))
        (tried 0))
    (dolist (arm arms)
      (incf tried)
      (a:when-let ((bindings (match-by-example-arm node arm tokens)))
        (let ((expanded (expand-node
                         (instantiate-by-example-template node arm bindings))))
          (return-from expand-by-example-call expanded))))
    (expand-error node
                  "macro `~a` matched no by-example arm (~d arm~:p tried)"
                  (symbol-name (macro-info-name info)) tried)))

(defun match-by-example-arm (call-node arm input-tokens)
  (let* ((pattern (first (node-args arm)))
         (pat-tokens (token-group-tokens pattern))
         (bindings (make-hash-table :test 'eq)))
    (match-pattern-range call-node pat-tokens 0 (length pat-tokens)
                         input-tokens 0 (length input-tokens)
                         bindings)))

(defun match-pattern-range (call-node pat pidx pend input ii iend bindings)
  "Backtracking matcher for raw by-example patterns.
Literal tokens match by content. `name: kind` binds one fragment; `...name:
kind` binds a sequence. Candidate hole extents are tried only at balanced token
boundaries so a hole never captures half of a nested group."
  (if (= pidx pend)
      (and (= ii iend) bindings)
      (multiple-value-bind (holep name kind sequence-p next-pi)
          (pattern-hole-at pat pidx pend)
        (if holep
            (loop for end from (if sequence-p ii (1+ ii)) to iend
                  when (balanced-token-slice-p input ii end)
                    do (multiple-value-bind (ok binding)
                           (try-parse-hole-binding call-node input ii end
                                                   kind sequence-p)
                         (when ok
                           (a:when-let ((next-bindings
                                          (extend-pattern-bindings bindings name
                                                                   binding)))
                             (a:when-let ((matched
	                                            (match-pattern-range
	                                             call-node pat next-pi pend
	                                             input end iend next-bindings)))
	                               (return matched))))))
            (when (and (< ii iend)
                       (token-content-equal (aref pat pidx) (aref input ii)))
              (match-pattern-range call-node pat (1+ pidx) pend
                                   input (1+ ii) iend bindings))))))

(defun pattern-hole-at (tokens i end)
  (let ((tok (and (< i end) (aref tokens i))))
    (cond
      ((and tok
            (eq (token-type tok) :ellipsis)
            (< (+ i 3) end)
            (eq (token-type (aref tokens (1+ i))) :ident)
            (eq (token-type (aref tokens (+ i 2))) :colon)
            (eq (token-type (aref tokens (+ i 3))) :ident))
       (let ((kind (token-value (aref tokens (+ i 3)))))
         (values t (token-value (aref tokens (1+ i))) kind t (+ i 4))))
      ((and tok
            (eq (token-type tok) :ident)
            (< (+ i 2) end)
            (eq (token-type (aref tokens (1+ i))) :colon)
            (eq (token-type (aref tokens (+ i 2))) :ident))
       (let ((kind (token-value (aref tokens (+ i 2)))))
         (values t (token-value tok) kind nil (+ i 3))))
      (t (values nil nil nil nil i)))))

(defun balanced-token-slice-p (tokens start end)
  (let ((depth 0))
    (loop for i from start below end
          for type = (token-type (aref tokens i))
          do (case type
               ((:lparen :lbracket :lbrace :dot-lbrace) (incf depth))
               ((:rparen :rbracket :rbrace)
                (decf depth)
                (when (minusp depth) (return-from balanced-token-slice-p nil)))))
    (zerop depth)))

(defun try-parse-hole-binding (call-node tokens start end kind sequence-p)
  (unless (member kind +hole-kinds+)
    (expand-error call-node
                  "unknown by-example hole kind `~a`"
                  (symbol-name kind)))
  (let ((slice (subseq tokens start end)))
    (handler-case
        (values t
                (make-macro-binding
                 kind sequence-p
                 (if sequence-p
                     (parse-hole-sequence slice kind call-node)
                     (inherit-marks (parse-fragment slice kind call-node)
                                    (node-meta call-node)))
                 slice))
      (sputter-error () (values nil nil)))))

(defun parse-hole-sequence (tokens kind call-node)
  (ecase kind
    (:|stmt| (parse-stmt-sequence tokens call-node))
    (:|arm| (parse-arm-sequence tokens call-node))
    ;; v0.1 only needs statement and arm sequence holes, but accepting
    ;; comma-separated repetitions for the other fragment kinds costs little and
    ;; keeps the matcher unsurprising.
    ((:|expr| :|block| :|ident| :|atom| :|literal| :|type|)
     (if (zerop (length tokens))
         '()
         (mapcar (lambda (slice)
                   (inherit-marks (parse-fragment slice kind call-node)
                                  (node-meta call-node)))
                 (split-payload tokens))))))

(defun parser-for-token-slice (tokens context)
  (let* ((file (or (and (node-p context) (meta-file (node-meta context)))
                   "<macro>"))
         (last (if (plusp (length tokens))
                   (aref tokens (1- (length tokens)))
                   (let ((m (and (node-p context) (node-meta context))))
                     (%make-token :eof nil (or (and m (meta-line m)) 0)
                                  (or (and m (meta-col m)) 0) ""))))
         (eof (%make-token :eof nil (token-line last) (token-col last) "")))
    (%make-parser (concatenate 'simple-vector tokens (vector eof)) file)))

(defun parse-stmt-sequence (tokens call-node)
  (let ((p (parser-for-token-slice tokens call-node))
        (out '()))
    (loop until (p-at p :eof)
          do (push (values (parse-statement p)) out))
    (nreverse out)))

(defun parse-arm-sequence (tokens call-node)
  (declare (ignore call-node))
  ;; Macro-pattern `arm` is a by-example arm fragment, not a surface `switch`
  ;; arm: its LHS can itself be an expr hole (`cond { c: expr => ... }`).
  ;; Keep it raw and only split at top-level commas so templates can splice it
  ;; back into another macro invocation.
  (mapcar #'make-token-group
          (remove-if (lambda (slice) (zerop (length slice)))
                     (split-payload tokens))))

(defun extend-pattern-bindings (bindings name binding)
  (a:if-let ((old (gethash name bindings)))
    (and (binding-equal-p old binding) bindings)
    (let ((copy (copy-hash-table-eq bindings)))
      (setf (gethash name copy) binding)
      copy)))

(defun copy-hash-table-eq (table)
  (let ((copy (make-hash-table :test 'eq)))
    (maphash (lambda (k v) (setf (gethash k copy) v)) table)
    copy))

(defun binding-equal-p (a b)
  (and (eq (macro-binding-kind a) (macro-binding-kind b))
       (eq (macro-binding-sequence-p a) (macro-binding-sequence-p b))
       (token-vector-equal (macro-binding-tokens a) (macro-binding-tokens b))
       (if (macro-binding-sequence-p a)
           (and (= (length (macro-binding-value a))
                   (length (macro-binding-value b)))
                (every #'node-equal (macro-binding-value a)
                       (macro-binding-value b)))
           (node-equal (macro-binding-value a) (macro-binding-value b)))))

(defun token-vector-equal (a b)
  (and (= (length a) (length b))
       (loop for i from 0 below (length a)
             always (token-content-equal (aref a i) (aref b i)))))

(defvar *by-example-sentinel-counter* 0)

(defun instantiate-by-example-template (call-node arm bindings)
  (let* ((template (second (node-args arm)))
         (tokens (token-group-tokens template)))
    (multiple-value-bind (prepared sentinels)
        (prepare-template-tokens tokens bindings)
      (let* ((parsed (parse-template-node prepared call-node))
             (mark (incf *mark-counter*))
             (marked (mark-template-tree parsed mark))
             (spliced (splice-template-sentinels marked sentinels mark)))
        (rename-template-binders spliced mark)))))

(defun prepare-template-tokens (tokens bindings)
  "Replace template hole references with fresh sentinel identifiers that the
real parser accepts. Sequence statement holes become sentinel statements;
sequence arm holes stay raw so they can be spliced into token-groups."
  (let ((out '())
        (sentinels (make-hash-table :test 'eq)))
    (labels ((binding-for (name) (gethash name bindings))
             (sentinel-token (name binding tok)
               (let* ((text (format nil "__sput_hole_~d_~a"
                                    (incf *by-example-sentinel-counter*)
                                    (symbol-name name)))
                      (kw (name-keyword text)))
                 (setf (gethash kw sentinels) binding)
                 (%make-token :ident kw (token-line tok) (token-col tok) text)))
             (push-token (tok) (push tok out))
             (push-semi-like (tok)
               (push (%make-token :semi nil (token-line tok) (token-col tok) ";")
                     out))
             (matching-index (open-index)
               (let ((depth 0))
                 (loop for j from open-index below (length tokens)
                       for type = (token-type (aref tokens j))
                       do (case type
                            ((:lparen :lbracket :lbrace :dot-lbrace)
                             (incf depth))
                            ((:rparen :rbracket :rbrace)
                             (decf depth)
                             (when (zerop depth) (return j))))))))
      (loop with i = 0
            while (< i (length tokens))
            do (let ((tok (aref tokens i)))
                 (cond
                   ;; raw(x)/inject(x) must see the literal x, not the hole x.
                   ((and (eq (token-type tok) :ident)
                         (member (token-value tok) '(:|raw| :|inject|))
                         (< (1+ i) (length tokens))
                         (eq (token-type (aref tokens (1+ i))) :lparen))
                    (let ((end (or (matching-index (1+ i)) (1+ i))))
                      (loop for j from i to end do (push-token (aref tokens j)))
                      (setf i (1+ end))))
                   ((and (eq (token-type tok) :ellipsis)
                         (< (1+ i) (length tokens))
                         (eq (token-type (aref tokens (1+ i))) :ident))
                    (let* ((name (token-value (aref tokens (1+ i))))
                           (binding (binding-for name)))
                      (if (and binding (macro-binding-sequence-p binding))
                          (progn
                            (push-token (sentinel-token name binding tok))
                            (when (eq (macro-binding-kind binding) :|stmt|)
                              (push-semi-like tok))
                            (incf i 2))
                          (progn (push-token tok) (incf i)))))
                   ((and (eq (token-type tok) :ident)
                         (binding-for (token-value tok))
                         (not (macro-binding-sequence-p
                               (binding-for (token-value tok)))))
                    (push-token (sentinel-token (token-value tok)
                                                (binding-for (token-value tok))
                                                tok))
                    (incf i))
                   (t (push-token tok) (incf i))))))
    (values (coerce (nreverse out) 'simple-vector) sentinels)))

(defun parse-template-node (tokens call-node)
  (handler-case
      (parse-fragment tokens :|expr| call-node :in-quote t)
    (sputter-error (expr-error)
      (handler-case
          (parse-fragment tokens :|stmt| call-node :in-quote t)
        (sputter-error ()
          (error expr-error))))))

(defun mark-template-tree (x mark)
  (if (node-p x)
      (make-node (node-head x)
                 (mapcar (lambda (a) (mark-template-tree a mark))
                         (node-args x))
                 :meta (mark-meta (node-meta x) (list mark)))
      x))

(defun strip-mark-from-meta (m mark)
  (%make-meta :file (meta-file m) :line (meta-line m) :col (meta-col m)
              :scopes (remove mark (meta-scopes m))
              :synthetic (meta-synthetic m)
              :end-line (meta-end-line m)))

(defun splice-template-sentinels (x sentinels mark)
  (cond
    ((not (node-p x)) x)
    ((ident-node-p x)
     (a:if-let ((binding (gethash (ident-name x) sentinels)))
       (if (macro-binding-sequence-p binding)
           x
           (macro-binding-value binding))
       x))
    ((eq (node-head x) :raw)
     (let ((ident (first (node-args x))))
       (make-ident (ident-name ident) :meta (node-meta x))))
    ((eq (node-head x) :inject)
     (let* ((ident (first (node-args x)))
            (m (node-meta x)))
       (make-ident (ident-name ident)
                   :meta (%make-meta :file (meta-file m) :line (meta-line m)
                                     :col (meta-col m)
                                     :synthetic (meta-synthetic m)
                                     :end-line (meta-end-line m)))))
    ((eq (node-head x) :insert)
     ;; M6 by-example macros have no separate macro-body evaluator; treating
     ;; insert(e) as an explicit splice of e covers the useful template case.
     (splice-template-sentinels (first (node-args x)) sentinels mark))
    ((eq (node-head x) :block)
     (splice-template-block x sentinels mark))
    ((eq (node-head x) :macro_call)
     (splice-template-macro-call x sentinels mark))
    (t (make-node (node-head x)
                  (mapcar (lambda (a)
                            (splice-template-sentinels a sentinels mark))
                          (node-args x))
                  :meta (node-meta x)))))

(defun splice-template-block (block sentinels mark)
  (let* ((args (node-args block))
         (stmts (butlast args))
         (value (a:lastcar args))
         (new-stmts '()))
    (dolist (stmt stmts)
      (if (and (ident-node-p stmt)
               (gethash (ident-name stmt) sentinels))
          (let ((binding (gethash (ident-name stmt) sentinels)))
            (if (macro-binding-sequence-p binding)
                (setf new-stmts
                      (append (reverse (macro-binding-value binding))
                              new-stmts))
                (push (macro-binding-value binding) new-stmts)))
          (push (splice-template-sentinels stmt sentinels mark) new-stmts)))
    (make-node :block
               (append (nreverse new-stmts)
                       (list (splice-template-sentinels value sentinels mark)))
               :meta (node-meta block))))

(defun splice-template-macro-call (node sentinels mark)
  (destructuring-bind (name payload &optional style) (node-args node)
    (multiple-value-bind (tokens changed)
        (splice-template-token-group (token-group-tokens payload) sentinels)
      (make-node :macro_call
                 (list name (make-token-group tokens) (or style :by-example))
                 ;; If user tokens were spliced into the raw payload, do not let
                 ;; this template macro-call's mark be inherited by those user
                 ;; fragments during the next expansion step.
                 :meta (if changed
                           (strip-mark-from-meta (node-meta node) mark)
                           (node-meta node))))))

(defun splice-template-token-group (tokens sentinels)
  (let ((out '())
        (changed nil))
    (loop for i from 0 below (length tokens)
          for tok = (aref tokens i)
          do (if (and (eq (token-type tok) :ident)
                      (gethash (token-value tok) sentinels))
                 (let ((binding (gethash (token-value tok) sentinels)))
                   (setf changed t)
                   (loop for replacement across (macro-binding-tokens binding)
                         do (push replacement out)))
                 (push tok out)))
    (values (coerce (nreverse out) 'simple-vector) changed)))

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

(defun install-by-example-macro (node)
  (destructuring-bind (name . arms) (node-args node)
    (register-by-example-macro (ident-name name) arms)
    (ident-name name)))

(defun expand-module (stmts)
  "Expand a whole (already parsed) module: macro definitions install in
order (define-before-use) and stay in the list for the caller to render as
consumed; everything else expands to fixpoint."
  (mapcar (lambda (stmt)
            (cond
              ((and (node-p stmt) (eq (node-head stmt) :macro_fn_def))
               (install-macro-fn (expand-macro-def-body stmt))
               stmt)
              ((and (node-p stmt) (eq (node-head stmt) :macro_def))
               (install-by-example-macro stmt)
               stmt)
              (t
               (let ((expanded (expand-node stmt)))
                 (assert-no-macro-space expanded "the expander")
                 expanded))))
          stmts))

(defun expand-macro-def-body (node)
  "Macro calls inside a macro body (outside its quotes) expand at definition
time — macros can use macros."
  (make-node (node-head node)
             (mapcar #'expand-node (node-args node))
             :meta (node-meta node)))
