;;;; rt.lisp — runtime support (SPEC §3.3, §5.5): conditions, panics with
;;;; frames, the operator builtins, equality, field/index access, and the
;;;; user-facing seed builtins. The false singleton and truthiness live in
;;;; node.lisp (the AST layer needs them first).

(in-package #:sputter.impl)

;; Forward declarations for later files (print.lisp, emit.lisp): these are
;; called at runtime only; the declaims keep the compiler quiet at load.
(declaim (ftype (function (t) string) show-value print-node dump-string)
         (ftype (function (symbol) string) demangle-symbol))

;;; --- conditions (SPEC §8) ---------------------------------------------------
;;; Every condition that can escape user code or the pipeline is a
;;; SPUTTER-ERROR; the CLI/REPL boundary renders them Sputter-side. A raw SBCL
;;; condition reaching the user is a bug (I2).

(define-condition sputter-error (error)
  ((message :initarg :message :reader sputter-error-message)
   (file :initarg :file :initform nil :reader sputter-error-file)
   (line :initarg :line :initform nil :reader sputter-error-line)
   (col :initarg :col :initform nil :reader sputter-error-col))
  (:report (lambda (c s) (format s "~a" (sputter-error-message c)))))

(define-condition sputter-parse-error (sputter-error) ()
  (:documentation "Lexing and parsing errors. Always carries a precise span."))

(define-condition sputter-expand-error (sputter-error) ()
  (:documentation "Macro-expansion errors (M5+)."))

(define-condition sputter-lower-error (sputter-error) ()
  (:documentation "Lowering errors: immutability violations, unknown types... (M2+)."))

(define-condition sputter-panic (sputter-error)
  ((frames :initarg :frames :initform nil :reader sputter-panic-frames))
  (:documentation "Runtime panic: `unreachable`, no-match, bad field...
FRAMES is a best-effort list of (demangled-name file line col) user frames,
captured at signal time via the span table (SPEC §8)."))

(defun sputter-error-at (kind file line col fmt &rest args)
  "Signal a Sputter condition of KIND with a source span."
  (error kind :message (apply #'format nil fmt args)
              :file file :line line :col col))

(defun render-sputter-error (c stream)
  "The §8 rendering contract: `error: <message>` plus a span when known,
plus best-effort user frames for runtime panics."
  (format stream "error: ~a~%" (sputter-error-message c))
  (when (and (sputter-error-file c) (sputter-error-line c))
    (format stream "  at ~a:~d~@[:~d~]~%"
            (sputter-error-file c) (sputter-error-line c)
            (sputter-error-col c)))
  (when (typep c 'sputter-panic)
    (loop for (name file line col) in (sputter-panic-frames c)
          do (format stream "  in ~a (~a:~d~@[:~d~])~%" name file line col))))

;;; --- the value model at runtime (SPEC §5.5) ---------------------------------

(deftype sput-bool ()
  "Sputter booleans: T or the false singleton (§7's `bool` declaration)."
  '(or (eql t) sput-false))

(defvar *span-table* (make-hash-table :test 'eq)
  "Mangled CL function symbol -> (file line col). Emitter-maintained (SPEC §8);
lives here so panics can resolve frames at signal time.")

(defvar *global-values* (make-hash-table :test 'eq)
  "Top-level let/var bindings: name-keyword -> value. A table rather than
symbol value cells so locals shadow lexically and sessions reset cleanly.")

(defun sput-global (name)
  (multiple-value-bind (v found) (gethash name *global-values*)
    (if found
        v
        ;; unreachable when the lowerer is correct: it only emits
        ;; global-var refs for registered globals
        (rt-panic "`~a` is not defined" (symbol-name name)))))

(defun sput-global-set (name value)
  (setf (gethash name *global-values*) value))

(defun capture-user-frames (&optional (limit 8))
  "Best-effort user frames from the host stack, innermost first."
  (let ((frames '())
        (last nil))
    (dolist (sym (host-backtrace-symbols))
      (let ((span (gethash sym *span-table*)))
        (when (and span (not (eq sym last)) (< (length frames) limit))
          (setf last sym)
          (push (cons (demangle-symbol sym) span) frames))))
    (mapcar (lambda (f) (cons (car f) (copy-list (cdr f))))
            (nreverse frames))))

(defun sput-panic (message &key file line col)
  "Signal a runtime panic (SPEC §8), capturing user frames at the throw."
  (error 'sputter-panic
         :message message :file file :line line :col col
         :frames (capture-user-frames)))

(defun rt-panic (fmt &rest args)
  (sput-panic (apply #'format nil fmt args)))

;;; --- operator builtins (SPEC §7) ---------------------------------------------
;;; Operators lower to these; each is inline-declaimed so SBCL specializes
;;; where declarations allow, and each panics in Sputter prose on type abuse
;;; (a raw CL type error reaching the user would breach the Waterline).

(declaim (inline sput-bool-of check-numbers))

(defun sput-bool-of (generalized)
  "CL generalized boolean -> Sputter boolean (comparisons must never return
CL NIL, which is Sputter's nil, not false)."
  (if generalized t +sput-false+))

(defun check-numbers (op a b)
  (unless (and (numberp a) (numberp b))
    (rt-panic "`~a` needs numbers, got ~a and ~a" op (show-value a) (show-value b))))

(declaim (inline sput-add sput-sub sput-mul sput-lt sput-le sput-gt sput-ge))

(defun sput-add (a b) (check-numbers "+" a b) (+ a b))
(defun sput-sub (a b) (check-numbers "-" a b) (- a b))
(defun sput-mul (a b) (check-numbers "*" a b) (* a b))

(defun sput-div (a b)
  "Integer/integer division truncates (Zig); anything float divides exactly."
  (check-numbers "/" a b)
  (when (zerop b) (rt-panic "division by zero"))
  (if (and (integerp a) (integerp b))
      (values (truncate a b))
      (/ a b)))

(defun sput-rem (a b)
  "Truncated remainder (C/Zig `%`; see DECISIONS.md on rem vs mod)."
  (check-numbers "%" a b)
  (when (zerop b) (rt-panic "division by zero in `%`"))
  (rem a b))

(defun sput-neg (a)
  (unless (numberp a)
    (rt-panic "unary `-` needs a number, got ~a" (show-value a)))
  (- a))

(defun sput-lt (a b) (check-numbers "<" a b) (sput-bool-of (< a b)))
(defun sput-le (a b) (check-numbers "<=" a b) (sput-bool-of (<= a b)))
(defun sput-gt (a b) (check-numbers ">" a b) (sput-bool-of (> a b)))
(defun sput-ge (a b) (check-numbers ">=" a b) (sput-bool-of (>= a b)))

(defun sput-not (a)
  (if (truthy a) +sput-false+ t))

;;; --- tagged values and records (SPEC §5.5) -------------------------------------

(defstruct (tagged (:constructor %make-tagged (tag vals)))
  (tag nil :type keyword)
  (vals #() :type simple-vector))

(defmethod print-object ((x tagged) stream)
  ;; Host-level repr only; the user-facing renderer is `show` (I2).
  (print-unreadable-object (x stream)
    (format stream "sput-tagged ~a/~d" (tagged-tag x) (length (tagged-vals x)))))

(defun make-tagged (tag &rest vals)
  (%make-tagged tag (coerce vals 'simple-vector)))

;; Records are hash tables with keyword keys (SPEC §5.5), immutable by fiat.
;; Insertion order is remembered under a hidden uninterned-symbol key so
;; `show`/`dump` render fields in source order (keywords never collide with
;; an uninterned symbol, and every rt accessor filters it).
(defvar +record-order+ (make-symbol "RECORD-ORDER"))

(defun make-record (&rest kvs)
  (let ((r (make-hash-table :test 'eq))
        (order '()))
    (loop for (k v) on kvs by #'cddr
          do (check-type k keyword)
             (unless (nth-value 1 (gethash k r))
               (push k order))
             (setf (gethash k r) v))
    (setf (gethash +record-order+ r) (nreverse order))
    r))

(defun record-p (x) (hash-table-p x))

(defun record-keys (r)
  (gethash +record-order+ r))

(defun record-ref (r k)
  (gethash k r))

(defun record-has-p (r k)
  (and (keywordp k) (nth-value 1 (gethash k r))))

(defun sput-equal (a b)
  "Structural `==` (SPEC §5.5): numbers by =, strings by string=, atoms and
booleans by identity, lists/records/tagged/nodes recursively (node meta is
provenance, not identity). Returns a CL generalized boolean; sput-eq wraps."
  (cond ((and (numberp a) (numberp b)) (= a b))
        ((and (stringp a) (stringp b)) (string= a b))
        ((and (consp a) (consp b))
         (and (sput-equal (car a) (car b)) (sput-equal (cdr a) (cdr b))))
        ((and (tagged-p a) (tagged-p b))
         (and (eq (tagged-tag a) (tagged-tag b))
              (= (length (tagged-vals a)) (length (tagged-vals b)))
              (every #'sput-equal (tagged-vals a) (tagged-vals b))))
        ((and (record-p a) (record-p b))
         (let ((ka (record-keys a)) (kb (record-keys b)))
           (and (= (length ka) (length kb))
                (every (lambda (k)
                         (and (record-has-p b k)
                              (sput-equal (record-ref a k) (record-ref b k))))
                       ka))))
        ((and (node-p a) (node-p b))
         (and (eq (node-head a) (node-head b))
              (= (length (node-args a)) (length (node-args b)))
              (every #'sput-equal (node-args a) (node-args b))))
        (t (eq a b))))

(defun sput-eq (a b) (sput-bool-of (sput-equal a b)))
(defun sput-ne (a b) (sput-bool-of (not (sput-equal a b))))

(defun sput-concat (a b)
  "`++` concatenates strings with strings and lists with lists (SPEC §5.3)."
  (cond ((and (stringp a) (stringp b)) (concatenate 'string a b))
        ((and (listp a) (listp b)) (append a b))
        (t (rt-panic "`++` concatenates strings or lists, got ~a and ~a"
                     (show-value a) (show-value b)))))

;;; --- data access (minimal in M2; records/tagged complete it in M3) -----------

(defun sput-field (obj name)
  (cond ((record-p obj)
         (multiple-value-bind (v found) (gethash name obj)
           (if found v (rt-panic "no field .~a on ~a" (symbol-name name)
                                 (show-value obj)))))
        ((tagged-p obj)
         (if (eq name :|tag|)
             (tagged-tag obj)
             (rt-panic "tagged values expose .tag, not .~a" (symbol-name name))))
        ((node-p obj)
         (case name
           (:|head| (head-atom (node-head obj)))
           (:|args| (node-args obj))
           (:|meta| (sput-meta obj))
           (t (rt-panic "nodes expose .head, .meta, and .args, not .~a"
                        (symbol-name name)))))
        (t (rt-panic "~a has no fields (wanted .~a)"
                     (show-value obj) (symbol-name name)))))

;;; --- list support (spread, for..in, match) -------------------------------------

(defun sput-list-append (&rest segments)
  "Spread desugar: every segment must be a list."
  (dolist (s segments)
    (unless (listp s)
      (rt-panic "cannot spread ~a into a list" (show-value s))))
  (apply #'append segments))

(defun sput-check-list (x)
  (unless (listp x)
    (rt-panic "for..in iterates lists, got ~a" (show-value x)))
  x)

(defun rt-no-match (v &optional file line col)
  (sput-panic (format nil "no switch arm matched ~a" (show-value v))
              :file file :line line :col col))

(defun match-fields-p (x)
  "Record patterns apply to records and to nodes (SPEC §4.4)."
  (or (record-p x) (node-p x)))

(defun match-field-has-p (x k)
  (if (record-p x)
      (record-has-p x k)
      (member k '(:|head| :|args| :|meta|) :test #'eq)))

;;; --- list/collection builtins (prelude-registered) -------------------------------

(defun sput-map (xs f)
  (sput-check-listy "map" xs)
  (mapcar (lambda (x) (funcall f x)) xs))

(defun sput-filter (xs f)
  (sput-check-listy "filter" xs)
  (remove-if-not (lambda (x) (truthy (funcall f x))) xs))

(defun sput-reduce (xs init f)
  (sput-check-listy "reduce" xs)
  (let ((acc init))
    (dolist (x xs acc)
      (setf acc (funcall f acc x)))))

(defun sput-sum (xs)
  (sput-check-listy "sum" xs)
  (let ((total 0))
    (dolist (x xs total)
      (setf total (sput-add total x)))))

(defun sput-len (x)
  (cond ((listp x) (length x))
        ((stringp x) (length x))
        ((record-p x) (length (record-keys x)))
        ((tagged-p x) (length (tagged-vals x)))
        (t (rt-panic "len wants a list, string, record, or tagged value, got ~a"
                     (show-value x)))))

(defun sput-push (xs x)
  "Persistent list append for pipeline-friendly code: `xs |> push(x)`."
  (sput-check-listy "push" xs)
  (append xs (list x)))

(defun sput-str (x)
  "Runtime string conversion. Strings are already strings; other values use
the same Sputter literal renderer as `show`."
  (if (stringp x) x (show-value x)))

(defun sput-atom-name (x)
  "Internal M8 helper: return an atom keyword's bare language spelling."
  (unless (keywordp x)
    (rt-panic "atom-name wants an atom, got ~a" (show-value x)))
  (symbol-name x))

(defun sput-check-listy (who xs)
  (unless (listp xs)
    (rt-panic "~a wants a list, got ~a" who (show-value xs))))

;;; --- nodes as runtime values (SPEC §4.4, M4) --------------------------------------

(defun lift-splice (v)
  "Validate a bare-name splice inside a quote: nodes splice as nodes,
scalars lift to literals; nothing else fits in a syntax tree."
  (if (or (node-p v) (scalarp v))
      v
      (rt-panic "cannot splice ~a into a quote (nodes and scalars only; `...name` splices lists of nodes, M6)"
                (show-value v))))

;;; --- hygiene marks (SPEC §5.8.5) ---------------------------------------------------
;;; The expander binds *expansion-mark* around each macro invocation;
;;; template instantiation stamps it into meta.scopes of template-literal
;;; nodes. Spliced values pass through untouched.

(defvar *expansion-mark* nil)

(defun marked-meta (m)
  (%make-meta :file (meta-file m) :line (meta-line m) :col (meta-col m)
              :scopes (cons *expansion-mark* (meta-scopes m))
              :synthetic (meta-synthetic m)))

(defun template-instantiate (x)
  "Deep-copy a quoted template subtree, stamping the current expansion mark.
Outside an expansion this is the identity (term-level quotes are unmarked)."
  (if (null *expansion-mark*)
      x
      (labels ((walk (e)
                 (if (node-p e)
                     (make-node (node-head e)
                                (mapcar #'walk (node-args e))
                                :meta (marked-meta (node-meta e)))
                     e)))
        (walk x))))

(defun %rebuild-node (head meta args)
  "Quote-lowering support: rebuild a node around spliced children (marked
when inside an expansion — the rebuilt node is template text)."
  (dolist (a args)
    (unless (or (node-p a) (scalarp a) (token-group-p a))
      (rt-panic "cannot splice ~a into a quote" (show-value a))))
  (make-node head args
             :meta (if *expansion-mark* (marked-meta meta) meta)))

(defun %make-marked-ident (name meta)
  "raw(name): a template-literal identifier, hygienic like any other
template text (SPEC §5.8.3)."
  (make-ident name :meta (if *expansion-mark* (marked-meta meta) meta)))

(defun %make-unmarked-ident (name meta)
  "inject(name): an identifier with no marks — it resolves at the call site
(anaphora, SPEC §5.8.3)."
  (make-ident name :meta (%make-meta :file (meta-file meta)
                                     :line (meta-line meta)
                                     :col (meta-col meta)
                                     :synthetic (meta-synthetic meta))))

;;; --- comptime ident builders (SPEC §5.8.3 bind-first idiom) -------------------------

(defvar *gensym-ident-counter* 0)

(defun sput-concat-ident (&rest parts)
  "concat_ident: strings, atoms, and identifier nodes concatenate into a
fresh (unmarked) identifier node."
  (make-ident
   (name-keyword
    (apply #'concatenate 'string
           (mapcar (lambda (p)
                     (cond ((stringp p) p)
                           ((keywordp p) (symbol-name p))
                           ((ident-node-p p) (symbol-name (ident-name p)))
                           (t (rt-panic "concat_ident wants strings, atoms, or identifiers, got ~a"
                                        (show-value p)))))
                   parts)))))

(defun sput-gensym-ident (base)
  (let ((base-name (cond ((stringp base) base)
                         ((keywordp base) (symbol-name base))
                         ((ident-node-p base) (symbol-name (ident-name base)))
                         (t (rt-panic "gensym_ident wants a string, atom, or identifier, got ~a"
                                      (show-value base))))))
    (make-ident (name-keyword (format nil "~a__g~d" base-name
                                      (incf *gensym-ident-counter*))))))

(defun check-node (who x)
  (unless (node-p x)
    (rt-panic "~a wants a node, got ~a" who (show-value x)))
  x)

(defun sput-head (n) (head-atom (node-head (check-node "head" n))))

(defun sput-args (n) (node-args (check-node "args" n)))

(defun sput-meta (n)
  "Node meta exposed as a record (SPEC §4.1's .{ .file, .line, .col,
.scopes, .synthetic })."
  (let ((m (node-meta (check-node "meta" n))))
    (make-record :|file| (meta-file m)
                 :|line| (meta-line m)
                 :|col| (meta-col m)
                 :|scopes| (copy-list (meta-scopes m))
                 :|synthetic| (if (meta-synthetic m) t +sput-false+))))

(defun sput-node-ctor (head args)
  "node(head, args) — meta synthesized (SPEC §4.4). The atom `.add` maps to
the internal head keyword."
  (unless (keywordp head)
    (rt-panic "node() wants an atom head, got ~a" (show-value head)))
  (unless (listp args)
    (rt-panic "node() wants a list of args, got ~a" (show-value args)))
  (dolist (a args)
    (unless (or (node-p a) (scalarp a))
      (rt-panic "node args must be nodes or scalars, got ~a" (show-value a))))
  (make-node (atom-head head) args))

(defun sput-prewalk (x f)
  "prewalk with runtime validation: F must return nodes or scalars.
Macro-call token payloads are opaque — they pass through without visiting F."
  (if (token-group-p x)
      x
      (let ((y (funcall f x)))
        (unless (or (node-p y) (scalarp y))
          (rt-panic "prewalk fn returned ~a (nodes and scalars only)" (show-value y)))
        (if (node-p y)
            (make-node (node-head y)
                       (mapcar (lambda (a) (sput-prewalk a f)) (node-args y))
                       :meta (node-meta y))
            y))))

(defun sput-postwalk (x f)
  (if (token-group-p x)
      x
      (let* ((walked (if (node-p x)
                         (make-node (node-head x)
                                    (mapcar (lambda (a) (sput-postwalk a f))
                                            (node-args x))
                                    :meta (node-meta x))
                         x))
             (y (funcall f walked)))
        (unless (or (node-p y) (scalarp y))
          (rt-panic "postwalk fn returned ~a (nodes and scalars only)" (show-value y)))
        y)))

(defun sput-print (x)
  "print(node) -> str: canonical surface syntax (SPEC §5.7)."
  (unless (or (node-p x) (scalarp x))
    (rt-panic "print wants a node, got ~a" (show-value x)))
  (print-node x))

(defun sput-dump (x)
  "dump(node) -> str: the node as a Sputter data literal (SPEC §5.7)."
  (unless (or (node-p x) (scalarp x))
    (rt-panic "dump wants a node, got ~a" (show-value x)))
  (dump-string x))

(defun sput-index (obj i)
  (cond ((and (listp obj) (integerp i))
         (let ((len (length obj)))
           (if (and (<= 0 i) (< i len))
               (nth i obj)
               (rt-panic "index ~d is out of bounds for a list of ~d" i len))))
        ((listp obj) (rt-panic "list indexes must be integers, got ~a" (show-value i)))
        (t (rt-panic "cannot index into ~a" (show-value obj)))))

;;; --- test registry (M7 prelude macro target) -----------------------------------

(defvar *registered-tests* '()
  "A fresh `sput test` session collects (name . thunk) pairs here.")

(defun reset-registered-tests ()
  (setf *registered-tests* '()))

(defun registered-tests ()
  (nreverse *registered-tests*))

(defun sput-register-test (name thunk)
  "Runtime target for the prelude `test` macro."
  (unless (stringp name)
    (rt-panic "test name must be a string, got ~a" (show-value name)))
  (unless (functionp thunk)
    (rt-panic "test body did not compile to a thunk"))
  (push (cons name thunk) *registered-tests*)
  nil)

;;; --- user-facing builtins (registered by the prelude) --------------------------

(defun sput-show (v)
  (show-value v))

(defun sput-println (v)
  "println: strings print raw; everything else through `show` (§5.7)."
  (write-string (if (stringp v) v (show-value v)) *standard-output*)
  (terpri *standard-output*)
  nil)

(defun sput-panic-builtin (msg)
  (sput-panic (if (stringp msg) msg (show-value msg))))
