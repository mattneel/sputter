;;;; rt.lisp — runtime support (SPEC §3.3, §5.5): conditions, panics with
;;;; frames, the operator builtins, equality, field/index access, and the
;;;; user-facing seed builtins. The false singleton and truthiness live in
;;;; node.lisp (the AST layer needs them first).

(in-package #:sputter.impl)

;; Forward declarations for later files (print.lisp, emit.lisp): these are
;; called at runtime only; the declaims keep the compiler quiet at load.
(declaim (ftype (function (t) string) show-value)
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
           (:|head| (node-head obj))
           (:|args| (node-args obj))
           (t (rt-panic "nodes expose .head and .args (.meta arrives in M4), not .~a"
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

(defun sput-check-listy (who xs)
  (unless (listp xs)
    (rt-panic "~a wants a list, got ~a" who (show-value xs))))

(defun sput-index (obj i)
  (cond ((and (listp obj) (integerp i))
         (let ((len (length obj)))
           (if (and (<= 0 i) (< i len))
               (nth i obj)
               (rt-panic "index ~d is out of bounds for a list of ~d" i len))))
        ((listp obj) (rt-panic "list indexes must be integers, got ~a" (show-value i)))
        (t (rt-panic "cannot index into ~a" (show-value obj)))))

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
