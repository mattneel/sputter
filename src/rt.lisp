;;;; rt.lisp — runtime support (SPEC §3.3, §5.5). M0 ships only the value
;;;; model's foundation: the false singleton and truthiness. The rest (panic,
;;;; sput-equal, records, tagged values, field access) arrives with M2/M3.

(in-package #:sputter.impl)

;; `false` is a distinct singleton (Elixir model: nil /= false, both falsy) —
;; CL NIL cannot play both roles (SPEC §5.5, §13.2). DEFVAR, not DEFPARAMETER:
;; reloading this file must not mint a second false.
(defstruct (sput-false (:constructor %make-sput-false)))

(defvar +sput-false+ (%make-sput-false)
  "The unique runtime value of the Sputter literal `false`.")

(defmethod print-object ((x sput-false) stream)
  ;; Host-level debugging repr only; the user-facing renderer is `show` (I2).
  (print-unreadable-object (x stream)
    (write-string "sput-false" stream)))

(declaim (inline truthy))
(defun truthy (x)
  "Sputter truthiness: `false` and `nil` are falsy; everything else is truthy."
  (not (or (null x) (sput-false-p x))))

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

(define-condition sputter-panic (sputter-error) ()
  (:documentation "Runtime panic: `unreachable`, no-match, bad field... (M2+)."))

(defun sputter-error-at (kind file line col fmt &rest args)
  "Signal a Sputter condition of KIND with a source span."
  (error kind :message (apply #'format nil fmt args)
              :file file :line line :col col))

(defun render-sputter-error (c stream)
  "The §8 rendering contract: `error: <message>` plus a span when known."
  (format stream "error: ~a~%" (sputter-error-message c))
  (when (and (sputter-error-file c) (sputter-error-line c))
    (format stream "  at ~a:~d~@[:~d~]~%"
            (sputter-error-file c) (sputter-error-line c)
            (sputter-error-col c))))
