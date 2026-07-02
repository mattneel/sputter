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
