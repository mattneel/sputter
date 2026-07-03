;;;; host.lisp — SBCL-specific calls, quarantined (SPEC §2, §3.3).
;;;; Every host-specific call in the implementation goes through here so the
;;;; host surface stays greppable. SBCL-only is acceptable per the spec.

(in-package #:sputter.impl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-introspect))

;; The one documented optimize declaim (SPEC §7): SBCL performs tail-call
;; elimination under its default policy; keeping DEBUG at 1 preserves that for
;; emitted user code compiled into this image. Sputter documents self-recursion
;; depth as implementation-defined.
(declaim (optimize (speed 1) (safety 1) (debug 1)))

(defun host-argv ()
  "Command-line arguments handed to the sput CLI (program/script name excluded)."
  (let ((args (rest sb-ext:*posix-argv*)))
    ;; Saved images are launched as `sbcl --core sput.image -- ...`; the
    ;; separator is host ceremony, not a Sputter argument.
    (if (and args (string= (first args) "--"))
        (rest args)
        args)))

(defun host-getenv (name)
  (sb-ext:posix-getenv name))

(defun host-exit (code)
  (sb-ext:exit :code code :abort nil))

(defun host-save-image (path)
  "Save the already-loaded toolchain as an SBCL core and exit."
  (sb-ext:save-lisp-and-die path :toplevel #'cli-main :executable nil))

(defun host-print-backtrace (condition stream)
  "Raw host condition + backtrace, for SPUTTER_HOST_BACKTRACE=1 only (SPEC §8)."
  (format stream "~a~%" condition)
  (sb-debug:print-backtrace :stream stream :count 50))

(defun host-backtrace-symbols ()
  "Function-name symbols currently on the host stack, innermost first.
Best-effort: used only to resolve user frames through the span table."
  (let ((frames (ignore-errors (sb-debug:list-backtrace :count 80))))
    (loop for f in frames
          for head = (if (consp f) (first f) f)
          when (symbolp head) collect head)))

(defun host-eval (form)
  "Evaluate an emitted CL form in-image. Host compile chatter is muffled:
derived-type warnings surface later (§7 deferral, see DECISIONS.md)."
  (handler-bind ((warning #'muffle-warning))
    (eval form)))

(declaim (ftype (function (symbol) string) demangle-symbol)) ; emit.lisp

(defun host-function-info (f)
  "(values demangled-name-or-nil arity-or-nil), best-effort, for `show`."
  (let* ((raw (ignore-errors (sb-kernel:%fun-name f)))
         (name (and (symbolp raw)
                    (symbol-package raw)
                    (eq (symbol-package raw) (find-package '#:sputter))
                    (demangle-symbol raw)))
         (arity (ignore-errors
                  (let ((ll (sb-introspect:function-lambda-list f)))
                    (or (position-if (lambda (s)
                                       (and (symbolp s)
                                            (plusp (length (symbol-name s)))
                                            (char= (char (symbol-name s) 0) #\&)))
                                     ll)
                        (length ll))))))
    (values name arity)))
