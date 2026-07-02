;;;; host.lisp — SBCL-specific calls, quarantined (SPEC §2, §3.3).
;;;; Every host-specific call in the implementation goes through here so the
;;;; host surface stays greppable. SBCL-only is acceptable per the spec.

(in-package #:sputter.impl)

;; The one documented optimize declaim (SPEC §7): SBCL performs tail-call
;; elimination under its default policy; keeping DEBUG at 1 preserves that for
;; emitted user code compiled into this image. Sputter documents self-recursion
;; depth as implementation-defined.
(declaim (optimize (speed 1) (safety 1) (debug 1)))

(defun host-argv ()
  "Command-line arguments handed to the sput CLI (program/script name excluded)."
  (rest sb-ext:*posix-argv*))

(defun host-getenv (name)
  (sb-ext:posix-getenv name))

(defun host-exit (code)
  (sb-ext:exit :code code :abort nil))

(defun host-print-backtrace (condition stream)
  "Raw host condition + backtrace, for SPUTTER_HOST_BACKTRACE=1 only (SPEC §8)."
  (format stream "~a~%" condition)
  (sb-debug:print-backtrace :stream stream :count 50))
