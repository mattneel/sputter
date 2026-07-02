;;;; cli.lisp — sput run|expand|fmt|repl|test (SPEC §9).
;;;; M1: fmt + expand (identity). run/repl land with M2, test with M7.
;;;; All conditions are caught here and rendered Sputter-side (SPEC §8).

(in-package #:sputter.impl)

(defparameter +usage+
  "Sputter — a C-family film deposited on a Lisp substrate.

usage: sput <command> [arguments]

commands:
  run file.sput...     compile + execute in a fresh image state
  expand file.sput     fully expand; print the module as surface syntax
                       (--dump prints data literals instead)
  fmt file.sput        parse + print canonically (--check for CI)
  repl                 interactive session (tip: rlwrap sput repl)
  test file.sput...    run `test \"name\" { ... }` blocks, report pass/fail
  help                 show this message
")

;;; --- the §8 boundary -----------------------------------------------------------

(defun host-backtrace-wanted-p ()
  (let ((v (host-getenv "SPUTTER_HOST_BACKTRACE")))
    (and v (string/= v "") (string/= v "0"))))

(defun maybe-host-backtrace (c)
  ;; The sole sanctioned I2 exception (SPEC §8): raw host details, printed
  ;; only on request and only *after* the Sputter rendering.
  (when (host-backtrace-wanted-p)
    (format *error-output* "~%--- host backtrace (SPUTTER_HOST_BACKTRACE) ---~%")
    (host-print-backtrace c *error-output*)))

(defun call-with-error-boundary (thunk)
  (handler-case (funcall thunk)
    (sputter-error (c)
      (render-sputter-error c *error-output*)
      (maybe-host-backtrace c)
      1)
    (error (c)
      (format *error-output*
              "error: internal error in the sput toolchain (this is a bug — set SPUTTER_HOST_BACKTRACE=1 for details)~%")
      (maybe-host-backtrace c)
      1)))

(defmacro with-error-boundary (() &body body)
  `(call-with-error-boundary (lambda () ,@body)))

;;; --- shared helpers ---------------------------------------------------------------

(defun flag-p (s) (a:starts-with-subseq "--" s))

(defun read-source-file (path)
  (unless (probe-file path)
    (error 'sputter-error :message (format nil "no such file: ~a" path)))
  (uiop:read-file-string path))

(defun check-flags (flags allowed command)
  (dolist (f flags)
    (unless (member f allowed :test #'string=)
      (error 'sputter-error
             :message (format nil "unknown flag `~a` for `sput ~a`" f command)))))

(defun parse-file (path)
  (parse-module (read-source-file path) :file path))

;;; --- commands -----------------------------------------------------------------------

(defun cmd-fmt (args)
  (let ((flags (remove-if-not #'flag-p args))
        (files (remove-if #'flag-p args)))
    (check-flags flags '("--check") "fmt")
    (when (null files)
      (error 'sputter-error :message "`sput fmt` needs a file"))
    (if (member "--check" flags :test #'string=)
        (let ((dirty '()))
          (dolist (f files)
            (let ((src (read-source-file f)))
              (unless (string= src (print-module (parse-module src :file f)))
                (push f dirty)
                (format *error-output* "would reformat: ~a~%" f))))
          (if dirty 1 0))
        (progn
          (unless (= 1 (length files))
            (error 'sputter-error
                   :message "`sput fmt` writes to stdout; pass exactly one file (or use --check)"))
          (write-string (print-module (parse-file (first files)))
                        *standard-output*)
          0))))

(defun cmd-expand (args)
  (let ((flags (remove-if-not #'flag-p args))
        (files (remove-if #'flag-p args)))
    (check-flags flags '("--dump") "expand")
    (when (member "--dump" flags :test #'string=)
      (error 'sputter-error
             :message "`sput expand --dump` arrives with M4; not supported yet"))
    (unless (= 1 (length files))
      (error 'sputter-error :message "`sput expand` needs exactly one file"))
    (write-string (print-module (expand-module (parse-file (first files))))
                  *standard-output*)
    0))

;;; --- dispatch ----------------------------------------------------------------------

(defun cli-dispatch (argv)
  "Dispatch one sput invocation; return the process exit code.
Everything written here is user-facing: Sputter prose only (I2)."
  (let ((command (first argv)))
    (cond
      ((or (null command)
           (member command '("help" "--help" "-h") :test #'string=))
       (write-string +usage+ *standard-output*)
       0)
      ((string= command "fmt")
       (with-error-boundary () (cmd-fmt (rest argv))))
      ((string= command "expand")
       (with-error-boundary () (cmd-expand (rest argv))))
      ((member command '("run" "repl" "test") :test #'string=)
       (format *error-output*
               "error: `sput ~a` is not implemented yet (it arrives with a later milestone)~%"
               command)
       1)
      (t
       (format *error-output* "error: unknown command `~a`~%~%" command)
       (write-string +usage+ *error-output*)
       2))))

(defun cli-main ()
  "Entry point for bin/sput."
  (host-exit (cli-dispatch (host-argv))))
