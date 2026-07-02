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

(defun render-host-condition (c stream)
  "Render recognizable host conditions escaping *user code* in Sputter prose.
Returns true when handled; anything unrecognized is an implementation bug."
  (typecase c
    ((or undefined-function unbound-variable)
     (let ((name (cell-error-name c)))
       (when (and (symbolp name)
                  (eq (symbol-package name) (find-package '#:sputter)))
         (format stream "error: `~a` is not defined~%" (demangle-symbol name))
         t)))
    (type-error
     (format stream "error: type mismatch: expected ~a, got ~a~%"
             (sputter-type-name (type-error-expected-type c))
             (show-value (type-error-datum c)))
     t)
    (t nil)))

(defun call-with-error-boundary (thunk)
  (handler-case (funcall thunk)
    (sputter-error (c)
      (render-sputter-error c *error-output*)
      (maybe-host-backtrace c)
      1)
    (error (c)
      (unless (render-host-condition c *error-output*)
        (format *error-output*
                "error: internal error in the sput toolchain (this is a bug — set SPUTTER_HOST_BACKTRACE=1 for details)~%"))
      (maybe-host-backtrace c)
      1)
    ;; storage-condition is NOT an error subtype: without this clause a
    ;; control-stack blowup would reach the user as a raw SBCL dump (I2)
    (storage-condition (c)
      (declare (ignorable c))
      (format *error-output*
              "error: the host ran out of stack or memory processing this input~%")
      1)))

(defmacro with-error-boundary (() &body body)
  `(call-with-error-boundary (lambda () ,@body)))

;;; --- shared helpers ---------------------------------------------------------------

(defun flag-p (s) (a:starts-with-subseq "--" s))

(defun read-source-file (path)
  (unless (uiop:file-exists-p path)     ; directories are not source files
    (error 'sputter-error :message (format nil "no such file: ~a" path)))
  (handler-case (uiop:read-file-string path :external-format :utf-8)
    (error ()
      (error 'sputter-error
             :message (format nil "cannot read ~a as UTF-8 text" path)))))

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

(defun cmd-run (args)
  (let ((flags (remove-if-not #'flag-p args))
        (files (remove-if #'flag-p args)))
    (check-flags flags '("--echo-last") "run") ; --echo-last: golden-harness tool
    (when (null files)
      (error 'sputter-error :message "`sput run` needs at least one file"))
    (reset-globals)
    (let ((last nil))
      (dolist (f files)
        (setf last (run-file f)))
      (when (member "--echo-last" flags :test #'string=)
        (format *standard-output* "~a~%" (show-value last))))
    0))

;;; --- repl (SPEC §9) ---------------------------------------------------------------

(defun repl-entry-complete-p (src)
  "Balanced-brace heuristic: an entry is complete when it lexes and all
brackets are balanced. A lex error counts as complete — it surfaces now."
  (let ((tokens (handler-case (lex src :file "<repl>")
                  (sputter-error () (return-from repl-entry-complete-p t)))))
    (let ((depth 0))
      (loop for tok across tokens
            do (case (token-type tok)
                 ((:lbrace :lparen :lbracket :dot-lbrace) (incf depth))
                 ((:rbrace :rparen :rbracket) (decf depth))))
      (<= depth 0))))

(defun read-repl-entry (in out)
  "Read one balanced-brace multiline entry. NIL at end of input."
  (format out "sput> ")
  (force-output out)
  (let ((acc nil))
    (loop
      (let ((line (read-line in nil nil)))
        (cond ((null line)
               (return (and acc (format nil "~{~a~^~%~}" (reverse acc)))))
              (t
               (push line acc)
               (let ((src (format nil "~{~a~^~%~}" (reverse acc))))
                 (if (repl-entry-complete-p src)
                     (return src)
                     (progn (format out "  ... ")
                            (force-output out))))))))))

(defun eval-repl-entry (src)
  "Parse one REPL entry (expression or statements) and evaluate it.
Returns the value to echo."
  (let ((parsed (handler-case (list :expr (parse-expression src :file "<repl>"))
                  (sputter-parse-error ()
                    (list :module (parse-module src :file "<repl>"))))))
    (ecase (first parsed)
      (:expr (eval-top-form (second parsed)))
      (:module
       (let ((value nil))
         (dolist (s (second parsed) value)
           (setf value (eval-top-form s))))))))

(defun cmd-repl (args)
  (declare (ignore args))
  (reset-globals)
  (format *standard-output* "Sputter v0.1 — Ctrl-D exits; rlwrap recommended.~%")
  (loop
    (let ((entry (read-repl-entry *standard-input* *standard-output*)))
      (when (null entry) (return 0))
      (when (plusp (length (string-trim '(#\Space #\Tab #\Newline) entry)))
        (handler-case
            (format *standard-output* "~a~%" (show-value (eval-repl-entry entry)))
          (sputter-error (c)
            (render-sputter-error c *error-output*)
            (maybe-host-backtrace c))
          (error (c)
            (unless (render-host-condition c *error-output*)
              (format *error-output*
                      "error: internal error in the sput toolchain (this is a bug)~%"))
            (maybe-host-backtrace c))
          (storage-condition (c)
            (declare (ignorable c))
            (format *error-output*
                    "error: the host ran out of stack or memory processing this input~%")))))))

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
      ((string= command "run")
       (with-error-boundary () (cmd-run (rest argv))))
      ((string= command "repl")
       (with-error-boundary () (cmd-repl (rest argv))))
      ((string= command "test")
       (format *error-output*
               "error: `sput test` is not implemented yet (it arrives with M7)~%")
       1)
      (t
       (format *error-output* "error: unknown command `~a`~%~%" command)
       (write-string +usage+ *error-output*)
       2))))

(defun cli-main ()
  "Entry point for bin/sput."
  (host-exit (cli-dispatch (host-argv))))
