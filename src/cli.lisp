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
  build-image [path]   save a preloaded SBCL core (default: bin/sput.image)
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
            ;; Parsing is macro-aware (define-before-use); each file must start
            ;; from a clean registry so in-process CLI calls cannot leak macros
            ;; into later fmt checks.
            (reset-globals)
            (let ((src (read-source-file f)))
              (unless (string= src (print-module (parse-module src :file f)))
                (push f dirty)
                (format *error-output* "would reformat: ~a~%" f))))
          (if dirty 1 0))
        (progn
          (unless (= 1 (length files))
            (error 'sputter-error
                   :message "`sput fmt` writes to stdout; pass exactly one file (or use --check)"))
          (reset-globals)
          (write-string (print-module (parse-file (first files)))
                        *standard-output*)
          0))))

(defun cmd-expand (args)
  (let ((flags (remove-if-not #'flag-p args))
        (files (remove-if #'flag-p args)))
    (check-flags flags '("--dump") "expand")
    (unless (= 1 (length files))
      (error 'sputter-error :message "`sput expand` needs exactly one file"))
    ;; Macro signatures are parser state; do not let a prior in-process command
    ;; influence what this file treats as a macro invocation.
    (reset-globals)
    (let* ((expanded (expand-module (parse-file (first files))))
           (dump (member "--dump" flags :test #'string=))
           (has-macro-def
             (some #'macro-def-node-p expanded))
           (chunks
             (mapcar (lambda (form)
                       (if (macro-def-node-p form)
                           ;; §9: macro definitions print as comments noting
                           ;; they were consumed
                           (format nil "// macro `~a` consumed by expansion~%"
                                   (symbol-name (ident-name (first (node-args form)))))
                           (if dump
                               (format nil "~a~%" (dump-string form))
                               (print-module (list form)))))
                     expanded)))
      (if dump
          ;; Keep M4's dump contract: one dump per top-level form, blank-line separated.
          (format *standard-output* "~{~a~^~%~}" chunks)
          (if has-macro-def
              ;; Macro definitions are consumed and rendered as comments (§9), so
              ;; chunk rendering is necessary for mixed node/comment streams.
              (format *standard-output* "~{~a~}" chunks)
              ;; Ordinary macro-free expansion stays on the whole-module printer so
              ;; source-driven blank-line preservation remains intact.
              (write-string (print-module expanded) *standard-output*))))
    0))

(defun macro-def-node-p (form)
  (and (node-p form) (member (node-head form) '(:macro_fn_def :macro_def))))

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

;;; --- test runner (SPEC §9, M7) ----------------------------------------------------

(defun render-condition-text (condition)
  (with-output-to-string (s)
    (cond
      ((typep condition 'sputter-error)
       (render-sputter-error condition s))
      ((typep condition 'storage-condition)
       (format s "error: the host ran out of stack or memory processing this input~%"))
      (t
       (unless (render-host-condition condition s)
         (format s "error: internal error in the sput toolchain (this is a bug)~%"))))))

(defun write-indented-lines (text &optional (stream *standard-output*))
  (dolist (line (uiop:split-string text :separator '(#\Newline)))
    (unless (string= line "")
      (format stream "  ~a~%" line))))

(defun run-one-registered-test (name thunk)
  "Run one collected test thunk. Returns true on pass, false on failure."
  (handler-case
      (progn
        (funcall thunk)
        (format *standard-output* "ok - ~a~%" name)
        t)
    (sputter-error (c)
      (format *standard-output* "not ok - ~a~%" name)
      (write-indented-lines (render-condition-text c))
      nil)
    (storage-condition (c)
      (format *standard-output* "not ok - ~a~%" name)
      (write-indented-lines (render-condition-text c))
      nil)
    (error (c)
      (format *standard-output* "not ok - ~a~%" name)
      (write-indented-lines (render-condition-text c))
      nil)))

(defun cmd-test (args)
  (let ((flags (remove-if-not #'flag-p args))
        (files (remove-if #'flag-p args)))
    (check-flags flags '() "test")
    (when (null files)
      (error 'sputter-error :message "`sput test` needs at least one file"))
    (reset-globals)
    ;; Loading files registers test thunks via the prelude `test` macro.
    (dolist (f files)
      (run-file f))
    (let ((tests (registered-tests))
          (passed 0)
          (failed 0))
      (dolist (test tests)
        (destructuring-bind (name . thunk) test
          (if (run-one-registered-test name thunk)
              (incf passed)
              (incf failed))))
      (format *standard-output* "~d test~:p, ~d passed, ~d failed~%"
              (+ passed failed) passed failed)
      (if (zerop failed) 0 1))))

;;; --- build-image (SPEC §9, M7) ----------------------------------------------------

(defun default-image-path ()
  (let ((root (or (host-getenv "SPUTTER_ROOT")
                  (namestring (uiop:getcwd)))))
    (uiop:native-namestring
     (merge-pathnames "bin/sput.image"
                      (uiop:ensure-directory-pathname root)))))

(defun cmd-build-image (args)
  (let ((flags (remove-if-not #'flag-p args))
        (files (remove-if #'flag-p args)))
    (check-flags flags '() "build-image")
    (when (> (length files) 1)
      (error 'sputter-error :message "`sput build-image` takes at most one output path"))
    (let ((path (or (first files) (default-image-path))))
      (format *standard-output* "saving image: ~a~%" path)
      (finish-output *standard-output*)
      (host-save-image path)
      0)))

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
       (with-error-boundary () (cmd-test (rest argv))))
      ((string= command "build-image")
       (with-error-boundary () (cmd-build-image (rest argv))))
      (t
       (format *error-output* "error: unknown command `~a`~%~%" command)
       (write-string +usage+ *error-output*)
       2))))

(defun cli-main ()
  "Entry point for bin/sput (script shim and saved image alike)."
  (host-exit (cli-dispatch (host-argv))))
