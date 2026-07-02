;;;; cli.lisp — sput run|expand|fmt|repl|test (SPEC §9).
;;;; M0: usage and dispatch skeleton; subcommands land with their milestones.

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

(defparameter +commands+ '("run" "expand" "fmt" "repl" "test"))

(defun cli-dispatch (argv)
  "Dispatch one sput invocation; return the process exit code.
Everything written here is user-facing: Sputter prose only (I2)."
  (let ((command (first argv)))
    (cond
      ((or (null command)
           (member command '("help" "--help" "-h") :test #'string=))
       (write-string +usage+ *standard-output*)
       0)
      ((member command +commands+ :test #'string=)
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
