;;;; prelude.lisp — stage-0 stdlib registration (SPEC §3.3). The prelude is
;;;; the set of globals every Sputter program sees. M2 ships the seed
;;;; (println/show/panic); the list grows in M3/M7 and later migrates to
;;;; prelude.sput (stage 1).

(in-package #:sputter.impl)

(defparameter +prelude-builtins+
  '(("println" . sput-println)
    ("show" . sput-show)
    ("panic" . sput-panic-builtin))
  "Sputter name -> implementation symbol. Every entry is a function (values,
not macros — the cl. bridge and the prelude are functions-only, SPEC §7).")

(defun register-prelude ()
  (loop for (name . sym) in +prelude-builtins+
        do (register-global-fn (name-keyword name) sym)))

(defun reset-globals ()
  "Fresh image state for a run/repl session (SPEC §9). Stale DEFUNs from a
previous in-process session stay fbound but become unreachable: the lowerer
resolves names through *globals* only."
  (clrhash *globals*)
  (clrhash *global-values*)
  (clrhash *span-table*)
  (register-prelude))

(register-prelude)