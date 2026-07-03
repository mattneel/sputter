;;;; prelude.lisp — stage-0 stdlib registration (SPEC §3.3). The prelude is
;;;; the set of globals every Sputter program sees. M2 ships the seed
;;;; (println/show/panic); the list grows in M3/M7 and later migrates to
;;;; prelude.sput (stage 1).

(in-package #:sputter.impl)

(defparameter +prelude-builtins+
  '(("println" . sput-println)
    ("show" . sput-show)
    ("panic" . sput-panic-builtin)
    ;; collections (SPEC §11 M7 names them; the §10.1 tour needs them now)
    ("map" . sput-map)
    ("filter" . sput-filter)
    ("reduce" . sput-reduce)
    ("sum" . sput-sum)
    ("len" . sput-len)
    ("push" . sput-push)
    ("str" . sput-str)
    ;; internal helper for the M8 stage-1 printer module.
    ("__sput_atom_name" . sput-atom-name)
    ;; nodes as values (SPEC §4.4, M4)
    ("head" . sput-head)
    ("args" . sput-args)
    ("meta" . sput-meta)
    ("node" . sput-node-ctor)
    ("prewalk" . sput-prewalk)
    ("postwalk" . sput-postwalk)
    ("print" . sput-print)
    ("dump" . sput-dump)
    ;; internal target for the M7 `test` macro; the leading underscores keep
    ;; it out of the documented prelude but it is still plain Sputter surface.
    ("__sput_register_test" . sput-register-test)
    ;; comptime ident builders (SPEC §5.8.3)
    ("concat_ident" . sput-concat-ident)
    ("gensym_ident" . sput-gensym-ident))
  "Sputter name -> implementation symbol. Every entry is a function (values,
not macros — the cl. bridge and the prelude are functions-only, SPEC §7).")

(defparameter +prelude-macro-sources+
  (list
   "macro fn check(cond: expr) expr {
    let text = print(cond);
    quote {
        if !cond {
            panic(\"check failed: \" ++ text)
        }
    }
}
"
   "macro test {
    { test name: literal { ...body: stmt } } =>
        { __sput_register_test(name, fn() { ...body }) },
}
")
  "Prelude macros are written in Sputter surface syntax, then installed into
the stage-0 registry on each fresh session (M7 dogfood).")

(defun register-prelude ()
  (loop for (name . sym) in +prelude-builtins+
        do (register-global-fn (name-keyword name) sym))
  (register-prelude-macros))

(defun register-prelude-macros ()
  (dolist (src +prelude-macro-sources+)
    (dolist (form (parse-module src :file "<prelude>"))
      (eval-top-form form))))

(defun reset-globals ()
  "Fresh image state for a run/repl session (SPEC §9). Stale DEFUNs from a
previous in-process session stay fbound but become unreachable: the lowerer
resolves names through *globals* only."
  (clrhash *globals*)
  (clrhash *global-values*)
  (clrhash *span-table*)
  (reset-registered-tests)
  (reset-macros)
  (register-prelude))

(register-prelude)
