;;;; expand.lisp — hygienic macro expansion (SPEC §5.8). M1 ships the driver
;;;; skeleton and its negative-space assertions; macro machinery lands in M5/M6.

(in-package #:sputter.impl)

(defparameter +macro-space-heads+
  '(:macro_call :quote :splice_seq :hole :raw :inject :insert
    :macro_def :macro_fn_def)
  "Heads the expander is responsible for eliminating (SPEC §4.3).")

(defun assert-no-macro-space (x where)
  "Negative space (SPEC §2): no macro-space head may survive past WHERE."
  (prewalk x (lambda (e)
               (when (node-p e)
                 (assert (not (member (node-head e) +macro-space-heads+)) ()
                         "macro-space head ~a reached ~a" (node-head e) where))
               e))
  x)

(defun expand-module (stmts)
  "Expand every top-level statement to fixpoint. M1: identity — there are no
macros yet, and the walk asserts exactly that."
  (mapcar #'expand-node stmts))

(defun expand-node (node)
  (assert-no-macro-space node "the expander (M1 has no macros)")
  node)
