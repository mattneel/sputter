;;;; expand.lisp — hygienic macro expansion (SPEC §5.8). M4: quotes are data —
;;;; the expander never descends into them, and they survive it (the lowerer
;;;; eliminates them into node-construction code). The macro machinery itself
;;;; lands in M5/M6.

(in-package #:sputter.impl)

(defparameter +expander-owned-heads+
  '(:macro_call :macro_def :macro_fn_def :hole :splice_seq)
  "Heads the expander is responsible for eliminating (SPEC §4.3). :quote (and
the raw/insert/inject forms living inside quote bodies) belong to the
lowerer instead — quoted code is data until it is spliced back into a
program (see DECISIONS.md).")

(defun assert-no-macro-space (x where)
  "Negative space (SPEC §2): no expander-owned head may survive past WHERE.
Quote bodies are exempt — a quoted macro call is data, not a call."
  (labels ((walk (e)
             (when (node-p e)
               (unless (eq (node-head e) :quote)
                 (assert (not (member (node-head e) +expander-owned-heads+)) ()
                         "macro-space head ~a reached ~a" (node-head e) where)
                 (dolist (a (node-args e)) (walk a))))))
    (walk x))
  x)

(defun expand-module (stmts)
  "Expand every top-level statement to fixpoint. M4: identity — there are no
macros yet, and the walk asserts exactly that."
  (mapcar #'expand-node stmts))

(defun expand-node (node)
  (assert-no-macro-space node "the expander (no macros before M5)")
  node)