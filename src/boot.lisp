;;;; boot.lisp — `sbcl --script` entry for bin/sput (SPEC §9; M0 shim).
;;;; Runs outside ASDF and outside any init file: load Quicklisp when present
;;;; (the frozen deps live there), fall back to bare ASDF, then hand off to
;;;; the CLI. Keep this file boring; a saved image supersedes it in M7.

(require :asdf)

(let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file setup)
    (load setup)))

(let* ((root (or (sb-ext:posix-getenv "SPUTTER_ROOT")
                 (namestring
                  (merge-pathnames ".." (directory-namestring *load-truename*))))))
  (push (uiop:ensure-directory-pathname root) asdf:*central-registry*))

;; Compile chatter is host noise; the user asked for a Sputter tool (I2).
(handler-bind ((warning #'muffle-warning))
  (let ((*compile-verbose* nil)
        (*compile-print* nil)
        (*load-verbose* nil))
    (if (find-package '#:ql)
        (uiop:symbol-call '#:ql '#:quickload :sputter :silent t)
        (asdf:load-system :sputter :verbose nil))))

(uiop:symbol-call '#:sputter.impl '#:cli-main)
