;;;; stage1-print-test.lisp — M8: compile the Sputter printer beachhead and
;;;; diff it against the stage-0 printer over the golden corpus.

(defpackage #:sputter.tests.stage1-print
  (:use #:cl #:rove)
  (:local-nicknames (#:s #:sputter.impl)
                    (#:h #:sputter.tests.harness)
                    (#:a #:alexandria)))

(in-package #:sputter.tests.stage1-print)

(defun stage1-printer-path ()
  (asdf:system-relative-pathname :sputter "src-sput/print.sput"))

(defun load-stage1-printer ()
  "Fresh state + stage-0 compilation of the Sputter printer module."
  (s:reset-globals)
  (s:run-file (namestring (stage1-printer-path)))
  (let ((sym (s:mangle :|stage1_print_node|)))
    (assert (fboundp sym) () "stage1_print_node was not compiled")
    (symbol-function sym)))

(deftest compiles-with-stage0
  (let ((printer (load-stage1-printer)))
    (ok (functionp printer) "src-sput/print.sput compiles to a callable function")))

(deftest simple-inline-renderer
  (let ((printer (load-stage1-printer)))
    (dolist (src '("1 + 2 * 3"
                   "(1 + 2) * 3"
                   "f(1, .ok)"
                   "xs[0].name"
                   ".{ .a = 1, .b = [2, 3] }"
                   "quote { total + tax * 2 }"))
      (let ((node (s:parse-expression src)))
        (ok (string= (funcall printer node) (s:print-node node))
            (format nil "stage1 inline printer matches CL for ~a" src))))))

(deftest golden-corpus-diff
  (let ((forms 0)
        (files 0)
        (parse-skips 0))
    (dolist (src-file (h:golden-source-files))
      ;; Each corpus source is parsed in a fresh compiler state because macro
      ;; signatures are parse-time state. Then the Sputter printer is compiled
      ;; by stage 0 and asked to render every parsed top-level form.
      (let ((printer (load-stage1-printer)))
        (handler-case
            (let* ((src (uiop:read-file-string src-file))
                   (module (s:parse-module src :file (file-namestring src-file))))
              (incf files)
              (dolist (form module)
                (incf forms)
                (ok (string= (funcall printer form) (s:print-node form))
                    (format nil "src-sput printer matches CL for ~a form ~d"
                            (file-namestring src-file) forms))))
          (s:sputter-parse-error ()
            (incf parse-skips)))))
    (ok (plusp files) "at least one golden file parsed for M8")
    (ok (plusp forms) "at least one golden form was diffed")
    (ok (plusp parse-skips) "parse-error goldens are recognized and skipped")))
