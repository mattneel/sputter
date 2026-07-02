;;;; golden.lisp — the golden corpus runner (SPEC §3.3, I9).

(defpackage #:sputter.tests.golden
  (:use #:cl #:rove)
  (:local-nicknames (#:h #:sputter.tests.harness)))

(in-package #:sputter.tests.golden)

(defun run-golden-case (src mode args-fn expected-path)
  (multiple-value-bind (actual code)
      (h:run-cli (funcall args-fn src))
    (let ((code-ok (h:exit-code-ok-p src code)))
      (ok code-ok
          (format nil "~a [~a] exit code ~d ~:[breaks~;fits~] the err_* discipline"
                  (file-namestring src) mode code code-ok))
      (if (h:golden-update-p)
          (if code-ok
              (progn
                (with-open-file (s expected-path
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
                  (write-string actual s))
                (ok t (format nil "updated ~a" (file-namestring expected-path))))
              (ok nil (format nil "refusing to update ~a: exit ~d~%~a"
                              (file-namestring expected-path) code actual)))
          (let* ((expected (uiop:read-file-string expected-path))
                 (same (string= expected actual)))
            (ok same
                (format nil "~a [~a]~@[~%~a~]"
                        (file-namestring src) mode
                        (unless same (h:diff-report expected actual)))))))
    (ok (not (h:looks-like-sexp-p actual))
        (format nil "~a [~a] stays above the Waterline"
                (file-namestring src) mode))))

(deftest golden-corpus
  (let ((cases 0))
    (dolist (src (h:golden-source-files))
      (loop for (mode . args-fn) in h:+golden-modes+
            for expected-path = (h:expected-file src mode)
            when (probe-file expected-path)
              do (incf cases)
                 (run-golden-case src mode args-fn expected-path)))
    (when (zerop cases)
      (ok t "golden corpus empty (cases arrive with M1)"))))
