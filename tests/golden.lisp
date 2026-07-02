;;;; golden.lisp — the golden corpus runner (SPEC §3.3, I9).

(defpackage #:sputter.tests.golden
  (:use #:cl #:rove)
  (:local-nicknames (#:h #:sputter.tests.harness)))

(in-package #:sputter.tests.golden)

(deftest golden-corpus
  (let ((cases 0))
    (dolist (src (h:golden-source-files))
      (loop for (mode . args-fn) in h:+golden-modes+
            for expected-path = (h:expected-file src mode)
            when (probe-file expected-path)
              do (incf cases)
                 (multiple-value-bind (actual code)
                     (h:run-cli (funcall args-fn src))
                   (declare (ignorable code))
                   (if (h:golden-update-p)
                       (progn
                         (with-open-file (s expected-path
                                            :direction :output
                                            :if-exists :supersede
                                            :if-does-not-exist :create)
                           (write-string actual s))
                         (ok t (format nil "updated ~a"
                                       (file-namestring expected-path))))
                       (let* ((expected (uiop:read-file-string expected-path))
                              (same (string= expected actual)))
                         (ok same
                             (format nil "~a [~a]~@[~%~a~]"
                                     (file-namestring src) mode
                                     (unless same
                                       (h:diff-report expected actual)))))))))
    (when (zerop cases)
      (ok t "golden corpus empty (cases arrive with M1)"))))
