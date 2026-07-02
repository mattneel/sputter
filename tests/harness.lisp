;;;; harness.lisp — golden-test harness (SPEC §3.3).
;;;;
;;;; Convention: tests/golden/NAME.sput plus NAME.expected.MODE, where MODE is
;;;; one of expand, dump, fmt, run, show. Each existing expected file is one
;;;; golden case: the CLI runs in-process, output is diffed against the file.
;;;; SPUTTER_GOLDEN=update regenerates every *existing* expected file — touch
;;;; an empty one to add coverage for a new mode.

(defpackage #:sputter.tests.harness
  (:use #:cl)
  (:local-nicknames (#:impl #:sputter.impl))
  (:export #:+golden-modes+ #:golden-dir #:golden-source-files #:expected-file
           #:golden-update-p #:run-cli #:diff-report))

(in-package #:sputter.tests.harness)

(defparameter +golden-modes+
  ;; mode name -> function of source pathname returning a CLI argv.
  ;; `show` maps onto the runner once `sput run` exists (M2).
  (list (cons "expand" (lambda (f) (list "expand" (namestring f))))
        (cons "dump" (lambda (f) (list "expand" "--dump" (namestring f))))
        (cons "fmt" (lambda (f) (list "fmt" (namestring f))))
        (cons "run" (lambda (f) (list "run" (namestring f))))))

(defun golden-dir ()
  (asdf:system-relative-pathname :sputter "tests/golden/"))

(defun golden-source-files ()
  (sort (directory (merge-pathnames "*.sput" (golden-dir)))
        #'string< :key #'namestring))

(defun expected-file (source mode)
  "tests/golden/NAME.sput + MODE -> tests/golden/NAME.expected.MODE"
  (make-pathname :name (format nil "~a.expected" (pathname-name source))
                 :type mode :defaults source))

(defun golden-update-p ()
  (equal (impl:host-getenv "SPUTTER_GOLDEN") "update"))

(defun run-cli (argv)
  "Run the CLI dispatch in-process, capturing stdout+stderr interleaved.
Returns (values output-string exit-code)."
  (let ((out (make-string-output-stream)))
    (let* ((*standard-output* out)
           (*error-output* out)
           (code (impl:cli-dispatch argv)))
      (values (get-output-stream-string out) code))))

(defun diff-report (expected actual)
  "Cheap line-oriented diff for golden mismatches (host-side test output)."
  (with-output-to-string (s)
    (let ((elines (uiop:split-string expected :separator '(#\Newline)))
          (alines (uiop:split-string actual :separator '(#\Newline))))
      (loop for i from 0 below (max (length elines) (length alines))
            for e = (nth i elines)
            for a = (nth i alines)
            unless (equal e a)
              do (format s "  line ~d~%    expected: ~s~%    actual:   ~s~%"
                         (1+ i) e a)))))
