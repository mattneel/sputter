;;;; cli-test.lisp — unit tests for CLI dispatch (SPEC §9).

(defpackage #:sputter.tests.cli
  (:use #:cl #:rove)
  (:local-nicknames (#:h #:sputter.tests.harness)))

(in-package #:sputter.tests.cli)

(deftest usage
  (multiple-value-bind (out code) (h:run-cli '())
    (ok (zerop code) "bare `sput` exits 0")
    (ok (search "usage: sput" out) "bare `sput` prints usage"))
  (multiple-value-bind (out code) (h:run-cli '("help"))
    (ok (zerop code) "`sput help` exits 0")
    (ok (search "usage: sput" out) "`sput help` prints usage")))

(deftest unknown-command
  (multiple-value-bind (out code) (h:run-cli '("frobnicate"))
    (ok (= code 2) "unknown command exits 2")
    (ok (search "error: unknown command `frobnicate`" out)
        "unknown command is named in the error")))

(deftest not-yet-implemented
  (dolist (cmd '("run" "expand" "fmt" "repl" "test"))
    (multiple-value-bind (out code) (h:run-cli (list cmd "x.sput"))
      (ok (= code 1) (format nil "`sput ~a` exits 1 while unimplemented" cmd))
      (ok (search "not implemented" out)
          (format nil "`sput ~a` says so in prose" cmd)))))
