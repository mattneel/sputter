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

(deftest test-command-boundaries
  (multiple-value-bind (out code) (h:run-cli '("test"))
    (ok (= code 1) "`sput test` needs a file")
    (ok (search "error" out) "`sput test` reports the usage error"))
  (multiple-value-bind (out code) (h:run-cli '("test" "no-such.sput"))
    (ok (= code 1) "`sput test` on a missing file exits 1")
    (ok (search "error: no such file" out) "and says so in prose")))

(deftest run-boundaries
  (multiple-value-bind (out code) (h:run-cli '("run" "no-such.sput"))
    (ok (= code 1) "run on a missing file exits 1")
    (ok (search "error: no such file" out)))
  (multiple-value-bind (out code) (h:run-cli '("run"))
    (ok (= code 1) "run with no file exits 1")
    (ok (search "error" out))))

(deftest fmt-and-expand-boundaries
  (multiple-value-bind (out code) (h:run-cli '("fmt" "no-such-file.sput"))
    (ok (= code 1) "fmt on a missing file exits 1")
    (ok (search "error: no such file" out) "and says so in prose"))
  (multiple-value-bind (out code) (h:run-cli '("fmt"))
    (ok (= code 1) "fmt with no file exits 1")
    (ok (search "error" out)))
  (multiple-value-bind (out code) (h:run-cli '("fmt" "--frob" "x.sput"))
    (ok (= code 1) "unknown flags are errors")
    (ok (search "unknown flag" out)))
  (multiple-value-bind (out code) (h:run-cli '("fmt" "--check" "run_core.sput"))
    (ok (= code 0) "fmt --check accepts canonical files")
    (ok (string= out "") "fmt --check is quiet when clean"))
  (multiple-value-bind (out code) (h:run-cli '("expand" "--dump" "tour_core.sput"))
    (ok (= code 0) "expand --dump works (M4)")
    (ok (search ".head = .fn" out) "…and emits data literals")))
