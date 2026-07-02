;;;; sputter.asd — ASDF systems for Sputter (SPEC §3.3).

(asdf:defsystem #:sputter
  :description "Sputter: a Lisp with C-family surface syntax, hosted on SBCL."
  :depends-on (#:alexandria #:trivia)
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "host")
               (:file "node")
               (:file "rt")
               (:file "lex")
               (:file "parse")
               (:file "expand")
               (:file "print")
               (:file "plasma")
               (:file "emit")
               (:file "prelude")
               (:file "cli"))
  :in-order-to ((asdf:test-op (asdf:test-op #:sputter/tests))))

(asdf:defsystem #:sputter/tests
  :depends-on (#:sputter #:rove)
  :serial t
  :pathname "tests/"
  :components ((:file "harness")
               (:module "unit"
                :components ((:file "node-test")
                             (:file "cli-test")
                             (:file "lex-test")
                             (:file "parse-test")
                             (:file "print-test")
                             (:file "run-test")
                             (:file "data-test")
                             (:file "quote-test")))
               (:file "golden"))
  :perform (asdf:test-op (o c) (uiop:symbol-call :rove :run c)))
