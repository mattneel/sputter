;;;; print-test.lisp — unit tests for the surface printer (SPEC §5.7, I8).

(defpackage #:sputter.tests.print
  (:use #:cl #:rove)
  (:local-nicknames (#:s #:sputter.impl)
                    (#:a #:alexandria)
                    (#:h #:sputter.tests.harness)))

(in-package #:sputter.tests.print)

(defun i (name) (s:make-ident name))
(defun n (head &rest args) (s:make-node head args))
(defun pp (src) (s:print-node (s:parse-expression src)))

(deftest minimal-parens
  (ok (equal (pp "1 + 2 * 3") "1 + 2 * 3") "no parens where none are needed")
  (ok (equal (pp "(1 + 2) * 3") "(1 + 2) * 3") "parens the tree demands stay")
  (ok (equal (pp "a - (b - c)") "a - (b - c)") "right-nested subtraction keeps parens")
  (ok (equal (pp "(a - b) - c") "a - b - c") "left-nested subtraction drops parens")
  (ok (equal (pp "((((1))))") "1") "redundant parens vanish")
  (ok (equal (pp "(f + g)(x)") "(f + g)(x)") "computed callees keep parens")
  (ok (equal (pp "-(1 + 2)") "-(1 + 2)"))
  (ok (equal (pp "(a < b) == c") "(a < b) == c")
      "chain-breaking parens survive printing")
  (ok (equal (s:print-node (n :not (n :le (i "reserved") (i "capacity"))))
             "!(reserved <= capacity)")
      "the printer inserts parens the template never wrote (SPEC §10.3, I8)")
  (ok (equal (s:print-node (n :add (i "total") (n :mul (i "tax") 2)))
             "total + tax * 2")
      "synthesized trees print without defensive parens"))

(deftest literals
  (ok (equal (s:print-node 1.5d0) "1.5"))
  (ok (equal (s:print-node 2.0d10) "2.0e10"))
  (ok (equal (s:print-node 42) "42"))
  (ok (equal (s:print-node t) "true"))
  (ok (equal (s:print-node s:+sput-false+) "false"))
  (ok (equal (s:print-node nil) "nil"))
  (ok (equal (s:print-node (format nil "h~%i")) "\"h\\ni\"")
      "strings print with escapes")
  (ok (equal (s:print-node (format nil "~a" (code-char 7))) "\"\\x07\"")
      "control characters print as \\xNN"))

(deftest statements
  (ok (equal (s:print-node (n :let (i "x") nil 1)) "let x = 1;"))
  (ok (equal (s:print-node (n :var (i "x") (n :type_ident :|i64|) 1))
             "var x: i64 = 1;"))
  (ok (equal (s:print-node (n :return nil)) "return;"))
  (ok (equal (s:print-node (n :op_assign :add (i "x") 1)) "x += 1;")))

(deftest layout
  ;; named fn defs always take a multiline body
  (let ((out (s:print-module (s:parse-module "fn f() { 1 }"))))
    (ok (equal out (format nil "fn f() {~%    1~%}~%"))
        "named fns break canonically"))
  ;; a wide if breaks into the chain form
  (let* ((wide (make-string 120 :initial-element #\a))
         (out (s:print-module
               (s:parse-module
                (format nil "let x = if ~a { 1 } else { 2 };" wide)))))
    (ok (search (format nil "} else {~%") out) "wide ifs break")
    (ok (a:starts-with-subseq (format nil "let x = if ~a {" wide) out)
        "the binding prefix leads the broken form"))
  ;; multi-statement blocks never join inline
  (let ((out (s:print-module (s:parse-module "let x = { a(); b() };"))))
    (ok (search (format nil "{~%") out) "two-item blocks break")))

(deftest parse-print-property
  ;; Over the golden corpus: print is a fixpoint and preserves structure.
  (let ((files (remove-if (lambda (f)
                            (a:starts-with-subseq "err_" (pathname-name f)))
                          (h:golden-source-files))))
    (ok (plusp (length files)) "corpus has non-error files")
    (dolist (src-file files)
      (let* ((src (uiop:read-file-string src-file))
             (m1 (s:parse-module src :file (file-namestring src-file)))
             (out1 (s:print-module m1))
             (m2 (s:parse-module out1 :file "<printed>"))
             (out2 (s:print-module m2)))
        (ok (and (= (length m1) (length m2))
                 (every #'s:node-equal m1 m2))
            (format nil "~a: parse ∘ print preserves the tree"
                    (file-namestring src-file)))
        (ok (string= out1 out2)
            (format nil "~a: print is a fixpoint" (file-namestring src-file)))))))
