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

(defun refmt (src)
  (s:print-module (s:parse-module src)))

(deftest review-regressions
  ;; the pinning `;` survives printing (SPEC §5.4)
  (ok (equal (refmt "fn f(c) { if c { 1 } else { 2 }; }")
             (format nil "fn f(c) {~%    if c { 1 } else { 2 };~%}~%"))
      "a trailing `;` on a brace-form statement is printed back")
  (ok (equal (refmt "fn f(c) { if c { 1 } else { 2 } }")
             (format nil "fn f(c) {~%    if c { 1 } else { 2 }~%}~%"))
      "…and stays absent when the form is the value")
  ;; multi-item blocks in operand position render (no crash), reparse equal
  (let* ((src "let y = 1 + { p(); q() };")
         (out (refmt src)))
    (ok (equal out (format nil "let y = 1 + { p(); q() };~%"))
        "operand blocks force-inline rather than crash")
    (ok (every #'s:node-equal (s:parse-module src) (s:parse-module out))
        "…and round-trip"))
  ;; block-headed conditions print parenthesized
  (let ((out (refmt "let z = if ({ 1 }) { 2 } else { 3 };")))
    (ok (search "if ({ 1 })" out) "cond blocks keep their parens")
    (ok (every #'s:node-equal (s:parse-module out) (s:parse-module out))
        "…and reparse"))
  ;; statement-position: expressions leading with a stopper form print
  ;; parenthesized (in expression position, print-node needs no parens)
  (let* ((stmt (n :call (n :if (i "c") (n :block (i "f")) (n :block (i "g")))
                  (i "x")))
         (out (s:print-module (list stmt))))
    (ok (a:starts-with-subseq "(" out)
        "an expression statement cannot lead with a bare `if`")
    (ok (s:node-equal (first (s:parse-module out)) stmt) "…and reparses equal"))
  ;; negative scalars round-trip now that the parser folds them
  (ok (equal (s:print-node -5) "-5"))
  (ok (eql (s:parse-expression (s:print-node -1.5d0)) -1.5d0)))

(deftest m3-layout
  ;; switch always breaks: one arm per line, expr arms take trailing commas
  (ok (equal (refmt "let g = switch x { 1 => \"one\", else => \"many\" };")
             (format nil "let g = switch x {~%    1 => \"one\",~%    _ => \"many\",~%};~%"))
      "canonical switch layout (else canonicalizes to _)")
  ;; single pipes stay inline; chains of two or more break
  (ok (equal (refmt "let a = x |> f;") (format nil "let a = x |> f;~%")))
  (ok (equal (refmt "let a = x |> f |> g(1);")
             (format nil "let a = x~%    |> f~%    |> g(1);~%"))
      "pipe chains break one stage per line")
  ;; records break one field per line when too wide
  (let ((out (refmt (format nil "let r = .{ .alpha = ~s, .beta = ~s };"
                            (make-string 60 :initial-element #\a)
                            (make-string 60 :initial-element #\b)))))
    (ok (search (format nil ".{~%") out) "wide records break")
    (ok (search (format nil ",~%}") out) "…with trailing commas")))

(deftest parse-print-property
  ;; Over the golden corpus: print is a fixpoint and preserves structure.
  (let ((files (remove-if (lambda (f)
                            (a:starts-with-subseq "err_" (pathname-name f)))
                          (h:golden-source-files))))
    (ok (plusp (length files)) "corpus has non-error files")
    (dolist (src-file files)
      ;; Each corpus file is an independent module/CLI invocation. Macro
      ;; signatures are parse-time state, so keep this property test from
      ;; leaking one file's macro names into the next.
      (s:reset-globals)
      (let* ((src (uiop:read-file-string src-file))
             (m1 (s:parse-module src :file (file-namestring src-file)))
             (out1 (s:print-module m1)))
        (s:reset-globals)
        (let* ((m2 (s:parse-module out1 :file "<printed>"))
               (out2 (s:print-module m2)))
          (ok (and (= (length m1) (length m2))
                   (every #'s:node-equal m1 m2))
              (format nil "~a: parse ∘ print preserves the tree"
                      (file-namestring src-file)))
          (ok (string= out1 out2)
              (format nil "~a: print is a fixpoint" (file-namestring src-file))))))))
