;;;; run-test.lisp — unit tests for lower + emit + run (SPEC §6, §7, M2).

(defpackage #:sputter.tests.run
  (:use #:cl #:rove)
  (:local-nicknames (#:s #:sputter.impl)))

(in-package #:sputter.tests.run)

(defun eval-src (src)
  "Fresh session; returns the value of the last top-level form."
  (s:reset-globals)
  (let ((value nil))
    (dolist (stmt (s:parse-module src :file "<test>") value)
      (setf value (s:eval-top-form stmt)))))

(defun run-src (src)
  "Fresh session; returns SRC's printed output."
  (with-output-to-string (*standard-output*)
    (eval-src src)))

(defun warn-output (src)
  (with-output-to-string (*error-output*)
    (eval-src src)))

(defmacro lower-fails (src)
  `(ok (signals (eval-src ,src) 's:sputter-lower-error)
       (format nil "~s is a lowering error" ,src)))

(defmacro panics (src)
  `(ok (signals (eval-src ,src) 's:sputter-panic)
       (format nil "~s panics" ,src)))

(deftest arithmetic
  (ok (eql (eval-src "1 + 2 * 3;") 7))
  (ok (eql (eval-src "7 / 2;") 3) "integer division truncates (Zig)")
  (ok (eql (eval-src "-7 / 2;") -3) "toward zero, not floor")
  (ok (eql (eval-src "7.0 / 2;") 3.5d0) "float division divides exactly")
  (ok (eql (eval-src "-7 % 3;") -1) "% is truncated rem (C/Zig)")
  (ok (eql (eval-src "2.5 * 4.0;") 10.0d0))
  (panics "1 / 0;")
  (panics "1 % 0;")
  (panics "1 + \"a\";")
  (panics "\"a\" < \"b\";"))

(deftest equality-and-booleans
  (ok (eq (eval-src "1 == 1.0;") t) "numeric == crosses int/float (Elixir)")
  (ok (s:sput-false-p (eval-src "1 == 2;")))
  (ok (eq (eval-src "\"ab\" == \"ab\";") t))
  (ok (eq (eval-src "1 != 2;") t))
  (ok (eq (eval-src "1 < 2;") t) "comparisons return Sputter booleans")
  (ok (s:sput-false-p (eval-src "2 < 1;")) "…never CL NIL")
  (ok (eq (s:sput-equal :|ok| :|ok|) t) "atoms compare by identity")
  (ok (s:sput-false-p (eval-src "true == nil;"))))

(deftest truthiness-and-short-circuit
  (ok (eql (eval-src "if 0 { 1 } else { 2 };") 1) "zero is truthy")
  (ok (eql (eval-src "if nil { 1 } else { 2 };") 2))
  (ok (eql (eval-src "if false { 1 } else { 2 };") 2))
  (ok (eql (eval-src "if 1 < 2 { 1 } else { 2 };") 1))
  (ok (eql (eval-src "nil or 5;") 5) "or returns the deciding value")
  (ok (eql (eval-src "0 or 5;") 0) "…including a truthy lhs")
  (ok (s:sput-false-p (eval-src "false and 1;")) "and returns the falsy lhs")
  (ok (eql (eval-src "1 and 2;") 2))
  (ok (eql (eval-src "var t = 0; fn hit() { t = 1; } nil and hit(); t;") 0)
      "and short-circuits")
  (ok (eql (eval-src "var t = 0; fn hit() { t = 1; } 1 or hit(); t;") 0)
      "or short-circuits"))

(deftest strings
  (ok (equal (eval-src "\"con\" ++ \"cat\";") "concat"))
  (panics "\"a\" ++ 1;"))

(deftest bindings-and-scope
  (ok (eql (eval-src "let x = 1; x + 1;") 2))
  (ok (eql (eval-src "let x = 1; { let x = 10; x; } x;") 1)
      "block lets shadow, then unwind")
  (ok (eql (eval-src "let x = 1; { let x = x + 10; x };") 11)
      "inits see the outer binding")
  (ok (eql (eval-src "var n = 1; n = 5; n;") 5))
  (ok (eql (eval-src "var n = 1; n += 4; n;") 5))
  (ok (eql (eval-src "var total = 0; fn bump() { total += 1; } bump(); bump(); total;") 2)
      "functions mutate global vars")
  (lower-fails "let x = 1; x = 2;")
  (lower-fails "let x = 1; x += 2;")
  (lower-fails "fn f(a) { a = 1; } f(0);")
  (lower-fails "fn f() { g = 1; } f();")
  (lower-fails "let x = y;")
  (lower-fails "fn f() { } f = 2;"))

(deftest functions
  (ok (eql (eval-src "fn area(w: f64, h: f64) f64 { w * h } area(3.0, 4.0);")
           12.0d0))
  (ok (eql (eval-src "fn fact(n) { if n < 2 { 1 } else { n * fact(n - 1) } } fact(10);")
           3628800))
  (ok (eq (eval-src "fn is_even(n) { if n == 0 { true } else { is_odd(n - 1) } }
                     fn is_odd(n) { if n == 0 { false } else { is_even(n - 1) } }
                     is_even(10);")
          t)
      "mutual recursion works (late-bound global calls)")
  (ok (eql (eval-src "fn make_adder(n) { fn(x) { x + n } }
                      let add2 = make_adder(2);
                      add2(40);")
           42)
      "closures capture bindings")
  (ok (eql (eval-src "fn f() {
                        fn fact(n) { if n < 2 { 1 } else { n * fact(n - 1) } }
                        fact(5)
                      }
                      f();")
           120)
      "local named fns can recurse")
  (ok (eql (eval-src "fn f(x) { if x < 0 { return 0; } x } f(-5);") 0)
      "return exits early")
  (ok (eql (eval-src "fn f(x) { if x < 0 { return 0; } x } f(5);") 5))
  (ok (eql (eval-src "let g = fn(x) { if x < 0 { return 0; } x }; g(-3);") 0)
      "return works inside lambdas")
  (lower-fails "return 1;")
  (lower-fails "fn f(a, a) { a }"))

(deftest control
  (ok (eql (eval-src "var n = 0; while n < 5 { n += 2; } n;") 6))
  (ok (null (eval-src "var n = 0; while n < 3 { n += 1; };"))
      "while has value nil")
  (panics "unreachable;")
  (panics "panic(\"boom\");"))

(deftest emitted-declarations
  ;; §10.1 golden assertion: area emits with double-float declares
  (s:reset-globals)
  (let* ((stmts (s:parse-module "fn area(w: f64, h: f64) f64 { w * h }"))
         (p (first (s:lower-top-form (first stmts))))
         (form (s:emit-top-form (s:validate-plasma p)))
         (text (prin1-to-string form)))
    (ok (search "(TYPE DOUBLE-FLOAT" text) "params get double-float declares")
    (ok (search "(THE DOUBLE-FLOAT" text) "return type gets THE")
    (ok (eq (first form) 'defun) "named fns emit as defun")))

(deftest type-declarations
  (ok (eql (eval-src "fn g(x: i64) i64 { x } g(41) + 1;") 42))
  (ok (signals (eval-src "fn g(x: i64) i64 { x } g(\"nope\");") 'type-error)
      "declared types are enforced by the host")
  (ok (search "unknown type `zorp`"
              (warn-output "fn h(x: zorp) { x } h(1);"))
      "unknown type names warn and demote to any"))

(deftest show-and-println
  (ok (equal (s:show-value (list 1 2 3)) "[1, 2, 3]"))
  (ok (equal (s:show-value 1.5d0) "1.5"))
  (ok (equal (s:show-value "hi") "\"hi\""))
  (ok (equal (s:show-value :|ok|) ".ok"))
  (ok (equal (s:show-value t) "true"))
  (ok (equal (run-src "println(\"raw string\");")
             (format nil "raw string~%"))
      "println prints strings raw")
  (ok (equal (run-src "println(1.5);") (format nil "1.5~%")))
  (ok (equal (eval-src "fn area(w, h) { w * h } show(area);") "<fn area/2>")
      "show renders fns as <fn name/arity>"))

(deftest panic-frames
  (s:reset-globals)
  (let ((c (handler-case
               ;; inner() + 1 keeps the call out of tail position — SBCL's
               ;; TCO would otherwise (legitimately, §7) drop outer's frame
               (progn (eval-src "fn inner() { unreachable }
                                 fn outer() { inner() + 1 }
                                 outer();")
                      nil)
             (s:sputter-panic (c) c))))
    (ok c "the panic surfaced")
    (let ((names (mapcar #'first (s:sputter-panic-frames c))))
      (ok (member "inner" names :test #'equal) "frames name inner")
      (ok (member "outer" names :test #'equal) "frames name outer"))))

(deftest repl-machinery
  (ok (s::repl-entry-complete-p "1 + 2"))
  (ok (not (s::repl-entry-complete-p "fn f() {")))
  (ok (s::repl-entry-complete-p "fn f() { 1 }"))
  (ok (s::repl-entry-complete-p "\"unterminated")
      "lex errors count as complete (they surface immediately)")
  (s:reset-globals)
  (ok (eql (s::eval-repl-entry "let x = 5;") 5) "let echoes its value")
  (ok (eql (s::eval-repl-entry "x * 2") 10) "state persists across entries")
  (ok (functionp (s::eval-repl-entry "fn d(n) { n * 2 }")) "defs echo the fn"))