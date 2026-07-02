;;;; parse-test.lisp — unit tests for the grammar (SPEC §5.3–§5.4).

(defpackage #:sputter.tests.parse
  (:use #:cl #:rove)
  (:local-nicknames (#:s #:sputter.impl)))

(in-package #:sputter.tests.parse)

(defun i (name) (s:make-ident name))
(defun n (head &rest args) (s:make-node head args))
(defun pe (src) (s:parse-expression src))
(defun p1 (src) (first (s:parse-module src)))

(defmacro expr= (src tree &optional what)
  `(ok (s:node-equal (pe ,src) ,tree) (or ,what ,src)))

(defmacro stmt= (src tree &optional what)
  `(ok (s:node-equal (p1 ,src) ,tree) (or ,what ,src)))

(defmacro parse-fails (src)
  `(ok (signals (s:parse-module ,src) 's:sputter-parse-error)
       (format nil "~s does not parse" ,src)))

(deftest precedence
  (expr= "1 + 2 * 3" (n :add 1 (n :mul 2 3)))
  (expr= "(1 + 2) * 3" (n :mul (n :add 1 2) 3))
  (expr= "a - b - c" (n :sub (n :sub (i "a") (i "b")) (i "c"))
         "subtraction is left-associative")
  (expr= "-x.f" (n :neg (n :field (i "x") (intern "f" :keyword)))
         "unary binds looser than postfix")
  (expr= "!f(x)" (n :not (n :call (i "f") (i "x")))
         "! applies to the whole call")
  (expr= "1 < 2 and 3 < 4 or flag"
         (n :or (n :and (n :lt 1 2) (n :lt 3 4)) (i "flag"))
         "cmp < and < or")
  (expr= "a ++ b + c" (n :add (n :concat (i "a") (i "b")) (i "c"))
         "++ sits at the additive level, left-assoc")
  (expr= "a |> f" (n :pipe (i "a") (i "f")) "|> parses to a .pipe node")
  (expr= "a |> f(b) |> g"
         (n :pipe (n :pipe (i "a") (n :call (i "f") (i "b"))) (i "g"))
         "|> is left-associative and binds loosest")
  (expr= "x / y % z * w"
         (n :mul (n :rem (n :div (i "x") (i "y")) (i "z")) (i "w"))))

(deftest non-chainable-comparisons
  (parse-fails "let x = a < b < c;")
  (parse-fails "let x = a == b != c;")
  (ok (s:node-equal (pe "(a < b) == c")
                    (n :eq (n :lt (i "a") (i "b")) (i "c")))
      "parenthesized comparisons combine fine"))

(deftest postfix-chains
  (expr= "obj.handler(x)[0].y"
         (n :field
            (n :index
               (n :call (n :field (i "obj") (intern "handler" :keyword)) (i "x"))
               0)
            (intern "y" :keyword)))
  (expr= "f()" (n :call (i "f")) "empty call")
  (expr= "f(a, b,)" (n :call (i "f") (i "a") (i "b")) "trailing comma in calls"))

(deftest if-expressions
  (stmt= "let s = if a { 1 } else { 2 };"
         (n :let (i "s") nil
            (n :if (i "a") (n :block 1) (n :block 2))))
  (stmt= "let s = if a { 1 } else if b { 2 } else { 3 };"
         (n :let (i "s") nil
            (n :if (i "a") (n :block 1)
               (n :if (i "b") (n :block 2) (n :block 3))))
         "else-if chains nest in the else slot")
  (expr= "if a { 1 }" (n :if (i "a") (n :block 1) nil)
         "if without else has a nil else slot")
  (parse-fails "let x = if { 1 } { 2 };")  ; brace-free cond rule
  (parse-fails "if a 1;"))                 ; braces are mandatory

(deftest blocks-and-the-semicolon-rule
  (stmt= "{ a; b }" (n :block (i "a") (i "b"))
         "trailing expr without ; is the value")
  (stmt= "{ a; b; }" (n :block (i "a") (i "b") nil)
         "trailing ; makes the value nil")
  (stmt= "{}" (n :block nil) "empty block has value nil")
  (stmt= "{ let x = 1; x }"
         (n :block (n :let (i "x") nil 1) (i "x")))
  (stmt= "{ if a { b(); } c }"
         (n :block
            (n :if (i "a") (n :block (n :call (i "b")) nil) nil)
            (i "c"))
         "}-terminated statements need no ;")
  (parse-fails "{ a b }")
  (parse-fails "a")                      ; top level needs ;
  (parse-fails "{ let x = 1 }"))         ; let always needs ;

(deftest bindings-and-assignment
  (stmt= "let x = 1;" (n :let (i "x") nil 1))
  (stmt= "let x: i64 = 1;"
         (n :let (i "x") (n :type_ident :|i64|) 1))
  (stmt= "var y = 2;" (n :var (i "y") nil 2))
  (stmt= "x = 1;" (n :assign (i "x") 1))
  (stmt= "x += 1;" (n :op_assign :add (i "x") 1))
  (stmt= "x ++= \"s\";" (n :op_assign :concat (i "x") "s"))
  (parse-fails "x.f = 1;")               ; no field assignment in v0.1
  (parse-fails "-x = 1;")
  (parse-fails "f(x) = 1;"))

(deftest functions
  (stmt= "fn f(a: i64, b) i64 { a }"
         (n :fn (i "f")
            (n :param (i "a") (n :type_ident :|i64|))
            (n :param (i "b") nil)
            (n :type_ident :|i64|)
            (n :block (i "a"))))
  (stmt= "fn f() { }" (n :fn (i "f") nil (n :block nil))
         "no params, no ret type, empty body")
  (stmt= "fn f() g { h }"
         (n :fn (i "f") (n :type_ident :|g|) (n :block (i "h")))
         "TypeExpr split: ident after ) is the return type")
  (stmt= "let g = fn(x) { x };"
         (n :let (i "g") nil
            (n :fn nil (n :param (i "x") nil) nil (n :block (i "x"))))
         "lambdas are fn nodes with a nil name")
  (parse-fails "let g = fn h(x) { x };") ; lambdas cannot be named
  (ok (s:parse-module "fn outer() { fn inner() { 1 } inner() }")
      "nested named fn defs are statements"))

(deftest control-statements
  (stmt= "while a { b; }"
         (n :while (i "a") (n :block (i "b") nil)))
  (stmt= "return;" (n :return nil))
  (stmt= "return 1;" (n :return 1))
  (stmt= "{ unreachable }" (n :block (n :unreachable))
         "unreachable is an expression")
  (parse-fails "if a { 1 } + 2;")        ; statement-position if does not continue
  (ok (s:node-equal (p1 "let x = if a { 1 } else { 2 } + 1;")
                    (n :let (i "x") nil
                       (n :add (n :if (i "a") (n :block 1) (n :block 2)) 1)))
      "expression-position if does continue into operators"))

(deftest future-syntax-is-a-clean-error
  (parse-fails "macro m { }"))

(deftest review-regressions
  ;; the Rust `;` rule on }-terminated statements (M1 review, SPEC §5.4)
  (stmt= "{ if c { 1 } else { 2 }; }"
         (n :block (n :if (i "c") (n :block 1) (n :block 2)) nil)
         "a trailing `;` pins a brace-form as a statement — value nil")
  (stmt= "{ if c { 1 } else { 2 } }"
         (n :block (n :if (i "c") (n :block 1) (n :block 2)))
         "without `;` it is the block's value")
  (stmt= "{ { 1 }; }" (n :block (n :block 1) nil))
  ;; nesting depth guard (raw stack exhaustion must never escape, I2)
  (ok (signals
       (s:parse-module
        (format nil "let x = ~a1~a;"
                (make-string 600 :initial-element #\()
                (make-string 600 :initial-element #\))))
       's:sputter-parse-error)
      "deep nesting is a spanned Sputter error")
  ;; the no-brace-cond rule must not leak into nested bodies
  (ok (s:parse-module "while c { { 1 } }")
      "blocks inside a loop body parse despite the paren-free condition")
  (ok (s:parse-module "if c { let f = fn() { { 1 } }; }")
      "…even nested through lambdas")
  ;; negated literals fold to negative scalars (round-trip support)
  (ok (eql (pe "-5") -5))
  (ok (eql (pe "-1.5") -1.5d0))
  (ok (s:node-equal (pe "-x") (n :neg (i "x"))) "non-literals keep .neg"))
