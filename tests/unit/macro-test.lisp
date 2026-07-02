;;;; macro-test.lisp — M5: procedural macros, hygiene, kind checks.

(defpackage #:sputter.tests.macro
  (:use #:cl #:rove)
  (:local-nicknames (#:s #:sputter.impl)))

(in-package #:sputter.tests.macro)

(defun i (name) (s:make-ident name))
(defun n (head &rest args) (s:make-node head args))

(defun eval-src (src)
  (s:reset-globals)
  (let ((value nil))
    (dolist (stmt (s:parse-module src :file "<macro-test>") value)
      (setf value (s:eval-top-form stmt)))))

(defun expanded-user-forms (src)
  "Parse+expand SRC from clean state, dropping consumed macro definitions."
  (s:reset-globals)
  (remove-if (lambda (form)
               (and (s:node-p form) (eq (s:node-head form) :macro_fn_def)))
             (s:expand-module (s:parse-module src :file "<macro-test>"))))

(defun expand-print (src)
  (s:print-module (expanded-user-forms src)))

(defmacro expands-error (src)
  `(ok (signals (expanded-user-forms ,src) 's:sputter-expand-error)
       (format nil "~s is an expansion error" ,src)))

(deftest macro-fn-parse-shapes
  (s:reset-globals)
  (let* ((forms (s:parse-module
                 "macro fn check(cond: expr) expr { quote { cond } } check(x + 1);"
                 :file "<macro-test>"))
         (def (first forms))
         (call (second forms)))
    (ok (eq (s:node-head def) :macro_fn_def) "macro fn defs get their own head")
    (ok (eq (s:ident-name (first (s:node-args def))) :|check|)
        "the macro name is an identifier node")
    (ok (s:node-equal (second (s:node-args def))
                      (n :param (i "cond") :|expr|))
        "macro parameters carry macro-land kinds, not TypeExpr nodes")
    (ok (eq (nth (- (length (s:node-args def)) 2) (s:node-args def)) :|expr|)
        "the return kind is a kind keyword")
    (ok (eq (s:node-head call) :macro_call)
        "known macro invocations claim raw call syntax at parse time")
    (ok (eq (first (s:node-args call)) :|check|))
    (ok (s::token-group-p (second (s:node-args call)))
        "the raw extent is stored as a token group")))

(deftest procedural-check-expands-like-the-spec
  (ok (equal (expand-print
              "macro fn check(cond: expr) expr {
                   let text = print(cond);
                   quote {
                       if !cond {
                           panic(\"check failed: \" ++ text)
                       }
                   }
               }

               check(reserved <= capacity);")
             (format nil
                     "if !(reserved <= capacity) { panic(\"check failed: \" ++ \"reserved <= capacity\") }~%"))
      "§10.3: print(cond) runs at expand time and the printer inserts ! parens"))

(deftest macro-fixpoint-and-insert
  (ok (equal (expand-print
              "macro fn one() expr { quote { 1 } }
               macro fn two() expr { quote { one() } }
               two();")
             (format nil "1;~%"))
      "expansion runs to fixpoint when a macro returns a macro call")
  (ok (eql (eval-src
            "macro fn plus_one(e: expr) expr {
                 let one = quote { 1 };
                 quote { e + insert(one) }
             }
             plus_one(41);")
           42)
      "insert(expr) evaluates at expansion time and splices the resulting node"))

(deftest hygiene-raw-and-inject
  (ok (eql (eval-src
            "macro fn twice(e: expr) expr {
                 quote { { let tmp = e; tmp + tmp } }
             }

             fn direction1() {
                 let tmp = 100;
                 twice(tmp) + tmp
             }

             direction1();")
           300)
      "template binders do not capture call-site identifiers, and vice versa")
  (ok (eql (eval-src
            "fn helper() { 42 }

             macro fn call_helper() expr {
                 quote { helper() }
             }

             fn direction2() {
                 let helper = 0;
                 call_helper() + helper
             }

             direction2();")
           42)
      "free template identifiers resolve in the macro definition environment")
  (ok (eql (eval-src
            "macro fn with_it(val: expr, body: expr) expr {
                 quote { { let inject(it) = val; body } }
             }

             with_it(41, it + 1);")
           42)
      "inject(name) introduces an intentional call-site binding")
  (ok (eql (eval-src
            "fn x() { 7 }

             macro fn call_x_with(x: expr) expr {
                 quote { raw(x)() + x }
             }

             call_x_with(3);")
           10)
      "raw(name) keeps a literal template identifier instead of splicing a hole"))

(deftest kind-checks-and-arity
  (expands-error
   "macro fn bad() expr { quote(stmt) { let x = 1; } }
    bad();")
  (expands-error
   "macro fn lit(x: literal) expr { quote { x } }
    lit(.ok);")
  (expands-error
   "macro fn one(x: expr) expr { quote { x } }
    one(1, 2);")
  (expands-error
   "macro fn one(x: expr) expr { quote { x } }
    one();"))
