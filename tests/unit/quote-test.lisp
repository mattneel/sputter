;;;; quote-test.lisp — M4: quote, dump, node stdlib, nodes in switch.

(defpackage #:sputter.tests.quote
  (:use #:cl #:rove)
  (:local-nicknames (#:s #:sputter.impl)))

(in-package #:sputter.tests.quote)

(defun eval-src (src)
  (s:reset-globals)
  (let ((value nil))
    (dolist (stmt (s:parse-module src :file "<test>") value)
      (setf value (s:eval-top-form stmt)))))

(defmacro panics (src)
  `(ok (signals (eval-src ,src) 's:sputter-panic)
       (format nil "~s panics" ,src)))

(deftest quote-values
  (let ((n (eval-src "quote { 1 + 2 };")))
    (ok (s:node-p n) "quote evaluates to a node")
    (ok (eq (s:node-head n) :add))
    (ok (equal (s:node-args n) '(1 2)) "scalars sit directly in args"))
  (ok (eq (eval-src "head(quote { a + b });") :|add|)
      "head() returns the lowercase head atom")
  (ok (eql (eval-src "len(args(quote { f(x, y, z) }));") 4)
      "args() returns callee + arguments")
  (ok (equal (eval-src "meta(quote { x }).file;") "<test>")
      "meta() exposes the source file")
  (ok (s:sput-false-p (eval-src "meta(quote { x }).synthetic;")))
  (ok (eq (eval-src "meta(node(.add, [1, 2])).synthetic;") t)
      "node() synthesizes meta")
  (ok (eq (eval-src "quote { a + b } == quote { a + b };") t)
      "== on nodes ignores meta")
  (ok (eq (eval-src "quote { 1 } == 1;") t)
      "a quoted literal IS the literal (scalars self-quote)"))

(deftest quote-kinds
  (ok (eq (eval-src "head(quote(stmt) { let y = 1; });") :|let|))
  (ok (eql (eval-src "len(quote(stmts) { f(); g(); });") 2)
      "quote(stmts) yields a list of nodes")
  (ok (eq (eval-src "head(quote(block) { f(); g() });") :|block|))
  (ok (eq (eval-src "head(quote(type) { i64 });") :|type_ident|))
  (ok (eq (eval-src "head(quote(arm) { .ok(v) => v });") :|arm|))
  (ok (signals (eval-src "quote(zorp) { 1 };") 's:sputter-parse-error)
      "unknown quote kinds are parse errors"))

(deftest splicing
  (ok (equal (eval-src "fn f(x) { quote { x + 1 } } print(f(5));") "5 + 1")
      "params splice by bare name, scalars lift to literals")
  (ok (equal (eval-src "fn f() { let y = quote { a }; quote { y * 2 } } print(f());")
             "a * 2")
      "locals splice nodes as nodes")
  (ok (equal (eval-src "fn wrap(inner) { quote { 1 + inner } }
                        print(wrap(quote { 2 * 3 }));")
             "1 + 2 * 3")
      "spliced trees keep their structure (parens are the printer's job)")
  (ok (equal (eval-src "fn f() { 1 } print(quote { f() });") "f()")
      "global fn names stay literal syntax")
  (ok (equal (eval-src "let g = 5; print(quote { g });") "g")
      "top-level bindings are global, not lexical: no splice")
  (ok (equal (eval-src "fn f(x) { quote { quote { x } } } print(f(1));")
             "quote { x }")
      "nested quotes stay syntax (depth games are insert()'s job, M5)")
  (panics "fn f(x) { quote { x + 1 } } f([1, 2]);")
  (panics "fn f(x) { quote { x } } f(f);"))

(deftest node-construction-and-walks
  (ok (equal (eval-src "print(node(.mul, [node(.ident, [.a]), 7]));") "a * 7")
      "node() builds printable trees")
  (ok (equal (eval-src
              "print(prewalk(quote { a + b * 2 }, fn(e) {
                   switch e {
                       .{ .head = .ident } => node(.ident, [.z]),
                       else => e,
                   }
               }));")
             "z + z * 2")
      "prewalk rewrites from Sputter")
  (ok (eql (eval-src
            "var count = 0;
             postwalk(quote { a + b * 2 }, fn(e) { count += 1; e });
             count;")
           7)
      "postwalk visits every element")
  (panics "prewalk(quote { 1 + 2 }, fn(e) { println });")
  (panics "print(node(.frobnicate, []));"))

(deftest nodes-in-switch
  (ok (eq (eval-src "switch quote { a + b } {
                       .{ .head = .add, .args = [lhs, rhs] } => head(rhs),
                       else => .nope,
                     };")
          :|ident|)
      "record patterns destructure nodes")
  (ok (equal (eval-src "switch quote { f(1) } {
                          .{ .head = .call } => \"call\",
                          else => \"other\",
                        };")
             "call"))
  (ok (eq (eval-src "switch .{ .head = 1 } {
                       .{ .head = h } => .record_matched,
                       else => .no,
                     };")
          :|record_matched|)
      "plain records still match record patterns"))

(deftest dump-shape
  (let ((d (eval-src "dump(quote { total + tax * 2 });")))
    (ok (search ".{ .head = .add, .meta = .{ .file = \"<test>\"" d)
        "dump opens with head and meta (§5.7 shape)")
    (ok (search ".args = [
    .{ .head = .ident" d)
        "node args break one per line")
    (ok (search ".args = [.total] }" d)
        "scalar-only nodes stay inline")
    (ok (search "        2,
    ]}" d)
        "nested nodes close with ]}"))
  (ok (equal (eval-src "dump(42);") "42") "dump of a scalar is its literal"))

(deftest dump-eval-roundtrip
  ;; §10.4: dump, read the data literal back, rebuild, print
  (s:reset-globals)
  (let* ((n (s:parse-expression "total + tax * 2"))
         (d (s::dump-string n))
         (src (format nil
                      "fn rebuild(d) {
                           switch d {
                               .{ .head = h, .args = as } => node(h, map(as, rebuild)),
                               else => d,
                           }
                       }
                       print(rebuild(~a));" d))
         (out (let ((value nil))
                (dolist (stmt (s:parse-module src :file "<roundtrip>") value)
                  (setf value (s:eval-top-form stmt))))))
    (ok (equal out "total + tax * 2")
        "dump → read back → rebuild → print round-trips (§10.4)")))

(deftest quote-printing
  (flet ((refmt (src) (s:print-module (s:parse-module src))))
    (ok (equal (refmt "let q = quote { x + 1 };")
               (format nil "let q = quote { x + 1 };~%")))
    (ok (equal (refmt "let q = quote(expr) { x };")
               (format nil "let q = quote { x };~%"))
        "the default kind is elided canonically")
    (ok (equal (refmt "let q = quote(stmts) { f(); g(); };")
               (format nil "let q = quote(stmts) { f(); g(); };~%")))
    (ok (equal (refmt "let q = quote(arm) { .ok(v) => v };")
               (format nil "let q = quote(arm) { .ok(v) => v };~%")))
    (let* ((m1 (s:parse-module "let q = quote(block) { f(); g() };"))
           (out (s:print-module m1))
           (m2 (s:parse-module out)))
      (ok (every #'s:node-equal m1 m2) "quote(block) round-trips"))))