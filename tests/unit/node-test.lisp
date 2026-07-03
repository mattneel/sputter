;;;; node-test.lisp — unit tests for the node model (SPEC §4).

(defpackage #:sputter.tests.node
  (:use #:cl #:rove)
  (:local-nicknames (#:s #:sputter.impl)))

(in-package #:sputter.tests.node)

(deftest construction
  (let* ((m (s:make-source-meta "demo.sput" 12 3))
         (n (s:make-node :add (list (s:make-ident "total" :meta m) 2) :meta m)))
    (ok (s:node-p n) "make-node builds a node")
    (ok (eq (s:node-head n) :add) "head is the given keyword")
    (ok (= (length (s:node-args n)) 2) "args are kept")
    (ok (equal (s:meta-file (s:node-meta n)) "demo.sput") "meta carries the file")
    (ok (= (s:meta-line (s:node-meta n)) 12) "meta carries the line")
    (ok (s:meta-synthetic (s:node-meta (s:make-ident "x")))
        "meta defaults to synthetic")
    (ok (null (s:meta-scopes m)) "scopes default empty"))
  (ok (signals (s:make-node "add" '()) 'error)
      "head must be a keyword")
  (ok (signals (s:make-node :add (list (make-hash-table))) 'error)
      "args must be nodes or scalars"))

(deftest identifiers
  (let ((n (s:make-ident "total")))
    (ok (s:ident-node-p n) "make-ident builds an .ident node")
    (ok (eq (s:ident-name n) (intern "total" :keyword))
        "name is a case-sensitive keyword")
    (ok (not (eq (s:ident-name n) (intern "TOTAL" :keyword)))
        "case is preserved, not folded"))
  (ok (not (s:ident-node-p 42)) "scalars are not identifier nodes"))

(deftest scalars
  (ok (s:scalarp 42) "integers are scalars")
  (ok (s:scalarp 1.5d0) "floats are scalars")
  (ok (s:scalarp "hi") "strings are scalars")
  (ok (s:scalarp :ok) "atoms are scalars")
  (ok (s:scalarp t) "true is a scalar")
  (ok (s:scalarp s:+sput-false+) "false is a scalar")
  (ok (s:scalarp s:+sput-nil+) "nil (the singleton) is a scalar")
  (ok (not (s:scalarp nil)) "CL NIL is not a scalar — it is [] and structural absence")
  (ok (not (s:scalarp (make-hash-table))) "host objects are not scalars")
  (ok (not (s:scalarp 'foo)) "non-keyword symbols are not scalars"))

(deftest truthiness
  (ok (s:truthy t) "true is truthy")
  (ok (s:truthy 0) "zero is truthy")
  (ok (s:truthy "") "the empty string is truthy")
  (ok (s:truthy :ok) "atoms are truthy")
  (ok (not (s:truthy s:+sput-nil+)) "nil is falsy")
  (ok (s:truthy nil) "[] is truthy (a list, not nil — §13.18)")
  (ok (not (s:truthy s:+sput-false+)) "false is falsy")
  (ok (not (eq s:+sput-false+ nil)) "false and nil are distinct values"))

(defun sample-tree ()
  ;; a + (b * 2)
  (s:make-node :add
               (list (s:make-ident "a")
                     (s:make-node :mul (list (s:make-ident "b") 2)))))

(deftest walkers
  (let ((tree (sample-tree)))
    (ok (s:node-equal tree (s:prewalk tree #'identity))
        "prewalk identity rebuilds an equal tree")
    (ok (s:node-equal tree (s:postwalk tree #'identity))
        "postwalk identity rebuilds an equal tree")
    (let ((renamed (s:prewalk tree
                              (lambda (x)
                                (if (s:ident-node-p x) (s:make-ident "z") x)))))
      (ok (eq (s:ident-name (first (s:node-args renamed))) (intern "z" :keyword))
          "prewalk can rewrite identifier nodes"))
    (let ((doubled (s:postwalk tree
                               (lambda (x) (if (integerp x) (* 2 x) x)))))
      (ok (eql (second (s:node-args (second (s:node-args doubled)))) 4)
          "postwalk reaches scalar leaves"))
    (let (order)
      (s:prewalk tree
                 (lambda (x)
                   (push (if (s:node-p x) (s:node-head x) x) order)
                   x))
      (ok (equal (reverse order)
                 (list :add :ident (intern "a" :keyword)
                       :mul :ident (intern "b" :keyword) 2))
          "prewalk visits parents before children"))
    (let (order)
      (s:postwalk tree
                  (lambda (x)
                    (push (if (s:node-p x) (s:node-head x) x) order)
                    x))
      (ok (equal (reverse order)
                 (list (intern "a" :keyword) :ident
                       (intern "b" :keyword) :ident 2 :mul :add))
          "postwalk visits children before parents"))
    (ok (eql (s:prewalk 7 (lambda (x) (if (integerp x) (1+ x) x))) 8)
        "walking a bare scalar applies f once")))

(deftest structural-equality
  (let ((m1 (s:make-source-meta "a.sput" 1 1))
        (m2 (s:make-source-meta "b.sput" 99 9)))
    (ok (s:node-equal (s:make-ident "x" :meta m1) (s:make-ident "x" :meta m2))
        "node-equal ignores meta")
    (ok (not (s:node-equal (s:make-ident "x") (s:make-ident "y")))
        "different names differ")
    (ok (not (s:node-equal 1 1.0d0))
        "tree equality keeps 1 and 1.0 distinct (unlike term-level ==)")
    (ok (s:node-equal "hi" "hi") "strings compare by content")
    (ok (s:node-equal (sample-tree) (sample-tree)) "deep trees compare")
    (ok (not (s:node-equal (sample-tree) (s:make-ident "a")))
        "shape mismatches differ")))

(deftest opaque-printing
  (let* ((m (s:make-source-meta "demo.sput" 12 3))
         (n (s:make-node :add (list 1 2) :meta m)))
    (ok (search "#<sput-node ADD demo.sput:12:3>" (princ-to-string n))
        "nodes print opaquely with head and span")
    (ok (search "#<sput-node IDENT>" (princ-to-string (s:make-ident "x")))
        "synthetic nodes print without a span")
    (ok (search "#<sput-false>" (princ-to-string s:+sput-false+))
        "the false singleton prints opaquely")))
