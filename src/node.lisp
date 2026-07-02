;;;; node.lisp — the node model (SPEC §4): node & meta structs, constructors,
;;;; walkers, opaque host-level printing.
;;;;
;;;; One shape for everything, Elixir-style, in Zig clothes. Structs, not
;;;; lists — this makes the Waterline (I2) structurally hard to violate.

(in-package #:sputter.impl)

;;; --- meta ------------------------------------------------------------------

(defstruct (meta (:constructor %make-meta))
  (file nil :type (or null string))
  (line nil :type (or null (integer 0)))
  (col nil :type (or null (integer 0)))
  ;; Hygiene mark set (SPEC §5.8.5). Shaped for sets-of-scopes later.
  (scopes '() :type list)
  ;; Printer/expander-owned nodes with no source span.
  (synthetic nil :type boolean))

(defun make-source-meta (file line col)
  (check-type file string)
  (check-type line (integer 0))
  (check-type col (integer 0))
  (%make-meta :file file :line line :col col))

(defun synthetic-meta ()
  (%make-meta :synthetic t))

(defun meta-span-string (m)
  "\"file:line:col\" when M carries a source span, else NIL."
  (when (and (meta-p m) (meta-file m) (meta-line m))
    (format nil "~a:~d~@[:~d~]" (meta-file m) (meta-line m) (meta-col m))))

;;; --- node ------------------------------------------------------------------

(defstruct (node (:constructor %make-node (head meta args)))
  (head nil :type keyword)
  (meta nil :type meta)
  ;; Elements are nodes or scalars (SPEC §4.1). Scalars self-quote.
  (args '() :type list))

(defun scalarp (x)
  "True when X is a Sputter scalar: integer, float, string, boolean, atom, nil."
  (or (integerp x) (floatp x) (stringp x) (keywordp x)
      (eq x t) (sput-false-p x) (null x)))

(defun arg-elem-p (x)
  (or (node-p x) (scalarp x)))

(defun make-node (head args &key (meta (synthetic-meta)))
  "The one public node constructor. Meta defaults to synthetic."
  (check-type head keyword)
  (check-type meta meta)
  (assert (and (listp args) (every #'arg-elem-p args)) (args)
          "node args must be nodes or scalars, got: ~s" args)
  (%make-node head meta args))

;;; --- identifiers -----------------------------------------------------------

(defun name-keyword (name)
  "Intern the string NAME as a case-sensitive keyword (`\"total\"` -> :|total|)."
  (check-type name string)
  (values (intern name :keyword)))

(defun make-ident (name &key (meta (synthetic-meta)))
  "Identifier node: head .ident, args [.name] (SPEC §4.1)."
  (make-node :ident
             (list (if (keywordp name) name (name-keyword name)))
             :meta meta))

(defun ident-node-p (x)
  (and (node-p x) (eq (node-head x) :ident)))

(defun ident-name (n)
  "The name keyword inside an identifier node."
  (assert (ident-node-p n) (n) "not an identifier node: ~s" n)
  (first (node-args n)))

;;; --- walkers ---------------------------------------------------------------

(defun prewalk (x f)
  "Rebuild the tree, applying F to every element top-down (SPEC §4.4).
F sees each element before its children; the children of F's *result* are
walked. When F turns a node into a scalar, recursion stops there."
  (let ((y (funcall f x)))
    (if (node-p y)
        (%make-node (node-head y) (node-meta y)
                    (mapcar (lambda (a) (prewalk a f)) (node-args y)))
        y)))

(defun postwalk (x f)
  "Rebuild the tree, applying F to every element bottom-up (SPEC §4.4)."
  (funcall f
           (if (node-p x)
               (%make-node (node-head x) (node-meta x)
                           (mapcar (lambda (a) (postwalk a f)) (node-args x)))
               x)))

;;; --- structural equality ---------------------------------------------------

(defun node-equal (a b)
  "Structural equality on node trees, ignoring meta (provenance, not identity).
Scalars compare by EQL (so 1 and 1.0 differ — this is tree equality, not the
term-level `==` of SPEC §5.5); strings by STRING=."
  (cond ((and (node-p a) (node-p b))
         (and (eq (node-head a) (node-head b))
              (= (length (node-args a)) (length (node-args b)))
              (every #'node-equal (node-args a) (node-args b))))
        ((and (stringp a) (stringp b)) (string= a b))
        (t (eql a b))))

;;; --- opaque host printing --------------------------------------------------

(defmethod print-object ((n node) stream)
  ;; Opaque on purpose (I2): host-level debugging output must never be
  ;; mistaken for code. The Sputter renderers are print/dump/show (§5.7).
  (print-unreadable-object (n stream)
    (format stream "sput-node ~:@(~a~)~@[ ~a~]"
            (node-head n) (meta-span-string (node-meta n)))))

(defmethod print-object ((m meta) stream)
  (print-unreadable-object (m stream)
    (format stream "sput-meta~@[ ~a~]~:[~; synthetic~]"
            (meta-span-string m) (meta-synthetic m))))
