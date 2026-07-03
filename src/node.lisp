;;;; node.lisp — the node model (SPEC §4): node & meta structs, constructors,
;;;; walkers, opaque host-level printing.
;;;;
;;;; One shape for everything, Elixir-style, in Zig clothes. Structs, not
;;;; lists — this makes the Waterline (I2) structurally hard to violate.

(in-package #:sputter.impl)

;;; --- the false singleton -----------------------------------------------------
;;; Part of the value model (SPEC §5.5) but defined here: `scalarp` below needs
;;; it, and everything in rt.lisp needs the node accessors. `false` is a
;;; distinct singleton (Elixir model: nil /= false, both falsy) — CL NIL cannot
;;; play both roles (§13.2). DEFVAR, not DEFPARAMETER: reloading this file must
;;; not mint a second false.

(defstruct (sput-false (:constructor %make-sput-false)))

(defvar +sput-false+ (%make-sput-false)
  "The unique runtime value of the Sputter literal `false`.")

(defmethod print-object ((x sput-false) stream)
  ;; Host-level debugging repr only; the user-facing renderer is `show` (I2).
  (print-unreadable-object (x stream)
    (write-string "sput-false" stream)))

;; `nil` is likewise a distinct singleton (SPEC §5.5, §13.18 — human veto
;; 2026-07-02): the empty list is a list, not nil. CL NIL means only `[]`
;; at runtime, and structural absence (a missing else/type/name) in node args.
(defstruct (sput-nil (:constructor %make-sput-nil)))

(defvar +sput-nil+ (%make-sput-nil)
  "The unique runtime value of the Sputter literal `nil`.")

(defmethod print-object ((x sput-nil) stream)
  (print-unreadable-object (x stream)
    (write-string "sput-nil" stream)))

(declaim (inline truthy))
(defun truthy (x)
  "Sputter truthiness: `false` and `nil` are falsy; everything else — the
empty list included — is truthy (SPEC §5.5)."
  (not (or (sput-nil-p x) (sput-false-p x))))

(defun absent-p (x)
  "Structural absence in a node slot: parser-built trees use CL NIL; user- or
macro-built trees may carry the nil literal. Both read as 'not there'."
  (or (null x) (sput-nil-p x)))

;;; --- meta ------------------------------------------------------------------

(defstruct (meta (:constructor %make-meta))
  (file nil :type (or null string))
  (line nil :type (or null (integer 0)))
  (col nil :type (or null (integer 0)))
  ;; Hygiene mark set (SPEC §5.8.5). Shaped for sets-of-scopes later.
  (scopes '() :type list)
  ;; Printer/expander-owned nodes with no source span.
  (synthetic nil :type boolean)
  ;; Internal (never exposed through meta()): the line of a statement's last
  ;; token, parser-recorded so blank-line preservation sees closing braces.
  (end-line nil :type (or null (integer 0))))

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
  "True when X is a Sputter scalar: integer, float, string, boolean, atom,
nil. CL NIL is *not* a scalar — it is the empty list (a value, not syntax)
and the structural-absence marker in node slots."
  (or (integerp x) (floatp x) (stringp x) (keywordp x)
      (eq x t) (sput-false-p x) (sput-nil-p x)))

(declaim (ftype (function (t t) t) token-group-equal)) ; lex.lisp

;; A macro invocation's raw extent (SPEC §5.8.6): balanced token groups
;; collected by the parser, sub-parsed by kind at expansion time. Eliminated
;; by the expander; asserting they never survive is part of negative space.
(defstruct (token-group (:constructor make-token-group (tokens)))
  (tokens #() :type simple-vector))

(defmethod print-object ((g token-group) stream)
  (print-unreadable-object (g stream)
    (format stream "sput-token-group ~d tokens" (length (token-group-tokens g)))))

(defun arg-elem-p (x)
  ;; Non-keyword symbols are admitted for one internal reason: p.host_call
  ;; carries a resolved CL symbol (SPEC §6); meta objects ride in p.lit when
  ;; quote-lowering rebuilds nodes at runtime; token-groups are macro-call
  ;; payloads (§5.8.6). None is a surface scalar.
  (or (node-p x) (scalarp x) (symbolp x) (meta-p x) (token-group-p x)))

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

;; Internally heads are reader-cased keywords (:ADD); the language's head
;; atoms are lowercase (`.add`). These two bridge the boundary: every place
;; a head crosses into user-land (head(), .head, dump) downcases, and
;; node() construction upcases.
(defun head-atom (head-kw)
  (name-keyword (string-downcase (symbol-name head-kw))))

(defun atom-head (atom-kw)
  (values (intern (string-upcase (symbol-name atom-kw)) :keyword)))

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
term-level `==` of SPEC §5.5); strings by STRING=; macro-call payloads by
token content."
  (cond ((and (node-p a) (node-p b))
         (and (eq (node-head a) (node-head b))
              (= (length (node-args a)) (length (node-args b)))
              (every #'node-equal (node-args a) (node-args b))))
        ((and (stringp a) (stringp b)) (string= a b))
        ((and (token-group-p a) (token-group-p b)) (token-group-equal a b))
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
