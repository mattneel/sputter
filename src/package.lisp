;;;; package.lisp — package definitions (SPEC §3.3).

(defpackage #:sputter.impl
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:documentation
   "Stage-0 Sputter implementation: everything below the Waterline (SPEC I2).
Common Lisp and s-expressions exist only in here; no user-facing channel
ever emits them.")
  (:export
   ;; host.lisp
   #:host-argv #:host-getenv #:host-exit
   ;; rt.lisp (grows in M2)
   #:+sput-false+ #:sput-false-p #:truthy
   ;; node.lisp — the node model (SPEC §4)
   #:node #:node-p #:node-head #:node-meta #:node-args #:make-node
   #:meta #:meta-p #:meta-file #:meta-line #:meta-col #:meta-scopes
   #:meta-synthetic #:make-source-meta #:synthetic-meta #:meta-span-string
   #:scalarp #:arg-elem-p
   #:make-ident #:ident-node-p #:ident-name #:name-keyword
   #:prewalk #:postwalk #:node-equal
   ;; cli.lisp
   #:cli-dispatch #:cli-main))

(defpackage #:sputter
  (:use)
  (:documentation
   "User-land namespace: mangled Sputter identifiers land here (SPEC §5.1).
Deliberately empty and :use-less so user `fn` names can never collide with
host or implementation symbols."))
