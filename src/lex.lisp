;;;; lex.lisp — lexical structure (SPEC §5.1).
;;;; Every token carries line/col so parse errors always have precise spans.

(in-package #:sputter.impl)

(defstruct (token (:constructor %make-token (type value line col text)))
  (type nil :type keyword)
  value
  (line 1 :type (integer 1))
  (col 1 :type (integer 1))
  (text "" :type string))

(defmethod print-object ((tok token) stream)
  (print-unreadable-object (tok stream)
    (format stream "sput-token ~a~@[ ~s~] @~d:~d"
            (token-type tok) (token-value tok) (token-line tok) (token-col tok))))

(defparameter +keywords+
  (a:alist-hash-table
   '(("fn" . :fn) ("macro" . :macro) ("let" . :let) ("var" . :var)
     ("if" . :if) ("else" . :else) ("switch" . :switch) ("while" . :while)
     ("for" . :for) ("in" . :in) ("quote" . :quote) ("and" . :and)
     ("or" . :or) ("return" . :return) ("true" . :true) ("false" . :false)
     ("nil" . :nil) ("unreachable" . :unreachable))
   :test 'equal)
  "Reserved words (SPEC §5.1). raw/inject/insert/test are known forms, not reserved.")

(defparameter +operators+
  ;; Longest first: maximal munch.
  '(("++=" . :concat-assign)
    ("++" . :plus-plus) ("==" . :eq-eq) ("!=" . :bang-eq) ("<=" . :le)
    (">=" . :ge) ("=>" . :fat-arrow) ("|>" . :pipe-gt)
    ("+=" . :plus-assign) ("-=" . :minus-assign) ("*=" . :star-assign)
    ("/=" . :slash-assign)
    ("+" . :plus) ("-" . :minus) ("*" . :star) ("/" . :slash) ("%" . :percent)
    ("<" . :lt) (">" . :gt) ("!" . :bang) ("=" . :assign)
    ("(" . :lparen) (")" . :rparen) ("[" . :lbracket) ("]" . :rbracket)
    ("{" . :lbrace) ("}" . :rbrace) ("," . :comma) (";" . :semi)
    (":" . :colon)))

;;; --- lexer state -------------------------------------------------------------

(defstruct (lexer (:constructor %make-lexer (source file)))
  (source "" :type string)
  (file "?" :type string)
  (pos 0 :type fixnum)
  (line 1 :type (integer 1))
  (col 1 :type (integer 1)))

(defun lx-at-end-p (lx &optional (offset 0))
  (>= (+ (lexer-pos lx) offset) (length (lexer-source lx))))

(defun lx-peek (lx &optional (offset 0))
  (unless (lx-at-end-p lx offset)
    (char (lexer-source lx) (+ (lexer-pos lx) offset))))

(defun lx-advance (lx)
  (let ((ch (lx-peek lx)))
    (assert ch () "lexer advanced past end of input")
    (incf (lexer-pos lx))
    (if (char= ch #\Newline)
        (setf (lexer-line lx) (1+ (lexer-line lx))
              (lexer-col lx) 1)
        (incf (lexer-col lx)))
    ch))

(defun lex-error (lx line col fmt &rest args)
  (apply #'sputter-error-at 'sputter-parse-error
         (lexer-file lx) line col fmt args))

;;; --- character classes -------------------------------------------------------

(defun ident-start-p (ch)
  ;; ASCII only, per the spec's [A-Za-z_][A-Za-z0-9_]* (§5.1) — no Unicode
  ;; identifiers in v0.1.
  (and ch (or (char<= #\a ch #\z) (char<= #\A ch #\Z) (char= ch #\_))))

(defun ident-char-p (ch)
  (and ch (or (ident-start-p ch) (char<= #\0 ch #\9))))

(defun dec-digit-p (ch)
  (and ch (digit-char-p ch 10)))

(defun hex-digit-p (ch)
  (and ch (digit-char-p ch 16)))

;;; --- token production ----------------------------------------------------------

(defun lex (source &key (file "<input>"))
  "Lex SOURCE into a simple-vector of tokens ending with an :eof token."
  (check-type source string)
  (check-type file string)
  (let ((lx (%make-lexer source file))
        (tokens '()))
    (loop
      (skip-trivia lx)
      (let ((line (lexer-line lx))
            (col (lexer-col lx)))
        (when (lx-at-end-p lx)
          (push (%make-token :eof nil line col "") tokens)
          (return))
        (push (lex-token lx line col) tokens)))
    (coerce (nreverse tokens) 'simple-vector)))

(defun skip-trivia (lx)
  "Skip whitespace and // comments (/// doc comments are reserved, ignored)."
  (loop
    (let ((ch (lx-peek lx)))
      (cond ((null ch) (return))
            ((member ch '(#\Space #\Tab #\Newline #\Return #\Page))
             (lx-advance lx))
            ((and (char= ch #\/) (eql (lx-peek lx 1) #\/))
             (loop until (or (lx-at-end-p lx) (char= (lx-peek lx) #\Newline))
                   do (lx-advance lx)))
            (t (return))))))

(defun lex-token (lx line col)
  (let ((ch (lx-peek lx)))
    (cond ((dec-digit-p ch) (lex-number lx line col))
          ((ident-start-p ch) (lex-word lx line col))
          ((char= ch #\") (lex-string lx line col))
          ((char= ch #\.) (lex-dot lx line col))
          (t (lex-operator lx line col)))))

;;; --- numbers (SPEC §5.1) -----------------------------------------------------

(defun lex-number (lx line col)
  (let ((start (lexer-pos lx)))
    (flet ((take-digits (pred what)
             (unless (funcall pred (lx-peek lx))
               (lex-error lx (lexer-line lx) (lexer-col lx)
                          "expected ~a in number literal" what))
             (let ((run-start (lexer-pos lx)))
               (loop while (or (funcall pred (lx-peek lx))
                               (eql (lx-peek lx) #\_))
                     do (lx-advance lx))
               ;; underscores group digits: never trailing, never doubled
               (let ((run (subseq (lexer-source lx) run-start (lexer-pos lx))))
                 (when (or (char= (char run (1- (length run))) #\_)
                           (search "__" run))
                   (lex-error lx line col
                              "misplaced `_` in number literal (underscores go between digits)")))))
           (lexeme () (subseq (lexer-source lx) start (lexer-pos lx))))
      (cond
        ;; hex: 0x...
        ((and (char= (lx-peek lx) #\0)
              (member (lx-peek lx 1) '(#\x #\X)))
         (lx-advance lx) (lx-advance lx)
         (take-digits #'hex-digit-p "hex digits")
         (when (ident-start-p (lx-peek lx))
           (lex-error lx line col "malformed number literal `~a...`" (lexeme)))
         (%make-token :int (parse-integer (remove #\_ (lexeme))
                                          :start 2 :radix 16)
                      line col (lexeme)))
        ;; decimal int, possibly a float
        (t
         (take-digits #'dec-digit-p "digits")
         (let ((floatp nil))
           ;; fraction: only when a digit follows the dot (so `1.foo` stays
           ;; an int + field access)
           (when (and (eql (lx-peek lx) #\.) (dec-digit-p (lx-peek lx 1)))
             (setf floatp t)
             (lx-advance lx)
             (take-digits #'dec-digit-p "digits"))
           ;; exponent: only after a fraction (2e10 is malformed; write 2.0e10)
           (when (and floatp (member (lx-peek lx) '(#\e #\E)))
             (lx-advance lx)
             (when (member (lx-peek lx) '(#\+ #\-))
               (lx-advance lx))
             (take-digits #'dec-digit-p "exponent digits"))
           (when (ident-start-p (lx-peek lx))
             (lex-error lx line col "malformed number literal `~a...`" (lexeme)))
           (if floatp
               (%make-token :float
                            (or (parse-float-literal (remove #\_ (lexeme)))
                                (lex-error lx line col
                                           "float literal `~a` is out of range"
                                           (lexeme)))
                            line col (lexeme))
               (%make-token :int (parse-integer (remove #\_ (lexeme)))
                            line col (lexeme)))))))))

(defun parse-float-literal (text)
  "Parse a lexer-validated float literal into a double-float; NIL when the
host reader rejects it (overflow like 1.0e999 must surface as a Sputter
error, not a host reader-error)."
  (handler-case
      (let ((*read-eval* nil)
            (*read-default-float-format* 'double-float))
        (let ((value (read-from-string text)))
          (and (typep value 'double-float) value)))
    (error () nil)))

;;; --- words ---------------------------------------------------------------------

(defun lex-word (lx line col)
  (let ((start (lexer-pos lx)))
    (loop while (ident-char-p (lx-peek lx)) do (lx-advance lx))
    (let* ((text (subseq (lexer-source lx) start (lexer-pos lx)))
           (kw (gethash text +keywords+)))
      (if kw
          (%make-token :kw kw line col text)
          (%make-token :ident (name-keyword text) line col text)))))

;;; --- strings (SPEC §5.1: \n \t \\ \" \xNN) ----------------------------------

(defun lex-string (lx line col)
  (lx-advance lx)                       ; opening quote
  (let ((out (make-string-output-stream)))
    (loop
      (let ((ch (lx-peek lx)))
        (cond
          ((or (null ch) (char= ch #\Newline))
           (lex-error lx line col "unterminated string literal"))
          ((char= ch #\")
           (lx-advance lx)
           (let ((value (get-output-stream-string out)))
             (return (%make-token :string value line col value))))
          ((char= ch #\\)
           (lx-advance lx)
           (let ((esc (lx-peek lx)))
             (case esc
               (#\n (lx-advance lx) (write-char #\Newline out))
               (#\t (lx-advance lx) (write-char #\Tab out))
               (#\\ (lx-advance lx) (write-char #\\ out))
               (#\" (lx-advance lx) (write-char #\" out))
               (#\x (lx-advance lx)
                    (let ((h1 (lx-peek lx)) (h2 (lx-peek lx 1)))
                      (unless (and (hex-digit-p h1) (hex-digit-p h2))
                        (lex-error lx (lexer-line lx) (lexer-col lx)
                                   "\\x escape needs two hex digits"))
                      (lx-advance lx) (lx-advance lx)
                      (write-char (code-char (+ (* 16 (digit-char-p h1 16))
                                                (digit-char-p h2 16)))
                                  out)))
               (t (lex-error lx (lexer-line lx) (lexer-col lx)
                             "unknown escape `\\~@[~a~]` in string literal" esc)))))
          (t (lx-advance lx) (write-char ch out)))))))

;;; --- dots: atoms, field access, records, ellipsis (SPEC §5.1) ---------------

(defun lex-dot (lx line col)
  (lx-advance lx)                       ; the dot
  (let ((ch (lx-peek lx)))
    (cond
      ((eql ch #\{)
       (lx-advance lx)
       (%make-token :dot-lbrace nil line col ".{"))
      ((eql ch #\.)
       (lx-advance lx)
       (if (eql (lx-peek lx) #\.)
           (progn (lx-advance lx)
                  (%make-token :ellipsis nil line col "..."))
           (lex-error lx line col "unexpected `..` (spread is written `...`)")))
      ((ident-start-p ch)
       (let ((start (lexer-pos lx)))
         (loop while (ident-char-p (lx-peek lx)) do (lx-advance lx))
         (let ((name (subseq (lexer-source lx) start (lexer-pos lx))))
           (%make-token :dot-ident (name-keyword name) line col
                        (concatenate 'string "." name)))))
      (t (lex-error lx line col
                    "unexpected `.` (atoms and field access need a name: `.foo`)")))))

;;; --- operators and punctuation ------------------------------------------------

(defun lex-operator (lx line col)
  (let ((source (lexer-source lx))
        (pos (lexer-pos lx)))
    (loop for (text . type) in +operators+
          when (and (<= (+ pos (length text)) (length source))
                    (string= source text :start1 pos :end1 (+ pos (length text))))
            do (dotimes (i (length text)) (lx-advance lx))
               (return-from lex-operator (%make-token type nil line col text)))
    (lex-error lx line col "unexpected character `~a`" (lx-peek lx))))
