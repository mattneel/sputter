;;;; lex-test.lisp — unit tests for the lexer (SPEC §5.1).

(defpackage #:sputter.tests.lex
  (:use #:cl #:rove)
  (:local-nicknames (#:s #:sputter.impl)))

(in-package #:sputter.tests.lex)

(defun ttypes (src)
  (map 'list #'s:token-type (s:lex src)))

(defun tvalues (src)
  (map 'list #'s:token-value (s:lex src)))

(defmacro lex-fails (src)
  `(ok (signals (s:lex ,src) 's:sputter-parse-error)
       (format nil "~s does not lex" ,src)))

(deftest basics
  (ok (equal (ttypes "fn area(w: f64) f64 { w * h }")
             '(:kw :ident :lparen :ident :colon :ident :rparen :ident
               :lbrace :ident :star :ident :rbrace :eof))
      "a fn header lexes to the expected token types")
  (ok (equal (tvalues "fn foo")
             (list :fn (intern "foo" :keyword) nil))
      "keywords carry their keyword; identifiers carry a case-sensitive name")
  (ok (equal (ttypes "") '(:eof)) "empty input is just :eof"))

(deftest positions
  (let ((toks (s:lex (format nil "let x = 1;~%x = 2;"))))
    (let ((second-line (find-if (lambda (tok) (= (s:token-line tok) 2)) toks)))
      (ok second-line "tokens after a newline are on line 2")
      (ok (= (s:token-col second-line) 1) "column resets to 1 after newline"))
    (ok (= (s:token-col (elt toks 1)) 5) "columns are 1-based")))

(deftest numbers
  (ok (equal (tvalues "1_000_000") '(1000000 nil)) "underscores in integers")
  (ok (equal (tvalues "0xFF") '(255 nil)) "hex literals")
  (ok (equal (tvalues "0xdead_beef") (list #xdeadbeef nil)) "hex with underscores")
  (ok (eql (first (tvalues "1.5")) 1.5d0) "floats are double-floats")
  (ok (eql (first (tvalues "2.0e10")) 2.0d10) "exponent floats")
  (ok (eql (first (tvalues "1.0e-5")) 1.0d-5) "negative exponents")
  (ok (equal (ttypes "1.foo") '(:int :dot-ident :eof))
      "1.foo is an int then field access, not a float")
  (lex-fails "1abc")
  (lex-fails "2e10")                    ; write 2.0e10
  (lex-fails "0x")
  (lex-fails "1.5e"))

(deftest strings
  (ok (equal (first (tvalues "\"hi\"")) "hi") "plain string")
  (ok (equal (first (tvalues "\"a\\nb\\tc\"")) (format nil "a~%b~ac" #\Tab))
      "\\n and \\t escapes")
  (ok (equal (first (tvalues "\"q\\\"q\\\\\"")) "q\"q\\") "\\\" and \\\\ escapes")
  (ok (equal (first (tvalues "\"\\x41\"")) "A") "\\xNN escapes")
  (lex-fails "\"abc")                   ; unterminated
  (lex-fails "\"a\\qb\"")               ; unknown escape
  (lex-fails (format nil "\"a~%b\""))   ; raw newline
  (lex-fails "\"\\x4\""))               ; short hex escape

(deftest maximal-munch
  (ok (equal (ttypes "a ++= b") '(:ident :concat-assign :ident :eof)))
  (ok (equal (ttypes "a ++ b") '(:ident :plus-plus :ident :eof)))
  (ok (equal (ttypes "a += b") '(:ident :plus-assign :ident :eof)))
  (ok (equal (ttypes "a <= b") '(:ident :le :ident :eof)))
  (ok (equal (ttypes "a < = b") '(:ident :lt :assign :ident :eof)))
  (ok (equal (ttypes "a => b") '(:ident :fat-arrow :ident :eof)))
  (ok (equal (ttypes "a |> b") '(:ident :pipe-gt :ident :eof)))
  (ok (equal (ttypes "a == b != c")
             '(:ident :eq-eq :ident :bang-eq :ident :eof))))

(deftest comments
  (ok (equal (ttypes (format nil "1 // hi there~%2")) '(:int :int :eof))
      "// comments run to end of line")
  (ok (equal (ttypes (format nil "/// doc comment~%1")) '(:int :eof))
      "/// is reserved but ignored in v0.1")
  (ok (equal (ttypes "// only a comment") '(:eof))))

(deftest dots
  (ok (equal (ttypes ".foo") '(:dot-ident :eof)))
  (ok (equal (tvalues ".foo") (list (intern "foo" :keyword) nil))
      "dot-ident value is the case-sensitive name")
  (ok (equal (ttypes ".{") '(:dot-lbrace :eof)))
  (ok (equal (ttypes "...rest") '(:ellipsis :ident :eof)))
  (ok (equal (ttypes "x.y.z") '(:ident :dot-ident :dot-ident :eof)))
  (lex-fails ". x")
  (lex-fails "..")
  (lex-fails ".5"))

(deftest rejects
  (lex-fails "|")
  (lex-fails "&")
  (lex-fails "~")
  (lex-fails "@")
  (lex-fails "$"))
