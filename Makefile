SBCL ?= sbcl
QL_SETUP := $(HOME)/quicklisp/setup.lisp

.PHONY: test golden-update clean

test:
	$(SBCL) --noinform --non-interactive \
	  --eval '(require :asdf)' \
	  --eval '(when (probe-file "$(QL_SETUP)") (load "$(QL_SETUP)"))' \
	  --eval '(push (uiop:getcwd) asdf:*central-registry*)' \
	  --eval '(if (find-package :ql) (uiop:symbol-call :ql :quickload :sputter/tests :silent t) (asdf:load-system :sputter/tests))' \
	  --eval '(unless (uiop:symbol-call :rove :run :sputter/tests) (uiop:quit 1))'

golden-update:
	SPUTTER_GOLDEN=update $(MAKE) test

clean:
	find . -name '*.fasl' -delete
