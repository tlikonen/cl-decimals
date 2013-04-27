MAKE_DOC := make-doc.lisp
SBCL := sbcl
DOC := README

$(DOC).md: decimals.lisp $(MAKE_DOC)
	$(SBCL) --noinform --no-userinit --load $(MAKE_DOC) --quit >$@

$(DOC).html: $(DOC).md
	markdown $< >$@

clean:
	rm -f -- $(DOC).html

.PHONY: clean
