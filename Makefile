MAKE_DOC := make-doc.lisp
SBCL := sbcl
DOC := README

$(DOC).md: decimals.lisp $(MAKE_DOC)
	$(SBCL) --script $(MAKE_DOC) >$@

$(DOC).html: $(DOC).md
	markdown $< >$@

clean:
	rm -f -- $(DOC).html

.PHONY: clean
