.PHONY: test pubdocs

sourcefiles = $(shell ffind --full-path --dir src --literal .lisp)
docfiles = $(shell ls docs/*.markdown)
apidoc = docs/03-reference.markdown

test:
	sbcl --noinform --load test/run.lisp  --eval '(quit)'

src/utils.lisp: src/make-utilities.lisp
	cd src && sbcl --noinform --load make-utilities.lisp  --eval '(quit)'

$(apidoc): $(sourcefiles) docs/api.lisp
	sbcl --noinform --load docs/api.lisp  --eval '(quit)'

docs: docs/build/index.html

docs/build/index.html: $(docfiles)
	cd docs && ~/.virtualenvs/d/bin/d

pubdocs: docs
	hg -R ~/src/sjl.bitbucket.org pull -u
	rsync --delete -a ./docs/build/ ~/src/sjl.bitbucket.org/bones
	hg -R ~/src/sjl.bitbucket.org commit -Am 'bones: Update site.'
	hg -R ~/src/sjl.bitbucket.org push

