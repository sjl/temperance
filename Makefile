.PHONY: test pubdocs bench

sourcefiles = $(shell ffind --full-path --dir src --literal .lisp)
docfiles = $(shell ls docs/*.markdown)
apidoc = docs/03-reference.markdown

test:
	sbcl-rlwrap --noinform --load test/run.lisp  --eval '(quit)'

src/quickutils.lisp: src/make-quickutils.lisp
	cd src && sbcl-rlwrap --noinform --load make-quickutils.lisp  --eval '(quit)'

$(apidoc): $(sourcefiles) docs/api.lisp
	sbcl-rlwrap --noinform --load docs/api.lisp  --eval '(quit)'

docs: docs/build/index.html

docs/build/index.html: $(docfiles)
	cd docs && ~/.virtualenvs/d/bin/d

pubdocs: docs
	hg -R ~/src/sjl.bitbucket.org pull -u
	rsync --delete -a ./docs/build/ ~/src/sjl.bitbucket.org/bones
	hg -R ~/src/sjl.bitbucket.org commit -Am 'bones: Update site.'
	hg -R ~/src/sjl.bitbucket.org push

bench:
	sbcl-rlwrap --noinform --load examples/bench.lisp  --eval '(quit)'
