.PHONY: test pubdocs test-sbcl test-ccl test-ecl

sourcefiles = $(shell ffind --full-path --dir src --literal .lisp)
docfiles = $(shell ls docs/*.markdown)
apidoc = docs/03-reference.markdown

# Testing ---------------------------------------------------------------------
test: test-sbcl test-ccl test-ecl

test-sbcl:
	echo; figlet -kf computer 'SBCL' | sed -Ee 's/ +$$//' | tr -s '\n' | lolcat --freq=0.25; echo
	ros run -L sbcl --load test/run.lisp

test-ccl:
	echo; figlet -kf slant 'CCL' | sed -Ee 's/ +$$//' | tr -s '\n' | lolcat --freq=0.25; echo
	ros run -L ccl-bin --load test/run.lisp

test-ecl:
	echo; figlet -kf roman 'ECL' | sed -Ee 's/ +$$//' | tr -s '\n' | lolcat --freq=0.25; echo
	ros run -L ecl --load test/run.lisp


# Quickutils ------------------------------------------------------------------
vendor/quickutils.lisp: vendor/make-quickutils.lisp
	cd vendor && ros run -L sbcl --load make-quickutils.lisp


# Documentation ---------------------------------------------------------------
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
