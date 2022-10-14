.PHONY: install
dep:
	cask install

.PHONY: test
test:
	cask exec ert-runner

.PHONY: doc
doc:
	cask exec emacs -batch -l doc/create-docs.el -f create-docs
