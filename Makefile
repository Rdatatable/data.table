
R ?= R

.PHONY: all
all:
	$(MAKE) clean
	$(MAKE) build
	$(MAKE) install
	$(MAKE) test
	$(MAKE) check

.PHONY: some
some:
	$(MAKE) clean
	$(MAKE) build
	$(MAKE) install
	$(MAKE) test

.PHONY: clean
clean:
	rm -f data.table_1.12.1.tar.gz

.PHONY: build
build:
	$(R) CMD build . --no-build-vignettes

.PHONY: install
install:
	$(R) CMD INSTALL data.table_1.12.1.tar.gz

.PHONY: uninstall
uninstall:
	$(R) CMD REMOVE data.table || true

.PHONY: test
test:
	$(R) -e 'require(data.table); test.data.table()'

.PHONY: check
check:
	_R_CHECK_CRAN_INCOMING_REMOTE_=false $(R) CMD check data.table_1.12.1.tar.gz --as-cran --ignore-vignettes --no-stop-on-test-error

