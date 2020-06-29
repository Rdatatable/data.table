
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
	$(RM) data.table_1.12.9.tar.gz
	$(RM) src/*.o
	$(RM) src/*.so

.PHONY: build
build:
	$(R) CMD build . --no-build-vignettes

.PHONY: install
install:
	$(R) CMD INSTALL data.table_1.12.9.tar.gz

.PHONY: uninstall
uninstall:
	$(R) CMD REMOVE data.table || true

.PHONY: test
test:
	$(R) -e 'require(data.table); test.data.table()'

.PHONY: check
check:
	_R_CHECK_CRAN_INCOMING_REMOTE_=false $(R) CMD check data.table_1.12.9.tar.gz --as-cran --ignore-vignettes --no-stop-on-test-error

.PHONY: revision
revision:
	echo "Revision: $(shell git rev-parse HEAD)" >> DESCRIPTION
