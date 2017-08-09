
R ?= R

.PHONY: all
all:
	$(MAKE) clean
	$(MAKE) build
	$(MAKE) install
	$(MAKE) test

.PHONY: clean
clean:
	rm -f data.table_1.10.5.tar.gz

.PHONY: build
build:
	$(R) CMD build . --no-build-vignettes

.PHONY: install
install:
	$(R) CMD install data.table_1.10.5.tar.gz

.PHONY: test
test:
	$(R) -e 'require(data.table); test.data.table()'
