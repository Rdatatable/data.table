# data.table developer

## Utilities

### [`cc.R`](./cc.R)

Developer helper script providing `cc` function. If one starts R session in `data.table` project root directory `.dev/cc.R` file should be automatically sourced (due to local `.Rprofile` file) making `cc()` (and `dd()`) function available straightaway.

```r
cc(test=FALSE, clean=FALSE, debug=FALSE, omp=!debug, cc_dir, path=Sys.getenv("PROJ_PATH"), CC="gcc")
```

Use `cc()` to re-compile all C sources and attach all `data.table` R functions (including non-exported ones).
One might want to re-compile and run main test script `"tests.Rraw"` automatically, then `cc(test=TRUE)` should be used. As of now running main tests with `cc(T)` requires to uninstall package to avoid S4 classes namespace issues (see [#3808](https://github.com/Rdatatable/data.table/issues/3808)).
When working on a bigger feature, one may want to put new unit tests into dedicated test file, then `cc("feature.Rraw")` can be used to run only chosen test script.

Usage of `cc()` from `R`:
```r
# re-compile and attach
cc()
# change some files, re-compile and re-attach
cc()
# compile, reload and run main test script
cc(T)
# clean, compile, reload
cc(F, T)
# clean, compile using specific compiler version, reload
cc(F, T, CC="gcc-8")
```

Usage of `dd()` from `R -d gdb`:
```r
run
dd()
# Ctrl-C
# break file.c:line
# c
# test and step between R and C
```
For more details see [Debugging compiled code](https://cloud.r-project.org/doc/manuals/R-exts.html#Debugging-compiled-code).

### [`Makefile`](./../Makefile)

We provide `make` aliases to R commands commonly used during package development, see simplified examples below.
```sh
make build && make check
# R CMD build .
# R CMD check data.table_*.tar.gz
```
If changes were made to vignettes one should call `R CMD` explicitly as `make`'s `build` or `check` are actually ignoring vignettes. See [`Makefile`](./../Makefile) for exact commands.

```sh
make build && make install && make test
# R CMD build .
# R CMD INSTALL data.table_*.tar.gz
# R -e 'require(data.table); test.data.table()'
```
To speed up testing of changes one can use `cc()` function instead of `make` commands.

### [`CRAN_Release.cmd`](./CRAN_Release.cmd)

Procedure of multiple different checks that has to be performed as a CRAN release process.

### [`revdep.R`](./revdep.R)

Script used to check breaking changes in `data.table` on reverse dependencies from CRAN and BioC.

## Windows users

If a developer is using Windows OS we suggests to install [MinGW-w64](https://mingw-w64.org) (or similar software) in order to operate in Linux-like environment. This will allow one to use `cc()` R function, `make` commands, and many others Linux built-in productive utilities. Note that recent versions of Windows OS might be shipped with Linux embedded.
