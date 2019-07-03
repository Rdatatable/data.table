
# data.table <a href="http://r-datatable.com"><img src="man/figures/logo.png" align="right" height="140" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/data.table)](https://cran.r-project.org/package=data.table)
[![Travis build
status](https://travis-ci.org/Rdatatable/data.table.svg?branch=master)](https://travis-ci.org/hadley/data.table)
[![Codecov test
coverage](https://codecov.io/gh/Rdatatable/data.table/branch/master/graph/badge.svg)](https://codecov.io/gh/hadley/data.table?branch=master)
<!-- badges: end -->

data.table provides a high-performance and programmer-friendly version
of R’s `data.frame`. It makes it easy to perform grouped summaries and
perform flexible joins (including ordered, rolling, and overlapping
range joins), and is optimised for high-performance on large data sets,
using parallelism and indices to make code as fast as possible.
data.table has no external dependencies and relies only on R 3.1.0,
making it well suited for use in production.

data.table also includes `fread()` for fast and friendly file reading,
`fwrite()` for fast file writing, and a collection of functions for fast
and convenient vectorised operations including `frank()`, `froll()`, and
`forder()`.

data.table is widely used by the R community. As of July 2019, it was
used by over 680 CRAN and Bioconductor packages and was the [9th most
starred](http://www.r-pkg.org/starred) R package on GitHub. If you need
help, the data.table community is active StackOverflow, with nearly
[9,000 questions](http://stackoverflow.com/questions/tagged/data.table).

## Installation

You can install the released version of data.table from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("data.table")
```

See [the data.table
wiki](https://github.com/Rdatatable/data.table/wiki/Installation) for
instructions on installing the development version.

## Cheatsheets

<a href="https://github.com/rstudio/cheatsheets/raw/master/datatable.pdf"><img src="man/figures/cheatsheet.png" width="615" height="242"/></a>

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(data.table)
## basic example code
```
