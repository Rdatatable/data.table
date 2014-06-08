# data.table

[![Build Status](https://travis-ci.org/Rdatatable/datatable.svg?branch=master)](https://travis-ci.org/Rdatatable/datatable)

R package `data.table` extends `data.frame`.

Fast aggregation of large data (e.g. 100GB in RAM), fast ordered joins, fast add/modify/delete of columns by reference by group using no copies at all, cells can contain vectors, chained queries and a fast file reader (`fread`). Offers a natural and flexible syntax, for faster development.

The main benefit of `data.table` is its syntax - ability to combine `where`, `select|update` and `by` into one query without having to string together a sequence of isolated function calls. Infact, speed is only secondary. 

> ####  `data.table` builds on base-R functionality to reduce two types of time:
> 1) Programming time (easier to write, read, debug and maintain).  
> 2) Compute time.  

# Operations using data.table:

    * DT[X]                          # fast join for large data, DT and X are data.tables.
    * DT[, sum(b*c), by=a]           # fast aggregation, a, b and c are column names
    * DT[i, b := 3.14]               # add new column (or modify existing column) by reference
    * DT[, p := x/sum(x), by=grp]    # fast sub-assignment (to column b) by reference.
    * fread('big.csv')               # is 3+ times faster than read.csv(, colClasses, nrow, etc).

# Installation

All even numbered releases (ex: 1.9.0, 1.9.2 etc.) are stable versions available on CRAN. Similarly all odd numbered releases are development versions.

## Stable version

The current stable release is [v1.9.2 on CRAN](http://cran.r-project.org/web/packages/data.table/index.html), released 27 Feb 2014. To install, open an R session and type:

    install.packages("data.table")

## Development version

The current development version is 1.9.3. If you're interested in staying up-to-date, you can do so by installing the latest commit using `devtools` as follows:

    devtools:::install_github("datatable", "Rdatatable")

# How to get started?

To be updated...

# Getting help

## Stackoverflow

Stackoverflow's [data.table tag](http://stackoverflow.com/questions/tagged/r+data.table) is an excellent place to get started. You can search if your question has already been answered, and if not, you can post a question [with a nice reproducible example](http://stackoverflow.com/questions/5963269/how-to-make-a-great-r-reproducible-example) there. At the time of writing, 93.7% of the questions under the data.table tag have answers.

## Mailing list

Another place to ask questions is the [data.table mailing list](https://r-forge.r-project.org/mail/?group_id=240). It requires a subscription, which is fairly straightforward. Once you've subscribed to the mailing list, you can start posting by sending an email to **datatable-help @ lists.r-forge.r-project.org**.

You can browse the questions asked previously on the mailing list on [Nabble](http://r.789695.n4.nabble.com/datatable-help-f2315188.html), [Gmane](http://dir.gmane.org/gmane.comp.lang.r.datatable), [HTML archive](http://lists.r-forge.r-project.org/pipermail/datatable-help) or [RSS feed](http://rss.gmane.org/gmane.comp.lang.r.datatable).

