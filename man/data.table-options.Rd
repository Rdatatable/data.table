\name{data.table-options}
\alias{data.table-options}
\alias{data.table.options}
\alias{datatable.options}
\alias{datatable-options}

\title{Global Options for the data.table Package}

\description{
  The data.table package uses a number of global options to control its
  behavior. These are regular R options that can be set with options()
  and retrieved with getOption(). For example:
  \preformatted{
    # Get the current value of an option
    getOption("datatable.print.topn")

    # Set a new value for an option
    options(datatable.print.topn = 10)
  }
  This page provides a comprehensive, up-to-date list of all user-configurable
  options.
}

\section{Printing Options}{
  \describe{
    \item{\code{datatable.print.topn}}{An integer. When a data.table is printed,
      only the first topn and last topn rows are displayed.
      Default: \code{5L}. See \code{\link{print.data.table}}.}
    \item{\code{datatable.print.nrows}}{An integer. The total number of rows
      to print before the topn logic is triggered.
      Default: \code{100L}.}
    \item{\code{datatable.print.class}}{A logical. If \code{TRUE}, the class of
      each column is printed below its name.
      Default: \code{FALSE}.}
    \item{\code{datatable.print.keys}}{A logical. If \code{TRUE}, the table's
      keys are printed above the data.
      Default: \code{FALSE}.}
    \item{\code{datatable.show.indices}}{A logical. A synonym for `datatable.print.keys` for historical reasons.
      Default: \code{TRUE}.}
    \item{\code{datatable.print.trunc.cols}}{A logical. If \code{TRUE} and a
      table has more columns than fit on the screen, it truncates the middle
      columns.
      Default: \code{FALSE}.}
    \item{\code{datatable.prettyprint.char}}{An integer. The maximum number of
      characters to display in a character column cell before truncating.
      Default: \code{100L}.}
    \item{\code{datatable.print.colnames}}{A logical. If \code{TRUE}, prints column names.
      Default: \code{TRUE}.}
    \item{\code{datatable.print.rownames}}{A logical. If \code{TRUE}, prints row numbers.
      Default: \code{TRUE}.}
  }
}

\section{File I/O Options (fread and fwrite)}{
  \describe{
    \item{\code{datatable.fread.input.cmd.message}}{A logical. If \code{TRUE},
      `fread` will print the shell command it is using when the input is a
      command (e.g., `fread("grep ...")`).
      Default: \code{TRUE}. See \code{\link{fread}}.}
    \item{\code{datatable.fread.datatable}}{A logical. If \code{TRUE}, `fread`
      returns a `data.table`. If `FALSE`, it returns a `data.frame`.
      Default: \code{TRUE}.}
    \item{\code{datatable.integer64}}{A character string. Controls how `fread`
      handles 64-bit integers. Can be "integer64", "double", or "character".
      Default: \code{"integer64"}.}
    \item{\code{datatable.logical01}}{A logical. If \code{TRUE}, `fread` will
      interpret columns containing only 0 and 1 as logical.
      Default: \code{FALSE}.}
    \item{\code{datatable.keepLeadingZeros}}{A logical. If \code{TRUE}, `fread`
      preserves leading zeros in character columns by reading them as strings;
      otherwise they may be coerced to numeric.
      Default: \code{FALSE}.}
    \item{\code{datatable.logicalYN}}{A logical. If \code{TRUE}, `fread`
      will interpret "Y" and "N" as logical.
      Default: \code{FALSE}.}
    \item{\code{datatable.na.strings}}{A character vector. Global default for strings that
      `fread` should interpret as `NA`.
      Default: \code{"NA"}.}
    \item{\code{datatable.fwrite.sep}}{A character string. The default separator
      used by `fwrite`.
      Default: \code{","}.}
    \item{\code{datatable.showProgress}}{An integer or logical. Controls whether
      long-running operations like `fread` display a progress bar. Default
      is \code{interactive()}.}
  }
}

\section{Join and Subset Options}{
  \describe{
    \item{\code{datatable.allow.cartesian}}{A logical. A safety feature. If `FALSE`, a join
      is not allowed if the result would have more rows than the largest of the two tables.
      Default: \code{FALSE}. See \code{\link{data.table}}.}
    \item{\code{datatable.nomatch}}{Controls the behavior of non-matching rows in
      a join. The default is `NA`. Can be set to `0L` to drop non-matching rows.
      Default: \code{NA}.}
  }
}

\section{Performance and Indexing Options}{
  \describe{
    \item{\code{datatable.auto.index}}{A logical. If \code{TRUE}, `data.table`
      automatically creates a secondary index on-the-fly when a column is first
      used in a subset, speeding up all subsequent queries.
      Default: \code{TRUE}.}
    \item{\code{datatable.use.index}}{A logical. A global switch to control
      whether existing secondary indices are used for subsetting.
      Default: \code{TRUE}.}
    \item{\code{datatable.forder.auto.index}}{A logical. Similar to `datatable.auto.index`,
      but applies to ordering operations (`forder`).
      Default: \code{TRUE}.}
    \item{\code{datatable.optimize}}{An integer controlling the GForce query
      optimization engine. The default enables all possible optimizations.
      See \code{\link{datatable.optimize}}.
      Default: \code{Inf}.}
    \item{\code{datatable.alloccol}}{An integer. Controls the number of column
      slots to pre-allocate, improving performance when adding many columns.
      See \code{\link{alloc.col}}.
      Default: \code{1024L}.}
    \item{\code{datatable.reuse.sorting}}{A logical. If `TRUE`, `data.table`
      can reuse the sorted order of a table in joins, improving performance.
      Default: \code{TRUE}.}
  }
}

\section{Development and Verbosity Options}{
  \describe{
    \item{\code{datatable.quiet}}{A logical. The master switch to suppress all
      `data.table` status messages, including the startup message.
      Default: \code{FALSE}.}
    \item{\code{datatable.verbose}}{A logical. If \code{TRUE}, `data.table` will
      print detailed diagnostic information as it processes a query.
      Default: \code{FALSE}.}
    \item{\code{datatable.enlist}}{Experimental feature. If set to a function
      (e.g., `list`), the `j` expression can return a `list`, which will then
      be "enlisted" into columns in the result.
      Default: \code{NULL}.}
  }
}

\seealso{
  \code{\link{options}},
  \code{\link{getOption}},
  \code{\link{data.table}}
}

\keyword{data}
\keyword{utilities}
