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
  options. NB: If you're reading this on the web, make sure the version numbers match with what you have installed.
}

\section{Printing Options}{
  See \code{\link{print.data.table}} for a full description of printing data.tables.
  \describe{
   \item{\code{datatable.print.topn}}{An integer, default \code{5L}. When a data.table is printed,
     only the first topn and last topn rows are displayed.}
   \item{\code{datatable.print.nrows}}{An integer, default \code{100L}. The total number of rows
     to print before the topn logic is triggered.}
   \item{\code{datatable.print.class}}{A logical, default \code{FALSE}. If \code{TRUE}, the class of
     each column is printed below its name.}
   \item{\code{datatable.print.keys}}{A logical, default \code{FALSE}. If \code{TRUE}, the table's
     keys are printed above the data.}
   \item{\code{datatable.show.indices}}{A logical, default \code{TRUE}. A synonym for \code{datatable.print.keys} for historical reasons.}
   \item{\code{datatable.print.trunc.cols}}{A logical, default \code{FALSE}. If \code{TRUE} and a
     table has more columns than fit on the screen, it truncates the middle columns.}
   \item{\code{datatable.prettyprint.char}}{An integer, default \code{100L}. The maximum number of
     characters to display in a character column cell before truncating.}
   \item{\code{datatable.print.colnames}}{A logical, default \code{TRUE}. If \code{TRUE}, prints column names.}
   \item{\code{datatable.print.rownames}}{A logical, default \code{TRUE}. If \code{TRUE}, prints row numbers.}
  }
}

\section{File I/O Options (fread and fwrite)}{
  See \code{\link{fread}} and \code{\link{fwrite}} for a full description of data.table I/O.
  \describe{
    \item{\code{datatable.fread.input.cmd.message}}{A logical, default \code{TRUE}. If \code{TRUE},
      \code{fread} will print the shell command it is using when the input is a
      command (e.g., \code{fread("grep ...")}).}
    \item{\code{datatable.fread.datatable}}{A logical, default \code{TRUE}. If \code{TRUE}, \code{fread}
      returns a \code{data.table}. If \code{FALSE}, it returns a \code{data.frame}.}
    \item{\code{datatable.integer64}}{A character string, default \code{"integer64"}. Controls how \code{fread}
      handles 64-bit integers. Can be "integer64", "double", or "character".}
    \item{\code{datatable.logical01}}{A logical, default \code{FALSE}. If \code{TRUE}, \code{fread} will
      interpret columns containing only 0 and 1 as logical.}
    \item{\code{datatable.keepLeadingZeros}}{A logical, default \code{FALSE}. If \code{TRUE}, \code{fread}
      preserves leading zeros in character columns by reading them as strings;
      otherwise they may be coerced to numeric.}
    \item{\code{datatable.logicalYN}}{A logical, default \code{FALSE}. If \code{TRUE}, \code{fread}
      will interpret "Y" and "N" as logical.}
    \item{\code{datatable.na.strings}}{A character vector, default \code{"NA"}. Global default for strings that
      \code{fread} should interpret as \code{NA}.}
    \item{\code{datatable.fwrite.sep}}{A character string, default \code{","}. The default separator
      used by \code{fwrite}.}
    \item{\code{datatable.showProgress}}{An integer or logical, default \code{\link[base]{interactive}()}. Controls whether
      long-running operations like \code{fread} display a progress bar.}
  }
}

\section{Join and Subset Options}{
  \describe{
    \item{\code{datatable.allow.cartesian}}{A logical, default \code{FALSE}. Controls the default value of the 
    \code{allow.cartesian} parameter; see \code{\link{data.table}}. If the value of this parameter is FALSE, an error is raised as a safeguard against an explosive Cartesian join.}
    \item{\code{datatable.join.many}}{A logical. Stub description to be embellished later in PR #4370. }
  }
}

\section{Performance and Indexing Options}{
  \describe{
    \item{\code{datatable.auto.index}}{A logical, default \code{TRUE}. If \code{TRUE}, \code{data.table}
      automatically creates a secondary index on-the-fly when a column is first
      used in a subset, speeding up all subsequent queries.}
    \item{\code{datatable.use.index}}{A logical, default \code{TRUE}. A global switch to control
      whether existing secondary indices are used for subsetting.}
    \item{\code{datatable.forder.auto.index}}{A logical, default \code{TRUE}. Similar to \code{datatable.auto.index},
      but applies to ordering operations (\code{forder}).}
    \item{\code{datatable.optimize}}{A numeric, default \code{Inf}. Controls the GForce query
      optimization engine. The default enables all possible optimizations.
      See \code{\link{datatable.optimize}}.}
    \item{\code{datatable.alloccol}}{An integer, default \code{1024L}. Controls the number of column
      slots to pre-allocate, improving performance when adding many columns.
      See \code{\link{alloc.col}}.}
    \item{\code{datatable.reuse.sorting}}{A logical, default \code{TRUE}. If \code{TRUE}, \code{data.table}
      can reuse the sorted order of a table in joins, improving performance.}
  }
}

\section{Development and Verbosity Options}{
  \describe{
    \item{\code{datatable.quiet}}{A logical, default \code{FALSE}. The master switch to suppress all
      \code{data.table} status messages, including the startup message.}
    \item{\code{datatable.verbose}}{A logical, default \code{FALSE}. If \code{TRUE}, \code{data.table} will
      print detailed diagnostic information as it processes a query.}
    \item{\code{datatable.enlist}}{Experimental feature. Default is \code{NULL}. If set to a function
      (e.g., \code{list}), the \code{j} expression can return a \code{list}, which will then
      be "enlisted" into columns in the result.}
  }
}

\seealso{
  \code{\link[base]{options}},
  \code{\link[base]{getOption}},
  \code{\link{data.table}}
}

\keyword{data}
\keyword{utilities}
