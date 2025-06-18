\name{data.table-options}
\alias{data.table-options}
\alias{data.table.options}

\title{Global Options for the data.table Package}

\description{
  The data.table package uses a number of global options to control its
  behavior. These are regular R options that can be set with options()
  and retrieved with getOption().

  This page provides a comprehensive, up-to-date list of all user-configurable
  options.
}

\usage{
\preformatted{
# Get the current value of an option
getOption("datatable.print.topn")

# Set a new value for an option
options(datatable.print.topn = 10)
}
}

\section{Printing Options}{
\details{
    \describe{
        \item{\code{datatable.print.topn}}{An integer. When a data.table is printed,
            only the first topn and last topn rows are displayed. This prevents
            cluttering the console for large tables.
            Default: \code{5L}. See \code{\link{print.data.table}}.}
        \item{\code{datatable.print.nrows}}{An integer. The total number of rows
            to print before the topn logic is triggered.
            Default: \code{100L}.}
        \item{\code{datatable.print.class}}{A logical. If \code{TRUE}, the class of
            each column is printed below the column name.
            Default: \code{FALSE}.}
        \item{\code{datatable.print.keys}}{A logical. If \code{TRUE}, the table's
            keys are printed above the data.
            Default: \code{FALSE}.}
        \item{\code{datatable.print.trunc.cols}}{A logical. If \code{TRUE} and a
            table has more columns than fit on the screen, it truncates the middle
            columns for printing (e.g., a, b, ..., z). If \code{FALSE}, it wraps
            the columns to the next line.
            Default: \code{FALSE}.}
        \item{\code{datatable.prettyprint.char}}{An integer. The maximum number of
            characters to display in a character column cell before truncating
            with ....
            Default: \code{100L}.}
        }
        }

        \section{File I/O Options (fread and fwrite)}{
        \describe{
        \item{\code{datatable.fread.input.cmd.message}}{A logical. If \code{TRUE},
            fread will print the shell command it is using when the input is a
            command (e.g., fread("grep ...")).
            Default: \code{TRUE}. See \code{\link{fread}}.}
        \item{\code{datatable.fread.datatable}}{A logical. If \code{TRUE}, fread
            returns a data.table. If FALSE, it returns a data.frame.
            Default: \code{TRUE}.}
        \item{\code{datatable.integer64}}{A character string. Controls how fread
            handles 64-bit integers. Can be "integer64" (requires bit64 package),
            "double" (loses precision), or "character" (reads as text).
            Default: \code{"integer64"}.}
        \item{\code{datatable.logical01}}{A logical. If \code{TRUE}, fread will
            interpret columns containing only 0 and 1 as logical FALSE/TRUE.
            Default: \code{FALSE}.}
        \item{\code{datatable.showProgress}}{An integer or logical. Controls whether
            long-running operations like fread display a progress bar. Default
            is \code{interactive()}, showing it only in interactive R sessions.}
        }
        }

        \section{Join and Subset Options}{
        \describe{
        \item{\code{datatable.allow.cartesian}}{A logical. A safety feature to prevent
            accidental memory-exploding joins. If FALSE (default), a join is not
            allowed if the result would be more rows than max(nrow(x), nrow(i)),
            which occurs when a row in i matches more than one row in x.
            Default: \code{FALSE}. See \code{\link{data.table}}.}
        \item{\code{datatable.nomatch}}{Controls the behavior of non-matching rows in
            a join. The default is NA, which returns NA for columns of x when a
            row in i has no match. Can be set to 0L to drop non-matching rows,
            behaving like an inner join.
            Default: \code{NA}.}
        }
        }

        \section{Performance and Indexing Options}{
        \describe{
        \item{\code{datatable.auto.index}}{A logical. If \code{TRUE}, data.table
            automatically creates a secondary index on-the-fly the first time a column
            is used in a query (e.g., DT[col == 'value']). This dramatically
            speeds up all subsequent queries on that same column.
            Default: \code{TRUE}.}
        \item{\code{datatable.use.index}}{A logical. A global switch to control
            whether existing indices are used for subsetting.
            Default: \code{TRUE}.}
        \item{\code{datatable.optimize}}{An integer controlling the GForce query
            optimization engine. Set to Inf to enable all possible optimizations,
            which data.table does by default. See \code{\link{datatable.optimize}}.
            Default: \code{Inf}.}
        \item{\code{datatable.alloccol}}{An integer. data.table pre-allocates
            memory for a certain number of columns when first created. This option
            controls the length of this pre-allocation, improving performance when
            adding many columns via :=. See \code{\link{alloc.col}}.
            Default: \code{1024L}.}
        }
        }

        \section{Development and Verbosity Options}{
        \describe{
        \item{\code{datatable.verbose}}{A logical. If \code{TRUE}, data.table will
            print detailed, step-by-step diagnostic information as it processes a
            query. Extremely useful for debugging and performance tuning.
            Default: \code{FALSE}.}
        \item{\code{datatable.pedantic}}{A logical. If \code{TRUE}, data.table
            enters a "pedantic" mode, issuing helpful warnings for situations that
            are not technically errors but might be unintentional (e.g., when a
            variable in j is found in the global environment instead of inside the
            data.table).
            Default: \code{FALSE}.}
        \item{\code{datatable.dfdispatchwarn}}{A logical. If \code{TRUE}, warns
            when a generic function from another package (e.g., dplyr::filter) is
            applied to a data.table. This can be a useful reminder that you are not
            using data.table's optimized methods.
            Default: \code{TRUE}.}
        }
        }

        \section{Internal Strings (Not User Options)}{
        \describe{
        \item{The following strings are not options to be set by users.}{They are
        included here to assist developers searching the source code. They are typically
        parts of URLs found in code comments or documentation.
        \itemize{
            \item \code{datatable.com}
            \item \code{datatable.github.io}
            \item \code{datatable.gitlab.io}
        }
        }
    }
  }
}

\seealso{
  \code{\link{options}},
  \code{\link{getOption}},
  \code{\link{data.table}}
}

\keyword{data}
\keyword{utilities}