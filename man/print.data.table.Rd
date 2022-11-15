\name{print.data.table}
\alias{print.data.table}
\alias{format_col}
\alias{format_col.default}
\alias{format_col.POSIXct}
\alias{format_col.expression}
\alias{format_list_item}
\alias{format_list_item.default}
\title{ data.table Printing Options }
\description{
  \code{print.data.table} extends the functionalities of \code{print.data.frame}.

  Key enhancements include automatic output compression of many observations and concise column-wise \code{class} summary.

  \code{format_col} and \code{format_list_item} generics provide flexibility for end-users to define custom printing methods for generic classes.
}
\usage{
  \method{print}{data.table}(x,
    topn=getOption("datatable.print.topn"),             # default: 5
    nrows=getOption("datatable.print.nrows"),           # default: 100
    class=getOption("datatable.print.class"),           # default: TRUE
    row.names=getOption("datatable.print.rownames"),    # default: TRUE
    col.names=getOption("datatable.print.colnames"),    # default: "auto"
    print.keys=getOption("datatable.print.keys"),       # default: TRUE
    trunc.cols=getOption("datatable.print.trunc.cols"), # default: FALSE
    quote=FALSE,
    timezone=FALSE, \dots)

  format_col(x, \dots)
  \method{format_col}{default}(x, \dots)
  \method{format_col}{POSIXct}(x, \dots, timezone=FALSE)
  \method{format_col}{expression}(x, \dots)

  format_list_item(x, \dots)
  \method{format_list_item}{default}(x, \dots)
}
\arguments{
  \item{x}{ A \code{data.table}. }
  \item{topn}{ The number of rows to be printed from the beginning and end of tables with more than \code{nrows} rows. }
  \item{nrows}{ The number of rows which will be printed before truncation is enforced. }
  \item{class}{ If \code{TRUE}, the resulting output will include above each column its storage class (or a self-evident abbreviation thereof). }
  \item{row.names}{ If \code{TRUE}, row indices will be printed alongside \code{x}. }
  \item{col.names}{ One of three flavours for controlling the display of column names in output. \code{"auto"} includes column names above the data, as well as below the table if \code{nrow(x) > 20}. \code{"top"} excludes this lower register when applicable, and \code{"none"} suppresses column names altogether (as well as column classes if \code{class = TRUE}. }
  \item{print.keys}{ If \code{TRUE}, any \code{\link{key}} and/or \code{\link[=indices]{index}} currently assigned to \code{x} will be printed prior to the preview of the data. }
  \item{trunc.cols}{ If \code{TRUE}, only the columns that can be printed in the console without wrapping the columns to new lines will be printed (similar to \code{tibbles}). }
  \item{quote}{ If \code{TRUE}, all output will appear in quotes, as in \code{print.default}. }
  \item{timezone}{ If \code{TRUE}, time columns of class POSIXct or POSIXlt will be printed with their timezones (if attribute is available). }
  \item{\dots}{ Other arguments ultimately passed to \code{format}. }
}
\value{
  \code{print.data.table} returns \code{x} invisibly.

  \code{format_col} returns a \code{length(x)}-size \code{character} vector.

  \code{format_list_item} returns a length-1 \code{character} scalar.
}
\details{
  By default, with an eye to the typically large number of observations in a \code{data.table}, only the beginning and end of the object are displayed (specifically, \code{head(x, topn)} and \code{tail(x, topn)} are displayed unless \code{nrow(x) < nrows}, in which case all rows will print).

  \code{format_col} is applied at a column level; for example, \code{format_col.POSIXct} is used to tag the time zones of \code{POSIXct} columns. \code{format_list_item} is applied to the elements (rows) of \code{list} columns; see Examples. The default \code{format_col} method uses \code{\link[utils]{getS3method}} to test if a \code{format} method exists for the column, and if so uses it. Otherwise, the default \code{format_list_item} method uses the S3 format method (if one exists) for each item of a \code{list} column.
}
\seealso{\code{\link{print.default}}}
\examples{
  #output compression
  DT <- data.table(a = 1:1000)
  print(DT, nrows = 100, topn = 4)

  #`quote` can be used to identify whitespace
  DT <- data.table(blanks = c(" 12", " 34"),
                   noblanks = c("12", "34"))
  print(DT, quote = TRUE)

  #`class` provides handy column type summaries at a glance
  DT <- data.table(a = vector("integer", 3),
                   b = vector("complex", 3),
                   c = as.IDate(paste0("2016-02-0", 1:3)))
  print(DT, class = TRUE)

  #`row.names` can be eliminated to save space
  DT <- data.table(a = 1:3)
  print(DT, row.names = FALSE)

  #`print.keys` can alert which columns are currently keys
  DT <- data.table(a=1:3, b=4:6, c=7:9, key="b,a")
  setindexv(DT, c("a", "b"))
  setindexv(DT, "a")
  print(DT, print.keys=TRUE)

  # `trunc.cols` will make it so only columns that fit in console will be printed
  #    with a message that states the variables not shown
  old_width = options("width" = 40)
  DT <- data.table(thing_11 = vector("integer", 3),
                   thing_21 = vector("complex", 3),
                   thing_31 = as.IDate(paste0("2016-02-0", 1:3)),
                   thing_41 = "aasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf",
                   thing_51 = vector("integer", 3),
                   thing_61 = vector("complex", 3))
  print(DT, trunc.cols=TRUE)
  options(old_width)

  # Formatting customization
  format_col.complex = function(x, ...) sprintf('(\%.1f, \%.1fi)', Re(x), Im(x))
  x = data.table(z = c(1 + 3i, 2 - 1i, pi + 2.718i))
  print(x)

  iris = as.data.table(iris)
  iris_agg = iris[ , .(reg = list(lm(Sepal.Length ~ Petal.Length))), by = Species]
  format_list_item.lm = function(x, ...) sprintf('<lm:\%s>', format(x$call$formula))
  print(iris_agg)
}

