\name{print.data.table}
\alias{print.data.table}
\title{ data.table Printing Options }
\description{
  \code{print.data.table} extends the functionalities of \code{print.data.frame}.

  Key enhancements include automatic output compression of many observations and concise column-wise \code{class} summary.
}
\usage{
  \method{print}{data.table}(x,
    topn=getOption("datatable.print.topn"),         # default: 5
    nrows=getOption("datatable.print.nrows"),       # default: 100
    print.class=getOption("datatable.print.class"), # default: FALSE
    by=NULL,row.names=TRUE,quote=FALSE,...)
}
\arguments{
  \item{x}{ A \code{data.table}. }
  \item{topn}{ The number of rows to be printed from the beginning and end of tables with more than \code{nrows} rows. }
  \item{nrows}{ The number of rows which will be printed before truncation is enforced. }
  \item{print.class}{ If \code{TRUE}, the resulting output will include above each column its storage class (or a self-evident abbreviation thereof). }
  \item{by}{ Columns \code{by} which \code{x} to demarcate with white space when printing. See Details.}
  \item{row.names}{ If \code{TRUE}, row indices will be printed alongside \code{x}. }
  \item{quote}{ If \code{TRUE}, all output will appear in quotes, as in \code{print.default}. }
  \item{\dots}{ Other arguments ultimately passed to \code{format}. }
}
\details{
  By default, with an eye to the typically large number of observations in a code{data.table}, only the beginning and end of the object are displayed (specifically, \code{head(x, topn)} and \code{tail(x, topn)} are displayed unless \code{nrow(x) < nrows}, in which case all rows will print).
  
  \code{by} will, for ready at-a-glance evaluation of a grouped table, produce a table with extra rows of white space (or the user's choice set \emph{via} \code{options(datatable.print.byfill)}), inserted between each group as defined by \code{x[ , .SD, by = by]}. This is explicitly designed to be used on sorted tables; it is an error to attempt otherwise. Truncation via \code{topn} or \code{nrows} occurs first, so tinker with those options if you want a more detailed glimpse.
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
  
  #`print.class` provides handy column type summaries at a glance
  DT <- data.table(a = vector("integer", 3), 
                   b = vector("complex", 3),
                   c = as.IDate(paste0("2016-02-0", 1:3)))
  print(DT, print.class = TRUE)
  
  #`row.names` can be eliminated to save space
  DT <- data.table(a = 1:3)
  print(DT, row.names = FALSE)
}