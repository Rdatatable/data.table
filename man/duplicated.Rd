\name{duplicated}
\alias{duplicated}
\alias{duplicated.data.table}
\alias{unique}
\alias{unique.data.table}
\alias{anyDuplicated}
\alias{anyDuplicated.data.table}
\alias{uniqueN}
\title{ Determine Duplicate Rows }
\description{
     \code{duplicated} returns a logical vector indicating which rows of a \code{data.table} (by 
     key columns or when no key all columns) are duplicates of a row with smaller subscripts.

     \code{unique} returns a \code{data.table} with duplicated rows (by key) removed, or
     (when no key) duplicated rows by all columns removed.

     \code{anyDuplicated} returns the \emph{index} \code{i} of the first duplicated entry if there is one, and 0 otherwise. 

     \code{uniqueN} is equivalent to \code{length(unique(x))} but much faster for \code{atomic vectors}, \code{data.frames} and \code{data.tables}, for other types it dispatch to \code{length(unique(x))}. The number of unique rows are computed directly without materialising the intermediate unique data.table and is therefore memory efficient as well.

}
\usage{
\method{duplicated}{data.table}(x, incomparables=FALSE, fromLast=FALSE, by=key(x), ...)

\method{unique}{data.table}(x, incomparables=FALSE, fromLast=FALSE, by=key(x), ...)

\method{anyDuplicated}{data.table}(x, incomparables=FALSE, fromLast=FALSE, by=key(x), ...)

uniqueN(x, by=if (is.data.table(x)) key(x) else NULL)
}
\arguments{
  \item{x}{ Atomic vectors, lists, data.frames or data.tables.}
  \item{\dots}{ Not used at this time. }
  \item{incomparables}{ Not used. Here for S3 method consistency. }
  \item{fromLast}{ logical indicating if duplication should be considered from the reverse side, i.e., the last (or rightmost) of identical elements would correspond to \code{duplicated = FALSE}.}
  \item{by}{
    \code{character} or \code{integer} vector indicating which combinations of
    columns form \code{x} to use for uniqueness checks. Defaults to
    \code{key(x))} which, by default, only uses the keyed columns. \code{by=NULL}
    uses all columns and acts like the analogous
    \code{data.frame} methods.
  }
}
\details{
  Because data.tables are usually sorted by key, tests for duplication are   especially quick when only the keyed columns are considered. Unlike \code{\link[base]{unique.data.frame}}, \code{paste} is not used to ensure equality of floating point data. It is instead accomplished directly (for speed) whilst avoiding unexpected behaviour due to floating point representation by rounding the last two bytes off the significand (default) as explained in \code{\link{setNumericRounding}}.

  \code{v1.9.4} introduces \code{anyDuplicated} method for data.tables and is similar to base in functionality. It also implements the logical argument \code{fromLast} for all three functions, with default value \code{FALSE}. 

  Any combination of columns can be used to test for uniqueness (not just the
  key columns) and are specified via the \code{by} parameter. To get
  the analagous \code{data.frame} functionality, set \code{by} to \code{NULL}.
}
\value{
     \code{duplicated} returns a logical vector of length \code{nrow(x)}
  indicating which rows are duplicates.

     \code{unique} returns a data table with duplicated rows removed.

     \code{anyDuplicated} returns a integer value with the index of first duplicate. If none exists, 0L is returned.

     \code{uniqueN} returns the number of unique elements in the vector, \code{data.frame} or \code{data.table}.

}
\seealso{ \code{\link{setNumericRounding}}, \code{\link{data.table}}, \code{\link{duplicated}}, \code{\link{unique}}, \code{\link{all.equal}} }
\examples{
DT <- data.table(A = rep(1:3, each=4), B = rep(1:4, each=3), C = rep(1:2, 6), key = "A,B")
duplicated(DT)
unique(DT)

duplicated(DT, by="B")
unique(DT, by="B")

duplicated(DT, by=c("A", "C"))
unique(DT, by=c("A", "C"))

DT = data.table(a=c(2L,1L,2L), b=c(1L,2L,1L))   # no key
unique(DT)                   # rows 1 and 2 (row 3 is a duplicate of row 1)

DT = data.table(a=c(3.142, 4.2, 4.2, 3.142, 1.223, 1.223), b=rep(1,6))
unique(DT)                   # rows 1,2 and 5

DT = data.table(a=tan(pi*(1/4 + 1:10)), b=rep(1,10))   # example from ?all.equal
length(unique(DT$a))         # 10 strictly unique floating point values
all.equal(DT$a,rep(1,10))    # TRUE, all within tolerance of 1.0
DT[,which.min(a)]            # row 10, the strictly smallest floating point value
identical(unique(DT),DT[1])  # TRUE, stable within tolerance
identical(unique(DT),DT[10]) # FALSE

# fromLast=TRUE
DT <- data.table(A = rep(1:3, each=4), B = rep(1:4, each=3), C = rep(1:2, 6), key = "A,B")
duplicated(DT, by="B", fromLast=TRUE)
unique(DT, by="B", fromLast=TRUE)

# anyDuplicated
anyDuplicated(DT, by=c("A", "B"))    # 3L
any(duplicated(DT, by=c("A", "B")))  # TRUE

# uniqueN, unique rows on key columns
uniqueN(DT)
# uniqueN, unique rows on all all columns
uniqueN(DT, by=NULL)
# uniqueN while grouped by "A"
DT[, .(uN=uniqueN(.SD)), by=A]
}
\keyword{ data }

