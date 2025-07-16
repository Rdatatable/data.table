\name{setcolorder}
\alias{setcolorder}

\title{Fast column reordering of a data.table by reference}
\description{
  In \code{data.table} parlance, all \code{set*} functions change their input \emph{by reference}. That is, no copy is made at all, other than temporary working memory, which is as large as one column. The only other \code{data.table} operator that modifies input by reference is \code{\link{:=}}. Check out the \code{See Also} section below for other \code{set*} function \code{data.table} provides.

  \code{setcolorder} reorders the columns of data.table, \emph{by reference}, to the new order provided.
}

\usage{
setcolorder(x, neworder=key(x), before=NULL, after=NULL, skip_absent=FALSE)
}
\arguments{
  \item{x}{ A \code{data.table}. }
  \item{neworder}{ Character vector of the new column name ordering. May also be column numbers. If \code{length(neworder) < length(x)}, the specified columns are moved in order to the "front" of \code{x}. By default, \code{setcolorder} without a specified \code{neworder} moves the key columns in order to the "front" of \code{x}. }
  \item{before, after}{ If one of them (not both) was provided with a column name or number, \code{neworder} will be inserted before or after that column. }
  \item{skip_absent}{ Logical, default \code{FALSE}. If \code{neworder} includes columns not present in \code{x}, \code{TRUE} will silently ignore them, whereas \code{FALSE} will throw an error. }
}
\details{
  To reorder \code{data.table} columns, the idiomatic way is to use \code{setcolorder(x, neworder)}, instead of doing \code{x <- x[, ..neworder]} (or \code{x <- x[, neworder, with=FALSE]}). This is because the latter makes an entire copy of the \code{data.table}, which maybe unnecessary in most situations. \code{setcolorder} also allows column numbers instead of names for \code{neworder} argument, although we recommend using names as a good programming practice.
}
\value{
    The input is modified by reference, and returned (invisibly) so it can be used in compound statements. If you require a copy, take a copy first (using \code{DT2 = copy(DT)}). See \code{?copy}.
}
\seealso{ \code{\link{setkey}}, \code{\link{setorder}}, \code{\link{setattr}}, \code{\link{setnames}}, \code{\link{set}}, \code{\link{:=}}, \code{\link{setDT}}, \code{\link{setDF}}, \code{\link{copy}}, \code{\link{getNumericRounding}}, \code{\link{setNumericRounding}}
}
\examples{

set.seed(45L)
DT = data.table(A=sample(3, 10, TRUE),
         B=sample(letters[1:3], 10, TRUE), C=sample(10))

setcolorder(DT, c("C", "A", "B"))

#incomplete specification
setcolorder(DT, "A")

# insert new column as first column
set(DT, j="D", value=sample(10))
setcolorder(DT, "D", before=1)

# move column to last column place
setcolorder(DT, "A", after=ncol(DT))
}
\keyword{ data }

