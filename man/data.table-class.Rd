\name{data.table-class}
\docType{class}

\alias{class:data.table}
\alias{data.table-class}

\title{S4 Definition for data.table}
\description{
  A \code{data.table} can be used in S4 class definitions as either
  a parent class (inside a \code{contains} argument of \code{setClass}),
  or as an element of an S4 slot.
}

% \details{
% }


\author{ Steve Lianoglou }
\seealso{
  \code{\link{data.table}}, \code{\link{tables}}, \code{\link{J}}, \code{\link[base:order]{sort.list}}, \code{\link{copy}}, \code{\link{setDT}}, \code{\link{setDF}}, \code{\link{set}}, \code{\link{:=}}, \code{\link{setorder}}, \code{\link{setcolorder}}, \code{\link{setattr}}, \code{\link{setnames}}, \code{\link{chorder}}, \code{\link{setNumericRounding}}, \code{\link{.internal.selfref}}
}
\examples{
## Used in inheritance.
setClass('SuperDataTable', contains='data.table')

## Used in a slot
setClass('Something', representation(x='character', dt='data.table'))
x <- new("Something", x='check', dt=data.table(a=1:10, b=11:20))
}

\keyword{classes}
\keyword{methods}
