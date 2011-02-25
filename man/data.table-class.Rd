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
  \code{\link{data.table}}
}  

\examples{
  ## Used in inheritence.
  setClass('SuperDataTable', contains='data.table')
  
  ## Used in a slot
  setClass('Something', representation(x='character', dt='data.table'))
  x <- new("Something", x='check', dt=data.table(a=1:10, b=11:20))
}

\keyword{classes}
\keyword{methods}
