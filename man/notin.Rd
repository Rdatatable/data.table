\name{notin}
\alias{\%notin\%}
\title{
Convenience operator for checking if an example is not in a set of elements
}
\description{
Check whether an object is absent from a table, i.e., the logical inverse of \code{\link[=base]{in}}. See examples on how missing values are being handled.
}
\usage{
x \%notin\% table
}
\arguments{
   \item{x}{ Vector or \code{NULL}: the values to be matched. }
   \item{table}{ Vector or \code{NULL}: the values to be matched against. }
}
\value{
    Logical vector, \code{TRUE} for each element of \code{x} \emph{absent} from \code{table}, and \code{FALSE} for each element of \code{x} \emph{present} in \code{table}.
}
\seealso{ \code{\link[base]{match}}, \code{\link[data.table]{chmatch}}  }
\examples{
  11 \%notin\% 1:10 # TRUE
  "a" \%notin\% c("a", "b") # FALSE

  ## NAs on the LHS
  NA \%in\% 1:2
  NA \%notin\% 1:2
  ## NAs on the RHS
  NA \%in\% c(1:2,NA)
  NA \%notin\% c(1:2,NA)
}
