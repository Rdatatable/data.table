\name{notin}
\alias{\%notin\%}

\title{
Convenience for checking if an example is in a set of elements
}

\description{
Intended to behave opposite to  \code{\link[=base]{in}}
}

\usage{
example \%notin\% elements
}

\arguments{
   \item{example}{ vector or \code{NULL}: value to be matched }
   \item{elements}{ vector or \code{NULL}: values to check for a match }
}

\details{
 Internally, \code{\%notin\%} is a wrapper around \code{\link[=base]{match}} and \code{\link[=data.table]{chmatch}}.
}

\value{
    Logical vector, \code{TRUE} for each element of \code{example} \emph{absent} from \code{elements}, and \code{FALSE} for each element of \code{example} \emph{present} in \code{elements}.
}

\seealso{ \code{\link[base]{match}} }

\examples{
  11 \%notin\% 1:10
}

