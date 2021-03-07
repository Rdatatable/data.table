\name{notin}
\alias{notin}
\alias{\%notin\%}

\title{
Convenience for checking if an example is in a set of elements
}

\description{
Intended to behave opposite to  \code{\link[=base]{in}}
}

\usage{
notin(example, elements)
example \%notin\% elements
}

\arguments{
   \item{example}{ vector or \code{NULL}: value to be matched }
   \item{elements}{ vector or \code{NULL}: values to check for a match }
}

\details{
 Internally, \code{\%notin\%} is a wrapper around \code{\link[=base]{match}}, much like \code{\%in\%}.
}

\value{
    Logical vector, \code{TRUE} indicating whether each \code{example} was found in \code{elements}.
}

\seealso{ \code{\link[base]{match}} }

\examples{
  11 \%notin\% 1:10
}

