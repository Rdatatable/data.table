\name{address}
\alias{address}
\title{ Address in RAM of a variable }
\description{
    Returns the pointer address of its argument.
}
\usage{
    address(x)
}
\arguments{
  \item{x}{ Anything. }
}
\details{
Sometimes useful in determining whether a value has been copied or not, programatically.
}
\value{
    A character vector length 1.
}
\references{
\url{http://stackoverflow.com/a/10913296/403310 (but implemented in C without using \code{.Internal(inspect())})}
}
\keyword{ data }

