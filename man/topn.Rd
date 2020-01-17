\name{topn}
\alias{topn}
\title{ Top N values index}
\description{
The function \code{topn} is equivalent to \code{base} function \code{order} when used like \code{order(vec)[1:n]}. \code{topn} must be much faster than \code{order} for \code{n < 1000}.
}
\usage{
  topn(vec, n=6L, decreasing=FALSE)
}
\arguments{
  \item{vec}{ A numeric vector of type double or integer. Other types are not supported. }
  \item{n}{ An positve integer value greater or equal to 1. Maximum value is 1000. }
  \item{decreasing}{ A logical value to indicate whether to sort \code{vec} by decreasing or increasing value. Equivalent to argument \code{decreasing} in function \code{base::order}. Default value is \code{FALSE}.}
}
\value{
Return the index of the top \code{n} value (positive of negative) in vector \code{vec}.
}
\examples{
# Example 1: 

# Example 2: 

# Example 3: 

}
\keyword{ data }
