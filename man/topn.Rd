\name{topn}
\alias{topn}
\title{ Top N values index}
\description{
  \code{topn} is used to get the indices of the few values of an input. This is an extension of \code{\link{which.max}}/\code{\link{which.min}} which provide \emph{only} the first such index. 
  
  The output is the same as \code{order(vec)[1:n]}, but internally optimized not to sort the irrelevant elements of the input (and therefore much faster, for small \code{n} relative to input size).
}
\usage{
  topn(vec, n=6L, decreasing=FALSE)
}
\arguments{
  \item{vec}{ A numeric vector of type numeric or integer. Other types are not supported yet. }
  \item{n}{ A positive integer value greater or equal to 1. Maximum value is 1000. }
  \item{decreasing}{ A logical value (default \code{FALSE}) to indicate whether to sort \code{vec} in decreasing or increasing order. Equivalent to argument \code{decreasing} in function \code{base::order}. }
}
\value{
  \code{integer} vector of indices of the most extreme (according to \code{decreasing}) \code{n} values in vector \code{vec}.
}
\examples{
x = rnorm(1e6)

# Example 1: index of top 6 negative values 
topn(x, 6L)
order(x)[1:6]

# Example 2: index of top 6 positive values
topn(x, 6L, decreasing = TRUE)
order(x, decreasing = TRUE)[1:6]

# Example 3: top 6 negative values
x[topn(x, 6L)]
sort(x)[1:6]
}
\keyword{ data }
