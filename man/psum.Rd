\name{psum}
\alias{psum}
\alias{pprod}
\title{ Sum and Product}
\description{
Similar to \code{pmin} and \code{pmax} but for sum and product. Only works for integer and double. These functions do not recycle vectors.
}
\usage{
  psum(..., na.rm=FALSE)
  pprod(..., na.rm=FALSE)
}
\arguments{
  \item{...}{ Numeric arguments of type integer or double.}
  \item{na.rm}{ A logical indicating whether missing values should be removed. Default value is \code{FALSE}.}
}
\value{
Return the sum or product of all numeric argument. The value returned will be of the type of the highest argument types (integer < double) 
}
\examples{
x = c(1, 3, NA, 5)
y = c(2, NA, 4, 1)
z = c(3, 4, 4, 1)

# Example 1: psum 
psum(x, y, z, na.rm = FALSE)
psum(x, y, z, na.rm = TRUE)

# Example 2: pprod
pprod(x, y, z, na.rm = FALSE)
pprod(x, y, z, na.rm = TRUE)

}
\keyword{ data }
