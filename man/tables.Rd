\name{tables}
\alias{tables}
\title{ Lists all objects of class 'data.table' }
\description{
  Outputs one line per data.table providing name, number of rows, and column names.
}
\usage{
tables(mb = FALSE, order.col = "NAME", width = 80)
}
\arguments{
  \item{mb}{ TRUE adds size of the data.table in MB to the output. Calls object.size so can be very slow }
  \item{order.col}{ Quoted column name to sort the output by }
  \item{width}{ Number of characters to truncate the COLS output }
}
\details{
}
\value{
    A data.table containing the information printed.
}
\references{ }
\author{ Matt Dowle }
\note{ }
\seealso{ \code{\link{ls}}, \code{\link{objects}}, \code{\link{object.size}}~~~ }
\examples{
    DT = data.table(A=1:10,B=letters[1:10])
    DT2 = data.table(A=1:10000,ColB=10000:1)
    tables()
}
\keyword{ data }
