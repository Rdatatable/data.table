\name{tables}
\alias{tables}
\title{Display all objects of class 'data.table' }
\description{
  Lists all data.table's in memory, including number of rows, column names and any keys.
}
\usage{
tables(mb = FALSE, order.col = "NAME", width = 80, env=parent.frame(), silent=FALSE)
}
\arguments{
  \item{mb}{ TRUE adds size of the data.table in MB to the output. Calls object.size which appears slow. To be resolved.  }
  \item{order.col}{ Quoted column name to sort the output by }
  \item{width}{ Number of characters to truncate the COLS output }
  \item{env}{ Usually tables() is executed at the prompt where parent.frame() returns .GlobalEnv. tables() may also be useful inside functions where parent.frame() is the local scope of the function, or set it to .GlobalEnv }
  \item{silent}{ By default tables() is expected to be called at the prompt for its compact print output. silent=TRUE prints nothing. The data statistics are returned as a data.table, silently, whether silent is TRUE or FALSE }
}
% \details{
% }
\value{
    A data.table containing the information printed.
}
% \references{ }
\author{ Matthew Dowle }
% \note{ }
\seealso{ \code{\link{data.table}}, \code{\link{setkey}}, \code{\link{ls}}, \code{\link{objects}}, \code{\link{object.size}} }
\examples{
    DT = data.table(A=1:10,B=letters[1:10])
    DT2 = data.table(A=1:10000,ColB=10000:1)
    setkey(DT,B)
    tables()
}
\keyword{ data }
