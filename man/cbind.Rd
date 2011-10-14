\name{cbind.data.table}
%\alias{cbind.data.table}
\alias{rbind.data.table}
\title{ Bind an object to data.table }
\description{
   Binds columns (cbind) or rows (rbind) to a data.table, returning a data.table.
}
\usage{
%\method{cbind}{data.table}(...)
\method{rbind}{data.table}(...)
}
\arguments{
  \item{\dots}{ data.table followed by vectors, or data.tables. }
}
\details{
    Works as you'd expect, other than note this feature in \code{\link[base]{cbind}} :

    \dQuote{The data frame method will be used if \emph{at least one} argument is a data frame}.
    
    This means that \code{cbind(DT,DF)} dispatches to the S3 method \code{cbind.data.frame} even
    though \code{cbind.data.table} is provided by \code{data.table}. Therefore you cannot bind
    a \code{data.frame} to a \code{data.table} using \code{cbind()}. Use \code{data.table()} instead,
    or convert the object being bound to \code{data.table}.
}
\value{
    A data.table.
}
\seealso{ \code{\link[base]{cbind}}, \code{\link[base]{rbind}} }
\examples{
  DT = data.table(a=1:5, b=6:10)
  DF = data.frame(c=11:15)
  cbind(DT,DF)                      # strange matrix output containing 'Integer,5'
  data.table(DT,DF)                 # intended result
  cbind(DT,as.data.table(DF))       # another way, same intended result
}
\keyword{ data }

