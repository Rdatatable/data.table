\name{cbind}
\alias{cbind}
\alias{rbind}
\title{ Bind an object to data.table }
\description{
   Binds columns (cbind) or rows (rbind) to a data.table, returning a data.table.
}
\usage{
cbind(...)
rbind(...)
}
\arguments{
  \item{\dots}{ data.table followed by vectors, matrices, data.frames or data.tables. }
}
\details{
    These functions are masked in \code{data.table} because of this feature in \code{\link[base]{cbind}} :

    \dQuote{The data frame method will be used if \emph{at least one} argument is a data frame}.
    
    This means that \code{cbind(DT,DF)} dispatches to the S3 method \code{cbind.data.frame} even
    when \code{cbind.data.table} is provided by \code{data.table}. Therefore, we have masked these
    functions. Warning messages will be presented to the user (by \R) when the \code{data.table}
    package is loaded.
    
    If the first argument is not a \code{data.table}, then they revert to the base versions. 
}
\value{
    A data.table.
}
\seealso{ \code{\link[base]{cbind}}, \code{\link[base]{rbind}} }
% \examples{
% }
\keyword{ data }

