\name{dimnames<-.data.table}
\alias{dimnames<-.data.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dimnames<-.data.table(x, value)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{value}{ ~~Describe \code{value} here~~ }
}
\details{
  ~~ If necessary, more details than the __description__  above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ }

 ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, value)   # so that can do  colnames(dt)=<..>  as well as names(dt)=<..>
{
    if (!is.list(value) || length(value) != 2) stop("attempting to assign invalid object to dimnames of a data.table")
    if (!is.null(value[[1]])) stop("data.tables do not have rownames")
    if (ncol(x) != length(value[[2]])) stop("can't assign",length(value[[2]]),"colnames to a",ncol(x),"column data.table")
    names(x) <- as.character(value[[2]])
    x
  }
}
\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
