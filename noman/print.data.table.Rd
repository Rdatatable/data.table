\name{print.data.table}
\alias{print.data.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
print.data.table(x, digits = NULL, quote = FALSE, right = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{digits}{ ~~Describe \code{digits} here~~ }
  \item{quote}{ ~~Describe \code{quote} here~~ }
  \item{right}{ ~~Describe \code{right} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
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
function (x, digits = NULL, quote = FALSE, right = TRUE, ...)
{
    if (length(x) == 0) {
        cat("NULL data table\n")
    } else {
        print(as.matrix(x), digits=digits, quote=quote, right=right, ...)
        #mm = do.call("cbind", lapply(x, format, digits=digits))
        #print(mm, ..., quote = quote, right = right)
    }
    invisible(x)
  }
}
\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
