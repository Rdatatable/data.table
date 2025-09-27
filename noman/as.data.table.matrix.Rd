\name{as.data.table.matrix}
\alias{as.data.table.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
as.data.table.matrix(x, keep.rownames = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{keep.rownames}{ ~~Describe \code{keep.rownames} here~~ }
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
function(x, keep.rownames=FALSE)
{
    if (keep.rownames) return(data.table(rn=rownames(x), x, keep.rownames=FALSE))
    d <- dim(x)
    nrows <- d[1]
    ir <- seq(length = nrows)
    ncols <- d[2]
    ic <- seq(length = ncols)
    dn <- dimnames(x)
    collabs <- dn[[2]]
    if (any(empty <- nchar(collabs) == 0)) 
        collabs[empty] <- paste("V", ic, sep = "")[empty]
    value <- vector("list", ncols)
    if (mode(x) == "character") {
        for (i in ic) value[[i]] <- as.factor(x[, i])       # for efficiency.
    }
    else {
        for (i in ic) value[[i]] <- as.vector(x[, i])       # to drop any row.names that would otherwise be retained inside every column of the data.table
    }
    if (length(collabs) == ncols) 
        names(value) <- collabs
    else
        names(value) <- paste("V", ic, sep = "")
    class(value) <- "data.table"
    value
  }
}
\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
