\name{as.matrix.data.table}
\alias{as.matrix.data.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
as.matrix.data.table(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
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
function(x)
{
    dm <- dim(x)
    cn <- colnames(x)
    if (any(dm == 0))
        return(array(NA, dim = dm, dimnames = list(NULL, cn)))
    p <- dm[2]
    n <- dm[1]
    collabs <- as.list(cn)
    X <- x
    class(X) <- NULL
    non.numeric <- non.atomic <- FALSE
    all.logical <- TRUE
    for (j in 1:p) {
        xj <- X[[j]]
        if (length(dj <- dim(xj)) == 2 && dj[2] > 1) {
            if (inherits(xj, "data.table")) 
                xj <- X[[j]] <- as.matrix(X[[j]])
            dnj <- dimnames(xj)[[2]]
            collabs[[j]] <- paste(collabs[[j]], if (length(dnj) > 
                0) 
                dnj
            else 1:dj[2], sep = ".")
        }
        if (!is.logical(xj)) 
            all.logical <- FALSE
        if (length(levels(xj)) > 0 || !(is.numeric(xj) || is.complex(xj)) || 
            (!is.null(cl <- attr(xj, "class")) && any(cl %in% 
                c("Date", "POSIXct", "POSIXlt")))) 
            non.numeric <- TRUE
        if (!is.atomic(xj)) 
            non.atomic <- TRUE
    }
    if (non.atomic) {
        for (j in 1:p) {
            xj <- X[[j]]
            if (is.recursive(xj)) {
            }
            else X[[j]] <- as.list(as.vector(xj))
        }
    }
    else if (all.logical) {
    }
    else if (non.numeric) {
        for (j in 1:p) {
            if (is.character(X[[j]])) 
                next
            xj <- X[[j]]
            miss <- is.na(xj)
            xj <- if (length(levels(xj))) 
                as.vector(xj)
            else format(xj)
            is.na(xj) <- miss
            X[[j]] <- xj
        }
    }
    X <- unlist(X, recursive = FALSE, use.names = FALSE)
    dim(X) <- c(n, length(X)/n)
    dimnames(X) <- list(NULL, unlist(collabs, use.names = FALSE))
    X
  }
}
\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
