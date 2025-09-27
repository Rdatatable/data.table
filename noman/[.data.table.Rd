\name{[.data.table}
\alias{[.data.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
[.data.table(x, i, j, drop = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{i}{ ~~Describe \code{i} here~~ }
  \item{j}{ ~~Describe \code{j} here~~ }
  \item{drop}{ ~~Describe \code{drop} here~~ }
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
function (x, i, j, drop=FALSE)
{
    # If you do drop=TRUE, you want a single vector, and in that case just do table$col. But that doesn't allow any subsetting, so you might do ,drop=TRUE in that case,  or table[subset]$col but that copies all columns into the subset which may be inefficient
    # TO DO: Allow j to be an expression (or series of expressions) to be evalulated like a call to with()
    # "[.data.table" can't be replaced with a direct call to subset() because we want to allow row and column integer indexing too.
    
    mdrop <- missing(drop)
    if (mode(substitute(i))=="call") {
        # so 'i' can be any expression of column names.
        # i can also include objects from the calling frame e.g. TABLE[ColA %in% Lkp] where Lkp is a vector defined in the calling frame.
        i = eval(substitute(i), envir=x, enclos=parent.frame())
        if (is.logical(i)) i[is.na(i)] = FALSE  # To simplify statement so don't have to do TABLE[!is.na(ColA) & ColA==ColB]
    }
    cols <- names(x)
    if (missing(i)) {
        if (!missing(j))
            x <- x[j]
        cols <- names(x)
        if (any(is.na(cols)))
            stop("undefined columns selected")
    } else {
        if (is.character(i)) stop("data.table has no rownames to subset")
        if (!missing(j)) {
            x <- x[j]
            cols <- names(x)
            if (any(is.na(cols)))
                stop("undefined columns selected")
        }
        for (j in seq(along = x)) {
            xj <- x[[j]]
            x[[j]] <- if (length(dim(xj)) != 2) 
                xj[i]
            else xj[i, , drop = FALSE]
        }
    }
    if (drop) {
        drop <- FALSE
        n <- length(x)
        if (n == 1) {
            x <- x[[1]]
            drop <- TRUE
        }
        else if (n > 1) {
            xj <- x[[1]]
            nrow <- if (length(dim(xj)) == 2) 
                dim(xj)[1]
            else length(xj)
            if (!mdrop && nrow == 1) {
                drop <- TRUE
                names(x) <- cols
            }
        }
    }
    if (!drop) {
        names(x) <- cols
        if (any(duplicated(nm <- names(x)))) 
            names(x) <- make.unique(nm)
    }
    x
  }
}
\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
