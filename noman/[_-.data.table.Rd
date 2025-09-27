\name{[<-.data.table}
\alias{[<-.data.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
[<-.data.table(x, i, j, value)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{i}{ ~~Describe \code{i} here~~ }
  \item{j}{ ~~Describe \code{j} here~~ }
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
function (x, i, j, value) 
{
    # TO DO: copied from [<-.data.frame,  remove out all uses of row.names and data.frame
    # TO DO: test this method of assignment as I've tended to use $ on the left hand side.
    nA <- nargs()
    if (nA == 4) {
        has.i <- !missing(i)
        has.j <- !missing(j)
    }
    else if (nA == 3) {
        if (is.atomic(value)) 
            names(value) <- NULL
        if (missing(i) && missing(j)) {
            i <- j <- NULL
            has.i <- has.j <- FALSE
            if (is.null(value)) 
                return(x[logical(0)])
        }
        else {
            if (is.logical(i) && is.matrix(i) && all(dim(i) == dim(x))) {
                nreplace <- sum(i, na.rm = TRUE)
                if (!nreplace) 
                  return(x)
                N <- length(value)
                if (N > 0 && N < nreplace && (nreplace %% N) == 0) 
                  value <- rep(value, length.out = nreplace)
                if (length(value) != nreplace) 
                  stop("rhs is the wrong length for indexing by a logical matrix")
                n <- 0
                nv <- nrow(x)
                for (v in seq(len = dim(i)[2])) {
                  thisvar <- i[, v, drop = TRUE]
                  nv <- sum(thisvar, na.rm = TRUE)
                  if (nv) {
                    if (is.matrix(x[[v]])) 
                      x[[v]][thisvar, ] <- value[n + (1:nv)]
                    else x[[v]][thisvar] <- value[n + (1:nv)]
                  }
                  n <- n + nv
                }
                return(x)
            }
            if (is.matrix(i)) 
                stop("only logical matrix subscripts are allowed in replacement")
            j <- i
            i <- NULL
            has.i <- FALSE
            has.j <- TRUE
        }
    }
    else {
        stop("need 0, 1, or 2 subscripts")
    }
    if (has.j && length(j) == 0) 
        return(x)
    cl <- oldClass(x)
    class(x) <- NULL
    rows <- attr(x, "row.names")
    new.cols <- NULL
    nvars <- length(x)
    nrows <- length(rows)
    if (has.i) {
        if (any(is.na(i))) 
            stop("missing values are not allowed in subscripted assignments of data frames")
        if (char.i <- is.character(i)) {
            ii <- match(i, rows)
            nextra <- sum(new.rows <- is.na(ii))
            if (nextra > 0) {
                ii[new.rows] <- seq(from = nrows + 1, length = nextra)
                new.rows <- i[new.rows]
            }
            i <- ii
        }
        if (all(i >= 0) && (nn <- max(i)) > nrows) {
            if (!char.i) {
                nrr <- as.character((nrows + 1):nn)
                if (inherits(value, "data.frame") && (dim(value)[1]) >= 
                  length(nrr)) {
                  new.rows <- attr(value, "row.names")[1:length(nrr)]
                  repl <- duplicated(new.rows) | match(new.rows, 
                    rows, 0)
                  if (any(repl)) 
                    new.rows[repl] <- nrr[repl]
                }
                else new.rows <- nrr
            }
            x <- xpdrows.data.frame(x, rows, new.rows)
            rows <- attr(x, "row.names")
            nrows <- length(rows)
        }
        iseq <- seq(along = rows)[i]
        if (any(is.na(iseq))) 
            stop("non-existent rows not allowed")
    }
    else iseq <- NULL
    if (has.j) {
        if (any(is.na(j))) 
            stop("missing values are not allowed in subscripted assignments of data frames")
        if (is.character(j)) {
            jj <- match(j, names(x))
            nnew <- sum(is.na(jj))
            if (nnew > 0) {
                n <- is.na(jj)
                jj[n] <- nvars + 1:nnew
                new.cols <- j[n]
            }
            jseq <- jj
        }
        else if (is.logical(j) || min(j) < 0) 
            jseq <- seq(along = x)[j]
        else {
            jseq <- j
            if (max(jseq) > nvars) {
                new.cols <- paste("V", seq(from = nvars + 1, 
                  to = max(jseq)), sep = "")
                if (length(new.cols) != sum(jseq > nvars)) 
                  stop("new columns would leave holes after existing columns")
                if (is.list(value) && !is.null(vnm <- names(value))) {
                  p <- length(jseq)
                  if (length(vnm) < p) 
                    vnm <- rep(vnm, length.out = p)
                  new.cols <- vnm[jseq > nvars]
                }
            }
        }
    }
    else jseq <- seq(along = x)
    if (any(duplicated(jseq))) 
        stop("duplicate subscripts for columns")
    n <- length(iseq)
    if (n == 0) 
        n <- nrows
    p <- length(jseq)
    m <- length(value)
    if (!is.list(value)) {
        if (p == 1) {
            N <- NROW(value)
            if (N > n) 
                stop(gettextf("replacement has %d rows, data has %d", 
                  N, n), domain = NA)
            if (N < n && N > 0) 
                if (n%%N == 0 && length(dim(value)) <= 1) 
                  value <- rep(value, length.out = n)
                else stop(gettextf("replacement has %d rows, data has %d", 
                  N, n), domain = NA)
            names(value) <- NULL
            value <- list(value)
        }
        else {
            if (m < n * p && (n * p)%%m) 
                stop(gettextf("replacement has %d items, need %d", 
                  m, n * p), domain = NA)
            value <- matrix(value, n, p)
            value <- split(value, col(value))
        }
        dimv <- c(n, p)
    }
    else {
        value <- unclass(value)
        lens <- sapply(value, NROW)
        for (k in seq(along = lens)) {
            N <- lens[k]
            if (n != N && length(dim(value[[k]])) == 2) 
                stop(gettextf("replacement element %d is a matrix/data frame of %d rows, need %d", 
                  k, N, n), domain = NA)
            if (N > 0 && N < n && n%%N) 
                stop(gettextf("replacement element %d has %d rows, need %d", 
                  k, N, n), domain = NA)
            if (N > 0 && N < n) 
                value[[k]] <- rep(value[[k]], length.out = n)
            if (N > n) {
                warning(gettextf("replacement element %d has %d rows to replace %d rows", 
                  k, N, n), domain = NA)
                value[[k]] <- value[[k]][1:n]
            }
        }
        dimv <- c(n, length(value))
    }
    nrowv <- dimv[1]
    if (nrowv < n && nrowv > 0) {
        if (n%%nrowv == 0) 
            value <- value[rep(1:nrowv, length.out = n), , drop = FALSE]
        else stop(gettextf("%d rows in value to replace %d rows", 
            nrowv, n), domain = NA)
    }
    else if (nrowv > n) 
        warning(gettextf("replacement data has %d rows to replace %d rows", 
            nrowv, n), domain = NA)
    ncolv <- dimv[2]
    jvseq <- seq(len = p)
    if (ncolv < p) 
        jvseq <- rep(1:ncolv, length.out = p)
    else if (ncolv > p) 
        warning(gettextf("provided %d variables to replace %d variables", 
            ncolv, p), domain = NA)
    if (length(new.cols)) {
        nm <- names(x)
        rows <- attr(x, "row.names")
        x <- c(x, vector("list", length(new.cols)))
        names(x) <- c(nm, new.cols)
        attr(x, "row.names") <- rows
    }
    if (has.i) 
        for (jjj in seq(len = p)) {
            jj <- jseq[jjj]
            vjj <- value[[jvseq[[jjj]]]]
            if (jj <= nvars) {
                if (length(dim(x[jj])) != 2) 
                  x[[jj]][iseq] <- vjj
                else x[[jj]][iseq, ] <- vjj
            }
            else {
                length(vjj) <- nrows
                x[[jj]] <- vjj
            }
        }
    else if (p > 0) 
        for (jjj in p:1) {
            jj <- jseq[jjj]
            x[[jj]] <- value[[jvseq[[jjj]]]]
            if (is.atomic(x[[jj]])) 
                names(x[[jj]]) <- NULL
        }
    if (length(new.cols) > 0) {
        new.cols <- names(x)
        if (any(duplicated(new.cols))) 
            names(x) <- make.unique(new.cols)
    }
    class(x) <- cl
    x
  }
}
\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
