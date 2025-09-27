\name{tables}
\alias{tables}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tables(mb = FALSE, order.col = "NAME", width = 80)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mb}{ ~~Describe \code{mb} here~~ }
  \item{order.col}{ ~~Describe \code{order.col} here~~ }
  \item{width}{ ~~Describe \code{width} here~~ }
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
function(mb=FALSE,order.col="NAME",width=80)
{
    # Prints name, size and colnames of all data.tables in .GlobalEnv
    tt = objects(env=.GlobalEnv)
    ss = which(sapply(tt, function(x) class(get(x,envir=.GlobalEnv))) == "data.table")
    if (!length(ss)) {
        cat("No objects of class data.table exist in .GlobalEnv\n")
        return(invisible())
    }
    tab = tt[ss]
    info = data.table(
        NAME=tab, 
        NROW=format(sprintf("%4s",prettyNum(as.character(sapply(tab, function(x) nrow(get(x,envir=.GlobalEnv)))),big.mark=",")),justify="right")
    )
    if (mb) {
        s = sapply(tab, function(x) ceiling(object.size(get(x,envir=.GlobalEnv))/1024^2))
        info$MB = format(s,justify="right")
        total = sum(s)
    }
    info$COLS = as.vector(sapply(tab, function(x) paste(colnames(get(x,envir=.GlobalEnv)),collapse=",")))
    info = info[order(get(order.col))]
    m = as.matrix(info)
    colnames(m)[2] = sprintf(paste("%",nchar(m[1,"NROW"]),"s",sep=""), "NROW")
    if (mb) colnames(m)[3] = sprintf(paste("%",nchar(m[1,"MB"]),"s",sep=""), "MB")
    m[,"COLS"] = substring(m[,"COLS"],1,width)
    print(m, quote=FALSE, right=FALSE)
    if (mb) cat("Total: ",prettyNum(as.character(total),big.mark=","),"MB\n",sep="")
    invisible(info)
  }
}
\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
