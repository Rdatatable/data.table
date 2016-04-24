\name{tables}
\alias{tables}
\alias{columns}
\title{Display 'data.table' metadata }
\description{
  Some convenience functions for concisely summarizing some metadata of all \code{data.table}s in memory (or an optionally specified environment).
}
\usage{
tables(mb=TRUE, order.col="NAME", width=80, 
       env=parent.frame(), silent=FALSE, pretty=TRUE,
       stats=FALSE, index=FALSE, address=FALSE)
columns(env=parent.frame(), silent=FALSE, pretty=TRUE,
        stats=FALSE, address=FALSE)
}
\arguments{
  \item{mb}{ \code{logical}; \code{TRUE} adds the rough size of each \code{data.table} in megabytes to the output under column \code{MB}.  }
  \item{order.col}{ Column name (\code{character}) by which to sort the output. }
  \item{width}{ \code{integer}; number of characters beyond which the output for each of the columns \code{COLS}, \code{KEY}, and \code{INDICES} are truncated. }
  \item{env}{ An \code{environment}, typically the \code{.GlobalEnv} by default, see Details. }
  \item{silent}{ \code{logical}; should the output be printed? }
  \item{pretty}{ \code{logical}; if \code{TRUE}, extra care is taken to make the output more digestible, e.g. by formatting large numbers with commas. }
  \item{stats}{ \code{logical};  see Details. }
  \item{index}{ \code{logical}; if \code{TRUE}, the column \code{INDICES} is added to indicate the indices assorted with each object, see \code{\link{indices}}. }
  \item{address}{ \code{logical}; if \code{TRUE}, the address in memory (as reported by \code{\link{address}}) of each item is added in the column \code{ADDRESS}. }
  
}
\details{
Usually \code{tables()} is executed at the prompt, where \code{parent.frame()} returns \code{.GlobalEnv}. \code{tables()} may also be useful inside functions where \code{parent.frame()} is the local scope of the function; in such a scenario, simply set it to \code{.GlobalEnv} to get the same behavior as at prompt.

Note that on older versions of \R, \code{object.size} may be slow, so setting \code{mb=FALSE} may speed up execution of \code{tables} significantly.

Setting \code{silent=TRUE} prints nothing; the metadata are returned as a \code{data.table}, invisibly, whether silent is \code{TRUE} or \code{FALSE}.

For \code{tables}, if \code{stats} is \code{TRUE}, the columns \code{UNQ_NROW} and \code{KEY_UNQ_NROW} are added, which tell the number of distinct rows and the number of distinct key combinations in each table, respectively. For \code{columns}, the added columns are \code{COUNT}, \code{COUNT_UNQ}, \code{COUNT_NA}, and \code{RATIO_NA}, which report the number of observations, the number of unique observations, the number of \code{NA} observations, and the ratio of \code{NA} observations to total observations, respectively. 
}
\value{
    A \code{data.table} containing the information printed.
}
\seealso{ \code{\link{data.table}}, \code{\link{setkey}}, \code{\link{ls}}, \code{\link{objects}}, \code{\link{object.size}} }
\examples{
DT = data.table(A=1:10, B=letters[1:10], key="B")
DT2 = data.table(A=1:10000, ColB=10000:1)
tables()
columns()
}
\keyword{ data }
