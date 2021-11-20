\name{tables}
\alias{tables}
\title{Display 'data.table' metadata }
\description{
  Convenience function for concisely summarizing some metadata of all \code{data.table}s in memory (or an optionally specified environment).
}
\usage{
tables(mb=TRUE, order.col="NAME", width=80,
       env=parent.frame(), silent=FALSE, index=FALSE)
}
\arguments{
  \item{mb}{ \code{logical}; \code{TRUE} adds the rough size of each \code{data.table} in megabytes to the output under column \code{MB}.  }
  \item{order.col}{ Column name (\code{character}) by which to sort the output. }
  \item{width}{ \code{integer}; number of characters beyond which the output for each of the columns \code{COLS}, \code{KEY}, and \code{INDICES} are truncated. }
  \item{env}{ An \code{environment}, typically the \code{.GlobalEnv} by default, see Details. }
  \item{silent}{ \code{logical}; should the output be printed? }
  \item{index}{ \code{logical}; if \code{TRUE}, the column \code{INDICES} is added to indicate the indices assorted with each object, see \code{\link{indices}}. }
}
\details{
Usually \code{tables()} is executed at the prompt, where \code{parent.frame()} returns \code{.GlobalEnv}. \code{tables()} may also be useful inside functions where \code{parent.frame()} is the local scope of the function; in such a scenario, simply set it to \code{.GlobalEnv} to get the same behaviour as at prompt.

Note that on older versions of \R, \code{object.size} may be slow, so setting \code{mb=FALSE} may speed up execution of \code{tables} significantly.

Setting \code{silent=TRUE} prints nothing; the metadata are returned as a \code{data.table}, invisibly, whether silent is \code{TRUE} or \code{FALSE}.
}
\value{
    A \code{data.table} containing the information printed.
}
\seealso{ \code{\link{data.table}}, \code{\link{setkey}}, \code{\link{ls}}, \code{\link{objects}}, \code{\link{object.size}} }
\examples{
DT = data.table(A=1:10, B=letters[1:10])
DT2 = data.table(A=1:10000, ColB=10000:1)
setkey(DT,B)
tables()
}
\keyword{ data }
