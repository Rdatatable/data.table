\name{as.data.table}
\alias{as.data.table}
\alias{as.data.table.array}
\alias{as.data.table.matrix}
\alias{as.data.table.list}
\alias{as.data.table.data.frame}
\alias{as.data.table.data.table}
\alias{as.data.table.factor}
\alias{as.data.table.ordered}
\alias{as.data.table.integer}
\alias{as.data.table.numeric}
\alias{as.data.table.logical}
\alias{as.data.table.character}
\alias{as.data.table.Date}
\alias{is.data.table}
\title{Coerce to data.table}
\description{
Functions to check if an object is \code{data.table}, or coerce it if possible.

}
\usage{
as.data.table(x, keep.rownames=FALSE, \dots)

\method{as.data.table}{data.table}(x, \dots, key=NULL)

\method{as.data.table}{array}(x, keep.rownames=FALSE, key=NULL, sorted=TRUE,
              value.name="value", na.rm=TRUE, \dots)

is.data.table(x)

}
\arguments{
  \item{x}{An R object.}
  \item{keep.rownames}{Default is \code{FALSE}. If \code{TRUE}, adds the input object's names as a separate column named \code{"rn"}. \code{keep.rownames = "id"} names the column \code{"id"} instead.}
  \item{key}{ Character vector of one or more column names which is passed to \code{\link{setkeyv}}. }
  \item{sorted}{logical used in \emph{array} method, default \code{TRUE} is overridden when \code{key} is provided. }
  \item{value.name}{character scalar used in \emph{array} method, default \code{"value"}.}
  \item{na.rm}{logical used in \emph{array} method, default \code{TRUE} will remove rows with \code{NA} values.}
  \item{\dots}{Additional arguments to be passed to or from other methods.}
}
\details{

  \code{as.data.table} is a generic function with many methods, and other packages can supply further methods.

  If a \code{list} is supplied, each element is converted to a column in the \code{data.table} with shorter elements recycled automatically. Similarly, each column of a \code{matrix} is converted separately.

  \code{character} objects are \emph{not} converted to \code{factor} types unlike \code{as.data.frame}.

  If a \code{data.frame} is supplied, all classes preceding \code{"data.frame"} are stripped. Similarly, for \code{data.table} as input, all classes preceding \code{"data.table"} are stripped. \code{as.data.table} methods returns a \emph{copy} of original data. To modify by reference see \code{\link{setDT}} and \code{\link{setDF}}.

  \code{keep.rownames} argument can be used to preserve the (row)names attribute in the resulting \code{data.table}.
}
\seealso{
  \code{\link{data.table}}, \code{\link{setDT}}, \code{\link{setDF}}, \code{\link{copy}}, \code{\link{setkey}}, \code{\link{J}}, \code{\link{SJ}}, \code{\link{CJ}}, \code{\link{merge.data.table}}, \code{\link{:=}}, \code{\link{setalloccol}}, \code{\link{truelength}}, \code{\link{rbindlist}}, \code{\link{setNumericRounding}}, \code{\link{datatable-optimize}}
}
\examples{
nn = c(a=0.1, b=0.2, c=0.3, d=0.4)
as.data.table(nn)
as.data.table(nn, keep.rownames=TRUE)
as.data.table(nn, keep.rownames="rownames")

# char object not converted to factor
cc = c(X="a", Y="b", Z="c")
as.data.table(cc)
as.data.table(cc, keep.rownames=TRUE)
as.data.table(cc, keep.rownames="rownames")

mm = matrix(1:4, ncol=2, dimnames=list(c("r1", "r2"), c("c1", "c2")))
as.data.table(mm)
as.data.table(mm, keep.rownames=TRUE)
as.data.table(mm, keep.rownames="rownames")
as.data.table(mm, key="c1")

ll = list(a=1:2, b=3:4)
as.data.table(ll)
as.data.table(ll, keep.rownames=TRUE)
as.data.table(ll, keep.rownames="rownames")

DF = data.frame(x=rep(c("x","y","z"),each=2), y=c(1,3,6), row.names=LETTERS[1:6])
as.data.table(DF)
as.data.table(DF, keep.rownames=TRUE)
as.data.table(DF, keep.rownames="rownames")

DT = data.table(x=rep(c("x","y","z"),each=2), y=c(1:6))
as.data.table(DT)
as.data.table(DT, key='x')

ar = rnorm(27)
ar[sample(27, 15)] = NA
dim(ar) = c(3L,3L,3L)
as.data.table(ar)
}
\keyword{ data }

