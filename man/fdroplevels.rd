\name{fdroplevels}
\alias{fdroplevels}
\alias{droplevels}
\alias{droplevels.data.table}
\title{Fast droplevels}
\description{
  Similar to \code{base::droplevels} but \emph{much faster}.
}

\usage{
fdroplevels(x, exclude = if (anyNA(levels(x))) NULL else NA, \dots)

\method{droplevels}{data.table}(x, except = NULL, exclude, in.place = FALSE, \dots)
}
\arguments{
  \item{x}{ \code{factor} or \code{data.table} where unused levels should be dropped. }
  \item{exclude}{ A \code{character} vector of factor levels which are dropped no matter of presented or not. }
  \item{except}{ An \code{integer} vector of indices of data.table columns which are not modified by dropping levels. }
  \item{in.place}{ logical (default is \code{FALSE}). If \code{TRUE} levels of factors of \code{data.table} are modified in-place. }
  \item{\dots}{ further arguments passed to methods }
}

\value{
  \code{fdroplevels} returns a \code{factor}.

  \code{droplevels} returns a \code{data.table} where levels are dropped at factor columns.
}

\examples{
# on vectors
x = factor(letters[1:10])
fdroplevels(x[1:5])
# exclude levels from drop
fdroplevels(x[1:5], exclude = c("a", "c"))

# on data.table
DT = data.table(a = factor(1:10), b = factor(letters[1:10]))
droplevels(head(DT))[["b"]]
# exclude levels
droplevels(head(DT), exclude = c("b", "c"))[["b"]]
# except columns from drop
droplevels(head(DT), except = 2)[["b"]]
droplevels(head(DT), except = 1)[["b"]]
}
\seealso{
  \code{\link{data.table}}, \code{\link{duplicated}}, \code{\link{unique}}
}
\keyword{ data }
