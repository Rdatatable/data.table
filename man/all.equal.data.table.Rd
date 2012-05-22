\name{all.equal}
\alias{all.equal}
\alias{all.equal.data.table}
\title{ Equality Test Between Two Data Tables }
\description{
  Performs some factor level ``stripping'' and other operations to allow
  for a convenient test of data equality between \code{data.table} objects.
}

\usage{
  \method{all.equal}{data.table}(target, current, trim.levels = TRUE, ...)
}

\arguments{
  \item{target, current}{
      \code{data.table}s to compare
  }

  \item{trim.levels}{
    A logical indicating whether or not to remove all unused levels in columns
    that are factors before running equality check.
  }

  \item{\dots}{
    Passed down to internal call of \code{\link{all.equal.list}}
  }
}

\details{
  This function is used primarily to make life easy with a testing harness
  built around \code{test_that}. A call to \code{test_that::(expect_equal|equal)}
  will ultimately dispatch to this method when making an "equality" check.
}

\value{
  Either \code{TRUE} or a vector of mode \code{"character"} describing the 
  differences between \code{target} and \code{current}.
}

\seealso{
  \code{\link{all.equal.list}}
}

\examples{
  dt1 <- data.table(A = letters[1:10], X = 1:10, key = "A")
  dt2 <- data.table(A = letters[5:14], Y = 1:10, key = "A")
  identical(all.equal(dt1, dt1), TRUE)
  is.character(all.equal(dt1, dt2))
}

