\name{all.equal}
\alias{all.equal}
\alias{all.equal.data.table}
\title{ Equality Test Between Two Data Tables }
\description{
  Convenient test of data equality between \code{data.table} objects. Performs some factor level \emph{stripping}.
}

\usage{
  \method{all.equal}{data.table}(target, current, trim.levels=TRUE, check.attributes=TRUE,
    ignore.col.order=FALSE, ignore.row.order=FALSE, tolerance=sqrt(.Machine$double.eps),
    \dots)
}

\arguments{
  \item{target, current}{
      \code{data.table}s to compare. If \code{current} is not a \code{data.table}, but \code{check.attributes} is FALSE,
      it will be coerced to one via \link{as.data.table}.
  }

  \item{trim.levels}{
    A logical indicating whether or not to remove all unused levels in columns
    that are factors before running equality check. It effect only when \code{check.attributes} is TRUE and \code{ignore.row.order} is FALSE.
  }

  \item{check.attributes}{
    A logical indicating whether or not to check attributes, will apply not only to data.table but also attributes of the columns. It will skip \code{c("row.names",".internal.selfref")} data.table attributes.
  }

  \item{ignore.col.order}{
    A logical indicating whether or not to ignore columns order in \code{data.table}.
  }

  \item{ignore.row.order}{
    A logical indicating whether or not to ignore rows order in \code{data.table}. This option requires datasets to use data types on which join can be made, so no support for \emph{list, complex, raw}, but still supports \link[bit64:bit64-package]{integer64}.
  }

  \item{tolerance}{
    A numeric value used when comparing numeric columns, by default \code{sqrt(.Machine$double.eps)}. Unless non-default value provided it will be forced to \code{0} if used together with \code{ignore.row.order} and duplicate rows detected or factor columns present.
  }

  \item{\dots}{
    Passed down to internal call of \code{\link[base]{all.equal}}.
  }
}

\details{
  For efficiency data.table method will exit on detected non-equality issues, unlike most \code{\link[base]{all.equal}} methods which process equality checks further. Besides that fact it also handles the most time consuming case of \code{ignore.row.order = TRUE} very efficiently.
}

\value{
  Either \code{TRUE} or a vector of mode \code{"character"} describing the
  differences between \code{target} and \code{current}.
}

\seealso{
  \code{\link[base]{all.equal}}
}

\examples{
dt1 <- data.table(A = letters[1:10], X = 1:10, key = "A")
dt2 <- data.table(A = letters[5:14], Y = 1:10, key = "A")
isTRUE(all.equal(dt1, dt1))
is.character(all.equal(dt1, dt2))

# ignore.col.order
x <- copy(dt1)
y <- dt1[, .(X, A)]
all.equal(x, y)
all.equal(x, y, ignore.col.order = TRUE)

# ignore.row.order
x <- setkeyv(copy(dt1), NULL)
y <- dt1[sample(nrow(dt1))]
all.equal(x, y)
all.equal(x, y, ignore.row.order = TRUE)

# check.attributes
x = copy(dt1)
y = setkeyv(copy(dt1), NULL)
all.equal(x, y)
all.equal(x, y, check.attributes = FALSE)
x = data.table(1L)
y = 1L
all.equal(x, y)
all.equal(x, y, check.attributes = FALSE)

# trim.levels
x <- data.table(A = factor(letters[1:10])[1:4]) # 10 levels
y <- data.table(A = factor(letters[1:5])[1:4]) # 5 levels
all.equal(x, y, trim.levels = FALSE)
all.equal(x, y, trim.levels = FALSE, check.attributes = FALSE)
all.equal(x, y)
}

