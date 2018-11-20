\name{split}
\alias{split}
\alias{split.data.table}
\title{ Split data.table into chunks in a list }
\description{
  Split method for data.table. Faster and more flexible. Be aware that processing list of data.tables will be generally much slower than manipulation in single data.table by group using \code{by} argument, read more on \code{\link{data.table}}.
}
\usage{
\method{split}{data.table}(x, f, drop = FALSE,
      by, sorted = FALSE, keep.by = TRUE, flatten = TRUE,
      \dots, verbose = getOption("datatable.verbose"))
}
\arguments{
  \item{x}{data.table }
  \item{f}{factor or list of factors. Same as \code{\link[base:split]{split.data.frame}}. Use \code{by} argument instead, this is just for consistency with data.frame method.}
  \item{drop}{logical. Default \code{FALSE} will not drop empty list elements caused by factor levels not referred by that factors. Works also with new arguments of split data.table method.}
  \item{by}{character vector. Column names on which split should be made. For \code{length(by) > 1L} and \code{flatten} FALSE it will result nested lists with data.tables on leafs.}
  \item{sorted}{When default \code{FALSE} it will retain the order of groups we are splitting on. When \code{TRUE} then sorted list(s) are returned. Does not have effect for \code{f} argument.}
  \item{keep.by}{logical default \code{TRUE}. Keep column provided to \code{by} argument.}
  \item{flatten}{logical default \code{TRUE} will unlist nested lists of data.tables. When using \code{f} results are always flattened to list of data.tables.}
  \item{\dots}{passed to data.frame way of processing when using \code{f} argument.}
  \item{verbose}{logical default \code{FALSE}. When \code{TRUE} it will print to console data.table split query used to split data.}
}
\details{
    Argument \code{f} is just for consistency in usage to data.frame method. Recommended is to use \code{by} argument instead, it will be faster, more flexible, and by default will preserve order according to order in data.
}
\value{
    List of \code{data.table}s. If using \code{flatten} FALSE and \code{length(by) > 1L} then recursively nested lists having \code{data.table}s as leafs of grouping according to \code{by} argument.
}
\seealso{ \code{\link{data.table}}, \code{\link{rbindlist}} }
\examples{
set.seed(123)
DT = data.table(x1 = rep(letters[1:2], 6),
                x2 = rep(letters[3:5], 4),
                x3 = rep(letters[5:8], 3),
                y = rnorm(12))
DT = DT[sample(.N)]
DF = as.data.frame(DT)

# split consistency with data.frame: `x, f, drop`
all.equal(
    split(DT, list(DT$x1, DT$x2)),
    lapply(split(DF, list(DF$x1, DF$x2)), setDT)
)

# nested list using `flatten` arguments
split(DT, by=c("x1", "x2"))
split(DT, by=c("x1", "x2"), flatten=FALSE)

# dealing with factors
fdt = DT[, c(lapply(.SD, as.factor), list(y=y)), .SDcols=x1:x3]
fdf = as.data.frame(fdt)
sdf = split(fdf, list(fdf$x1, fdf$x2))
all.equal(
    split(fdt, by=c("x1", "x2"), sorted=TRUE),
    lapply(sdf[sort(names(sdf))], setDT)
)

# factors having unused levels, drop FALSE, TRUE
fdt = DT[, .(x1 = as.factor(c(as.character(x1), "c"))[-13L],
             x2 = as.factor(c("a", as.character(x2)))[-1L],
             x3 = as.factor(c("a", as.character(x3), "z"))[c(-1L,-14L)],
             y = y)]
fdf = as.data.frame(fdt)
sdf = split(fdf, list(fdf$x1, fdf$x2))
all.equal(
    split(fdt, by=c("x1", "x2"), sorted=TRUE),
    lapply(sdf[sort(names(sdf))], setDT)
)
sdf = split(fdf, list(fdf$x1, fdf$x2), drop=TRUE)
all.equal(
    split(fdt, by=c("x1", "x2"), sorted=TRUE, drop=TRUE),
    lapply(sdf[sort(names(sdf))], setDT)
)
}
\keyword{ data }
