\name{datatable.optimize}
\alias{datatable-optimize}
\alias{datatable.optimize}
\alias{data.table-optimize}
\alias{data.table.optimize}
\alias{gforce}
\alias{GForce}
\alias{autoindex}
\alias{autoindexing}
\alias{auto-index}
\alias{auto-indexing}
\alias{rounding}
\title{Optimisations in data.table}
\description{
\code{data.table} internally optimises certain expressions in order to improve
performance. This section briefly summarises those optimisations.

Note that there's no additional input needed from the user to take advantage
of these optimisations. They happen automatically.

Run the code under the \emph{example} section to get a feel for the performance
benefits from these optimisations.

Note that for all optimizations involving efficient sorts, the caveat mentioned
in \code{\link{setorder}} applies -- whenever data.table does the sorting,
it does so in "C-locale". This has some subtle implications; see Examples.

}
\details{
\code{data.table} reads the global option \code{datatable.optimize} to figure
out what level of optimisation is required. The default value \code{Inf}
activates \emph{all} available optimisations.

For \code{getOption("datatable.optimize") >= 1}, these are the optimisations:

\itemize{
    \item The base function \code{order} is internally replaced with
    \code{data.table}'s \emph{fast ordering}. That is, \code{DT[order(\dots)]}
    gets internally optimised to \code{DT[forder(\dots)]}.

    \item The expression \code{DT[, lapply(.SD, fun), by=.]} gets optimised
    to \code{DT[, list(fun(a), fun(b), \dots), by=.]} where \code{a,b, \dots} are
    columns in \code{.SD}. This improves performance tremendously.

    \item Similarly, the expression \code{DT[, c(.N, lapply(.SD, fun)), by=.]}
    gets optimised to \code{DT[, list(.N, fun(a), fun(b), \dots)]}. \code{.N} is
    just for example here.

    \item \code{base::mean} function is internally optimised to use
    \code{data.table}'s \code{fastmean} function. \code{mean()} from \code{base}
    is an S3 generic and gets slow with many groups.
}

For \code{getOption("datatable.optimize") >= 2}, additional optimisations are implemented on top of the optimisations already shown above.

\itemize{

    \item Expressions in \code{j} which contain only the functions
    \code{min, max, mean, median, var, sd, sum, prod, first, last, head, tail} (for example,
    \code{DT[, list(mean(x), median(x), min(y), max(y)), by=z]}), they are very
    effectively optimised using what we call \emph{GForce}. These functions
    are automatically replaced with a corresponding GForce version
    with pattern \code{g*}, e.g., \code{prod} becomes \code{gprod}.

    Normally, once the rows belonging to each group are identified, the values
    corresponding to the group are gathered and the \code{j}-expression is
    evaluated. This can be improved by computing the result directly without
    having to gather the values or evaluating the expression for each group
    (which can get costly with large number of groups) by implementing it
    specifically for a particular function. As a result, it is extremely fast.

    \item In addition to all the functions above, `.N` is also optimised to
    use GForce, when used separately or when combined with the functions mentioned
    above. Note further that GForce-optimized functions must be used separately,
    i.e., code like \code{DT[ , max(x) - min(x), by=z]} will \emph{not} currently
    be optimized to use \code{gmax, gmin}.

    \item Expressions of the form \code{DT[i, j, by]} are also optimised when
    \code{i} is a \emph{subset} operation and \code{j} is any/all of the functions
    discussed above.
}

For \code{getOption("datatable.optimize") >= 3}, additional optimisations for subsets in i are implemented on top of the optimisations already shown above. Subsetting operations are - if possible - translated into joins to make use of blazing fast binary search using indices and keys. The following queries are optimized:

\itemize{

    \item Supported operators: \code{==}, \code{\%in\%}. Non-equi operators(>, <, etc.) are not supported yet because non-equi joins are slower than vector based subsets.
    \item Queries on multiple columns are supported, if the connector is '\code{&}', e.g. \code{DT[x == 2 & y == 3]} is supported, but \code{DT[x == 2 | y == 3]} is not.
    \item Optimization will currently be turned off when doing subset when cross product of elements provided to filter on exceeds > 1e4. This most likely happens if multiple \code{\%in\%}, or \code{\%chin\%} queries are combined, e.g. \code{DT[x \%in\% 1:100 & y \%in\% 1:200]} will not be optimized since \code{100 * 200 = 2e4 > 1e4}.
    \item Queries with multiple criteria on one column are \emph{not} supported, e.g. \code{DT[x == 2 & x \%in\% c(2,5)]} is not supported.
    \item Queries with non-missing j are supported, e.g. \code{DT[x == 3 & y == 5, .(new = x-y)]} or \code{DT[x == 3 & y == 5, new := x-y]} are supported. Also extends to queries using \code{with = FALSE}.
    \item "notjoin" queries, i.e. queries that start with \code{!}, are only supported if there are no \code{&} connections, e.g. \code{DT[!x==3]} is supported, but \code{DT[!x==3 & y == 4]} is not.
}

If in doubt, whether your query benefits from optimization, call it with the \code{verbose = TRUE} argument. You should see "Optimized subsetting\ldots".

\bold{Auto indexing:} In case a query is optimized, but no appropriate key or index is found, \code{data.table} automatically creates an \emph{index} on the first run. Any successive subsets on the same
column then reuse this index to \emph{binary search} (instead of
\emph{vector scan}) and is therefore fast.
Auto indexing can be switched off with the global option
\code{options(datatable.auto.index = FALSE)}. To switch off using existing
indices set global option \code{options(datatable.use.index = FALSE)}.
}
\seealso{ \code{\link{setNumericRounding}}, \code{\link{getNumericRounding}} }
\examples{
\dontrun{
old = options(datatable.optimize = Inf)

# Generate a big data.table with a relatively many columns
set.seed(1L)
DT = lapply(1:20, function(x) sample(c(-100:100), 5e6L, TRUE))
setDT(DT)[, id := sample(1e5, 5e6, TRUE)]
print(object.size(DT), units="Mb") # 400MB, not huge, but will do

# 'order' optimisation
options(datatable.optimize = 1L) # optimisation 'on'
system.time(ans1 <- DT[order(id)])
options(datatable.optimize = 0L) # optimisation 'off'
system.time(ans2 <- DT[order(id)])
identical(ans1, ans2)

# optimisation of 'lapply(.SD, fun)'
options(datatable.optimize = 1L) # optimisation 'on'
system.time(ans1 <- DT[, lapply(.SD, min), by=id])
options(datatable.optimize = 0L) # optimisation 'off'
system.time(ans2 <- DT[, lapply(.SD, min), by=id])
identical(ans1, ans2)

# optimisation of 'mean'
options(datatable.optimize = 1L) # optimisation 'on'
system.time(ans1 <- DT[, lapply(.SD, mean), by=id])
system.time(ans2 <- DT[, lapply(.SD, base::mean), by=id])
identical(ans1, ans2)

# optimisation of 'c(.N, lapply(.SD, ))'
options(datatable.optimize = 1L) # optimisation 'on'
system.time(ans1 <- DT[, c(.N, lapply(.SD, min)), by=id])
options(datatable.optimize = 0L) # optimisation 'off'
system.time(ans2 <- DT[, c(N=.N, lapply(.SD, min)), by=id])
identical(ans1, ans2)

# GForce
options(datatable.optimize = 2L) # optimisation 'on'
system.time(ans1 <- DT[, lapply(.SD, median), by=id])
system.time(ans2 <- DT[, lapply(.SD, function(x) as.numeric(stats::median(x))), by=id])
identical(ans1, ans2)

# optimized subsets
options(datatable.optimize = 2L)
system.time(ans1 <- DT[id == 100L]) # vector scan
system.time(ans2 <- DT[id == 100L]) # vector scan
system.time(DT[id \%in\% 100:500])    # vector scan

options(datatable.optimize = 3L)
system.time(ans1 <- DT[id == 100L]) # index + binary search subset
system.time(ans2 <- DT[id == 100L]) # only binary search subset
system.time(DT[id \%in\% 100:500])    # only binary search subset again

# sensitivity to collate order
old_lc_collate = Sys.getlocale("LC_COLLATE")

if (old_lc_collate == "C") {
  Sys.setlocale("LC_COLLATE", "")
}
DT = data.table(
  grp = rep(1:2, each = 4L),
  var = c("A", "a", "0", "1", "B", "b", "0", "1")
)
options(datatable.optimize = Inf)
DT[, .(max(var), min(var)), by=grp]
# GForce is deactivated because of the ad-hoc column 'tolower(var)',
#   through which the result for 'max(var)' may also change
DT[, .(max(var), min(tolower(var))), by=grp]

Sys.setlocale("LC_COLLATE", old_lc_collate)
options(old)
}}
\keyword{ data }

