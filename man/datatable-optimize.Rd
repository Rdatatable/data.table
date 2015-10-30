\name{datatable-optimize}
\alias{datatable-optimize}
\alias{gforce}
\alias{data.table-optimize}
\title{Optimisations in data.table}
\description{
    \code{data.table} internally optimises certain expressions so as to improve performance with users not having to change their code or do anything extra. This section briefly lists all available optimisations.

	Note that there's no additional input needed from the user to take advantage of these optimisations. They happen automatically.

	Run the code under example section to get a feel of the effect of optimisations.

}
\details{
	\code{data.table} reads the global option \code{datatable.optimize} to figure out what level of optimisation is required. The default value \code{Inf} activates \emph{all} available optimisations.

	At optimisation level \code{>= 1}, i.e., \code{getOption("datatable.optimize")} >= 1, these are the noteworthy optimisations:

	\itemize{

		\item The base function \code{order} is internally replaced with \code{data.table}'s \emph{fast ordering}. That is, \code{dt[order(...)]} gets internally optimised to \code{dt[forder(...)]}. 

		\item The expression \code{dt[, lapply(.SD, fun), by=.]} gets optimised to \code{dt[, list(fun(a), fun(b), ...), by=.]} where \code{a,b, ...} are columns in \code{.SD}. This improves performance tremendously.

		\item Similarly, the expression \code{dt[, c(.N, lapply(.SD, fun)), by=.]} gets optimised to \code{dt[, list(.N, fun(a), fun(b), ...)]}. Here, \code{.N} is just an example. It could be any other expression. The optimisation looks for \code{c} along with \code{lapply(.SD, fun)}.

		\item \code{base::mean} function is internally optimised to use \code{data.table}'s \code{fastmean} function. \code{mean()} from \code{base} is an S3 generic and gets slow on many groupings.
 	}


 	At optimisation level \code{>= 2}, i.e., \code{getOption("datatable.optimize")} >= 2, additional optimisations are implemented on top of the optimisations already shown above. 

 	\itemize{

 		\item When expressions in \code{j} which contains only these functions \code{min, max, mean, median}, for example, \code{dt[, list(mean(x), median(x), min(y), max(y)), by=z]}, they are very effectively optimised using, what we call, \emph{GForce}. These functions are replaced with \code{gmean, gmedian, gmin, gmax} instead. 

 		Normally, once the rows belonging to each groups is identified, the data corresponding to each group is gathered and the expression in \code{j} is \emph{evaluated} for the group. This can be improved by computing the result directly without having to gather data or evaluating the expression for each group (which can get costly with large number of groups) by implementing it specifically for a particular function. As a result, it is extremely fast.

 		\item In addition to all the functions above, `.N` is also optimised to use GForce. It, when used along with these functions, will use GForce.
 	}

 	\bold{Auto indexing:} In addition to the optimisations described above, \code{data.table} also allows for blazing fast subsets by creating an index on the first run. Any successive subsets on the same column then reuses this index to \emph{binary search} (instead of \emph{vector scan}) and is therefore fast.

 	At the moment, expressions of the form \code{dt[col == val]} and \code{dt[col \%in\% val]} are both optimised. We plan to expand this to more operators and more conditions in the future.

 	Auto indexing can be switched off with the global option \code{options(datatable.auto.index = FALSE)}.
}
\examples{
\dontrun{
# Generate a big data.table with a relatively many columns
set.seed(1L)
dt = lapply(1:20, function(x) sample(c(-100:100), 5e6L, TRUE))
setDT(dt)[, id := sample(1e5, 5e6, TRUE)]
print(object.size(dt), units="Mb") # 400MB, not huge, but will do

# 'order' optimisation
options(datatable.optimize = 1L) # optimisation 'on'
system.time(ans1 <- dt[order(id)])
options(datatable.optimize = 0L) # optimisation 'off'
system.time(ans2 <- dt[order(id)])
identical(ans1, ans2)

# optimisation of 'lapply(.SD, fun)'
options(datatable.optimize = 1L) # optimisation 'on'
system.time(ans1 <- dt[, lapply(.SD, min), by=id])
options(datatable.optimize = 0L) # optimisation 'off'
system.time(ans2 <- dt[, lapply(.SD, min), by=id])
identical(ans1, ans2)

# optimisation of 'mean'
options(datatable.optimize = 1L) # optimisation 'on'
system.time(ans1 <- dt[, lapply(.SD, mean), by=id])
system.time(ans2 <- dt[, lapply(.SD, base::mean), by=id])
identical(ans1, ans2)

# optimisation of 'c(.N, lapply(.SD, ))'
options(datatable.optimize = 1L) # optimisation 'on'
system.time(ans1 <- dt[, c(.N, lapply(.SD, min)), by=id])
options(datatable.optimize = 0L) # optimisation 'off'
system.time(ans2 <- dt[, c(N=.N, lapply(.SD, min)), by=id])
identical(ans1, ans2)

# GForce
options(datatable.optimize = 2L) # optimisation 'on'
system.time(ans1 <- dt[, lapply(.SD, median), by=id])
system.time(ans2 <- dt[, lapply(.SD, function(x) as.numeric(stats::median(x))), by=id])
identical(ans1, ans2)

# restore optimization
options(datatable.optimize = Inf)

# auto indexing
options(datatable.auto.index=FALSE)
system.time(ans1 <- dt[id == 100L]) # vector scan
system.time(ans2 <- dt[id == 100L]) # vector scan
system.time(dt[id %in% 100:500])    # vector scan

options(datatable.auto.index = TRUE)
system.time(ans1 <- dt[id == 100L]) # index + binary search subset
system.time(ans2 <- dt[id == 100L]) # only binary search subset
system.time(dt[id %in% 100:500])    # only binary search subset again

}}
\keyword{ data }

