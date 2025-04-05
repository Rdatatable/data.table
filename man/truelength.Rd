\name{truelength}
\alias{truelength}
\alias{setalloccol}
\alias{alloc.col}
\title{ Over-allocation access }
\description{
    These functions are experimental and somewhat advanced. By \emph{experimental} we mean their names might change and perhaps the syntax, argument names and types. So if you write a lot of code using them, you have been warned! They should work and be stable, though, so please report problems with them. \code{alloc.col} is just an alias to \code{setalloccol}. We recommend to use \code{setalloccol} (though \code{alloc.col} will continue to be supported) because the \code{set*} prefix in \code{setalloccol} makes it clear that its input argument is modified in-place.
}
\usage{
truelength(x)
setalloccol(DT,
    n = getOption("datatable.alloccol"),        # default: 1024L
    verbose = getOption("datatable.verbose"))   # default: FALSE
alloc.col(DT,
    n = getOption("datatable.alloccol"),        # default: 1024L
    verbose = getOption("datatable.verbose"))   # default: FALSE
}
\arguments{
\item{x}{ Any type of vector, including \code{data.table} which is a \code{list} vector of column pointers. }
\item{DT}{ A \code{data.table}. }
\item{n}{ The number of spare column pointer slots to ensure are available. If \code{DT} is a 1,000 column \code{data.table} with 24 spare slots remaining, \code{n=1024L} means grow the 24 spare slots to be 1024. \code{truelength(DT)} will then be 2024 in this example. }
\item{verbose}{ Output status and information. }
}
\details{
    When adding columns by reference using \code{:=}, we \emph{could} simply create a new column list vector (one longer) and memcpy over the old vector,
    with no copy of the column vectors themselves. That requires negligible use of space and time, and long ago we did just that.
    However, that copy of the list vector of column pointers only (but not the columns themselves), a \emph{shallow copy}, resulted in inconsistent behaviour
    in some circumstances. Therefore, data.table over-allocates the list vector of column pointers so that columns can be added fully by reference, consistently.

    When the allocated column pointer slots are used up, to add a new column \code{data.table} must reallocate that vector. If two or more variables are bound to
    the same data.table this shallow copy may or may not be desirable, but we don't think this will be a problem very often (more discussion may be required on
    data.table issue tracker). Setting \code{options(datatable.verbose=TRUE)} includes messages if and when a shallow copy is taken. To avoid shallow copies there
    are several options: use \code{\link{copy}} to make a deep copy first, use \code{setalloccol} to reallocate in advance, or, change the default allocation rule
    (perhaps in your .Rprofile); e.g., \code{options(datatable.alloccol=10000L)}.

    Please note: over-allocation of the column pointer vector is not for efficiency \emph{per se}; it is so that \code{:=} can add columns by reference without a shallow copy.
}
\value{
    \code{truelength(x)} returns the length of the vector allocated in memory. \code{length(x)} of those items are in use. Currently, it is just the list vector of column
    pointers that is over-allocated (i.e. \code{truelength(DT)}), not the column vectors themselves, which would in future allow fast row \code{insert()}. For tables loaded
    from disk however, \code{truelength} is 0, which is perhaps unexpected. \code{data.table} detects this state and over-allocates the loaded \code{data.table} when the
    next column addition occurs. All other operations on \code{data.table} (such as fast grouping and joins) do not need \code{truelength}.

    \code{setalloccol} \emph{reallocates} \code{DT} by reference. This may be useful for efficiency if you know you are about to going to add a lot of columns in a loop.
    It also returns the new \code{DT}, for convenience in compound queries.
}
\seealso{ \code{\link{copy}} }
\examples{
DT = data.table(a=1:3,b=4:6)
length(DT)                 # 2 column pointer slots used
truelength(DT)             # 1026 column pointer slots allocated
setalloccol(DT, 2048)
length(DT)                 # 2 used
truelength(DT)             # 2050 allocated, 2048 free
DT[,c:=7L]                 # add new column by assigning to spare slot
truelength(DT)-length(DT)  # 2047 slots spare
}
\keyword{ data }
