\name{truelength}
\alias{truelength}
\alias{alloc.col}
\title{ Over-allocation access }
\description{
    These functions are experimental and somewhat advanced. By \emph{experimental} we mean their names might change and perhaps the syntax, argument names and types (so if you write a lot of code using them, you have been warned!). They should work and be stable, though, so please report problems with them.
}
\usage{
truelength(x)
alloc.col(DT, n=getOption("datatable.alloccol",quote(max(100,2*ncol(DT)))))
}
\arguments{
\item{x}{ Any type of vector, including \code{data.table} which is a \code{list} vector of column pointers. }
\item{DT}{ A \code{data.table}. }
\item{n}{ The number of column pointer slots to reserve in memory, including existing columns. May be a numeric, or a quote()-ed expression (see default). If \code{DT} is a 10 column \code{data.table}, \code{n=1000} means grow the spare slots from 90 to 990, assuming the default of 100 has not been changed. }
}
\details{
    When adding columns by reference using \code{:=}, we \emph{could} simply create a new column list vector (one longer) and memcpy over the old vector, with no copy of the column vectors themselves. That requires negligibe use of space and time, and is what v1.7.2 did.  However, that copy of the list vector of column pointers only (but not the columns themselves), a \emph{shallow copy}, resulted in inconsistent behaviour in some circumstances. So, as from v1.7.3 data.table over allocates the list vector of column pointers so that columns can be added fully by reference, consistently.

    When the allocated column pointer slots are used up, to add a new column \code{data.table} must reallocate that vector, with a warning if two or more variables are bound to the same data.table (since the shallow copy can result in unexpected behaviour). To avoid this warning, there are several options: use \code{\link{copy}}, use \code{alloc.col} to reallocate in advance, wrap with \code{\link{suppressWarnings}} to indicate you anticipated the warning, or, change the default allocation rule (perhaps in your .Rprofile); e.g., \code{options(datatable.alloc.col=quote(1000))}.

    Please note : over allocation of the list vector of column pointers is not for efficiency, it's for consistency.
}
\value{
    \code{truelength(x)} returns the length of the vector allocated in memory. \code{length(x)} of those items are in use. Currently, it is the list vector of column pointers that is over-allocated (i.e. \code{truelength(DT)}), not the column vectors themselves (yet) which would in future allow fast row \code{insert()}.
    
    \code{alloc.col} \emph{reallocates} \code{DT} by reference. This may be useful for efficiency if you know you are about to going to add a lot of columns in a loop. It also returns the new DT, for convenience in compound queries.
}
\seealso{ \code{\link{copy}} }
\examples{
    DT = data.table(a=1:3,b=4:6)
    length(DT)                 # 2 column pointer slots used
    truelength(DT)             # 100 column pointer slots allocated
    alloc.col(DT,200)
    length(DT)                 # 2 used
    truelength(DT)             # 200 allocated
    DT[,c:=7L]                 # add new column
    truelength(DT)-length(DT)  # 197 slots spare
}
\keyword{ data }

