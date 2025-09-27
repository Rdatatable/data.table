\name{data.table}
\alias{data.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Just like a data.frame, but without row names }
\description{
  Same as data.frame() but the result has no row names. In some cases (see example) rownames alone are responsible for 90\% of the memory used by a data.frame. Removing them can therefore mean up to 10 times less memory, and be 10 times faster to create, and 10 times faster to copy. 1:nrow stored in character form is inefficient since rows can be indexed by their integer position. For example, DF[6,] and DF["6",] both work but the former is more efficient. 
}
\usage{
data.table(..., keep.rownames = FALSE, check.names = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{ Just as ... in data.frame() }
  \item{keep.rownames}{ If ... is a data.frame itself, TRUE will retain the rownames in the first column }
  \item{check.names}{ Just as in data.frame() }
}
\details{
    This class really does very little. The only reason for its existence is that the white book specifies that data.frame must have rownames.
    
    Most of the code is copied from base functions with the code manipulating row.names removed.
    
    A data.table is identical to a data.frame other than:\cr
    it doesn't have rownames\cr
    [,drop] by default is FALSE, so selecting a single row will always return a single row data.table not a vector\cr
    The comma is optional inside [], so DT[3] returns the 3rd row as a 1 row data.table\cr
    [] is like a call to subset()\cr
    [,...], is like a call to with().  (not yet implemented)\cr

    Motivation:\cr
    up to 10 times less memory\cr
    up to 10 times faster to create, and copy\cr
    simpler R code by allowing column name expressions within []\cr
    the white book defines rownames, so data.frame itself can't be changed ... => new class\cr
}
\value{
  Identical to the result of data.frame, but without the row.names attribute.
}
\references{ http://tolstoy.newcastle.edu.au/R/devel/05/12/3439.html }
\author{ Matt Dowle }
\note{ }
\seealso{ \code{\link{data.frame}} }
\examples{
nr = 1000000
D = rep(1:5,nr/5)
system.time(DF <<- data.frame(colA=D, colB=D))  # 2.08 
system.time(DT <<- data.table(colA=D, colB=D))  # 0.15  (over 10 times faster to create)
identical(as.data.table(DF), DT)
identical(dim(DT),dim(DF))
object.size(DF)/object.size(DT)                 # 10 times less memory

tt = subset(DF,colA>3)
ss = DT[colA>3]
identical(as.data.table(tt), ss)

mean(subset(DF,colA+colB>5,"colB"))
mean(DT[colA+colB>5]$colB)

tt = with(subset(DF,colA>3),colA+colB)
ss = with(DT[colA>3],colA+colB)                 # but could be:  DT[colA>3,colA+colB]  (not yet implemented)
identical(tt, ss)

tt = DF[with(DF,tapply(1:nrow(DF),colB,last)),] # select last row grouping by colB
ss = DT[tapply(1:nrow(DT),colB,last)]           # but could be:  DT[last,group=colB]  (not yet implemented)
identical(as.data.table(tt), ss)

Lkp=1:3
tt = DF[with(DF,colA \%in\% Lkp),]              
ss = DT[colA \%in\% Lkp]                        # expressions inside the [] can see objects in the calling frame
identical(as.data.table(tt), ss)

}
\keyword{ data }
