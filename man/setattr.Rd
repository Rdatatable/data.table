\name{setattr}
\alias{setattr}
\alias{setnames}
\title{ Set attributes to objects by reference }
\description{
  Note that in \code{data.table} parlance, all \code{set*} functions change their input \emph{by reference}. That is, no copy is made at all, other than temporary working memory, which is as large as one column.. The only other \code{data.table} operator that modifies input by reference is \code{\link{:=}}. Check out the \code{See Also} section below for other \code{set*} function \code{data.table} provides.
  
  \code{setnames} operates only on \code{data.frame}s and \code{data.table}s and is used for setting or changing column names \emph{by reference}.
  
  \code{setattr} is a more general function that allows setting of any attribute to an object \emph{by reference}.
}
\usage{
setattr(x,name,value)
setnames(x,old,new)
}
\arguments{
  \item{x}{ \code{setnames} accepts \code{data.frame} and \code{data.table}. \code{setattr} accepts any input; e.g, list, columns of a \code{data.frame} or \code{data.table}. }
  \item{name}{ The character attribute name. }
  \item{value}{ The value to assign to the attribute or \code{NULL} removes the attribute, if present. }
  \item{old}{ When \code{new} is provided, character names or numeric positions of column names to change. When \code{new} is not provided, the new column names, which must be the same length as the number of columns. See examples. }
  \item{new}{ Optional. New column names, the same length as \code{old}. } 
}
\details{
  The <- method copies the whole table and we know of no way to avoid that copy without a change in R itself. Please use the set* functions instead, which make no copy at all. That is, please use \code{setattr()} rather than \code{attr(x, name) <- value}, \code{setnames()} rather than \code{names(x) <- value} or \code{colnames(x) <- value} and checkout other \code{set*} functions available for setting keys, reordering rows and columns of \code{data.table}, adding columns \emph{by reference}. In particular, \code{setattr()} is useful in many situations to set attributes by reference and can be used on any object or part of an object, not just \code{data.table}s.
  
  }
\value{
    The input is modified by reference, and returned (invisibly) so it can be used in compound statements; e.g., \code{setnames(DT,"V1", "Y")[, .N, by=Y]}. If you require a copy, take a copy first (using \code{DT2=copy(DT)}). See \code{?copy}.

    Note that \code{setattr} is also in package \code{bit}. Both packages merely expose R's internal \code{setAttrib} function at C level, but differ in return value. \code{bit::setattr} returns \code{NULL} (invisibly) to remind you the function is used for its side effect. \code{data.table::setattr} returns the changed object (invisibly), for use in compound statements.
}
\seealso{ \code{\link{data.table}}, \code{\link{setkey}}, \code{\link{setorder}}, \code{\link{set}}, \code{\link{:=}}, \code{\link{setDT}}, \code{\link{copy}}
\if{html}{\out{<script type="text/javascript">var sc_project=6237851;var sc_invisible=1;var sc_security="518c93ca";</script><script type="text/javascript" src="http://www.statcounter.com/counter/counter.js"></script><noscript><div class="statcounter"><a title="web statistics" href="http://statcounter.com/free-web-stats/" target="_blank"><img class="statcounter" src="http://c.statcounter.com/6237851/0/518c93ca/1/" alt="web statistics"></a></div></noscript>}}
}
\examples{
    
    DF = data.frame(a=1:2,b=3:4)       # base data.frame to demo copies, as of R 2.15.1
    try(tracemem(DF))                  # try() for R sessions opted out of memory profiling
    colnames(DF)[1] <- "A"             # 4 copies of entire object
    names(DF)[1] <- "A"                # 3 copies of entire object
    names(DF) <- c("A", "b")           # 1 copy of entire object
    `names<-`(DF,c("A","b"))           # 1 copy of entire object
    
    # What if DF is large, say 10GB in RAM. Copy 10GB, even once, just to change a column name?

    DT = data.table(a=1:2,b=3:4,c=5:6)
    try(tracemem(DT))
    setnames(DT,"b","B")               # by name; no match() needed
    setnames(DT,3,"C")                 # by position
    setnames(DT,2:3,c("D","E"))        # multiple
    setnames(DT,c("a","E"),c("A","F")) # multiple by name
    setnames(DT,c("X","Y","Z"))        # replace all
    
    # And, no copy of DT was made by 'setnames()' at all.
    
    # set attributes - ex: names to a list.
    set.seed(1L)
    ll <- lapply(1:4, function(x) sample(10))
    try(tracemem(DT))
    setattr(ll, 'names', letters[1:4])
    
    # once again, no copy of 'll' was made by 'setattr()' at all.
}
\keyword{ data }

