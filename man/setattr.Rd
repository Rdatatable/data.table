\name{setattr}
\alias{setattr}
\alias{setnames}
\title{ Set attributes of objects by reference }
\description{
  In \code{data.table}, all \code{set*} functions change their input \emph{by reference}. That is, no copy is made at all, other than temporary working memory which is as large as one column. The only other \code{data.table} operator that modifies input by reference is \code{\link{:=}}. Check out the \code{See Also} section below for other \code{set*} function that \code{data.table} provides.
}
\usage{
setattr(x,name,value)
setnames(x,old,new,skip_absent=FALSE)
}
\arguments{
  \item{x}{ \code{setnames} accepts \code{data.frame} and \code{data.table}. \code{setattr} accepts any input; e.g, list, columns of a \code{data.frame} or \code{data.table}. }
  \item{name}{ The character attribute name. }
  \item{value}{ The value to assign to the attribute or \code{NULL} removes the attribute, if present. }
  \item{old}{ When \code{new} is provided, character names or numeric positions of column names to change. When \code{new} is not provided, a function or the new column names (i.e., it's implicitly treated as \code{new}; excluding \code{old} and explicitly naming \code{new} is equivalent). If a function, it will be called with the current column names and is supposed to return the new column names. The new column names must be the same length as the number of columns. See examples. }
  \item{new}{ Optional. It can be a function or the new column names. If a function, it will be called with \code{old} and expected to return the new column names. The new column names must be the same length as columns provided to \code{old} argument. }
  \item{skip_absent}{ Skip items in \code{old} that are missing (i.e. absent) in `names(x)`. Default \code{FALSE} halts with error if any are missing. }
}

\details{

  \code{setnames} operates on \code{data.table} and \code{data.frame} not other types like \code{list} and \code{vector}. It can be used to change names \emph{by name} with built-in checks and warnings (e.g., if any old names are missing or appear more than once).

  \code{setattr} is a more general function that allows setting of any attribute to an object \emph{by reference}.

  A very welcome change in R 3.1+ was that `names<-` and `colnames<-` no longer copy the \emph{entire} object as they used to (up to 4 times), see examples below. They now take a shallow copy. The `set*` functions in data.table are still useful because they don't even take a shallow copy. This allows changing names and attributes of a (usually very large) \code{data.table} in the global environment \emph{from within functions}. Like a database.

  }
\value{
    The input is modified by reference, and returned (invisibly) so it can be used in compound statements; e.g., \code{setnames(DT,"V1", "Y")[, .N, by=Y]}. If you require a copy, take a copy first (using \code{DT2=copy(DT)}). See \code{?copy}.

    Note that \code{setattr} is also in package \code{bit}. Both packages merely expose R's internal \code{setAttrib} function at C level but differ in return value. \code{bit::setattr} returns \code{NULL} (invisibly) to remind you the function is used for its side effect. \code{data.table::setattr} returns the changed object (invisibly) for use in compound statements.
}
\seealso{ \code{\link{data.table}}, \code{\link{setkey}}, \code{\link{setorder}}, \code{\link{setcolorder}}, \code{\link{set}}, \code{\link{:=}}, \code{\link{setDT}}, \code{\link{setDF}}, \code{\link{copy}}
}
\examples{

DT <- data.table(a = 1, b = 2, d = 3)

old <- c("a", "b", "c", "d")
new <- c("A", "B", "C", "D")

setnames(DT, old, new, skip_absent = TRUE) # skips old[3] because "c" is not a column name of DT

DF = data.frame(a=1:2,b=3:4)       # base data.frame to demo copies and syntax
if (capabilities()["profmem"])     # usually memory profiling is available but just in case
  tracemem(DF)
colnames(DF)[1] <- "A"             # 4 shallow copies (R >= 3.1, was 4 deep copies before)
names(DF)[1] <- "A"                # 3 shallow copies
names(DF) <- c("A", "b")           # 1 shallow copy
`names<-`(DF,c("A","b"))           # 1 shallow copy

DT = data.table(a=1:2,b=3:4,c=5:6) # compare to data.table
if (capabilities()["profmem"])
  tracemem(DT)                     # by reference, no deep or shallow copies
setnames(DT,"b","B")               # by name, no match() needed (warning if "b" is missing)
setnames(DT,3,"C")                 # by position with warning if 3 > ncol(DT)
setnames(DT,2:3,c("D","E"))        # multiple
setnames(DT,c("a","E"),c("A","F")) # multiple by name (warning if either "a" or "E" is missing)
setnames(DT,c("X","Y","Z"))        # replace all (length of names must be == ncol(DT))
setnames(DT,tolower)               # replace all names with their lower case
setnames(DT,2:3,toupper)           # replace the 2nd and 3rd names with their upper case

DT <- data.table(x = 1:3, y = 4:6, z = 7:9)
setnames(DT, -2, c("a", "b"))      # NEW FR #1443, allows -ve indices in 'old' argument

DT = data.table(a=1:3, b=4:6)
f = function(\dots) {
    # ...
    setattr(DT,"myFlag",TRUE)  # by reference
    # ...
    localDT = copy(DT)
    setattr(localDT,"myFlag2",TRUE)
    # ...
    invisible()
}
f()
attr(DT,"myFlag")   # TRUE
attr(DT,"myFlag2")  # NULL

}
\keyword{ data }
