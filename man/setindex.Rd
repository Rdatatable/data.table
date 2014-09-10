\name{setindex}
\alias{setindex}
\alias{setindexv}
\title{Create all indices combinations}
\usage{
setindex(dt, ..., verbose = getOption("datatable.verbose"))
setindexv(dt, cols, verbose = getOption("datatable.verbose"))
}
\arguments{
  \item{dt}{data.table based on which create indices.}
  \item{\dots}{ columns to create indices on.}
  \item{cols}{character vector of column names on which prepare indices.}
  \item{verbose}{ Output status and information. }
}
\description{
Function prepare all possibile keys combination, calculate keys for all those combinations and returns then as list. It do have side effect as data.table's \code{set*} functions usually has.
}
\value{
Returns list of indices combinations, each element of the list is a data.table.
}
\note{
Experimental version. It do not scale for memory in case when using more keys to index on. There is a open discussion related to possible improvements, see reference.
}
\section{Side effect}{
None at the moment, might change in future for kind of \code{setattr(DT,"index",IDX)} if it will optimized enough.
}
\examples{
\dontrun{
# import funs (to do not modify NAMESPACE file in master branch)
setindex <- getFromNamespace("setindex", "data.table")
setindexv <- getFromNamespace("setindexv", "data.table")
CJI <- getFromNamespace("CJI", "data.table")
# you can skip this but you need to use `data.table:::` prefix for functions

# populate DT
N=5e5; K=100
set.seed(1)
DT <- data.table(
  id1 = sample(sprintf("id\%03d",1:K), N, TRUE),      # large groups (char)
  id2 = sample(sprintf("id\%03d",1:K), N, TRUE),      # large groups (char)
  id3 = sample(sprintf("id\%010d",1:(N/K)), N, TRUE), # small groups (char)
  id4 = sample(K, N, TRUE),                          # large groups (int)
  id5 = sample(K, N, TRUE),                          # large groups (int)
  id6 = sample(N/K, N, TRUE),                        # small groups (int)
  v1 =  sample(5, N, TRUE),                          # int in range [1,5]
  v2 =  sample(5, N, TRUE),                          # int in range [1,5]
  v3 =  sample(round(runif(100,max=100),4), N, TRUE) # numeric e.g. 23.5749
)
# setkey
setkeyv(DT,cols = c("id1", "id2", "id3", "id4", "id5", "id6"))
# do indices list
IDX <- setindexv(DT,cols = c("id1", "id2", "id3", "id4", "id5", "id6"))
# preview indices combinations
names(IDX)
# take a look at few
IDX[c("2","2_4","2_4_6")]
# now use the IDX in CJI
?CJI
}
}
