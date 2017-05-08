\name{CJI}
\alias{CJI}
\title{Fully indexed cross join}
\usage{
CJI(index, ..., nomatch = 0)
}
\arguments{
  \item{IDX}{list of data.table precalculated indices combinations.}
  \item{...}{values on which to filter, length and order must match key of data.table, use \code{TRUE} for fields which you don't want to filter on.}
  \item{nomatch}{0 or NA, default 0. To be passed to \code{CJ} when joining to index.}
}
\description{
To be used inside data.table, same as \code{CJ} function. In case of not filter on particular fields pass \code{TRUE} value to \dots, see examples.
}
\value{
Subset of \code{dt} data.table according to arguments passed to \dots.
}
\references{
feature discussion: link will be provided soon.
}
\note{
Experimental version. It do not scale for memory in case when using more keys to index on, for more information read \code{\link{setindex}}.
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
# subset by selective keys
DT[CJI(IDX,"id027",TRUE,TRUE,TRUE,TRUE,TRUE)]
DT[CJI(IDX,TRUE,"id061",TRUE,TRUE,TRUE,TRUE)]
DT[CJI(IDX,"id027","id061",TRUE,TRUE,TRUE,TRUE)]
DT[CJI(IDX,"id027",TRUE,"id0000004541",TRUE,TRUE,TRUE)]
DT[CJI(IDX,TRUE,"id061","id0000001742",TRUE,TRUE,TRUE)]
DT[CJI(IDX,TRUE,"id061",TRUE,83,TRUE,TRUE)]
DT[CJI(IDX,TRUE,"id061",TRUE,83,TRUE,3267)]
# validate few
identical(DT[id2=="id061"],
          DT[CJI(IDX,TRUE,"id061",TRUE,TRUE,TRUE,TRUE)])
identical(DT[id2=="id061" & id4==83],
          DT[CJI(IDX,TRUE,"id061",TRUE,83,TRUE,TRUE)])
identical(DT[id2=="id061" & id4==83 & id6==3267],
          DT[CJI(IDX,TRUE,"id061",TRUE,83,TRUE,3267)])
# you can of course query by multiple values:
DT[CJI(IDX,TRUE,"id061",TRUE,83,c(23,68),c(4611,3394,4373))]
}
}
