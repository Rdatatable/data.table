library(data.table)
options(datatable.auto.index=FALSE)
######################## top.n ###################################
dt <- as.data.table(iris)
options(datatable.verbose=TRUE)

##dt[top.n(n, wt, by, ties = FALSE)]

## no wt
dt[top.n(3)] 
dt[top.n(-3)] 

dt[top.n(3, by = Species)]
dt[top.n(-3, by = Species)]
dt[top.n(3, by = Species, ties = TRUE)] #ties ignored

## wt
dt[top.n(3, Sepal.Length)]
dt[top.n(3, Sepal.Length, ties = TRUE)]

dt[top.n(3, Sepal.Length, Species)]
dt[top.n(3, Sepal.Length, Species, ties = TRUE)]

## no wt
options(datatable.verbose=FALSE)
identical(dt[top.n(3)],
          dt[1:min(.N, 3)]) #also head(dt, 3)

identical(dt[top.n(-3)],
          dt[max(1, .N - 3 + 1):.N]) #also tail(dt, 3)

identical(dt[top.n(3, by = Species)],
          dt[dt[, .I[1:min(.N, 3)], by = Species]$V1])

identical(dt[top.n(-3, by = Species)],
          dt[dt[, .I[(max(1, .N - 3 + 1):.N)], by = Species]$V1])

# wt
identical(dt[top.n(3, Sepal.Length)],
          dt[order(-Sepal.Length)[1:3]])

identical(dt[top.n(3, Sepal.Length, Species)],
          dt[dt[order(-Sepal.Length), .I[1:3], by = Species]$V1])

#0.5 GB groupby benchmark from H20
N = 10000000
K = 100
set.seed(108)

DT = data.table(
  id1 = sample(sprintf("id%03d",1:K), N, TRUE),      # large groups (char)
  id2 = sample(sprintf("id%03d",1:K), N, TRUE),      # large groups (char)
  id3 = sample(sprintf("id%010d",1:(N/K)), N, TRUE), # small groups (char)
  id4 = sample(K, N, TRUE),                          # large groups (int)
  id5 = sample(K, N, TRUE),                          # large groups (int)
  id6 = sample(N/K, N, TRUE),                        # small groups (int)
  v1 =  sample(5, N, TRUE),                          # int in range [1,5]
  v2 =  sample(5, N, TRUE),                          # int in range [1,5]
  v3 =  round(runif(N,max=100),4)                    # numeric e.g. 23.5749
)

system.time({res1 = DT[order(-v3), .(largest2_v3 = head(v3, 2L)), by = id6]})
system.time({res2 = DT[top.n(2L, v3, id6), .(id6, largest2_v3 = v3)]})
identical(res1, res2)
