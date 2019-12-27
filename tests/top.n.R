library(data.table)

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
