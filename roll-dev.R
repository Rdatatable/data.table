cc(F)
d = as.data.table(list(1:10/2, 3:12/4))
replicate(50, cat("====================================================================\n")) -> devnul

#ans = rollmean(d, c(3), verbose=T)
#as.data.table(ans)

d[, paste0("R",1:4) := rollmean(.SD, c(3, 4)), .SDcols=c("V1","V2")
  ][, paste0("TTR",1:4) := vrunMean(.SD, c(3, 4)), .SDcols=c("V1","V2")
    ][]

# ---- lets see how it compare to fortran rollmean from TTR

vrunMean = function(x, n) {
  # TTR rollmean (fortran) vectorized for x and n
  if (!is.list(x)) x = list(x)
  ans = lapply(x, function(x) {
    lapply(n, function(n) TTR::runMean(x, n))
  })
  unlist(ans, recursive=FALSE)
}

z = replicate(6, rnorm(1e6, 1000, 100), simplify=FALSE)
setDT(z)

w = c(20,50,100)
system.time(
  ans1<-rollmean(z, w)
)
#   user  system elapsed 
#  0.366   0.104   0.471
system.time(
  ans2<-vrunMean(z, w)
)
#   user  system elapsed 
#  1.455   0.263   1.720 

all.equal(ans1, unname(ans2))
#[1] TRUE

system.time(
  ans1<-rollmean(z, w)
)
#   user  system elapsed 
#  0.268   0.067   0.336
system.time(
  ans2<-vrunMean(z, w)
)
#   user  system elapsed 
#  1.162   0.048   1.224
