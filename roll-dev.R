cc(F)
d = as.data.table(list(1:10/2, 3:12/4))
replicate(50, cat("====================================================================\n")) -> devnul

#ans = rollmean(d, c(3), verbose=T)
#as.data.table(ans)

d[, paste0("R",1:4) := rollmean(.SD, c(3, 4)), .SDcols=c("V1","V2")][]

# ---- lets see how it compare to others rollmean

library(data.table)

# I will skip zoo rollmean because it is R implementation

library(TTR)
vrunMean = function(x, n) {
  # TTR rollmean (fortran) vectorized for x and n
  if (!is.list(x)) x = list(x)
  ans = lapply(x, function(x) {
    lapply(n, function(n) runMean(x, n))
  })
  unlist(ans, recursive=FALSE)
}

library(RcppRoll)
vroll_mean = function(x, n) {
  # RcppRoll rollmean (C++) vectorized for x and n
  if (!is.list(x)) x = list(x)
  ans = lapply(x, function(x) {
    lapply(n, function(n) roll_mean(x, n, fill=NA, align="right"))
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
#  0.650   0.104   0.764 
system.time(
  ans2<-vrunMean(z, w)
)
#   user  system elapsed 
#  1.486   0.256   1.745 
system.time(
  ans3<-vroll_mean(z, w)
)
#   user  system elapsed 
#  1.438   0.023   1.466 

all.equal(ans1, unname(ans2))
#[1] TRUE
all.equal(ans1, unname(ans3))
#[1] TRUE

system.time(
  ans1<-rollmean(z, w)
)
#   user  system elapsed 
#  0.272   0.100   0.377
system.time(
  ans2<-vrunMean(z, w)
)
#   user  system elapsed 
#  1.164   0.229   1.405 
system.time(
  ans3<-vroll_mean(z, w)
)
#   user  system elapsed 
#  1.483   0.000   1.489 
