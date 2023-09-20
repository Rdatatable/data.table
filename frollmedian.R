stopifnot(!system("R CMD SHLIB frollmedian.c"))
#dyn.unload("frollmedian.so")
dyn.load("frollmedian.so")

frollmedian = function(x, n){
  .Call("frollmedianR", as.double(x), as.integer(n))
}
rollmedian = function(x, k) {
  ans = rep(NA_real_, length(x))
  for (i in k:length(x)) {
    ans[i] = median(x[(i-k+1L):(i)])
  }
  ans
}

n = rep(10^c(2:4), each=5) + rep(1:5, 3)
k = c(10,11,12,13,14,15)
#k = c(11,13,15)
#k = c(10,12,14)
test = function(x, k) {
  if (isTRUE(all.equal(frollmedian(x, k), rollmedian(x, k)))) cat("OK\n")
  else stop("not equal")
  #list(frollmedian(x, k), rollmedian(x, k))
  #frollmedian(x, k)
}

if (TRUE) {
for (nn in n) {
  cat("n=",nn,"\n",sep="")
  x = rnorm(nn)
  for (kk in k) {
    cat("k=",kk,"\n",sep="")
    test(x, kk)
  }
}
}
#x = c(15, 10L, 7L, 11L, 6L, 13L, 12L, 14L, 8L, 7L, 9L, 10L, 7L, 6L, 8L, 10L)
#k = 4
#ans = frollmedian(x, k)
#cat("input:\n")
#x
#cat("median:\n")
#ans
#if (length(idx0<-which(ans!=rollmedian(x, k))-1L)) {
#  stop("diff at ", paste(idx0, collapse=", "))
#}
cat("OK\n")
