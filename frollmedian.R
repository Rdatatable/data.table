stopifnot(!system("R CMD SHLIB frollmedian.c"))
#dyn.unload("frollmedian.so")
dyn.load("frollmedian.so")

frollmedian = function(x, n){
  .Call("frollmedianR", as.double(x), as.integer(n))
}
rollmedian = function(x, k) {
  ans = rep(NA_real_, length(x))
  if (k <= length(x)) {
    for (i in k:length(x)) {
      ans[i] = median(x[(i-k+1L):(i)])
    }
  }
  ans
}

if (benchmark<-TRUE) {
  set.seed(108)
  stopifnot(requireNamespace("RcppRoll"), requireNamespace("roll"))
  rcpproll = function(x, k) RcppRoll::roll_medianr(x, k)
  roll = function(x, k) roll::roll_median(x, k)
  r = function(x, k) rollmedian(x, k)
  test = function(x, k) {
    t1<-system.time(a1<-rcpproll(x, k))
    t2<-system.time(a2<-roll(x, k))
    t3<-system.time(a3<-frollmedian(x, k))
    t4<-system.time(a4<-r(x, k))
    if (!isTRUE(all.equal(a1, a4))) stop("rcpproll != r")
    if (!isTRUE(all.equal(a2, a4))) stop("roll != r")
    if (!isTRUE(all.equal(a3, a4))) stop("dt != r")
    list(RcppRoll=t1[["elapsed"]], roll=t2[["elapsed"]], data.table=t3[["elapsed"]], R=t4[["elapsed"]])
  }
  #N = c(1000, 1501, 5001, 5002)*10 #c(10L, 15L, 20L, 25L, 99L, 100L, 101L, 999L, 1000L, 1001L)
  N = rep(10^c(2:4), each=5) + rep(1:5, 3)
  #K = c(20, 21, 100, 102, 500, 501, 1000, 1001)
  K = c(1:15, 20, 25, 50, 75, 100, 101, 120, 125, 199, 200, 201, 499, 500, 501)
  fk = function(k, x) {
    cat("  k=",k,"\n",sep="")
    test(x, k)
  }
  fn = function(n, K) {
    cat("n=",n,"\n",sep="")
    x = rnorm(n)
    data.table::rbindlist(lapply(setNames(nm=K), fk, x=x), idcol="k")
  }
  set.seed(108)
  t = data.table::rbindlist(lapply(setNames(nm=N), fn, K=K), idcol="n")
  d = data.table::melt(t, id=1:2, value.name="elapsed")
  d
}

#x = c(15, 10L, 7L, 11L, 6L, 13L, 12L, 14L, 8L, 7L, 9L, 10L, 7L, 6L, 8L, 10L, 15L)
#k = 9
#ans = frollmedian(x, k)
#cat("input:\n")
#x
#cat("median:\n")
#ans
#if (length(idx0<-which(ans!=rollmedian(x, k))-1L)) {
#  stop("diff at ", paste(idx0, collapse=", "))
#}
cat("OK\n")
