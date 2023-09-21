stopifnot(!system("R CMD SHLIB frollmedian.c"))
#dyn.unload("frollmedian.so")
dyn.load("frollmedian.so")

frollmedian = function(x, n, na.rm=FALSE, verbose=FALSE){
  .Call("frollmedianR", as.double(x), as.integer(n), as.logical(na.rm), as.logical(verbose))
}
rollmedian = function(x, k, na.rm=FALSE) {
  ans = rep(NA_real_, length(x))
  if (k <= length(x)) {
    for (i in k:length(x)) {
      ans[i] = median(x[(i-k+1L):(i)], na.rm=na.rm)
    }
  }
  ans
}

if (benchmark<-FALSE) {
  set.seed(108)
  stopifnot(requireNamespace("RcppRoll"), requireNamespace("roll"))
  rcpproll = function(x, k) RcppRoll::roll_medianr(x, k)
  roll = function(x, k) roll::roll_median(x, k)
  r = function(x, k) rollmedian(x, k)
  test = function(x, k) {
    #t1<-system.time(a1<-rcpproll(x, k))
    t2<-system.time(a2<-roll(x, k))
    t3<-system.time(a3<-frollmedian(x, k))
    #t4<-system.time(a4<-r(x, k))
    #if (!isTRUE(all.equal(a1, a4))) stop("rcpproll != r")
    #if (!isTRUE(all.equal(a2, a4))) stop("roll != r")
    #if (!isTRUE(all.equal(a3, a4))) stop("dt != r")
    if (!isTRUE(all.equal(a2, a3))) stop("dt != roll")
    #list(RcppRoll=t1[["elapsed"]], roll=t2[["elapsed"]], data.table=t3[["elapsed"]], R=t4[["elapsed"]])
    list(roll=t2[["elapsed"]], data.table=t3[["elapsed"]])
  }
  N = c(1e4, 1e5, 1e6)
  K = c(100, 101, 1000, 1001, 10000, 10001)
  #N = rep(10^c(2:4), each=5) + rep(1:5, 3)
  #K = c(1:15, 20, 25, 50, 75, 100, 101, 120, 125, 199, 200, 201, 499, 500, 501)
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
  t
}

if (tiny<-FALSE) {
  x = sample(c(15, 10L, 7L, 11L, 6L, 13L, 12L, 14L, 8L, 7L, 9L, 10L, 7L, 6L, 8L, 10L, 15L), 100, TRUE)
  k = 11
  ans = frollmedian(x, k, verbose=T)
  cat("input:\n")
  x
  cat("median:\n")
  ans
  if (length(idx0<-which(ans!=rollmedian(x, k))-1L)) {
    stop("diff at ", paste(idx0, collapse=", "))
  }
}

if (test_uneven<-FALSE) {
  runmed = function(x, k) {
    ans = stats::runmed(x, k, algorithm="Turlach")
    h = (k-1L)/2L
    n = length(x)
    c(rep(NA_real_, k-1L), ans[-c(1:h, (n-h+1L):n)])
  }
  set.seed(108)
  for (i in 1:1e2) {
    x = sample(5e5, 1e5, TRUE)
    k = sample(seq(3, 1001, by=2), 1L)
    stopifnot(isTRUE(all.equal(runmed(x, k), frollmedian(x, k))))
  }
}

if (test_algo<-FALSE) {
  set.seed(108)
  x = rnorm(1e6)
  cat("k=1e3+1\n")
  print(system.time(frollmedian(x, 1e3+1)))
  cat("k=1e4+1\n")
  print(system.time(frollmedian(x, 1e4+1)))
  cat("k=1e5+1\n")
  print(system.time(frollmedian(x, 1e5+1)))
}

if (test_regression<-FALSE) {
  set.seed(108)
  stopifnot(requireNamespace("roll", quietly=TRUE))
  roll = function(x, k, na.rm=FALSE) roll::roll_median(x, k, na_restore=!na.rm)
  N = c(1e2, 1e2+1, 1e3, 1e3+1)
  K = c(10, 11, 100, 101)
  for (n in N) {
    x = rnorm(n)
    for (k in K) {
      stopifnot(isTRUE(all.equal(roll(x, k), rollmedian(x, k))))
      stopifnot(isTRUE(all.equal(roll(x, k), frollmedian(x, k))))
    }
  }
  #for (n in N) {
  #  x = rnorm(n)
  #  na = sample(n, n/10)
  #  x[na] = NA_real_
  #  for (k in K) {
  #    stopifnot(isTRUE(all.equal(rollmedian(x, k, na.rm=FALSE), frollmedian(x, k, na.rm=FALSE))))
  #    stopifnot(isTRUE(all.equal(rollmedian(x, k, na.rm=TRUE), frollmedian(x, k, na.rm=TRUE))))
  #  }
  #}
}

x = c(1,2,3,4,NA,7,8,9,8,NA,NA,1,2,3)
#x = c(1,2,3,4,Inf,7,8,9,8,-Inf,Inf,1,2,3)
x
k = 3
frollmedian(x, k, na.rm=FALSE)
frollmedian(x, k, na.rm=TRUE)
rollmedian(x, k, na.rm=FALSE)
rollmedian(x, k, na.rm=TRUE)

cat("OK\n")
