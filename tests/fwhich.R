library(data.table)
options(datatable.auto.index=FALSE)

# speed up logical indexing with multiple conditions #4105 ----

fwhich = data.table:::fwhich

## like ----

set.seed(108)
N = 1e6L # timings for 1e8L, 20th, 36 GB mem required
foo = data.table(
  x = as.character(runif(n = N)),
  y = as.character(runif(n = N)),
  z = as.character(runif(n = N))
)
invisible({foo[c(1L,N)]; foo[c(1L,N)]; foo[c(1L,N)]}) # warmup

## easy
system.time(foo[like(x, "123")][like(y, "123")][like(z, "123")])
#   user  system elapsed
# 34.491   2.200  36.693
system.time(foo[like(x, "123") & like(y, "123") & like(z, "123")])
#   user  system elapsed
#102.829   6.408 109.241
system.time(foo[fwhich(like(x, "123") & like(y, "123") & like(z, "123"))])
#   user  system elapsed
# 33.188   1.156  34.346

## hard
system.time(foo[like(x, "*")][like(y, "*")][like(z, "*")])
#   user  system elapsed
# 82.554   9.927  92.484
system.time(foo[like(x, "*") & like(y, "*") & like(z, "*")])
#   user  system elapsed
# 41.066   4.920  45.988
system.time(foo[fwhich(like(x, "*") & like(y, "*") & like(z, "*"))])
#   user  system elapsed
# 74.307   8.320  82.324

## !easy
system.time(foo[!like(x, "123")][!like(y, "123")][!like(z, "123")])
#   user  system elapsed
#151.403  15.151 166.561
system.time(foo[!like(x, "123") & !like(y, "123") & !like(z, "123")])
#   user  system elapsed
#109.646   9.911 119.562
system.time(foo[fwhich(!like(x, "123") & !like(y, "123") & !like(z, "123"))])
#   user  system elapsed
#142.395  13.652 155.758

## !hard
system.time(foo[!like(x, "*")][!like(y, "*")][!like(z, "*")])
#   user  system elapsed
# 10.400   0.812  11.213
system.time(foo[!like(x, "*") & !like(y, "*") & !like(z, "*")])
#   user  system elapsed
# 34.604   2.864  37.470
system.time(foo[fwhich(!like(x, "*") & !like(y, "*") & !like(z, "*"))])
#   user  system elapsed
# 10.689   0.264  10.953

## ==, !=, %in%, !%in% ----

sample_all = function(unq_n, size) {
  stopifnot(unq_n <= size)
  unq_sub = seq_len(unq_n)
  sample(c(unq_sub, sample(unq_sub, size=max(size-unq_n, 0), replace=TRUE)))
}
set.seed(108)
N = 1e8L # timings for 1e8L, 20th, 6 GB mem required
foo = data.table(
  x = sample_all(N/10L, N),
  y = sample_all(N/10L, N),
  z = sample_all(N/10L, N)
)
invisible({foo[c(1L,N)]; foo[c(1L,N)]; foo[c(1L,N)]}) # warmup
if (N==1e6L) { #foo[.N/2L]
  s1 = 80332L; s2 = 8563L; s3 = 63039L
} else if (N==1e8L) {
  s1 = 7065182L; s2 = 7013931L; s3 = 8875689L
}
mid = as.integer(seq(as.integer(N*0.05), as.integer(N*0.95))) # mid ~0.9 obs

## easy
system.time(foo[x==s1][y==s2][z==s3])
#   user  system elapsed
#  0.429   0.157   0.463
system.time(foo[x==s1 & y==s2 & z==s3])
#   user  system elapsed
#  1.051   0.765   1.734
system.time(foo[fwhich(x==s1 & y==s2 & z==s3)])
#   user  system elapsed
#  0.071   0.000   0.071

## hard
system.time(foo[x%in%mid][y%in%mid][z%in%mid])
#   user  system elapsed
# 36.101   7.376  38.265
system.time(foo[x%in%mid & y%in%mid & z%in%mid])
#   user  system elapsed
# 25.043   4.376  28.430
#system.time(foo[fwhich(x%in%mid & y%in%mid & z%in%mid)])
# TOO SLOW!

## !easy
system.time(foo[x!=s1][y!=s2][z!=s3])
#   user  system elapsed
#  4.023   5.463   3.308
system.time(foo[x!=s1 & y!=s2 & z!=s3])
#   user  system elapsed
#  2.054   2.591   2.551
system.time(foo[fwhich(x!=s1 & y!=s2 & z!=s3)])
#   user  system elapsed
#  1.736   1.674   1.144

## !hard
system.time(foo[!x%in%mid][!y%in%mid][!z%in%mid])
#   user  system elapsed
# 35.832   7.684  39.046
system.time(foo[!x%in%mid & !y%in%mid & !z%in%mid])
#   user  system elapsed
# 25.540   5.131  29.200
system.time(foo[fwhich(!x%in%mid & !y%in%mid & !z%in%mid)])
#   user  system elapsed
#  1.728   1.651   1.192

# fast which #3663 ----

which_eq = data.table:::which_eq
set.seed(108)
N = 1e6L

x = sample(N, N, FALSE) # 1 match
v = as.integer(N/2L)
system.time(which_eq(x, v))
system.time(which(x==v))
system.time(which_eq(x, v, TRUE))
system.time(which(x!=v))

x = sample(N, N, FALSE) # 0 match
v = as.integer(N/2L)
system.time(which_eq(x, v))
system.time(which(x==v))
system.time(which_eq(x, v, TRUE))
system.time(which(x!=v))

n = as.integer(N/1e3L)
v = as.integer(n/2L)
x = sample(n, N, TRUE) # ~ N/1e3 matches
system.time(which_eq(x, v))
system.time(which(x==v))
system.time(which_eq(x, v, TRUE))
system.time(which(x!=v))

n = as.integer(N/10L)
v = as.integer(n/2L)
x = sample(n, N, TRUE) # ~ N/10 matches
system.time(which_eq(x, v))
system.time(which(x==v))
system.time(which_eq(x, v, TRUE))
system.time(which(x!=v))

# which_eq+fsintersect vs fwhich ----

fsintersect = data.table:::fsintersect
set.seed(108)
N = 1e6L
DT = data.table(
  v1 = sample(1e1L, N, TRUE),
  v2 = sample(1e2L, N, TRUE),
  v3 = sample(1e3L, N, TRUE)
)
invisible(DT[c(1L,N)])
s1 = as.integer(1e1L/2L)
s2 = as.integer(1e2L/2L)
s3 = as.integer(1e3L/2L)
options(datatable.auto.index=FALSE)

if (warmup_check<-FALSE) {
  options(datatable.verbose=TRUE)
  system.time(ans6<-DT[fwhich(v1==s1 & v2==s2 & v3==s3)])
  system.time(ans6<-DT[fwhich(v1==s1 & v2==s2 & v3==s3)])
  options(datatable.verbose=FALSE)
}

system.time(ans1<-DT[v1==s1 & v2==s2 & v3==s3])
system.time(ans2<-DT[v1==s1][v2==s2][v3==s3])
system.time(ans3<-DT[intersect(intersect(which(v1==s1), which(v2==s2)), which(v3==s3))])
system.time(ans4<-DT[fsintersect(fsintersect(which_eq(v1, s1), which_eq(v2, s2)), which_eq(v3, s3))])
system.time(ans5<-DT[which_eq(v3, s3, intersect=which_eq(v2, s2, intersect=which_eq(v1, s1)))]) # short-curcuit, internal style API, and 0-1 index shifting each time
system.time(ans6<-DT[fwhich(v1==s1 & v2==s2 & v3==s3)]) # built-in short-curcuit, nice API, no 0-1 index shifting each time, adds some overhead of processing language object
is.null(indices(DT)) && all.equal(ans1, ans2) && all.equal(ans1, ans3) && all.equal(ans1, ans4) && all.equal(ans1, ans5) && all.equal(ans1, ans6)
rm(list=paste0("ans",1:6))

system.time(ans1<-DT[v1!=s1 & v2!=s2 & v3!=s3])
system.time(ans2<-DT[v1!=s1][v2!=s2][v3!=s3])
system.time(ans3<-DT[intersect(intersect(which(v1!=s1), which(v2!=s2)), which(v3!=s3))])
system.time(ans4<-DT[fsintersect(fsintersect(which_eq(v1, s1, negate=TRUE), which_eq(v2, s2, negate=TRUE)), which_eq(v3, s3, negate=TRUE))])
system.time(ans5<-DT[which_eq(v3, s3, negate=TRUE, intersect=which_eq(v2, s2, negate=TRUE, intersect=which_eq(v1, s1, negate=TRUE)))])
system.time(ans6<-DT[fwhich(v1!=s1 & v2!=s2 & v3!=s3)])
is.null(indices(DT)) && all.equal(ans1, ans2) && all.equal(ans1, ans3) && all.equal(ans1, ans4) && all.equal(ans1, ans5) && all.equal(ans1, ans6)
rm(list=paste0("ans",1:6))

set.seed(108)
N = 1e7L
DT = data.table(
  v1 = sample(2L, N, TRUE),
  v2 = sample(3L, N, TRUE),
  v3 = sample(4L, N, TRUE)
)
invisible(DT[c(1L,N)])
s1 = 1L
s2 = 2L
s3 = 3L
options(datatable.auto.index=FALSE)

system.time(ans1<-DT[v1==s1 & v2==s2 & v3==s3])
system.time(ans2<-DT[v1==s1][v2==s2][v3==s3])
system.time(ans3<-DT[intersect(intersect(which(v1==s1), which(v2==s2)), which(v3==s3))])
system.time(ans4<-DT[fsintersect(fsintersect(which_eq(v1, s1), which_eq(v2, s2)), which_eq(v3, s3))])
system.time(ans5<-DT[which_eq(v3, s3, intersect=which_eq(v2, s2, intersect=which_eq(v1, s1)))])
system.time(ans6<-DT[fwhich(v1==s1 & v2==s2 & v3==s3)])
is.null(indices(DT)) && all.equal(ans1, ans2) && all.equal(ans1, ans3) && all.equal(ans1, ans4) && all.equal(ans1, ans5) && all.equal(ans1, ans6)
rm(list=paste0("ans",1:6))

system.time(ans1<-DT[v1!=s1 & v2!=s2 & v3!=s3])
system.time(ans2<-DT[v1!=s1][v2!=s2][v3!=s3])
system.time(ans3<-DT[intersect(intersect(which(v1!=s1), which(v2!=s2)), which(v3!=s3))])
system.time(ans4<-DT[fsintersect(fsintersect(which_eq(v1, s1, negate=TRUE), which_eq(v2, s2, negate=TRUE)), which_eq(v3, s3, negate=TRUE))])
system.time(ans5<-DT[which_eq(v3, s3, negate=TRUE, intersect=which_eq(v2, s2, negate=TRUE, intersect=which_eq(v1, s1, negate=TRUE)))])
system.time(ans6<-DT[fwhich(v1!=s1 & v2!=s2 & v3!=s3)])
is.null(indices(DT)) && all.equal(ans1, ans2) && all.equal(ans1, ans3) && all.equal(ans1, ans4) && all.equal(ans1, ans5) && all.equal(ans1, ans6)
rm(list=paste0("ans",1:6))

# fwhich mixed ops ----

DT = data.table(v1 = 1:10, v2 = 1:10, v3 = 1:10)
s1 = 2:9
s2 = 5L
s3 = 4:6
options(datatable.verbose=TRUE)
DT[fwhich(v1%in%s1 & v2%in%s2 & v3%in%s3)]
DT[fwhich(v1%in%s1 & v2==s2 & v3%in%s3)]
DT[fwhich(v1%in%s1 & v2!=6L & v3%in%s3)]
DT[fwhich(v1%in%s1 & v2%!in%s2 & v3%in%s3)]
options(datatable.verbose=FALSE)

# coerce int-num ----

# NA, NaNs ----

# TODO: type=double NA/NaN support

# example benchmark from post ----

library(data.table)
fwhich = data.table:::fwhich
options(datatable.auto.index=FALSE)

sample_all = function(unq_n, size) {
  stopifnot(unq_n <= size)
  unq_sub = seq_len(unq_n)
  sample(c(unq_sub, sample(unq_sub, size=max(size-unq_n, 0), replace=TRUE)))
}
set.seed(108)
N = 1e4L # timings for 1e8L, 20th, 6 GB mem required
foo = data.table(
  x = sample_all(N/10L, N),
  y = sample_all(N/10L, N),
  z = sample_all(N/10L, N)
)
invisible({foo[c(1L,N)]; foo[c(1L,N)]; foo[c(1L,N)]}) # warmup
if (N==1e3L) {
  s1 = 45L; s2 = 28L; s3 = 91L
} else if (N==1e4L) {
  s1 = 638L; s2 = 975L; s3 = 930L
} else if (N==1e6L) { #foo[.N/2L]
  s1 = 80332L; s2 = 8563L; s3 = 63039L
} else if (N==1e8L) {
  s1 = 7065182L; s2 = 7013931L; s3 = 8875689L
}
mid = as.integer(seq(as.integer(N*0.05), as.integer(N*0.95))) # mid ~0.9 obs
wh = function(x) eval(substitute(identical(which(.x, useNames=FALSE), fwhich(.x)), list(.x=substitute(x))), parent.frame())

## easy
system.time(foo[x==s1][y==s2][z==s3])
system.time(foo[x==s1 & y==s2 & z==s3])
system.time(foo[fwhich(x==s1 & y==s2 & z==s3)])
with(foo, wh(x==s1 & y==s2 & z==s3))

## hard
system.time(foo[x%in%mid][y%in%mid][z%in%mid])
system.time(foo[x%in%mid & y%in%mid & z%in%mid])
system.time(foo[fwhich(x%in%mid & y%in%mid & z%in%mid)])
# TOO SLOW!
with(foo, wh(x%in%mid & y%in%mid & z%in%mid))

## !easy
system.time(foo[x!=s1][y!=s2][z!=s3])
system.time(foo[x!=s1 & y!=s2 & z!=s3])
system.time(foo[fwhich(x!=s1 & y!=s2 & z!=s3)])
with(foo, wh(x!=s1 & y!=s2 & z!=s3))

## !hard
system.time(foo[!x%in%mid][!y%in%mid][!z%in%mid])
system.time(foo[!x%in%mid & !y%in%mid & !z%in%mid])
system.time(foo[fwhich(!x%in%mid & !y%in%mid & !z%in%mid)])
with(foo, wh(!x%in%mid & !y%in%mid & !z%in%mid))
