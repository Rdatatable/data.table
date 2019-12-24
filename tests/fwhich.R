if (!interactive()) {
  library(data.table)
} else {
  cc(F)
}

# Filter Helper Functions #4133 ----

foo <- data.table(
  x = as.character(runif(n = 10^6)),
  y = as.character(runif(n = 10^6)),
  z = as.character(runif(n = 10^6))
)
# easy
system.time(foo[like(x, "123")][like(y, "123")][like(z, "123")])
system.time(foo[like(x, "123") & like(y, "123") & like(z, "123")])
# hard
system.time(foo[like(x, "*")][like(y, "*")][like(z, "*")])
system.time(foo[like(x, "*") & like(y, "*") & like(z, "*")])

# A tibble: 3 x 13
#expression                                              min median `itr/sec` mem_alloc `gc/sec` n_itr
#<bch:expr>                                            <bch> <bch:>     <dbl> <bch:byt>    <dbl> <int>
#  1 foo[filter.at(c("x", "y", "z"), like(x, "123"))]      290ms  301ms      3.32     7.9MB    0.831     4
#2 foo[like(x, "123")][like(y, "123")][like(z, "123")]   291ms  291ms      3.35    8.13MB    0         5
#3 foo[like(x, "123") & like(y, "123") & like(z, "123")] 858ms  884ms      1.10    22.9MB    0         5

# Speed up logical indexing with multiple conditions #4105 ----


# which_equal, which_in #3663 ----

## which_eq ----

set.seed(108)
N = 1e7L

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

## fintersect ----

set.seed(108)
N = 1e6L

x = sample(N, N, FALSE) # 1 match
v = as.integer(N/2L)
w1 = which_eq(x, v)
w2 = which_eq(x, v+1L)
#system.time(fintersect(w1, w2))
system.time(intersect(w1, w2))

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

# which_eq + fintersect ----

set.seed(108)
N = 1e7L
DT = data.table(
  v1 = sample(1e1L, N, TRUE),
  v2 = sample(1e2L, N, TRUE),
  v3 = sample(1e3L, N, TRUE)
)
invisible(DT[])
s1 = as.integer(1e1L/2L)
s2 = as.integer(1e2L/2L)
s3 = as.integer(1e3L/2L)
options(datatable.auto.index=FALSE)

system.time(ans1<-DT[v1==s1 & v2==s2 & v3==s3])
system.time(ans2<-DT[v1==s1][v2==s2][v3==s3])
system.time(ans3<-DT[intersect(intersect(which(v1==s1), which(v2==s2)), which(v3==s3))])
system.time(ans4<-DT[fintersect(fintersect(which_eq(v1, s1), which_eq(v2, s2)), which_eq(v3, s3))])
system.time(ans5<-DT[which_eq(v3, s3, intersect=which_eq(v2, s2, intersect=which_eq(v1, s1)))]) # short-curcuit
#system.time(ans6<-DT[anding(v1==s1 & v2==s2 & v3==s3)]) # built-in short-curcuit (avoid 0-1 index shifting each time)
is.null(indices(DT)) && all.equal(ans1, ans2) && all.equal(ans1, ans3) && all.equal(ans1, ans4) && all.equal(ans1, ans5) #&& all.equal(ans1, ans6)

system.time(ans1<-DT[v1!=s1 & v2!=s2 & v3!=s3])
system.time(ans2<-DT[v1!=s1][v2!=s2][v3!=s3])
system.time(ans3<-DT[intersect(intersect(which(v1!=s1), which(v2!=s2)), which(v3!=s3))])
system.time(ans4<-DT[fintersect(fintersect(which_eq(v1, s1, negate=TRUE), which_eq(v2, s2, negate=TRUE)), which_eq(v3, s3, negate=TRUE))])
system.time(ans5<-DT[which_eq(v3, s3, negate=TRUE, intersect=which_eq(v2, s2, negate=TRUE, intersect=which_eq(v1, s1, negate=TRUE)))]) # short-curcuit
#system.time(ans6<-DT[anding(v1!=s1 & v2!=s2 & v3!=s3)]) # built-in short-curcuit (avoid 0-1 index shifting each time)
is.null(indices(DT)) && all.equal(ans1, ans2) && all.equal(ans1, ans3) && all.equal(ans1, ans4) && all.equal(ans1, ans5) #&& all.equal(ans1, ans6)

set.seed(108)
N = 1e7L
DT = data.table(
  v1 = sample(1L, N, TRUE), # const
  v2 = sample(2L, N, TRUE),
  v3 = sample(3L, N, TRUE)
)
invisible(DT[])
s1 = 1L
s2 = 2L
s3 = 3L
options(datatable.auto.index=FALSE)

system.time(ans1<-DT[v1==s1 & v2==s2 & v3==s3])
system.time(ans2<-DT[v1==s1][v2==s2][v3==s3])
system.time(ans3<-DT[intersect(intersect(which(v1==s1), which(v2==s2)), which(v3==s3))])
system.time(ans4<-DT[fintersect(fintersect(which_eq(v1, s1), which_eq(v2, s2)), which_eq(v3, s3))])
system.time(ans5<-DT[which_eq(v3, s3, intersect=which_eq(v2, s2, intersect=which_eq(v1, s1)))]) # short-curcuit
#system.time(ans6<-DT[anding(v1==s1 & v2==s2 & v3==s3)]) # built-in short-curcuit
is.null(indices(DT)) && all.equal(ans1, ans2) && all.equal(ans1, ans3) && all.equal(ans1, ans4) && all.equal(ans1, ans5) #&& all.equal(ans1, ans6)

system.time(ans1<-DT[v1!=s1 & v2!=s2 & v3!=s3])
system.time(ans2<-DT[v1!=s1][v2!=s2][v3!=s3])
system.time(ans3<-DT[intersect(intersect(which(v1!=s1), which(v2!=s2)), which(v3!=s3))])
system.time(ans4<-DT[fintersect(fintersect(which_eq(v1, s1, negate=TRUE), which_eq(v2, s2, negate=TRUE)), which_eq(v3, s3, negate=TRUE))])
system.time(ans5<-DT[which_eq(v3, s3, negate=TRUE, intersect=which_eq(v2, s2, negate=TRUE, intersect=which_eq(v1, s1, negate=TRUE)))]) # short-curcuit
#system.time(ans6<-DT[anding(v1!=s1 & v2!=s2 & v3!=s3)]) # built-in short-curcuit (avoid 0-1 index shifting each time)
is.null(indices(DT)) && all.equal(ans1, ans2) && all.equal(ans1, ans3) && all.equal(ans1, ans4) && all.equal(ans1, ans5) #&& all.equal(ans1, ans6)

# segfault debug
fintersect(c(1L,2L,3L), c(4L,5L))
fintersect(c(4L,5L), c(1L,2L,3L))
fintersect(integer(), c(1L,2L,3L))
fintersect(c(1L,2L,3L), integer())
fintersect(integer(), integer())
fintersect(c(2L,3L), c(1L,4L))
fintersect(c(1L,4L), c(2L,3L))
fintersect(c(2L,3L,4L), c(1L,3L,5L))
fintersect(c(1L,3L,5L), c(2L,3L,4L))


# end ----

