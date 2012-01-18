context("data.frame like functions (merge, subset, transform)")

###############################################################################
## Merge
test_that("`x` columns are valid (bug #1299)", {
    d1 <- data.table(x=c(1,3,8), y1=rnorm(3), key="x")
    d2 <- data.table(x=c(3,8,10), y2=rnorm(3), key="x")
    ans1 <- merge(d1, d2, by="x")
    ans2 <- cbind(d1[2:3], y2=d2[1:2]$y2)
    setkey(ans2, x)
    expect_equal(ans1, ans2, info="Original test #230")
})

test_that("`xkey` column names are valid in merge (bug#1299", {
    d1 <- data.table(xkey=c(1,3,8), y1=rnorm(3), key="xkey")
    d2 <- data.table(xkey=c(3,8,10), y2=rnorm(3), key="xkey")
    ans2 <- cbind(d1[2:3], y2=d2[1:2]$y2)
    setkey(ans2, xkey)
    expect_equal(merge(d1, d2, by="xkey"), ans2, info="Original test #238")
})

test_that("one column merges work (bug #1241)", {
    dt <- data.table(a=rep(1:2,each=3), b=1:6, key="a")
    y <- J(a=c(0,1), bb=c(10,11), key="a")
    expect_equal(merge(y, dt), data.table(a=1L, bb=11L, b=1:3, key="a"),
                 info="Original test #231")
    expect_equal(merge(y, dt, all=TRUE),
                 data.table(a=rep(c(0L,1L,2L),c(1,3,3)),
                            bb=rep(c(10L,11L,NA_integer_),c(1,3,3)),
                            b=c(NA_integer_,1:6), key="a"),
                 info="Original test #232")

    ## y with only a key column
    y <- J(a=c(0,1), key="a")
    expect_equal(merge(y,dt), data.table(a=1L, b=1:3, key="a"),
                 info="Original test #233")
    expect_equal(merge(y, dt, all=TRUE),
                 data.table(a=rep(c(0L,1L,2L),c(1,3,3)),
                            b=c(NA_integer_,1:6),key="a"),
                 info="Original test #234")
})

test_that("merging data.tables is almost like merging data.frames", {
    d1 <- data.table(a=sample(letters, 10), b=sample(1:100, 10), key='a')
    d2 <- data.table(a=d1$a, b=sample(1:50, 10), c=rnorm(10), key='a')
    dtm <- merge(d1, d2, by='a', suffixes=c(".xx", ".yy"))
    dtm.df <- as.data.frame(dtm)
    dfm <- merge(as.data.frame(d1), as.data.frame(d2), by='a', suffixes=c('.xx', '.yy'))

    expect_equal(unname(dtm.df), unname(dfm),
                 info="Testing contents/data after merge")
    expect_equal(colnames(dtm), colnames(dfm),
                 info="Original test #255 (testing suffixes parameter)")
})

test_that("`suffixes` behavior can be toggled to pre 1.5.4 behavior", {
    dt1 <- data.table(a=letters[1:5], b=1:5, key="a")
    dt2 <- data.table(a=letters[3:8], b=1:6, key="a")

    options(datatable.pre.suffixes=FALSE)
    expect_equal(colnames(merge(dt1, dt2)), c("a", "b.x", "b.y"))

    options(datatable.pre.suffixes=TRUE)
    expect_equal(colnames(merge(dt1, dt2)), c("a", "b", "b.1"),
                 info="Pre 1.5.4 behavior not working")

    options(datatable.pre.suffixes=FALSE)
})

###############################################################################
## subset
test_that("simple subset maintains keys", {
  dt <- data.table(a=sample(c('a', 'b', 'c'), 20, replace=TRUE),
                   b=sample(c('a', 'b', 'c'), 20, replace=TRUE),
                   c=sample(20), key='a')
  sub <- subset(dt, a == 'b')
  expect_equal(key(dt), key(sub))
})

test_that("subset using 'select' maintains key appropriately", {
  dt <- data.table(a=sample(c('a', 'b', 'c'), 20, replace=TRUE),
                   b=sample(c('a', 'b', 'c'), 20, replace=TRUE),
                   c=sample(20), key=c('a', 'b'))

  sub.1 <- subset(dt, a == 'a', select=c('c', 'b', 'a'))
  expect_equal(key(sub.1), key(dt), info="reordering columns")

  sub.2 <- subset(dt, a == 'a', select=c('a', 'c'))
  expect_equal(key(sub.2), 'a', info="selected columns are prefix of key")

  sub.3 <- subset(dt, a == 'a', select=c('b', 'c'))
  expect_true(is.null(key(sub.3)),
              info="selected columns do not from a key prefix")
  
  sub.4 <- subset(dt, a == 'cc')
  expect_equal(nrow(sub.4), 0)
  expect_true(is.null(key(sub.4)))
})

###############################################################################
## transform
test_that("transform maintains keys", {
  dt <- data.table(a=sample(c('a', 'b', 'c'), 20, replace=TRUE),
                   b=sample(c('a', 'b', 'c'), 20, replace=TRUE),
                   c=sample(20), key=c('a', 'b'))

  t1 <- transform(dt, d=c+4)
  expect_equal(key(t1), key(dt))
  expect_equal(t1$d, dt$c + 4, info="transform was successful")

  t2 <- transform(dt, d=c+4, a=sample(c('x', 'y', 'z'), 20, replace=TRUE))
  expect_true(is.null(key(t2)), info="transforming a key column nukes the key")
  
  ## This is probably not necessary, but let's just check that transforming
  ## a key column doesn't twist around the rows in the result.
  for (col in c('b', 'c')) {
    msg <- sprintf("mutating-key-transform maintains other columns [%s]", col)
    expect_equal(t2[[col]], dt[[col]], info=msg)
  }
})
