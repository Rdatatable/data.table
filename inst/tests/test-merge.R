context("Merging")

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

################################################################################
## New merge tests to follow
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
