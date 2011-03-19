
if (!exists(".devtesting")) {
    require(data.table)   # in dev the package should not be loaded
    require(ggplot2)      # the 2 ggplot tests take so long they get in the way
}

nfail = ntest = 0
test = function(num,x,y=NULL) {
    ntest <<- ntest + 1
    if (inherits(err<-try(x,TRUE),"try-error") || inherits(err<-try(y,TRUE),"try-error")) {
        cat("Test",num,err)
        nfail <<- nfail + 1
        return()
    }
    if (missing(y)) {
        if (x) return()
    } else {
        if (identical(x,y)) return()
        if (is.data.table(x) && is.data.table(y)) {
            # drop unused levels in factors
            if (length(x)) for (i in which(sapply(x,is.factor))) x[[i]] = factor(x[[i]])
            if (length(y)) for (i in which(sapply(y,is.factor))) y[[i]] = factor(y[[i]])
            if (length(attr(x,"row.names"))) attr(x,"row.names") = NULL  # for test 165+, i.e. x may have row names set from inheritance but y won't, consider these equal
            if (length(attr(y,"row.names"))) attr(y,"row.names") = NULL
            if (identical(x,y)) return()
        }
        if (is.factor(x) && is.factor(y)) {
            x = factor(x)
            y = factor(y)
            if (identical(x,y)) return()
        }           
    }
    cat("Test",num,"ran without errors but failed check:\n")
    print(x)
    if (is.data.table(x)) print(key(x))
    print(y)
    if (is.data.table(y)) print(key(y))
    nfail <<- nfail + 1
}

started.at = Sys.time()
TESTDT = data.table(a=as.integer(c(1,3,4,4,4,4,7)), b=as.integer(c(5,5,6,6,9,9,2)), v=1:7)
setkey(TESTDT,a,b)
# i.e.       a b v
#       [1,] 1 5 1
#       [2,] 3 5 2
#       [3,] 4 6 3
#       [4,] 4 6 4
#       [5,] 4 9 5
#       [6,] 4 9 6
#       [7,] 7 2 7
INT = function(...) { as.integer(c(...)) }
##########################
    
test(1, TESTDT[SJ(4,6),v,mult="first"], 3L)
test(2, TESTDT[SJ(4,6),v,mult="last"], 4L)
test(3, TESTDT[SJ(c(4,4,4),c(6,6,7)),v,mult="last",roll=TRUE], INT(4,4,4))
test(4, TESTDT[SJ(c(4,4,4),c(9,9,10)),v,mult="last",roll=TRUE], INT(6,6,6))
test(5, TESTDT[SJ(c(4,4,4),c(6,6,7)),v,mult="last",rolltolast=TRUE], INT(4,4,4))
test(6, TESTDT[SJ(c(4,4,4),c(9,9,10)),v,mult="last",rolltolast=TRUE], INT(6,6,NA))
test(7, TESTDT[SJ(c(4,4,4),c(9,9,10)),v,mult="first",rolltolast=TRUE], INT(5,5,NA))
test(8, TESTDT[SJ(c(-9,1,4,4,8),c(1,4,4,10,1)),v]$v, INT(NA,NA,NA,NA,NA))
test(9, TESTDT[SJ(c(-9,1,4,4,8),c(1,4,4,10,1)),v,roll=TRUE]$v, INT(NA,NA,NA,6,NA))
test(10, TESTDT[SJ(c(-9,1,4,4,8),c(1,4,4,10,1)),v,rolltolast=TRUE]$v, INT(NA,NA,NA,NA,NA))
test(11, TESTDT[SJ(c(-3,2,4,4,5,7,8)),v,mult="first"], INT(NA,NA,3,3,NA,7,NA))
test(12, TESTDT[SJ(c(-3,2,4,4,5,7,8)),v,mult="first",roll=TRUE], INT(NA,1,3,3,6,7,7))
test(13, TESTDT[SJ(c(-3,2,4,4,5,7,8)),v,mult="last"], INT(NA,NA,6,6,NA,7,NA))
test(14, TESTDT[SJ(c(-3,2,4,4,5,7,8)),v,mult="last",roll=TRUE], INT(NA,1,6,6,6,7,7))
test(15, TESTDT[SJ(c(-3,2,4,4,5,7,8)),v,mult="last",nomatch=0], INT(6,6,7))
test(16, TESTDT[SJ(c(4)),v][[2]], INT(3,4,5,6))
#test(17, suppressWarnings(TESTDT[SJ(c(4,4)),v,mult="all",incbycols=FALSE][[1]]), INT(3:6,3:6))
test(18, TESTDT[SJ(c(-3,2,4,8)),v,mult="all",nomatch=0][[2]], INT(3:6))
test(185, TESTDT[SJ(c(-3,2,4,8)),v,mult="all",nomatch=NA][[2]], INT(NA,NA,3:6,NA))
test(19, TESTDT[SJ(c(-3,2,4,8)),v,mult="all",roll=TRUE,nomatch=0][[2]], INT(1,3:6,7))
test(186, TESTDT[SJ(c(-3,2,4,8)),v,mult="all",roll=TRUE,nomatch=NA][[2]], INT(NA,1,3:6,7))
test(20, TESTDT[SJ(c(-3,2,4,8)),v,mult="all",rolltolast=TRUE,nomatch=0][[2]], INT(1,3:6))
test(187, TESTDT[SJ(c(-3,2,4,8)),v,mult="all",rolltolast=TRUE,nomatch=NA][[2]], INT(NA,1,3:6,NA))
test(21, TESTDT[SJ(c(-9,1,4,4,4,4,8),c(1,5,5,6,7,10,3)),v,mult="all",nomatch=0][[3]], INT(1,3:4))
test(188, TESTDT[SJ(c(-9,1,4,4,4,4,8),c(1,5,5,6,7,10,3)),v,mult="all",nomatch=NA][[3]], INT(NA,1,NA,3:4,NA,NA,NA))
test(22, TESTDT[SJ(c(-9,1,4,4,4,4,8),c(1,5,5,6,7,10,3)),v,mult="all",roll=TRUE,nomatch=0][[3]], INT(1,3:4,4,6))
test(189, TESTDT[SJ(c(-9,1,4,4,4,4,8),c(1,5,5,6,7,10,3)),v,mult="all",roll=TRUE,nomatch=NA][[3]], INT(NA,1,NA,3:4,4,6,NA))
test(23, TESTDT[SJ(c(-9,1,4,4,4,4,8),c(1,5,5,6,7,10,3)),v,mult="all",rolltolast=TRUE,nomatch=0][[3]], INT(1,3:4,4))
test(190, TESTDT[SJ(c(-9,1,4,4,4,4,8),c(1,5,5,6,7,10,3)),v,mult="all",rolltolast=TRUE,nomatch=NA][[3]], INT(NA,1,NA,3:4,4,NA,NA))
test(24, TESTDT[SJ(c(1,NA,4,NA,NA,4,4),c(5,5,6,6,7,9,10)),v,mult="all",roll=TRUE,nomatch=0][[3]], INT(1,3:4,5:6,6))
test(191, TESTDT[SJ(c(1,NA,4,NA,NA,4,4),c(5,5,6,6,7,9,10)),v,mult="all",roll=TRUE,nomatch=NA][[3]], INT(NA,NA,NA,1,3:4,5:6,6))
# Note that the NAs get sorted to the beginning by the SJ().

# i.e.       a b v      (same test matrix, repeating here for easier reading of the test cases below)
#       [1,] 1 5 1
#       [2,] 3 5 2
#       [3,] 4 6 3
#       [4,] 4 6 4
#       [5,] 4 9 5
#       [6,] 4 9 6
#       [7,] 7 2 7
test(25, TESTDT[SJ(4,6),v,mult="first"], 3L)
test(26, TESTDT[SJ(4,6),v,mult="last"], 4L)
test(27, TESTDT[J(c(4,4,4),c(7,6,6)),v,mult="last",roll=TRUE], INT(4,4,4))
test(28, TESTDT[J(c(4,4,4),c(10,9,9)),v,mult="last",roll=TRUE], INT(6,6,6))
test(29, TESTDT[J(c(4,4,4),c(7,6,6)),v,mult="last",rolltolast=TRUE], INT(4,4,4))
test(30, TESTDT[J(c(4,4,4),c(10,9,9)),v,mult="last",rolltolast=TRUE], INT(NA,6,6))
test(31, TESTDT[J(c(4,4,4),c(10,9,9)),v,mult="first",rolltolast=TRUE], INT(NA,5,5))
test(32, TESTDT[J(c(8,1,4,4,-9),c(1,4,4,10,1)),v]$v, INT(NA,NA,NA,NA,NA))
test(33, TESTDT[J(c(8,1,4,4,-9),c(1,4,4,10,1)),v,roll=TRUE]$v, INT(NA,NA,NA,6,NA))
test(34, TESTDT[J(c(8,1,4,4,-9),c(1,4,7,10,1)),v,rolltolast=TRUE]$v, INT(NA,NA,4,NA,NA))
test(35, TESTDT[J(c(5,4,-3,8,4,7,2)),v,mult="first"], INT(NA,3,NA,NA,3,7,NA))
test(36, TESTDT[J(c(5,4,-3,8,4,7,2)),v,mult="first",roll=TRUE], INT(6,3,NA,7,3,7,1))
test(37, TESTDT[J(c(5,4,-3,8,4,7,2)),v,mult="last"], INT(NA,6,NA,NA,6,7,NA))
test(38, TESTDT[J(c(5,4,-3,8,4,7,2)),v,mult="last",roll=TRUE], INT(6,6,NA,7,6,7,1))
test(39, TESTDT[J(c(5,4,-3,8,4,7,2)),v,mult="last",nomatch=0], INT(6,6,7))
test(40, TESTDT[J(c(4)),v,mult="all"][[2]], INT(3,4,5,6))
test(41, TESTDT[J(c(4,4)),v,mult="all"][[2]], INT(3:6,3:6))
test(42, TESTDT[J(c(8,2,4,-3)),v,mult="all",nomatch=0][[2]], INT(3:6))
test(192, TESTDT[J(c(8,2,4,-3)),v,mult="all",nomatch=NA][[2]], INT(NA,NA,3:6,NA))
test(43, TESTDT[J(c(8,2,4,-3)),v,mult="all",roll=TRUE,nomatch=0][[2]], INT(7,1,3:6))
test(193, TESTDT[J(c(8,2,4,-3)),v,mult="all",roll=TRUE,nomatch=NA][[2]], INT(7,1,3:6,NA))
#test(44, suppressWarnings(TESTDT[J(c(8,4,2,-3)),v,mult="all",rolltolast=TRUE,incbycols=FALSE][[1]]), INT(3:6,1))
test(45, TESTDT[J(c(-9,1,4,4,4,4,8),c(1,5,5,6,7,10,3)),v,mult="all",nomatch=0][[3]], INT(1,3:4))
test(194, TESTDT[J(c(-9,1,4,4,4,4,8),c(1,5,5,6,7,10,3)),v,mult="all",nomatch=NA][[3]], INT(NA,1,NA,3:4,NA,NA,NA))
test(46, TESTDT[J(c(-9,1,4,4,4,4,8),c(1,5,5,6,7,10,3)),v,mult="all",roll=TRUE,nomatch=0][[3]], INT(1,3:4,4,6))
test(195, TESTDT[J(c(-9,1,4,4,4,4,8),c(1,5,5,6,7,10,3)),v,mult="all",roll=TRUE,nomatch=NA][[3]], INT(NA,1,NA,3:4,4,6,NA))
test(47, TESTDT[J(c(-9,1,4,4,4,4,8),c(1,5,5,6,7,10,3)),v,mult="all",rolltolast=TRUE,nomatch=0][[3]], INT(1,3:4,4))
test(196, TESTDT[J(c(-9,1,4,4,4,4,8),c(1,5,5,6,7,10,3)),v,mult="all",rolltolast=TRUE,nomatch=NA][[3]], INT(NA,1,NA,3:4,4,NA,NA))
test(48, TESTDT[J(c(-9,NA,4,NA,1,4,4),c(1,5,9,6,5,9,10)),v,mult="all",roll=TRUE,nomatch=0][[3]], INT(5:6,1,5:6,6))  # this time the NAs stay where they are. Compare to test 24 above.
test(197, TESTDT[J(c(-9,NA,4,NA,1,4,4),c(1,5,9,6,5,9,10)),v,mult="all",roll=TRUE,nomatch=NA][[3]], INT(NA,NA,5:6,NA,1,5:6,6))
test(49, TESTDT[J(c(4,1,0,5,3,7,NA,4,1),c(6,5,1,10,5,2,1,6,NA)),v,nomatch=0]$v, INT(3,4,1,2,7,3,4))
test(198, TESTDT[J(c(4,1,0,5,3,7,NA,4,1),c(6,5,1,10,5,2,1,6,NA)),v,nomatch=NA]$v, INT(3,4,1,NA,NA,2,7,NA,3,4,NA))
test(50, TESTDT[J(c(4,1,0,5,3,7,NA,4,1),c(6,5,1,10,5,2,1,6,NA)),v,mult="last",nomatch=0], INT(4,1,2,7,4))
test(199, TESTDT[J(c(4,1,0,5,3,7,NA,4,1),c(6,5,1,10,5,2,1,6,NA)),v,mult="last",nomatch=NA], INT(4,1,NA,NA,2,7,NA,4,NA))

TESTDT$a = factor(letters[TESTDT$a])
setkey(TESTDT,a,b)
# i.e.       a b v
#       [1,] a 5 1
#       [2,] c 5 2
#       [3,] d 6 3
#       [4,] d 6 4
#       [5,] d 9 5
#       [6,] d 9 6
#       [7,] g 2 7
test(51, TESTDT[SJ(c("d","d","e","g"),c(6,7,1,2)),v,mult="all",roll=TRUE,nomatch=0][[3]], INT(3:4,4,7))  # will test sortedmatch for strings in the level match
test(200, TESTDT[SJ(c("d","d","e","g"),c(6,7,1,2)),v,mult="all",roll=TRUE,nomatch=NA][[3]], INT(3:4,4,NA,7))
test(52, TESTDT[J(c("g","d","e","d"),c(6,6,1,2)),v,mult="all",roll=TRUE,nomatch=0][[3]], INT(7,3:4))  # also will test sortedmatch for strings in the level match
test(201, TESTDT[J(c("g","d","e","d"),c(6,6,1,2)),v,mult="all",roll=TRUE,nomatch=NA][[3]], INT(7,3:4,NA,NA))

TESTDT$b = factor(letters[TESTDT$b])
setkey(TESTDT,a,b)
# i.e.
#         a b v
#    [1,] a e 1
#    [2,] c e 2
#    [3,] d f 3
#    [4,] d f 4
#    [5,] d i 5
#    [6,] d i 6
#    [7,] g b 7
test(53, TESTDT[SJ(c("d","d","e","g"),c("f","g","a","b")),v,mult="last"], INT(4,NA,NA,7))
test(54, TESTDT[J(c("g","d","e","d"),c("b","g","a","f")),v,mult="last"], INT(7,NA,NA,4))  # this tests (d,g) ok even though there is an NA in last match in the roll.
test(55, TESTDT[SJ(c("d","d","e","g"),c("f","g","a","b")),v,mult="first"], INT(3,NA,NA,7))
test(56, TESTDT[J(c("g","d","e","d"),c("b","g","a","f")),v,mult="first"], INT(7,NA,NA,3))
t = try(TESTDT[J(c("g","d","d","d","e","d"),c("b","g","k","b","a","f")),v,roll=TRUE],silent=TRUE)
test(57, inherits(t,"try-error"))  # When character, this works and evaluated to INT(7,4,6,NA,NA,3)
test(58, length(grep("Attempting roll join on factor column", t)))
t = try(TESTDT[J(c("g","d","d","d","e","d"),c("b","g","k","b","a","f")),v,rolltolast=TRUE],silent=TRUE)
test(59, inherits(t,"try-error"))  # When character evaluates to INT(7,4,NA,NA,NA,3)
test(60, length(grep("Attempting roll join on factor column", t)))

##  Add tests on sortedmatch as well to nail it down even further,  even though its called above.
if (!exists("sortedmatch")) sortedmatch = data.table:::sortedmatch        # for eval(body(test.data.table)) when data.table is in namespace only and isn't in development in .GlobalEnv
test(61, sortedmatch(c("a","h","f","h","j"), letters[1:8]), INT(1,8,6,8,NA))
test(62, sortedmatch(INT(5,2,4,3,NA,7), INT(2,4,5,7)), INT(3,1,2,NA,NA,4))
test(63, sortedmatch(1:3,INT(NULL)), INT(NA,NA,NA))

t = try(sortedmatch(c("a","h","f","j"), letters[8:1], check=TRUE), silent=TRUE)
test(64, inherits(t,"try-error"))   # v2 is not sorted
t = try(sortedmatch(letters[1:3], 1:3), silent=TRUE)
test(65, inherits(t,"try-error"))   # v1 and v2 must be the same storage.mode
t = try(sortedmatch(as.double(1:3), as.double(1:3)), silent=TRUE)
test(66, inherits(t,"try-error"))   # only character or integer accepted

# Test 67 removed. No longer use factors so debate/problem avoided.
# [.factor and c.factor are no longer present in data.table, not even hidden away
# X = factor(letters[1:10])
# test(67, levels(X[4:6]), letters[4:6])

test(68, "TESTDT" %in% tables(silent=TRUE)[,NAME])  # NAME is returned as a column in which we look for the string 
test(69, "TESTDT" %in% tables(silent=TRUE)[,as.character(NAME)]) # an old test (from when NAME was factor) but no harm in keeping it

a = "d"
# Variable Twister.  a in this scope has same name as a inside DT scope.
# Aug 2010 : As a result of bug 1005, and consistency with 'j' and 'by' we now allow self joins (test 183) in 'i'.
test(70, TESTDT[eval(J(a)),v], data.table(a="d",v=3:6))   # the eval() enabled you to use the 'a' in the calling scope, not 'a' in the TESTDT
test(71, TESTDT[eval(SJ(a)),v], data.table(a="d",v=3:6,key="a"))
test(72, TESTDT[eval(CJ(a)),v], data.table(a="d",v=3:6,key="a"))

test(73, TESTDT[,v], 1:7)
test(74, TESTDT[,3], 3)
test(74.5, TESTDT[,3L], 3L)
test(75, TESTDT[,"v"], "v")
test(76, TESTDT[,2:3], 2:3)  # See ?[.data.table that with=FALSE is required for the likely intended result
test(77, TESTDT[,2:3,with=FALSE], data.table(b=c("e","e","f","f","i","i","b"),v=1:7))
test(78, TESTDT[,c("b","v"),with=FALSE], data.table(b=c("e","e","f","f","i","i","b"),v=1:7))
colsVar = c("b","v")
test(79, TESTDT[,colsVar], colsVar)
test(80, TESTDT[,colsVar,with=FALSE], data.table(b=c("e","e","f","f","i","i","b"),v=1:7))

# works in test.data.table, but not eval(body(test.data.table)) when in R CMD check ... test(81, TESTDT[1:2,c(a,b)], factor(c("a","c","e","e")))
# It is expected the above to be common source of confusion. c(a,b) is evaluated within
# the frame of TESTDT, and c() creates one vector, not 2 column subset as in data.frame's.
# If 2 columns were required use list(a,b).  c() can be useful too, but is different.

test(82, TESTDT[,c("a","b")], c("a","b"))
test(83, TESTDT[,list("a","b")], data.table("a","b"))
# test(84, TESTDT[1:2,list(a,b)], list(c("a","c"), c("e","e")))  # should be a data.table
# test(85, TESTDT[1:2,DT(a,b)], data.table(a=c("a","c"), b=c("e","e")))  #DT now deprecated

test(86, TESTDT[,sum(v),by="b"], data.table(b=c("b","e","f","i"),V1=INT(7,3,7,11)))  # TESTDT is key'd by a,b, so correct that grouping by b should not be key'd in the result by default
test(87, TESTDT[,DT(MySum=sum(v)),by="b"], data.table(b=c("b","e","f","i"),MySum=INT(7,3,7,11)))
test(88, TESTDT[,DT(MySum=sum(v),Sq=v*v),by="b"][1:2], data.table(b=c("b","e"),MySum=INT(7,3),Sq=INT(49,1))) # silent repetition of MySum to match the v*v vector
# Test 89 dropped. Simplify argument no longer exists. by is now fast and always returns a data.table  ... test(89, TESTDT[,sum(v),by="b",simplify=FALSE], list(7L,3L,7L,11L))

setkey(TESTDT,b)
test(90, TESTDT[J(c("f","i")),sum(v),mult="all"], data.table(b=c("f","i"),V1=c(7L,11L)))  # aggregation via groups passed into i and mult="all"
test(91, TESTDT[SJ(c("f","i")),sum(v),mult="all"], data.table(b=c("f","i"),V1=c(7L,11L),key="b"))  # aggregation via groups passed into i and mult="all"
# Test 92 dropped same reason as 89 ... test(TESTDT[92, J(c("f","i")),sum(v),mult="all",simplify=FALSE], list(7L,11L))

test(93, TESTDT[c("f","i"), which=TRUE], 4:7)
test(94, TESTDT[c("i","f"), mult="last", which=TRUE], INT(7,5))

test(95, TESTDT["f",v]$v, 3:4)
test(96, TESTDT["f",v,mult="all"], data.table(b="f",v=3:4))
test(97, TESTDT[c("f","i","b"),DT(GroupSum=sum(v)),mult="all"], data.table(b=c("f","i","b"), GroupSum=c(7L,11L,7L)))  # mult="all" is required here since only b is key'd
# that line above doesn't create a key on the result so that the order fib is preserved.
test(98, TESTDT[SJ(c("f","i","b")),DT(GroupSum=sum(v)),mult="all"], data.table(b=c("b","f","i"), GroupSum=c(7L,7L,11L), key="b"))
# line above is the way to group, sort by group and setkey on the result by group.

(dt <- data.table(A = rep(1:3, each=4), B = rep(1:4, each=3), C = rep(1:2, 6), key = "A,B"))
test(99, unique(dt), data.table(dt[c(1L, 4L, 5L, 7L, 9L, 10L)], key="A,B"))

# test [<- for column assignment 
dt1 <- dt2 <- dt
test(100, {dt1[,"A"] <- 3; dt1}, {dt2$A <- 3; dt2})

# test transform and within
test(101, within(dt, {D <- B^2}), transform(dt, D = B^2))
test(102, within(dt, {A <- B^2}), transform(dt, A = B^2))

# test .SD object
test(103, dt[, sum(.SD$B), by = "A"], dt[, sum(B), by = "A"])
test(104, dt[, transform(.SD, D = min(B)), by = "A"], dt[, DT(B,C,D=min(B)), by = "A"])

# test numeric and comparison operations on a data table
test(105, all(dt + dt > dt))
test(106, all(dt + dt > 1))
test(107, dt + dt, dt * 2L)

# test a few other generics:
test(108, dt, data.table(t(t(dt)), key="A,B"))
test(109, all(!is.na(dt)))
dt2 <- dt
dt2$A[1] <- NA
test(110, sum(is.na(dt2)), 1L)
test(111, dt, na.omit(dt))
test(112, dt2[2:nrow(dt2),A], na.omit(dt2)$A)

# test [<- assignment:
dt2[is.na(dt2)] <- 1L
setkey(dt2, A, B)
test(113, dt, dt2)
dt2[, c("A", "B")] <- dt1[, c("A", "B"), with = FALSE]
test(114, dt1, dt2)
## doesn't work, yet:
##     dt2[rep(TRUE, nrow(dt)), c("A", "B")] <- dt1[, c("A", "B"), with = FALSE]
##     dt2[rep(TRUE, nrow(dt)), c("A")] <- dt1[, c("A"), with = FALSE]
##     test(dt, dt2))  stop("Test 112 failed")

# test the alternate form of setkey:
dt1 <- dt2 <- dt
setkey(dt1, "A")
setkey("dt2", "A")
test(115, dt1, dt2)

# Test dogroups works correctly for character/factor columns
test(116, TESTDT[,a[1],by="b"], data.table(b=c("b","e","f","i"), V1=c("g","a","d","d"), key="b"))
test(117, TESTDT[,list(a[1],v[1]),by="b"], data.table(b=c("b","e","f","i"), V1=c("g","a","d","d"), V2=INT(7,1,3,5), key="b"))

# We no longer check i for out of bounds, for consistency with data.frame. NA rows should be returned for i>nrow
test(118, TESTDT[8], data.table(a=as.character(NA), b=as.character(NA), v=as.integer(NA), key="b"))
test(119, TESTDT[6:9], data.table(a=c("d","d",NA,NA), b=c("i","i",NA,NA), v=c(5L,6L,NA,NA)))

n=10000
grp1=sample(1:50,n,replace=TRUE)
grp2=sample(1:50,n,replace=TRUE)
dt=data.table(x=rnorm(n),y=rnorm(n),grp1=grp1,grp2=grp2)
tt = system.time({ans = dt[,list(.Internal(mean(x)),.Internal(mean(y))),by="grp1,grp2"]})
test(120, tt[1] < 0.5)   # actually takes more like 0.068 << 0.5
i = sample(nrow(ans),1)
test(121, ans[i,c(V1,V2)], dt[grp1==ans[i,grp1] & grp2==ans[i,grp2], c(mean(x),mean(y))])
# To DO: add a data.frame aggregate method here and check data.table is faster

# Tests of 0 and 1 row tables
TESTDT = data.table(NULL)
test(122, TESTDT[1], TESTDT)
test(123, TESTDT[0], TESTDT)
test(124, TESTDT[1:10], TESTDT)
t = try(TESTDT["k"], silent=TRUE)
test(125, inherits(t,"try-error"))
test(126, length(grep("The data.table has no key", t)))

TESTDT = data.table(a=3L,v=2L,key="a")  # testing 1-row table
test(127, TESTDT[J(3)], TESTDT)
test(128, TESTDT[J(4)], data.table(a=4L,v=NA_integer_,key="a"))   # see tests 206-207 too re the [NA]
test(129, TESTDT[J(4),roll=TRUE], data.table(a=4L,v=2L,key="a"))  # the i values are in the result now (which make more sense for rolling joins, the x.a can still be accessed if need be)
test(130, TESTDT[J(4),rolltolast=TRUE], data.table(a=4L,v=NA_integer_,key="a"))
test(131, TESTDT[J(-4),roll=TRUE], data.table(a=-4L,v=NA_integer_,key="a"))

test(132, ncol(TESTDT[0]), 2L)
test(133, TESTDT[0][J(3)], data.table(a=3L,v=NA_integer_,key="a"))

# tests on data table names
x = 2L; `1x` = 4L
dt = data.table(a.1 = 1L, b_1 = 2L, "1b" = 3L, `a 1` = 4L, x, `1x`, 2*x) 
test(134, names(dt), c("a.1", "b_1", "X1b", "a.1.1", "x", "V6", "V7"))

dt = data.table(a.1 = 1L, b_1 = 2L, "1b" = 3L, `a 1` = 4L, x, `1x`, 2*x, check.names = FALSE)    
test(135, names(dt), c("a.1", "b_1", "1b", "a 1", "x", "V6", "V7")) # the last two terms differ from data.frame()

test(136, dt[,b_1, by="a.1"], data.table(a.1=1L,"b_1"=2L))
test(137, dt[,`a 1`, by="a.1"], data.table(a.1=1L,"a 1"=4L, check.names=FALSE))
test(138, dt[,a.1, by="`a 1`"], data.table(`a 1`=4L,a.1=1L, check.names=FALSE))     

# tests with NA's in factors
dt = data.table(a = c(NA, letters[1:5]), b = 1:6)
test(139, dt[,sum(b), by="a"], data.table(a = c(NA, letters[1:5]), V1 = 1:6))     

# tests to make sure rbind and grouping keep classes
dt = data.table(a = rep(as.Date("2010-01-01"), 4), b = rep("a",4))
test(140, rbind(dt,dt), data.table(a = rep(as.Date("2010-01-01"), 8), b = rep("a",8)))
test(141, dt[,list(a=a), by="b"], dt[,2:1, with = FALSE])

dt$a <- structure(as.integer(dt$a), class = "Date")
test(142, dt[,list(b=b), by="a"], dt)

dt = data.table(x=1:5,y=6:10)
test(143, tail(dt), dt)  # tail was failing if a column name was called x.

dt <- data.table(a = rep(1:3, each = 4), b = LETTERS[1:4], b2 = LETTERS[1:4])
test(144, dt[, .SD[3,], by=b], data.table(b=LETTERS[1:4],a=3L,b2=LETTERS[1:4]))

DT = data.table(x=rep(c("a","b"),c(2,3)),y=1:5)
xx = capture.output(ans <- DT[,{print(x);sum(y)},by=x])
test(145, xx, c("[1] a a","Levels: a b","[1] b b b","Levels: a b"))
test(146, ans, data.table(x=c("a","b"),V1=c(3L,12L)))

tt = try(DT[,MySum=sum(v)], silent=TRUE)    # feature request #204 done.
test(147, inherits(t,"try-error") && length(grep("unused argument", tt)))   # user meant DT[,list(MySum=sum(v))]

dt = data.table(a=c(1L,4L,5L), b=1:3, key="a")
test(148, dt[CJ(2:3),roll=TRUE], data.table(a=c(2L,3L),b=c(1L,1L),key="a"))
test(149, dt[J(2:3),roll=TRUE], data.table(a=c(2L,3L),b=c(1L,1L)))  # in future this will detect the subset is ordered and retain the key

# 150:158 test out of order factor levels in key columns
dt = data.table(x=factor(c("c","b","a"),levels=c("b","a","c")),y=1:3)
key(dt) = "x"
test(150, dt["b",y]$y, 2L)
# from Tom's post :
a = data.table(a=rep(1:5, 2), b=factor(letters[rep(1:5, each =2)], levels=letters[5:1]), key="b")  
test(151, a[J("b"),a]$a, 3:4)
# stretch tests further, two out of order levels, one gets key'd the other not :
a = data.table(x=factor(letters[rep(1:5, each =2)], levels=letters[5:1]),
               y=factor(letters[rep(c(6,9,7,10,8), each =2)], levels=letters[10:6]),
               z=1:10)
test(152, is.unsorted(levels(a$x)), TRUE)
test(153, is.unsorted(levels(a$y)), TRUE)
test(154, a[,sum(z),by=x][1,paste(x,V1)], "e 19")
before = a[,sum(z),by=y]
setkey(a,x)
test(155, is.unsorted(levels(a$x)), FALSE)
test(156, is.unsorted(levels(a$y)), TRUE)   # non-key columns are ok to have unsorted levels
test(157, a[,sum(z),by=x][1,paste(x,V1)], "a 3")
test(158, a[,sum(z),by=y], before)

# tests of by expression variables
DT = data.table( a=1:5, b=11:50, d=c("A","B","C","D"), f=1:5, grp=1:5 )
f = quote( list(d) )
test(159, DT[,mean(b),by=eval(f)], DT[,mean(b),by=list(d)])  # column f doesn't get in the way of expression f
foo = function( grp ) {
   DT[,mean(b),by=eval(grp)]
}
test(160, foo(quote(list(d))), DT[,mean(b),by=list(d)])
test(161, foo(quote(list(d,a))), DT[,mean(b),by=list(d,a)])
test(162, foo(quote(list(f))), DT[,mean(b),by=list(f)])
test(163, foo(quote(list(grp))), DT[,mean(b),by=list(grp)])  # grp local variable in foo doesn't conflict with column grp
test(164, foo(f), DT[,mean(b),by=d])

# checks that data.table inherits methods from data.frame in base ok
test(165, subset(DT,a>2), DT[a>2])
test(166, suppressWarnings(split(DT,DT$grp)[[2]]), DT[grp==2])
if ("package:ggplot2" %in% search()) {
    test(167,print(ggplot(DT,aes(b,f))+geom_point()),NULL)  # how to programmatically test it not only doesn't error but correct output, binary diff to pre-prepared pdf ?
    test(168,DT[,print(ggplot(.SD,aes(b,f))+geom_point()),by=list(grp%%2L)],data.table(grp=integer()))  # %%2 because there are 5 groups in DT data at this stage, just need 2 to test
    try(graphics.off(),silent=TRUE)
    #try(graphics.off(),silent=TRUE) # R CMD check doesn't like graphics it seems, even when inside try()
} else {
    cat("Tests 167 and 168 not run. If required call library(ggplot2) first.\n")
    # ggplot takes a long time e.g. increases runtime of test.data.table from under 1 second to over 10 seconds. So we don't include these by default.
    # From examples, the library(ggplot2) is done first, so that 'R CMD check' does include tests 167 and 168 
}
# test of . in formula, using inheritance
DT = data.table(y=1:100,x=101:200,y=201:300,grp=1:5)
test(169,DT[,as.list(lm(y~0+.,.SD)$coef),by=grp][2,x]-2<1e-10, TRUE)

DT <- data.table( a=1:4, d=c("A","B","C","D") )
g <- quote( list( d ) )
test(170, DT[,list(d)], DT[,eval(g)])

DT = data.table(A=c(25L,85L,25L,25L,85L), B=c("a","a","b","c","c"), C=c(2,65,9,82,823))
test(171, DT[ , data.table( A, C )[ A==25, C ] + data.table( A, C )[ A==85, C ], by=B ], data.table(B=c("a","c"),V1=c(67,905)))

test(172, DT[ , list(3,data.table( A, C )[ A==25, C ] + data.table( A, C )[ A==85, C ]), by=B ], data.table(B=c("a","b","c"),V1=3,V2=c(67,NA,905)))

# Test growing result in memory. Usually the guess is good though.
# This example returns no rows for first group so guess for up-front allocate needs a reallocate
DT = data.table(A=c(1L,1L,2L,2L,3L,3L), B=1:6)
test(173, DT[,B[B>3],by=A][,V1], c(4L,5L,6L))

# Example taken from Harish post to datatable-help on 11 July
DT <- data.table(
     A=c("a","a","b","b","d","c","a","d"),
     B=c("x1","x2","x2","x1","x2","x1","x1","x2"),
     C=c(5,2,3,4,9,5,1,9)
     )
test(174, DT[,C[C-min(C)<3],by=list(A,B)][,V1], c(1,2,4,3,5,9,9))
test(175, DT[,C[C-min(C)<5],by=list(A,B)][,V1], c(5,1,2,4,3,5,9,9))

# Tests of data.table sub-assignments: $<-.data.table & [<-.data.table
DT <- data.table(a = c("A", "Z"), b = 1:10, key = "a")
DT[J("A"),2] <- 100
DT[J("A"),"b"] <- 1:5
DT[1:3,"b"] <- 33
test(176, DT,  data.table(a = rep(c("A", "Z"), each = 5),
                          b = c(rep(33, 3), 4:5, seq(2, 10, by = 2)),
                          key = "a"))
DT[J("A"),"a"] <- "Z"
test(177, key(DT), NULL )

DT <- data.table(a = c("A", "Z"), b = 1:10, key = "a")
DT$b[1:5] <- 1:5
DT$b[1:3] <- 33
test(178, DT,  data.table(a = rep(c("A", "Z"), each = 5),
                          b = c(rep(33, 3), 4:5, seq(2, 10, by = 2)),
                          key = "a"))
DT$a <- 10:1
test(179, key(DT), NULL )

# Test logical in a key
DT = data.table(a=rep(1:3,each=2),b=c(TRUE,FALSE),v=1:6)
setkey(DT,a,b)
test(180, DT[J(2,FALSE),v]$v, 4L)
test(181, DT[,sum(v),by=b][,V1], c(12L,9L))

# Test fix for bug 1026 reported by Harish V
# this test needed a unique var name to generate error 'object 'b' not found'.
# Otherwise it finds 'b' in local scope.
colnames(DT)[2] = "buniquename314"   
bar = function( data, fcn ) {
    q = substitute( fcn )
    xx = data[,eval(q),by=a]
    yy = data[,eval(substitute(fcn)),by=a]
    identical(xx,yy)
}
test(182, bar( DT, sum(buniquename314) ), TRUE)

# Test bug 1005 reported by Branson Owen
DT = data.table(A = c("o", "x"), B = 1:10, key = "A")
test(183, DT[J(unique(A)), B]$B, DT$B)

# Test bug 709
xx = data.table(a=1:5,b=6:10)
test(184, xx[a>6,sum(b),by=a], 0L)   # aside: consistent with sum(NULL)==0

# Tests of bug 1015 highlight by Harish
# See thread "'by without by' now heeds nomatch=NA"
# Tests 185-201 were added in above next to originals
x <- data.table(a=c("a","b","d","e"),b=c("A","A","B","B"),d=c(1,2,3,4), key="a,b")
y <- data.table(g=c("a","b","c","d"),h=c("A","A","A","A"))
test(202, x[y], x[y,mult="all"])
test(203, x[y,d]$d, c(1,2,NA,NA))
test(204, x[y,list(d)], x[y,d])
test(205, x[y,list(d),mult="all"][,d], c(1,2,NA,NA))

# Test [NA] returns one NA row. NA is type *logical* so prior to
# change in v1.5, NA would get silently recycled and the whole table would
# be returned all NA (rarely useful and often confusing, but consistent
# with data.frame).
TESTDT = data.table(a=1:3,v=1:3,key="a")
test(206, TESTDT[NA], data.table(a=NA_integer_,v=NA_integer_,key="a"))
key(TESTDT) = NULL
test(207, TESTDT[NA], data.table(a=NA_integer_,v=NA_integer_))

# With inheritance, NROW and NCOL in base work nicely. No need for them in data.table.
test(208, NROW(TESTDT), 3L)
test(209, nrow(TESTDT), 3L)
test(210, NCOL(TESTDT), 2L)
test(211, ncol(TESTDT), 2L)

# Test infinite recursion error is trapped when a pre-1.5 data.table
# is used with 1.5 (bug #1008)
DT = data.table(a=1:6,key="a")
test(212, DT[J(3)]$a, 3L) # correct class c("data.table","data.frame")
class(DT) = "data.table"  # incorrect class
tt = try(DT[J(3)]$a, silent=TRUE)
test(213, inherits(tt,"try-error"))
test(214, length(grep("data.table inherits from data.frame", tt)))

# setkey now auto coerces double and character for convenience, and
# to solve bug #953
DF = data.frame(a=LETTERS[1:10], b=1:10, stringsAsFactors=FALSE)
DT = data.table(DF)
key(DT) = 'a'   # used to complain about character
test(215, DT["C",b]$b, 3L)
DT = data.table(DF,key="a")
test(216, DT["C",b]$b, 3L)
DT = data.table(a=c(1,2,3),v=1:3,key="a")
test(217, DT[J(2),v]$v, 2L)
DT = data.table(a=c(1,2.1,3),v=1:3)
tt = try(setkey(DT,a), silent=TRUE)
test(218, inherits(tt,"try-error"))
test(219, length(grep("losing information", tt)))

# tests of quote()-ed expressions in i. Bug #1058
DT = data.table(a=1:5,b=6:10,key="a")
q = quote(a>3)
test(220, DT[eval(q),b], 9:10)
test(221, DT[eval(parse(text="a>4")),b], 10L)
test(222, DT[eval(parse(text="J(2)")),b]$b, 7L)

# lists in calling scope should be ok as single names passed to by, bug #1060
DT = data.table(a=1:2,b=rnorm(10))
byfact = DT[,a]   # vector, ok before fix but check anyway
test(223, DT[,mean(b),by=byfact], DT[,mean(b),by=list(byfact)])
byfact = DT[,list(a)]  # this caused next line to fail before fix
test(224, DT[,mean(b),by=byfact], DT[,mean(b),by=as.list(byfact)])
test(225, DT[,mean(b),by=byfact], DT[,mean(b),by={byfact}])

# tests for building expressions via parse, bug #1243
dt1key<-data.table(A1=1:100,onekey=rep(1:2,each=50))
setkey(dt1key,onekey)
ASumExpr<-parse(text="quote(sum(A1))") # no need for quote but we test it anyway because that was work around when test 227 failed
ASumExprNoQ<-parse(text="sum(A1)")
ans = dt1key[,sum(A1),by=onekey]
test(226,ans,dt1key[,eval(eval(ASumExpr)),by=onekey])
test(227,ans,dt1key[,eval(ASumExprNoQ),by=onekey])

# test for uncommon grouping pattern on 1-row data.table, bug #1245
DT = data.table(a=1L,b=2L)
test(228,DT[,list(1:2),by=a],data.table(a=c(1L,1L),V1=1:2))

# special case j=.SD, bug #1247
DT = data.table(a=rep(1:2,each=2),b=1:4)
test(229,DT[,.SD,by=a],DT)
setkey(DT,a)
test(229.1,DT[,.SD,by=key(DT)],DT)

# merge bug with column 'x', bug #1229
d1 <- data.table(x=c(1,3,8),y1=rnorm(3), key="x")
d2 <- data.table(x=c(3,8,10),y2=rnorm(3), key="x")
ans1=merge(d1, d2, by="x")
ans2=cbind(d1[2:3],y2=d2[1:2]$y2);setkey(ans2,x)
test(230, ans1, ans2)

# one column merge, bug #1241
DT = data.table(a=rep(1:2,each=3),b=1:6,key="a")
y = J(a=c(0,1),bb=c(10,11),key="a")
test(231,merge(y,DT),data.table(a=1L,bb=11L,b=1:3,key="a"))
test(232,merge(y,DT,all=TRUE),data.table(a=rep(c(0L,1L,2L),c(1,3,3)),bb=rep(c(10L,11L,NA_integer_),c(1,3,3)),b=c(NA_integer_,1:6),key="a"))
y<-J(a=c(0,1),key="a") # y with only a key column
test(233,merge(y,DT),data.table(a=1L,b=1:3,key="a"))
test(234,merge(y,DT,all=TRUE),data.table(a=rep(c(0L,1L,2L),c(1,3,3)),b=c(NA_integer_,1:6),key="a"))

# 'by' when DT contains list columns
DT = data.table(a=c(1,1,2,3,3),key="a")
DT$b=list(1:2,1:3,1:4,1:5,1:6)
test(235,DT[,mean(unlist(b)),by=a],data.table(a=1:3,V1=c(1.8,2.5,mean(c(1:5,1:6))),key="a"))
test(236,DT[,sapply(b,mean),by=a],data.table(a=c(1,1,2,3,3),V1=c(1.5,2.0,2.5,3.0,3.5),key="a"))

# when i is a single name, it no longer evaluates within data.table scope
DT = data.table(a=1:5,b=rnorm(5),key="a")
a = J(4)
test(237,DT[a],DT[J(4)])

# repeat earlier test with xkey instead of x. xkey is internal to merge; the bigger problem Tom mentioned.
d1 <- data.table(xkey=c(1,3,8),y1=rnorm(3), key="xkey")
d2 <- data.table(xkey=c(3,8,10),y2=rnorm(3), key="xkey")
ans2=cbind(d1[2:3],y2=d2[1:2]$y2);setkey(ans2,xkey)
test(238, merge(d1, d2, by="xkey"), ans2)

# Join Inherited Scope, and X[Y] including Y's non-join columns
X=data.table(a=rep(1:3,c(3,3,2)),foo=1:8,key="a")
Y=data.table(a=2:3,bar=6:7)
test(239, X[Y,sum(foo)], data.table(a=2:3,V1=c(15L,15L)))
test(240, X[Y,sum(foo*bar)], data.table(a=2:3,V1=c(90L,105L)))
test(241, X[Y], data.table(a=rep(2:3,3:2),foo=4:8,bar=rep(6:7,3:2)))
test(242, X[Y,list(foo,bar)][,sum(foo*bar)], 195L)
test(243, X[Y][,sum(foo*bar)], 195L)
# not sure about these yet :
# test(244, X[Y,sum(foo*bar),mult="first"], data.table(a=2:3,V1=c(24L,49L)))
# test(245, X[Y,sum(foo*bar),mult="last"], data.table(a=2:3,V1=c(36L,56L)))

# joining to less than all X's key colums (in examples but can't see formal test)
X=data.table(a=rep(LETTERS[1:2],2:3),b=1:5,v=10:14,key="a,b")
test(246, X["A"], {tt=X[1:2];key(tt)=key(X);tt})  # key will be retained in future
test(247, X["C"]$v, NA_integer_)
test(248, nrow(X["C",nomatch=0]), 0L)

x=data.table( a=c("a","b","c"), b=1:3, key="a" )
y=data.table( a=c("b","d","e"), d=c(8,9,10) )
test(249, x[y], data.table(a=c("b","d","e"),b=c(2L,NA,NA),d=c(8,9,10)))  # keeps i join cols
test(250, x[y,mult="first"], data.table(a=c("b","d","e"),b=c(2L,NA,NA),d=c(8,9,10))) # same

x=data.table( a=c("a","b","b","c"), b=1:4, key="a" )
y=data.table(a=c("b","d","b"), d=c(8,9,10))
test(251, x[y], data.table(a=c("b","b","d","b","b"),b=c(2:3,NA,2:3),d=c(8,8,9,10,10)))

# auto coerce float to int in ad hoc by (just like setkey), FR#1051
DT = data.table(a=c(1,1,1,2,2),v=1:5)
test(252, DT[,sum(v),by=a], data.table(a=1:2,V1=c(6L,9L)))

# auto coerce character to factor in ad hoc by (just like setkey).
DT = data.table(a=c("A","A","A","B","B"),v=1:5)
DT$a = as.character(DT$a)  # because data.table() coerces to factor currently.
test(253, DT[,sum(v),by=a], data.table(a=factor(c("A","B")),V1=c(6L,9L)))

# fix for bug #1298 with by=key(DT) and divisibility error.
DT=data.table(a=c(1,1,1,2,2),b=1:5,key="a")
test(254, DT[,sum(b),by=key(DT)]$V1, c(6L,9L))

## Test that suffixes argument in merge.data.table acts the same way it does
## in merge.data.frame
# d1 <- data.table(a=sample(letters, 10), b=sample(1:100, 10), key='a')
# d2 <- data.table(a=d1$a, b=sample(1:50, 10), c=rnorm(10), key='a')
# dtm <- merge(d1, d2, by='a', suffixes=c(".x", ".y"))
# dfm <- merge(as.data.frame(d1), as.data.frame(d2), by='a', suffixes=c('.x', '.y'))
# test(255, as.data.frame(dtm), dfm)


## Test S4-isms.
## This copied over from Steve's file on github; used to test the locked binding issue
## Steve - over to you on this bit. Maybe S4 should have it's own test file.
## The locked binding error is gone, but R CMD check gives me :
##   Found the following significant warnings:
##   Warning: undefined slot classes in definition of "S4Composition": info(class "data.table")

## A class with a data.table slot
# setClass("S4Composition", representation(info="data.table"))
# 
# DF = data.frame(a=sample(letters, 10), b=1:10)
# DT = as.data.table(DF)
# test(256, as(DF, 'data.table'), DT)
# test(257, as(DT, 'data.frame'), DF)
# dt.comp <- new("S4Composition", info=DT)
# test(258, dt.comp@info, DT)
    
# I put the setClass into AllS4.R, wasn't sure if that was right thing to do.
# setClass("S4Composition", representation(info="data.table"))
# dt.comp <- new("S4Composition", info=DT)
# test(259, dt.comp@info, DT)
## 
## Test S4 method dispatching (?)
## defining generic functions in here cuases errors in testing
##
# setGeneric("dtGet", function(x, what) standardGeneric("dtGet"))
# setMethod("dtGet", c(x="S4Composition", what="missing"),
# function(x, what) {
#     x@info
# })
# setMethod("dtGet", c(x="S4Composition", what="ANY"),
# function(x, what) {
#    x@info[[what]]
# })
# test(260, dtGet(dt.comp), DT)
# test(261, dtGet(dt.comp, 1), DT[[1]])
# test(262, dtGet(dt.comp, 'b'), DT$b)



##########################
if (nfail > 0) {
    stop(nfail," errors in test.data.table()")
    # important to stop here, so than 'R CMD check' fails
}
cat("All",ntest,"tests in test.data.table() completed ok in",timetaken(started.at),"\n")


