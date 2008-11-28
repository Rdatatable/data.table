test.data.table = function()
{
    # TODO: Move this function to RUnit.
    
    started.at = Sys.time()
    TESTDT = data.table(a=as.integer(c(1,3,4,4,4,4,7)), b=as.integer(c(5,5,6,6,9,9,2)), v=1:7)
    a=b=v=NAME=NA    # Otherwise R CMD check warns "no visible binding for global variable" on line below. setkey 'sees' the variables within the scope of its first argument, which R CMD check doesn't know.
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
    if (TESTDT[SJ(4,6),v,mult="first"] != 3) stop("Test 1 failed")
    if (TESTDT[SJ(4,6),v,mult="last"] != 4) stop("Test 2 failed")
    if (!identical(TESTDT[SJ(c(4,4,4),c(6,6,7)),v,mult="last",roll=TRUE], INT(4,4,4))) stop("Test 3 failed")
    if (!identical(TESTDT[SJ(c(4,4,4),c(9,9,10)),v,mult="last",roll=TRUE], INT(6,6,6))) stop("Test 4 failed")
    if (!identical(TESTDT[SJ(c(4,4,4),c(6,6,7)),v,mult="last",rolltolast=TRUE], INT(4,4,4))) stop("Test 5 failed")
    if (!identical(TESTDT[SJ(c(4,4,4),c(9,9,10)),v,mult="last",rolltolast=TRUE], INT(6,6,NA))) stop("Test 6 failed")
    if (!identical(TESTDT[SJ(c(4,4,4),c(9,9,10)),v,mult="first",rolltolast=TRUE], INT(5,5,NA))) stop("Test 7 failed")
    if (!identical(TESTDT[SJ(c(-9,1,4,4,8),c(1,4,4,10,1)),v], INT(NA,NA,NA,NA,NA))) stop("Test 8 failed")
    if (!identical(TESTDT[SJ(c(-9,1,4,4,8),c(1,4,4,10,1)),v,roll=TRUE], INT(NA,NA,NA,6,NA))) stop("Test 9 failed")
    if (!identical(TESTDT[SJ(c(-9,1,4,4,8),c(1,4,4,10,1)),v,rolltolast=TRUE], INT(NA,NA,NA,NA,NA))) stop("Test 10 failed")
    if (!identical(TESTDT[SJ(c(-3,2,4,4,5,7,8)),v,mult="first"], INT(NA,NA,3,3,NA,7,NA))) stop("Test 11 failed")
    if (!identical(TESTDT[SJ(c(-3,2,4,4,5,7,8)),v,mult="first",roll=TRUE], INT(NA,1,3,3,6,7,7))) stop("Test 12 failed")
    if (!identical(TESTDT[SJ(c(-3,2,4,4,5,7,8)),v,mult="last"], INT(NA,NA,6,6,NA,7,NA))) stop("Test 13 failed")
    if (!identical(TESTDT[SJ(c(-3,2,4,4,5,7,8)),v,mult="last",roll=TRUE], INT(NA,1,6,6,6,7,7))) stop("Test 14 failed")
    if (!identical(TESTDT[SJ(c(-3,2,4,4,5,7,8)),v,mult="last",nomatch=0], INT(6,6,7))) stop("Test 15 failed")
    if (!identical(TESTDT[SJ(c(4)),v][[2]], INT(3,4,5,6))) stop("Test 16 failed")
    if (!identical(suppressWarnings(TESTDT[SJ(c(4,4)),v,mult="all",incbycols=FALSE][[1]]), INT(3:6,3:6))) stop("Test 17 failed")
    if (!identical(TESTDT[SJ(c(-3,2,4,8)),v,mult="all"][[2]], INT(3:6))) stop("Test 18 failed")
    if (!identical(TESTDT[SJ(c(-3,2,4,8)),v,mult="all",roll=TRUE][[2]], INT(1,3:6,7))) stop("Test 19 failed")
    if (!identical(TESTDT[SJ(c(-3,2,4,8)),v,mult="all",rolltolast=TRUE][[2]], INT(1,3:6))) stop("Test 20 failed")
    if (!identical(TESTDT[SJ(c(-9,1,4,4,4,4,8),c(1,5,5,6,7,10,3)),v,mult="all"][[3]], INT(1,3:4))) stop("Test 21 failed")
    if (!identical(TESTDT[SJ(c(-9,1,4,4,4,4,8),c(1,5,5,6,7,10,3)),v,mult="all",roll=TRUE][[3]], INT(1,3:4,4,6))) stop("Test 22 failed")
    if (!identical(TESTDT[SJ(c(-9,1,4,4,4,4,8),c(1,5,5,6,7,10,3)),v,mult="all",rolltolast=TRUE][[3]], INT(1,3:4,4))) stop("Test 23 failed")
    if (!identical(TESTDT[SJ(c(1,NA,4,NA,NA,4,4),c(5,5,6,6,7,9,10)),v,mult="all",roll=TRUE][[3]], INT(1,3:4,5:6,6))) stop("Test 24 failed")  # The SJ sorted the NAs in the key to the end
    # Note that the NAs get sorted to the beginning by the SJ().
    
    # i.e.       a b v      (same test matrix, repeating here for easier reading of the test cases below)
    #       [1,] 1 5 1
    #       [2,] 3 5 2
    #       [3,] 4 6 3
    #       [4,] 4 6 4
    #       [5,] 4 9 5
    #       [6,] 4 9 6
    #       [7,] 7 2 7
    if (TESTDT[SJ(4,6),v,mult="first"] != 3) stop("Test 25 failed")
    if (TESTDT[SJ(4,6),v,mult="last"] != 4) stop("Test 26 failed")
    if (!identical(TESTDT[J(c(4,4,4),c(7,6,6)),v,mult="last",roll=TRUE], INT(4,4,4))) stop("Test 27 failed")
    if (!identical(TESTDT[J(c(4,4,4),c(10,9,9)),v,mult="last",roll=TRUE], INT(6,6,6))) stop("Test 28 failed")
    if (!identical(TESTDT[J(c(4,4,4),c(7,6,6)),v,mult="last",rolltolast=TRUE], INT(4,4,4))) stop("Test 29 failed")
    if (!identical(TESTDT[J(c(4,4,4),c(10,9,9)),v,mult="last",rolltolast=TRUE], INT(NA,6,6))) stop("Test 30 failed")
    if (!identical(TESTDT[J(c(4,4,4),c(10,9,9)),v,mult="first",rolltolast=TRUE], INT(NA,5,5))) stop("Test 31 failed")
    if (!identical(TESTDT[J(c(8,1,4,4,-9),c(1,4,4,10,1)),v], INT(NA,NA,NA,NA,NA))) stop("Test 32 failed")
    if (!identical(TESTDT[J(c(8,1,4,4,-9),c(1,4,4,10,1)),v,roll=TRUE], INT(NA,NA,NA,6,NA))) stop("Test 33 failed")
    if (!identical(TESTDT[J(c(8,1,4,4,-9),c(1,4,7,10,1)),v,rolltolast=TRUE], INT(NA,NA,4,NA,NA))) stop("Test 34 failed")
    if (!identical(TESTDT[J(c(5,4,-3,8,4,7,2)),v,mult="first"], INT(NA,3,NA,NA,3,7,NA))) stop("Test 35 failed")
    if (!identical(TESTDT[J(c(5,4,-3,8,4,7,2)),v,mult="first",roll=TRUE], INT(6,3,NA,7,3,7,1))) stop("Test 36 failed")
    if (!identical(TESTDT[J(c(5,4,-3,8,4,7,2)),v,mult="last"], INT(NA,6,NA,NA,6,7,NA))) stop("Test 37 failed")
    if (!identical(TESTDT[J(c(5,4,-3,8,4,7,2)),v,mult="last",roll=TRUE], INT(6,6,NA,7,6,7,1))) stop("Test 38 failed")
    if (!identical(TESTDT[J(c(5,4,-3,8,4,7,2)),v,mult="last",nomatch=0], INT(6,6,7))) stop("Test 39 failed")
    if (!identical(TESTDT[J(c(4)),v,mult="all"][[2]], INT(3,4,5,6))) stop("Test 40 failed")
    if (!identical(TESTDT[J(c(4,4)),v,mult="all"][[2]], INT(3:6,3:6))) stop("Test 41 failed")
    if (!identical(TESTDT[J(c(8,2,4,-3)),v,mult="all"][[2]], INT(3:6))) stop("Test 42 failed")
    if (!identical(TESTDT[J(c(8,2,4,-3)),v,mult="all",roll=TRUE][[2]], INT(7,1,3:6))) stop("Test 43 failed")
    if (!identical(suppressWarnings(TESTDT[J(c(8,4,2,-3)),v,mult="all",rolltolast=TRUE,incbycols=FALSE][[1]]), INT(3:6,1))) stop("Test 44 failed")
    if (!identical(TESTDT[J(c(-9,1,4,4,4,4,8),c(1,5,5,6,7,10,3)),v,mult="all"][[3]], INT(1,3:4))) stop("Test 45 failed")
    if (!identical(TESTDT[J(c(-9,1,4,4,4,4,8),c(1,5,5,6,7,10,3)),v,mult="all",roll=TRUE][[3]], INT(1,3:4,4,6))) stop("Test 46 failed")
    if (!identical(TESTDT[J(c(-9,1,4,4,4,4,8),c(1,5,5,6,7,10,3)),v,mult="all",rolltolast=TRUE][[3]], INT(1,3:4,4))) stop("Test 47 failed")
    if (!identical(TESTDT[J(c(-9,NA,4,NA,1,4,4),c(1,5,9,6,5,9,10)),v,mult="all",roll=TRUE][[3]], INT(5:6,1,5:6,6))) stop("Test 48 failed")  # this time the NAs stay where they are. Compare to test 24 above.

    if (!identical(TESTDT[J(c(4,1,0,5,3,7,NA,4,1),c(6,5,1,10,5,2,1,6,NA)),v], INT(3,1,NA,NA,2,7,NA,3,NA))) stop("Test 49 failed")
    if (!identical(TESTDT[J(c(4,1,0,5,3,7,NA,4,1),c(6,5,1,10,5,2,1,6,NA)),v,mult="last"], INT(4,1,NA,NA,2,7,NA,4,NA))) stop("Test 50 failed")
    
    TESTDT$a = factor(letters[TESTDT$a])
    # i.e.       a b v
    #       [1,] a 5 1
    #       [2,] c 5 2
    #       [3,] d 6 3
    #       [4,] d 6 4
    #       [5,] d 9 5
    #       [6,] d 9 6
    #       [7,] g 2 7
    if (!identical(TESTDT[SJ(c("d","d","e","g"),c(6,7,1,2)),v,mult="all",roll=TRUE][[3]], INT(3:4,4,7))) stop("Test 51 failed")   # will test sortedmatch for strings in the level match
    if (!identical(TESTDT[J(c("g","d","e","d"),c(6,6,1,2)),v,mult="all",roll=TRUE][[3]], INT(7,3:4))) stop("Test 52 failed")   # also will test sortedmatch for strings in the level match
    
    TESTDT$b = factor(letters[TESTDT$b])
    # i.e.
    #         a b v
    #    [1,] a e 1
    #    [2,] c e 2
    #    [3,] d f 3
    #    [4,] d f 4
    #    [5,] d i 5
    #    [6,] d i 6
    #    [7,] g b 7
    if (!identical(TESTDT[SJ(c("d","d","e","g"),c("f","g","a","b")),v,mult="last"], INT(4,NA,NA,7))) stop("Test 53 failed")
    if (!identical(TESTDT[J(c("g","d","e","d"),c("b","g","a","f")),v,mult="last"], INT(7,NA,NA,4))) stop("Test 54 failed")  # this tests (d,g) ok even though there is an NA in last factor level match in the roll.
    if (!identical(TESTDT[SJ(c("d","d","e","g"),c("f","g","a","b")),v,mult="first"], INT(3,NA,NA,7))) stop("Test 55 failed")
    if (!identical(TESTDT[J(c("g","d","e","d"),c("b","g","a","f")),v,mult="first"], INT(7,NA,NA,3))) stop("Test 56 failed")  # this tests (d,g) ok even though there is an NA in last factor level match in the roll.
    t = try(TESTDT[J(c("g","d","e","d"),c("b","g","a","f")),v,roll=TRUE],silent=TRUE)
    if (!inherits(t,"try-error")) stop("Test 57 failed")
    if (!length(grep("Attempting roll join on factor column", t))) stop("Test 58 failed")
    t = try(TESTDT[J(c("g","d","e","d"),c("b","g","a","f")),v,rolltolast=TRUE],silent=TRUE)
    if (!inherits(t,"try-error")) stop("Test 59 failed")
    if (!length(grep("Attempting roll join on factor column", t))) stop("Test 60 failed")
    
    ##  Add tests on sortedmatch as well to nail it down even further,  even though its called above.
    
    if (!identical(sortedmatch(c("a","h","f","h","j"), letters[1:8]), INT(1,8,6,8,NA))) stop("Test 61 failed")
    if (!identical(sortedmatch(INT(5,2,4,3,NA,7), INT(2,4,5,7)), INT(3,1,2,NA,NA,4))) stop("Test 62 failed")
    if (!identical(sortedmatch(1:3,INT(NULL)), INT(NA,NA,NA))) stop("Test 63 failed")
    
    t = try(sortedmatch(c("a","h","f","j"), letters[8:1], check=TRUE), silent=TRUE)
    if (!inherits(t,"try-error")) stop("Test 64 failed")    # v2 is not sorted
    t = try(sortedmatch(letters[1:3], 1:3), silent=TRUE)
    if (!inherits(t,"try-error")) stop("Test 65 failed")    # v1 and v2 must be the same storage.mode
    t = try(sortedmatch(as.double(1:3), as.double(1:3)), silent=TRUE)
    if (!inherits(t,"try-error")) stop("Test 66 failed")    # only character or integer accepted
    
    # Test for [.factor masked and dropping unused factor levels ok.
    X = factor(letters[1:10])
    if (!identical(levels(X[4:6]), letters[4:6])) stop("Test 67 failed")
    
    if (!"TESTDT" %in% tables(silent=TRUE)[,NAME]) stop("Test 68 failed")  # NAME is returned as a factor in which we look for the string 
    if (!"TESTDT" %in% tables(silent=TRUE)[,as.character(NAME)]) stop("Test 69 failed") # can (so will in this test function) convet the factor column to character first, but don't need to
    
    a = "d"     # Variable Twister.  a in this scope has same name as a inside DT scope.
    if (!identical(TESTDT[J(a),DT(v)], DT(a="d",v=3:6))) stop("Test 70 failed")  # J(a) means use a we just set above,  not a inside the DT which would result in a self join of the whole table. Would only occur if there is a variable name conflict as deliberately created here.
    if (!identical(TESTDT[SJ(a),DT(v)], DT(a="d",v=3:6,key="a"))) stop("Test 71 failed")
    if (!identical(TESTDT[CJ(a),DT(v)], DT(a="d",v=3:6,key="a"))) stop("Test 72 failed")
    
    if (!identical(TESTDT[,v], 1:7)) stop("Test 73 failed")
    if (!identical(TESTDT[,3], DT(v=INT(1:7)))) stop("Test 74 failed")
    if (!identical(TESTDT[,"v"], DT(v=INT(1:7)))) stop("Test 75 failed")
    if (!identical(TESTDT[,2:3], INT(2:3))) stop("Test 76 failed")  # See ?[.data.table that with=FALSE is required for the likely intended result
    if (!identical(TESTDT[,2:3,with=FALSE], data.table(b=c("e","e","f","f","i","i","b"),v=1:7))) stop("Test 77 failed")
    if (!identical(TESTDT[,c("b","v"),with=FALSE], data.table(b=c("e","e","f","f","i","i","b"),v=1:7))) stop("Test 78 failed")
    colsVar = c("b","v")
    if (!identical(TESTDT[,colsVar], colsVar)) stop("Test 79 failed")
    if (!identical(TESTDT[,colsVar,with=FALSE], data.table(b=c("e","e","f","f","i","i","b"),v=1:7))) stop("Test 80 failed")
    
    if (!identical(TESTDT[1:2,c(a,b)], factor(c("a","c","e","e")))) stop("Test 81 failed")
    # It is expected the above to be common source of confusion. c(a,b) is evaluated within
    # the frame of TESTDT, and c() creates one long vector, not 2 column subset as in data.frame's.
    # Instead of c(a,b) use DT(a,b).
    
    if (!identical(TESTDT[,c("a","b")], c("a","b"))) stop("Test 82 failed")
    if (!identical(TESTDT[,list("a","b")], list("a","b"))) stop("Test 83 failed")
    if (!identical(TESTDT[1:2,list(a,b)], list(factor(c("a","c")), factor(c("e","e"))))) stop("Test 84 failed")
    if (!identical(TESTDT[1:2,DT(a,b)], data.table(a=c("a","c"), b=c("e","e")))) stop("Test 85 failed")

    if (!identical(TESTDT[,sum(v),by="b"], DT(b=c("b","e","f","i"),V1=INT(7,3,7,11)))) stop("Test 86 failed")  # TESTDT is key'd by a,b, so correct that grouping by b should not be key'd in the result by default
    if (!identical(TESTDT[,DT(MySum=sum(v)),by="b"], DT(b=c("b","e","f","i"),MySum=INT(7,3,7,11)))) stop("Test 87 failed")
    if (!identical(TESTDT[,DT(MySum=sum(v),Sq=v*v),by="b"][1:2], DT(b=c("b","e"),MySum=INT(7,3),Sq=INT(49,1)))) stop("Test 88 failed")   # silent repetition of MySum to match the v*v vector
    if (!identical(TESTDT[,sum(v),by="b",simplify=FALSE], list(7L,3L,7L,11L))) stop("Test 89 failed")
    
    setkey(TESTDT,b)
    if (!identical(TESTDT[J(c("f","i")),sum(v),mult="all"], DT(b=c("f","i"),V1=c(7L,11L)))) stop("Test 90 failed")  # aggregation via groups passed into i and mult="all"
    if (!identical(TESTDT[SJ(c("f","i")),sum(v),mult="all"], DT(b=c("f","i"),V1=c(7L,11L),key="b"))) stop("Test 91 failed")  # aggregation via groups passed into i and mult="all"
    if (!identical(TESTDT[J(c("f","i")),sum(v),mult="all",simplify=FALSE], list(7L,11L))) stop("Test 92 failed")
    
    if (!identical(TESTDT[J(c("f","i")), which=TRUE], INT(4,6))) stop("Test 93 failed")
    if (!identical(TESTDT[J(c("i","f")), mult="last", which=TRUE], INT(7,5))) stop("Test 94 failed")
    
    if (!identical(TESTDT["f",v], 3L)) stop("Test 95 failed")
    if (!identical(TESTDT["f",v,mult="all"], DT(b="f",V1=3:4))) stop("Test 96 failed")
    if (!identical(TESTDT[c("f","i","b"),DT(GroupSum=sum(v)),mult="all"], DT(b=c("f","i","b"), GroupSum=c(7L,11L,7L)))) stop("Test 97 failed")  # mult="all" is required here since only b is key'd
    # that line above doesn't create a key on the result so that the order fib is preserved.
    if (!identical(TESTDT[SJ(c("f","i","b")),DT(GroupSum=sum(v)),mult="all"], DT(b=c("b","f","i"), GroupSum=c(7L,7L,11L), key="b"))) stop("Test 98 failed")
    # line above is the way to group, sort by group and setkey on the result by group.
    
    cat("All 98 tests in test.data.table() completed ok in",time.taken(started.at),"\n")    
    # should normally complete in under 1 sec, unless perhaps if a gc was triggered
    invisible()
}
