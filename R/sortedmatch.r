sortedmatch = function(v1,v2, nomatch=NA, check=FALSE)
{
    # just like match but v2 must be a sorted unique character vector e.g. factor levels, or a sorted unique integer vector
    # by default we don't check that v2 is sorted, although the check implemented here is efficient
    if (storage.mode(v1)!=storage.mode(v2)) stop("v1 (",storage.mode(v1),") and v2 (",storage.mode(v2),") differ in storage mode")
    funct = switch(storage.mode(v1), "character"="sortedstringmatch", "integer"="sortedintegermatch", NA)
    if (is.na(funct)) stop(storage.mode(v1)," storage.mode not supported, only integer or character please")
    if (check && any(!c(TRUE,take(v2)<v2[-1]))) stop("v2 is not sorted or has duplicates")
    ans = integer(length(v1))
    .Call(funct,ans,v1,v2,as.integer(nomatch),PACKAGE="data.table")
    ans
}
