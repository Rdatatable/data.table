vecref = function(x,offset,len)
{
    ans = .Call("vecref",x,offset,len,PACKAGE="data.table")
    # the C checks the types of offset and len
    if (is.factor(x)) {
        u = sort(unique(ans))
        levels(ans) = levels(x)[u]
        ans[] = sortedmatch(ans,u)
        class(ans)="factor"
    }
    ans
}
