
# c.factor was intended to be deprecated but we need it for rbind. No longer used by 'by'.
# In dogroups, we now drop factor levels in .SD, work with integers, and add levels back afterwards.

c.factor = function(...)
{
    args <- list(...)
    for (i in seq_along(args)) if (!is.factor(args[[i]])) args[[i]] = as.factor(args[[i]])
    # The first must be factor otherwise we wouldn't be inside c.factor, its checked anyway in the line above.
    newlevels = unique(unlist(lapply(args,levels),recursive=TRUE,use.names=TRUE))
    if (length(o <- forder(newlevels)))
        newlevels <- newlevels[o]
    nm <- names(unlist(args,recursive=TRUE,use.names=TRUE))
    ans = unlist(lapply(args, function(x) {   # Its faster this way when there are many references to the same level, which is normally the case
        m = match(levels(x), newlevels)
        m[as.integer(x)]
    }))
    structure(ans, levels = newlevels, names = nm, class = "factor")
}

#"[.factor" = function(x, ...)
#{
#    # change default action of factors to drop unused levels. This saves memory space and copying. It also makes tapply() work as you expect since the levels contain the unique values only, otherwise you get many NAs for the unused factor levels.
#    # The base::"[.factor" first creates the integer subset, with a pointer to the original levels, then 'if(drop)' calls factor() to then remove the unused levels.
#    # Here we force drop=TRUE for efficiency always (not changeable), and do the operation more efficiently.
#    # If you really want R's default [.factor,  then call the base version directly using  base::"[.factor"()
#    # This [.factor is within the data.table NAMESPACE so users should not see it, other than e.g. subsetting factor columns
#    # R's default [.factor assings constrasts also. Not considered here.
#    y <- NextMethod("[")
#    u = unique(y)
#    su = u[fastorder(list(u), na.last = NA)]
#    attr(y, "levels") = attr(x, "levels")[su]  # relying on the original factor levels being sorted
#    y[] = chmatch(y, su)
#    class(y) = oldClass(x)
#    y
#}


