
cedta.override = c("gWidgetsWWW","statET","FastRWeb")
# user may add more to this using :
# assignInNamespace("cedta.override", c(data.table:::cedta.override,"<nsname>"), "data.table")

cedta = function(n=2L) {
    # Calling Environment Data Table Aware
    te = topenv(parent.frame(n))
    if (!isNamespace(te)) return(TRUE)  # e.g. DT queries in .GlobalEnv
    nsname = getNamespaceName(te)
    ans = nsname == "data.table" ||
        "data.table" %chin% names(getNamespaceImports(te)) ||
        "data.table" %chin% tryCatch(get(".Depends",paste("package",nsname,sep=":"),inherits=FALSE),error=function(e)NULL) ||
        (nsname == "utils" && exists("debugger.look",parent.frame(n+1L))) ||
        nsname %chin% cedta.override ||
        identical(TRUE, tryCatch(get(".datatable.aware",asNamespace(nsname),inherits=FALSE),error=function(e)NULL))
    if (!ans && getOption("datatable.verbose"))
        cat("cedta decided '",nsname,"' wasn't data.table aware\n",sep="")
        # so we can trace the namespace name that may need to be added (very unusually)
    ans
}


