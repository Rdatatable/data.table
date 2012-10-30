
cedta.override = c("gWidgetsWWW")
# user may add more to this (e.g. adding statET) using :
# assignInNamespace("cedta.override",c("gWidgetsWWW","statET"),"data.table")

cedta = function(n=2L) {
    # Calling Environment Data Table Aware
    te = topenv(parent.frame(n))
    if (!isNamespace(te)) return(TRUE)  # e.g. DT queries in .GlobalEnv
    nsname = getNamespaceName(te)
    ans = nsname == "data.table" ||
        "data.table" %chin% names(getNamespaceImports(te)) ||
        "data.table" %chin% tryCatch(get(".Depends",paste("package",nsname,sep=":")),error=function(e)NULL) ||
        (nsname == "utils" && exists("debugger.look",parent.frame(n+1L))) ||
        nsname %chin% cedta.override
    if (!ans && getOption("datatable.verbose"))
        cat("cedta decided '",nsname,"' wasn't data.table aware\n",sep="")
        # so we can trace the namespace name that may need to be added (very unusually)
    ans
}


