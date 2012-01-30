cedta = function(n=2L) {
    # Calling Environment Data Table Aware
    te = topenv(parent.frame(n))
    if (!isNamespace(te)) return(TRUE)  # e.g. DT queries in .GlobalEnv
    nsname = getNamespaceName(te)
    nsname == "data.table" ||
    "data.table" %in% names(getNamespaceImports(te)) ||
    "data.table" %in% tryCatch(get(".Depends",paste("package",nsname,sep=":")),error=function(e)NULL) ||
    (nsname == "utils" && exists("debugger.look",parent.frame(n+1L)))
}


