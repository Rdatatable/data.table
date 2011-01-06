cedta = function() {
    # Calling Environment Data Table Aware
    te = topenv(parent.frame(2))
    if (!isNamespace(te)) return(TRUE)  # e.g. DT queries in .GlobalEnv
    nsname = getNamespaceName(te)
    nsname == "data.table" ||
    "data.table" %in% names(getNamespaceImports(te)) ||
    "data.table" %in% tryCatch(get(".Depends",paste("package",nsname,sep=":")),error=function(e)NULL) ||
    (nsname == "utils" && exists("debugger.look",parent.frame(3)))
}


