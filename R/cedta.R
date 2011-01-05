cedta = function() {
    # Calling Environment Data Table Aware
    te = topenv(parent.frame(2))
    !isNamespace(te) ||   # .GlobalEnv has no ns => dt-aware
    getNamespaceName(te) == "data.table" ||
    "data.table" %in% names(getNamespaceImports(te)) ||
    (getNamespaceName(te) == "utils" && exists("debugger.look",parent.frame(3)))  # fixes bug #1131 

    #pkg = tryCatch(as.environment(paste("package:",name,sep="")),function(e){NULL})
    #!is.null(pkg) && "data.table" %in% mget(".Depends",envir=pkg,ifnotfound=list(NULL))[[1]]
}


