cedta = function() {
    # Calling Environment Data Table Aware

    te = topenv(parent.frame(2))
    identical(te,.GlobalEnv) ||
    (isNamespace(te) &&
        (getNamespaceName(te) == "data.table" ||
         "data.table" %in% names(getNamespaceImports(te))))

    #pkg = tryCatch(as.environment(paste("package:",name,sep="")),function(e){NULL})
    #!is.null(pkg) && "data.table" %in% mget(".Depends",envir=pkg,ifnotfound=list(NULL))[[1]]
}


