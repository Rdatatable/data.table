
cedta.override = c("gWidgetsWWW","statET","FastRWeb","slidify","rmarkdown")
# These packages tend to be ones that run user code in their own environment and thus do not
# themselves Depend or Import data.table.
# If a new package needs to be added to this vector, a user may add to it using :
#   assignInNamespace("cedta.override", c(data.table:::cedta.override,"<nsname>"), "data.table")
# But please let us know so we can add the package to this vector in the package upstream, so other
# users don't have to tread the same path. Then you can remove your assignInNamepace() call.
# Or, packages like those above can set a variable in their namespace
#   .datatable.aware = TRUE
# which achieves the same thing.  Either way.
# http://stackoverflow.com/a/13131555/403310

cedta = function(n=2L) {
    # Calling Environment Data Table Aware
    te = topenv(parent.frame(n))
    if (!isNamespace(te)) return(TRUE)  # e.g. DT queries in .GlobalEnv
    nsname = getNamespaceName(te)
    ans = nsname == "data.table" ||
        "data.table" %chin% names(getNamespaceImports(te)) ||
        "data.table" %chin% tryCatch(get(".Depends",paste("package",nsname,sep=":"),inherits=FALSE),error=function(e)NULL) ||
        (nsname == "utils" && exists("debugger.look",parent.frame(n+1L))) || 
        (nsname == "base"  && all(c("FUN", "X") %in% ls(parent.frame(n)))  ) || # lapply
        nsname %chin% cedta.override ||
        identical(TRUE, tryCatch(get(".datatable.aware",asNamespace(nsname),inherits=FALSE),error=function(e)NULL))
    if (!ans && getOption("datatable.verbose"))
        cat("cedta decided '",nsname,"' wasn't data.table aware\n",sep="")
        # so we can trace the namespace name that may need to be added (very unusually)
    ans
}


