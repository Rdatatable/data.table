
cedta.override = NULL  # If no need arises, will deprecate.

cedta.pkgEvalsUserCode = c("gWidgetsWWW","statET","FastRWeb","slidify","rmarkdown","knitr", "IRkernel")
# These packages run user code in their own environment and thus do not
# themselves Depend or Import data.table. knitr's eval is passed envir=globalenv() so doesn't
# need to be listed here currently, but we include it in case it decides to change that.
# The others create their own environment to eval in.
# The packages might use data.frame syntax elsewhere in their package code so we just want
# the eval (only) of user code to be data.table-aware.

# If a new package needs to be added to this vector, a user may add to it using :
#   assignInNamespace("cedta.pkgEvalsUserCode", c(data.table:::cedta.pkgEvalsUserCode,"<nsname>"), "data.table")
# But please let us know so we can add the package to this vector in the package upstream, so other
# users don't have to tread the same path. Then you can remove your assignInNamepace() call.

# Packages may also set a variable in their namespace :
#   .datatable.aware = TRUE
# which makes them data.table-aware optionally and possibly variably.
# http://stackoverflow.com/a/13131555/403310

cedta <- function(n=2L) {
    # Calling Environment Data Table Aware
    te = topenv(parent.frame(n))
    if (!isNamespace(te)) {
        # e.g. DT queries at the prompt (.GlobalEnv) and knitr's eval(,envir=globalenv()) but not DF[...] inside knitr::kable v1.6
        return(TRUE)
    }
    nsname = getNamespaceName(te)
    ans = nsname == "data.table" ||
        "data.table" %chin% names(getNamespaceImports(te)) ||
        "data.table" %chin% tryCatch(get(".Depends",paste("package",nsname,sep=":"),inherits=FALSE),error=function(e)NULL) ||
        (nsname == "utils" && exists("debugger.look",parent.frame(n+1L))) || 
        (nsname == "base"  && all(c("FUN", "X") %in% ls(parent.frame(n)))  ) || # lapply
        (nsname %chin% cedta.pkgEvalsUserCode && any(sapply(sys.calls(), "[[", 1L)=="eval")) ||
        nsname %chin% cedta.override ||
        identical(TRUE, tryCatch(get(".datatable.aware",asNamespace(nsname),inherits=FALSE),error=function(e)NULL))
    if (!ans && getOption("datatable.verbose"))
        cat("cedta decided '",nsname,"' wasn't data.table aware\n",sep="")
        # so we can trace the namespace name that may need to be added (very unusually)
    ans
}


