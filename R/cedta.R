
cedta.override = NULL  # If no need arises, will deprecate.

cedta.pkgEvalsUserCode = c("gWidgetsWWW","statET","FastRWeb","slidify","rmarkdown","knitr","ezknitr","IRkernel", "rtvs", "purrr")
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

# cedta = Calling Environment Data.Table-Aware
cedta = function(n=2L) {
  # Calling Environment Data Table Aware
  ns = topenv(parent.frame(n))
  if (!isNamespace(ns)) {
    # e.g. DT queries at the prompt (.GlobalEnv) and knitr's eval(,envir=globalenv()) but not DF[...] inside knitr::kable v1.6
    return(TRUE)
  }
  nsname = getNamespaceName(ns)
  ans = nsname=="data.table" ||
    "data.table" %chin% names(getNamespaceImports(ns)) ||   # most common and recommended cases first for speed
    (nsname=="utils" &&
      (exists("debugger.look", parent.frame(n+1L)) ||
      (length(sc<-sys.calls())>=8L && sc[[length(sc)-7L]][[1L]]=='example')) ) || # 'example' for #2972
    (nsname=="base" && all(c("FUN", "X") %chin% ls(parent.frame(n)))) || # lapply
    (nsname %chin% cedta.pkgEvalsUserCode && any(sapply(sys.calls(), function(x) is.name(x[[1L]]) && (x[[1L]]=="eval" || x[[1L]]=="evalq")))) ||
    nsname %chin% cedta.override ||
    isTRUE(ns$.datatable.aware) ||  # As of Sep 2018: RCAS, caretEnsemble, dtplyr, rstanarm, rbokeh, CEMiTool, rqdatatable, RImmPort, BPRMeth, rlist
    tryCatch("data.table" %chin% get(".Depends",paste("package",nsname,sep=":"),inherits=FALSE),error=function(e)FALSE)  # both ns$.Depends and get(.Depends,ns) are not sufficient
  if (!ans && getOption("datatable.verbose")) {
    # nocov start
    cat("cedta decided '",nsname,"' wasn't data.table aware. Here is call stack with [[1L]] applied:\n",sep="")
    print(sapply(sys.calls(), "[[", 1L))
    # nocov end
    # so we can trace the namespace name that may need to be added (very unusually)
  }
  ans
}

