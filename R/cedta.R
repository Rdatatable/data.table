
cedta.override = NULL  # If no need arises, will deprecate.

cedta.pkgEvalsUserCode = c("gWidgetsWWW","statET","FastRWeb","slidify","rmarkdown","knitr","ezknitr","IRkernel", "rtvs")
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
    dependsNotImports(nsname)
  if (!ans && getOption("datatable.verbose")) {
    # nocov start
    cat("cedta decided '",nsname,"' wasn't data.table aware. Here is call stack with [[1L]] applied:\n",sep="")
    print(sapply(sys.calls(), "[[", 1L))
    # nocov end
    # so we can trace the namespace name that may need to be added (very unusually)
  }
  ans
}

dependsNotImports = function(pkg)
{
  # When a Depend package reaches here from its R CMD check, the call quite often looks like it come
  # from a different package; e.g. VIM passes data.table to ranger, batchtools passes to future.batchtools.
  # We want to catch all Depends-on-data.table anyway, hence use search().
  # Base packages: base, utils and stats reach here in our test suite (they don't have .Depends)
  s = search()
  s = s[sapply(s, function(x) any(
    tryCatch(get(".Depends",x,inherits=FALSE), error=function(e)NULL)
    # neither ns$.Depends nor get(.Depends,ns) are sufficient
    # another way may be packageDescription(pkg,fields="Depends") but we're avoiding that for speed to avoid the file open
    == "data.table"))]
  for (this in s) {
    # nocov start
    if (substring(this,1L,8L)!="package:") next  # internal error really, but 'next' so as not to break package if it does ever happen
    this = substring(this,9L)
    if (isTRUE((d<-publishDate(this))>="2019-11-01")) {
      stop("Package ",this," (published ",d,") depends on data.table. It should import data.table instead; please contact its maintainer. This error is issued for revisions published on or after 2019-11-01. For further details and prior communication please see https://github.com/Rdatatable/data.table/issues/3076.")
      # warning() doesn't help, has to be stop(). batchtools for example passes `R CMD check` fully ok when just warning(). It has
      # lots of good tests but warnings are suppressed using testthat. Other Depends also pass warnings in examples or vignettes.
      # In test with the date set to 2000-01-01, still only 16 out of the 73 Depends are caught. These 16 will be caught the next time they
      # update but until they update they will continue to work with not even message yet. The idea for now is just to stop the growth rate
      # in Depends. But even this stop() won't achieve that very well because 57 of the 73 will still pass fine when they next update. It
      # may be that they are not using DT[...] (e.g. just using fread/fwrite), or if they are then their tests/examples/vignettes aren't
      # covering those calls. In future we could call this dependsNotImports() from data.table(), fread() and fwrite(), perhaps.
    }
    # nocov end
  }
  # use exactly the same command as v1.12.2 and before, just in case the Depends package is somehow not on search()
  # Otherwise, we could return this: paste0("package:",pkg) %chin% s
  tryCatch("data.table" %chin% get(".Depends",paste0("package:",pkg),inherits=FALSE),error=function(e)FALSE)
}

publishDate = function(pkg) {
  # base::packageDate() was added in R 3.5.0, after our supported dependency of R 3.1.0
  # In addition to needing to backport it, base::packageDate() :
  # - falls back to Built if Date is not available; but we want the publish date otherwise nothing
  # - supports other date formats, since %Y-%m-%d is only 'strongly recommended' in R-exts. However
  #   note 6 states that CRAN requires %Y-%m-%d format.
  # - uses Packaged if Date is not available; we do want that; e.g. batchtools has no Date field
  # Our goal here is to work on all versions of R >= 3.1.0, and only warn if we're sure.
  # If all date fields are somehow missing or wrong format (neither possible on CRAN) then return NA
  # which will allow the Depend to continue to work without warning or message.
  # nocov start
  ans = as.character(packageDescription(pkg, fields=c("Date","Packaged","Date/Publication")))
  ans = as.Date(ans, format="%Y-%m-%d")
  ans[!is.na(ans)][1L] # take the first non-NA of those 3, just like base::packageDate()
  # nocov end
}

