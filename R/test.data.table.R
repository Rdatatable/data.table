test.data.table <- function(verbose=FALSE, pkg="pkg", silent=FALSE) {
  if (exists("test.data.table",.GlobalEnv,inherits=FALSE)) {
    # package developer
    # nocov start
    if ("package:data.table" %in% search()) stop("data.table package is loaded. Unload or start a fresh R session.")
    d = if (pkg %in% dir()) file.path(getwd(), pkg) else Sys.getenv("CC_DIR")
    d = file.path(d, "inst/tests")
    # nocov end
  } else {
    # R CMD check and user running test.data.table()
    d = paste0(getNamespaceInfo("data.table","path"),"/tests")
  }
  # for (fn in dir(d,"*.[rR]$",full=TRUE)) {  # testthat runs those
  oldenc = options(encoding="UTF-8")[[1L]]  # just for tests 708-712 on Windows
  # TO DO: reinstate solution for C locale of CRAN's Mac (R-Forge's Mac is ok)
  # oldlocale = Sys.getlocale("LC_CTYPE")
  # Sys.setlocale("LC_CTYPE", "")   # just for CRAN's Mac to get it off C locale (post to r-devel on 16 Jul 2012)

  envirs <- list()
  for (fn in file.path(d, 'tests.Rraw')) {    # not testthat
    cat("Running",fn,"\n")
    oldverbose = options(datatable.verbose=verbose)
    envirs[[fn]] = new.env(parent=.GlobalEnv)
    assign("testDir", function(x)file.path(d,x), envir=envirs[[fn]])
    if(isTRUE(silent)){
      try(sys.source(fn,envir=envirs[[fn]]), silent=silent)  # nocov
    } else {
      sys.source(fn,envir=envirs[[fn]])
    }
    options(oldverbose)
  }
  options(encoding=oldenc)
  # Sys.setlocale("LC_CTYPE", oldlocale)
  invisible(sum(sapply(envirs, `[[`, "nfail"))==0)
}

# Define test() and its globals here, for use in dev
# But primarily called by test.data.table() calling inst/tests/tests.Rraw
# Initialized at the top of tests.Raw ...
# nfail = ntest = lastnum = 0
# whichfail = NULL
# .devtesting = TRUE

# nocov start
compactprint <- function(DT, topn=2L) {
  tt = vapply_1c(DT,function(x)class(x)[1L])
  tt[tt=="integer64"] = "i64"
  tt = substring(tt, 1L, 3L)
  makeString = function(x) paste(x, collapse = ",")  # essentially toString.default
  cn = paste0(" [Key=",makeString(key(DT)),
             " Types=", makeString(substring(sapply(DT, typeof), 1L, 3L)),
             " Classes=", makeString(tt), "]")
  if (nrow(DT)) {
    print(copy(DT)[,(cn):=""], topn=topn)
  } else {
    print(DT)  # "Empty data.table (0 rows) of <ncol> columns ...
    if (ncol(DT)) cat(cn,"\n")
  }
  invisible()
}
# nocov end

INT = function(...) { as.integer(c(...)) }   # utility used in tests.Rraw

test <- function(num,x,y,error=NULL,warning=NULL,output=NULL) {
  # Usage:
  # i) tests that x equals y when both x and y are supplied, the most common usage
  # ii) tests that x is TRUE when y isn't supplied
  # iii) if error is supplied, y should be missing and x is tested to result in an error message matching the pattern
  # iv) if warning is supplied, y (if supplied) is checked to equal x, and x should result in a warning message matching the pattern
  # v) if output is supplied, x is evaluated and printed and the output is checked to match the pattern
  # At most one of error, warning or output may be supplied, all single character strings (passed to grep)
  # num just needs to be numeric and unique. We normally increment integers at the end, but inserts can be made using decimals e.g. 10,11,11.1,11.2,12,13,...
  # Motivations:
  # 1) we'd like to know all tests that fail not just stop at the first. This often helps by revealing a common feature across a set of
  #    failing tests
  # 2) test() tests more deeply than a diff on console output and uses a data.table appropriate definition of "equals" different
  #    from all.equal and different to identical related to row.names and unused factor levels
  # 3) each test has a unique id which we refer to in commit messages, emails etc.
  nfail = get("nfail", parent.frame())   # to cater for both test.data.table() and stepping through tests in dev
  whichfail = get("whichfail", parent.frame())
  all.equal.result = TRUE
  assign("ntest", get("ntest", parent.frame()) + 1L, parent.frame(), inherits=TRUE)   # bump number of tests run
  assign("lastnum", num, parent.frame(), inherits=TRUE)

  cat("\rRunning test id", num, "     ")
  flush.console()
  # This flush is for Windows to make sure last test number is written to file in CRAN and win-builder output where
  # console output is captured. \r seems especially prone to not being auto flushed. The downside is that the last 13
  # lines output are filled with the last 13 "running test num" lines rather than the last error output, but that's
  # better than the dev-time-lost when it crashes and it actually crashed much later than the last test number visible.

  xsub = substitute(x)
  ysub = substitute(y)
  if (is.null(output)) err <<- try(x,TRUE)
  else {
    out = gsub("NULL$","",paste(capture.output(print(err<<-try(x,TRUE))),collapse=""))
    out = gsub("\n","",gsub("\r","",out))  # ensure no \r or \n pollution on windows
    # We use .* to shorten what we test for (so the grep below needs fixed=FALSE)
    # but other characters should be matched literally
    output = gsub("\\","\\\\",output,fixed=TRUE)  # e.g numbers like 9.9e+10 should match the + literally
    output = gsub("[","\\[",output,fixed=TRUE)
    output = gsub("]","\\]",output,fixed=TRUE)
    output = gsub("(","\\(",output,fixed=TRUE)
    output = gsub(")","\\)",output,fixed=TRUE)
    output = gsub("+","\\+",output,fixed=TRUE)  # e.g numbers like 9.9e+10 should match the + literally
    output = gsub("\n","",output,fixed=TRUE)  # e.g numbers like 9.9e+10 should match the + literally
    if (!length(grep(output,out))) {
      # nocov start
      cat("Test",num,"didn't produce correct output:\n")
      cat(">",deparse(xsub),"\n")
      cat("Expected: '",output,"'\n",sep="")
      cat("Observed: '",out,"'\n",sep="")
      assign("nfail", nfail+1, parent.frame(), inherits=TRUE)
      assign("whichfail", c(whichfail, num), parent.frame(), inherits=TRUE)
      return()
      # nocov end
    }
  }
  if (!is.null(error) || !is.null(warning)) {
    type = ifelse(!is.null(error),"error","warning")
    if (type=="error" && !missing(y)) stop("Test ",num," is invalid: when error= is provided it doesn't make sense to pass y as well")
    patt = txt = ifelse(!is.null(error),error,warning)
    patt = gsub("[(]","[(]",patt)
    patt = gsub("[)]","[)]",patt)
    patt = gsub("\\^","\\\\^", patt)  # for test 923 containing 2^31 in error message
    observedtype = ifelse(length(grep("converted from warning",err)), "warning", "error")
    if (! (inherits(err,"try-error") &&
           length(grep(patt,err)) &&
           type==observedtype)) {
      # nocov start
      cat("Test",num,"didn't produce correct",type,":\n")
      cat(">",deparse(xsub),"\n")
      cat("Expected ",type,": '",txt,"'\n",sep="")
      if (!inherits(err,"try-error"))
        cat("Observed: no error or warning\n")
      else
        cat("Observed ",observedtype,": '",gsub("^[(]converted from warning[)] ","",gsub("\n$","",gsub("^Error.* : \n  ","",as.character(err)))),"'\n",sep="")
      assign("nfail", nfail+1L, parent.frame(), inherits=TRUE)   # Not the same as nfail <<- nfail + 1, it seems (when run via R CMD check)
      assign("whichfail", c(whichfail, num), parent.frame(), inherits=TRUE)
      return()
      # nocov end
    }
    if (type=="warning")
      err <- if (is.null(output)) x<-try(suppressWarnings(x),TRUE) else out<-paste(capture.output(x<-try(suppressWarnings(x),TRUE)),collapse="")
    else return()
  }
  if (inherits(err,"try-error") || (!missing(y) && inherits(err<-try(y,TRUE),"try-error"))) {
    # nocov start
    cat("Test",num,err)
    assign("nfail", nfail+1L, parent.frame(), inherits=TRUE)
    assign("whichfail", c(whichfail, num), parent.frame(), inherits=TRUE)
    return()
    # nocov end
  }
  if (missing(y)) {
    if (!is.null(output)) return()
    if (isTRUE(as.vector(x))) return()  # as.vector to drop names of a named vector such as returned by system.time
    # nocov start
    cat("Test",num,"expected TRUE but observed:\n")
    cat(">",deparse(xsub),"\n")
    if (is.data.table(x)) compactprint(x) else print(x)
    assign("nfail", nfail+1L, parent.frame(), inherits=TRUE)
    assign("whichfail", c(whichfail, num), parent.frame(), inherits=TRUE)
    return()
    # nocov end
  } else {
    if (identical(x,y)) return()
    if (is.data.table(x) && is.data.table(y)) {
      # TO DO:  test 166 doesn't pass with these :
      # if (!selfrefok(x)) stop("x selfref not ok")
      # if (!selfrefok(y)) stop("y selfref not ok")
      xc=copy(x)
      yc=copy(y)  # so we don't affect the original data which may be used in the next test
      # drop unused levels in factors
      if (length(x)) for (i in which(vapply_1b(x,is.factor))) {.xi=x[[i]];xc[,(i):=factor(.xi)]}
      if (length(y)) for (i in which(vapply_1b(y,is.factor))) {.yi=y[[i]];yc[,(i):=factor(.yi)]}
      setattr(xc,"row.names",NULL)  # for test 165+, i.e. x may have row names set from inheritance but y won't, consider these equal
      setattr(yc,"row.names",NULL)
      setattr(xc,"index",NULL)   # too onerous to create test RHS with the correct index as well, just check result
      setattr(yc,"index",NULL)
      if (identical(xc,yc) && identical(key(x),key(y))) return()  # check key on original x and y because := above might have cleared it on xc or yc
      if (isTRUE(all.equal.result<-all.equal(xc,yc)) && identical(key(x),key(y)) &&
        identical(vapply_1c(xc,typeof), vapply_1c(yc,typeof))) return()
    }
    if (is.atomic(x) && is.atomic(y) && isTRUE(all.equal.result<-all.equal(x,y)) && typeof(x)==typeof(y)) return()
    # For test 617 on r-prerel-solaris-sparc on 7 Mar 2013
  }
  # nocov start
  cat("Test",num,"ran without errors but failed check that x equals y:\n")
  cat("> x =",deparse(xsub),"\n")
  if (is.data.table(x)) compactprint(x) else {cat("First 6 of ", length(x)," (type '", typeof(x), "'): ", sep=""); print(head(x))}
  cat("> y =",deparse(ysub),"\n")
  if (is.data.table(y)) compactprint(y) else {cat("First 6 of ", length(y)," (type '", typeof(y), "'): ", sep=""); print(head(y))}
  if (!isTRUE(all.equal.result)) cat(all.equal.result,sep="\n")
  assign("nfail", nfail+1L, parent.frame(), inherits=TRUE)
  assign("whichfail", c(whichfail, num), parent.frame(), inherits=TRUE)
  invisible()
  # nocov end
}

