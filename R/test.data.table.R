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

test <- function(num,x,y=TRUE,error=NULL,warning=NULL,output=NULL) {
  # Usage:
  # i) tests that x equals y when both x and y are supplied, the most common usage
  # ii) tests that x is TRUE when y isn't supplied
  # iii) if error is supplied, y should be missing and x is tested to result in an error message matching the pattern
  # iv) if warning is supplied, y is checked to equal x, and x should result in a warning message matching the pattern
  # v) if output is supplied, x is evaluated and printed and the output is checked to match the pattern
  # num just needs to be numeric and unique. We normally increment integers at the end, but inserts can be made using decimals e.g. 10,11,11.1,11.2,12,13,...
  # Motivations:
  # 1) we'd like to know all tests that fail not just stop at the first. This often helps by revealing a common feature across a set of
  #    failing tests
  # 2) test() tests more deeply than a diff on console output and uses a data.table appropriate definition of "equals" different
  #    from all.equal and different to identical related to row.names and unused factor levels
  # 3) each test has a unique id which we refer to in commit messages, emails etc.
  # 4) test that a query generates exactly 2 warnings, that they are both the correct warning messages, and that the result is the one expected
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

  if (!missing(error) && !missing(y)) stop("Test ",num," is invalid: when error= is provided it does not make sense to pass y as well")
  check_specials = function(string, type) {
    # helper in dev to remind dev what might be wrong when changing or adding a new test
    # nocov start
    found = ""
    if (length(grep("[^[\\]([(]|[)])", string))) found = "parenthesis"
    else if (length(grep("[^[\\]([[]|[]])", string))) found = "square bracket"
    else if (length(grep("[^\\]\\+", string))) found = "'+'"
    else if (length(grep("[^\\]\\^", string))) found = "'^'"
    if (found!="") stop("Likely due to the unescaped ",found," in the ",type,"= string. Please avoid it using .* or preceed it with double backslash.")
    # nocov end
  }
  string_match = function(x, y) {
    if (length(grep(x,y,fixed=TRUE)))  # try treating x as literal first; useful for most messages containing ()[]+ characters
      return(TRUE)
    if (length(grep(x,y)))             # otherwise try regexp; commonly here when .* is used in x and if so, ()[]+ must then be escaped
      return(TRUE)
    return(FALSE)                      # nocov
  }

  xsub = substitute(x)
  ysub = substitute(y)

  actual.warns = NULL
  wHandler = function(w) {
    # Thanks to: https://stackoverflow.com/a/4947528/403310
    actual.warns <<- c(actual.warns, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
  actual.err = NULL
  eHandler = function(e) {
    actual.err <<- conditionMessage(e)
    e
  }
  if (is.null(output)) {
    x = tryCatch(withCallingHandlers(x, warning=wHandler), error=eHandler)
    # save the overhead of capture.output() since there are a lot of tests, often called in loops
  } else {
    out = capture.output(print(x <<- tryCatch(withCallingHandlers(x, warning=wHandler), error=eHandler)))
  }
  fail = FALSE
  if (length(warning) != length(actual.warns)) {
    # nocov start
    cat("Test",num,"produced",length(actual.warns),"warnings but expected",length(warnings),"\n")
    cat(paste("Expected:",warning), sep="\n")
    cat(paste("Observed:",actual.warns), sep="\n")
    fail = TRUE
    # nocov end
  } else {
    # the expected warning occurred and, if more than 1 warning, in the expected order
    for (i in seq_along(warning)) {
      if (!string_match(warning[i], actual.warns[i])) {
        # nocov start
        cat("Test",num,"didn't produce the correct warning:\n")
        cat("Expected: ", warning[i], "\n")
        cat("Observed: ", actual.warns[i], "\n")
        check_specials(warning[i], "warning")
        fail = TRUE
        # nocov end
      }
    }
  }
  if (length(error) != length(actual.err)) {
    # nocov start
    cat("Test",num," ")
    if (length(error)) cat("had no error but expected error: ", error, "\n")
    else cat("should not fail but failed with error: ", actual.err, "\n")
    fail = TRUE
    # nocov end
  } else if (length(error)) {
    if (!string_match(error, actual.err)) {
      # nocov start
      cat("Test",num,"didn't produce the correct error:\n")
      cat("Expected: ", error, "\n")
      cat("Observed: ", actual.err, "\n")
      check_specials(error, "error")
      fail = TRUE
      # nocov end
    }
  }

  if (!fail && !length(error) && length(output)) {
    if (out[length(out)] == "NULL") out = out[-length(out)]
    out = paste(out, collapse="\n")
    output = paste(output, collapse="\n")  # so that output= can be either a \n separated string, or a vector of strings.
    if (!string_match(output, out)) {
      # nocov start
      cat("Test",num,"didn't produce correct output:\n")
      cat("Expected: <<",gsub("\n","\\\\n",output),">>\n",sep="")  # \n printed as '\\n' so the two lines of output can be compared vertically
      cat("Observed: <<",gsub("\n","\\\\n",out),">>\n",sep="")
      check_specials(output, "output")
      fail = TRUE
      # nocov end
    }
  }
  if (!fail && !length(error) && (!length(output) || !missing(y))) {   # TODO test y when output=, too
    y = try(y,TRUE)
    if (identical(x,y)) return()
    if (is.data.table(x) && is.data.table(y)) {
      if (!selfrefok(x) || !selfrefok(y)) {
        # nocov start
        cat("Test ",num," ran without errors but selfrefok(", if(!selfrefok(x))"x"else"y", ") is FALSE\n", sep="")
        fail = TRUE
        # nocov end
      } else {
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
    }
    if (is.atomic(x) && is.atomic(y) && isTRUE(all.equal.result<-all.equal(x,y,check.names=!isTRUE(y))) && typeof(x)==typeof(y)) return()
    # For test 617 on r-prerel-solaris-sparc on 7 Mar 2013
    # nocov start
    if (!fail) {
      cat("Test",num,"ran without errors but failed check that x equals y:\n")
      cat("> x =",deparse(xsub),"\n")
      if (is.data.table(x)) compactprint(x) else {cat("First 6 of ", length(x)," (type '", typeof(x), "'): ", sep=""); print(head(x))}
      cat("> y =",deparse(ysub),"\n")
      if (is.data.table(y)) compactprint(y) else {cat("First 6 of ", length(y)," (type '", typeof(y), "'): ", sep=""); print(head(y))}
      if (!isTRUE(all.equal.result)) cat(all.equal.result,sep="\n")
      fail = TRUE
    }
    # nocov end
  }
  if (fail) {
    # nocov start
    assign("nfail", nfail+1L, parent.frame(), inherits=TRUE)   # Not the same as nfail <<- nfail + 1, it seems (when run via R CMD check)
    assign("whichfail", c(whichfail, num), parent.frame(), inherits=TRUE)
    # nocov end
  }
  invisible()
}

