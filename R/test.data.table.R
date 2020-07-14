test.data.table = function(script="tests.Rraw", verbose=FALSE, pkg=".", silent=FALSE, showProgress=interactive()&&!silent) {
  stopifnot(isTRUEorFALSE(verbose), isTRUEorFALSE(silent), isTRUEorFALSE(showProgress))
  if (exists("test.data.table", .GlobalEnv,inherits=FALSE)) {
    # package developer
    # nocov start
    if ("package:data.table" %chin% search()) stop("data.table package is loaded. Unload or start a fresh R session.")
    rootdir = if (pkg!="." && pkg %chin% dir()) file.path(getwd(), pkg) else Sys.getenv("PROJ_PATH")
    subdir = file.path("inst","tests")
    # nocov end
  } else {
    # i) R CMD check and ii) user running test.data.table()
    rootdir = getNamespaceInfo("data.table","path")
    subdir = "tests"
  }
  fulldir = file.path(rootdir, subdir)

  stopifnot(is.character(script), length(script)==1L, !is.na(script), nzchar(script))
  if (!grepl(".Rraw$", script))
    stop("script must end with '.Rraw'. If a file ending '.Rraw.bz2' exists, that will be found and used.") # nocov

  if (identical(script,"*.Rraw")) {
    # nocov start
    scripts = dir(fulldir, "*.Rraw.*")
    scripts = scripts[!grepl("bench|other", scripts)]
    scripts = gsub("[.]bz2$","",scripts)
    for (fn in scripts) {test.data.table(script=fn, verbose=verbose, pkg=pkg, silent=silent, showProgress=showProgress); cat("\n");}
    return(invisible())
    # nocov end
  }

  if (!identical(basename(script), script)) {
    # nocov start
    subdir = dirname(script)
    fulldir = normalizePath(subdir, mustWork=FALSE)
    fn = basename(script)
    # nocov end
  } else {
    fn = script
  }

  if (!file.exists(file.path(fulldir, fn))) {
    # see end of CRAN_Release.cmd where *.Rraw are compressed just for CRAN release; #3937
    # nocov start
    fn2 = paste0(fn,".bz2")
    if (!file.exists(file.path(fulldir, fn2)))
      stop(gettextf("Neither %s nor %s exist in %s",fn, fn2, fulldir, domain="R-data.table"))
    fn = fn2
    # nocov end
    # sys.source() below accepts .bz2 directly.
  }
  fn = setNames(file.path(fulldir, fn), file.path(subdir, fn))

  # These environment variables are restored to their previous state (including not defined) after sourcing test script
  oldEnv = Sys.getenv(c("_R_CHECK_LENGTH_1_LOGIC2_", "TZ"), unset=NA_character_)
  # From R 3.6.0 onwards, we can check that && and || are using only length-1 logicals (in the test suite)
  # rather than relying on x && y being equivalent to x[[1L]] && y[[1L]]  silently.
  Sys.setenv("_R_CHECK_LENGTH_1_LOGIC2_" = TRUE)
  # TZ is not changed here so that tests run under the user's timezone. But we save and restore it here anyway just in case
  # the test script stops early during a test that changes TZ (e.g. 2124 referred to in PR #4464).

  oldRNG = suppressWarnings(RNGversion("3.5.0"))
  # sample method changed in R 3.6 to remove bias; see #3431 for links and notes
  # This can be removed (and over 120 tests updated) if and when the oldest R version we test and support is moved to R 3.6

  # TO DO: reinstate solution for C locale of CRAN's Mac (R-Forge's Mac is ok)
  # oldlocale = Sys.getlocale("LC_CTYPE")
  # Sys.setlocale("LC_CTYPE", "")   # just for CRAN's Mac to get it off C locale (post to r-devel on 16 Jul 2012)

  # Control options in case user set them. The user's values are restored after the sys.source() below.
  if (is.null(options()$warnPartialMatchArgs))   options(warnPartialMatchArgs=FALSE)   # R 3.1.0 had a NULL default for these 3. Set to FALSE
  if (is.null(options()$warnPartialMatchAttr))   options(warnPartialMatchAttr=FALSE)   # now otherwise options(oldOptions) fails later.
  if (is.null(options()$warnPartialMatchDollar)) options(warnPartialMatchDollar=FALSE)
  oldOptions = options(
    datatable.verbose = verbose,
    encoding = "UTF-8",  # just for tests 708-712 on Windows
    scipen = 0L,  # fwrite now respects scipen
    datatable.optimize = Inf,
    datatable.alloccol = 1024L,
    datatable.print.class = FALSE,  # this is TRUE in cc.R and we like TRUE. But output= tests need to be updated (they assume FALSE currently)
    datatable.print.trunc.cols = FALSE, #4552
    datatable.rbindlist.check = NULL,
    datatable.integer64 = "integer64",
    warnPartialMatchArgs = base::getRversion()>="3.6.0", # ensure we don't rely on partial argument matching in internal code, #3664; >=3.6.0 for #3865
    warnPartialMatchAttr = TRUE,
    warnPartialMatchDollar = TRUE,
    width = max(getOption('width'), 80L), # some tests (e.g. 1066, 1293) rely on capturing output that will be garbled with small width
    datatable.old.fread.datetime.character = FALSE
  )

  cat("getDTthreads(verbose=TRUE):\n")         # for tracing on CRAN; output to log before anything is attempted
  getDTthreads(verbose=TRUE)                   # includes the returned value in the verbose output (rather than dangling '[1] 4'); e.g. "data.table is using 4 threads"
  cat("test.data.table() running:", fn, "\n")  # print fn to log before attempting anything on it (in case it is missing); on same line for slightly easier grep
  env = new.env(parent=.GlobalEnv)
  assign("testDir", function(x) file.path(fulldir, x), envir=env)

  # are R's messages being translated to a foreign language? #3039, #630
  txt = eval(parse(text="tryCatch(mean(not__exist__), error = function(e) e$message)"), envir=.GlobalEnv)
  foreign = txt != "object 'not__exist__' not found"
  if (foreign) {
    # nocov start
    cat("\n**** This R session's language is not English. Each test will still check that the correct number of errors and/or\n",
          "**** warnings are produced. However, to test the text of each error/warning too, please restart R with LANGUAGE=en\n\n", sep="")
    # nocov end
  }
  assign("foreign", foreign, envir=env)
  assign("nfail", 0L, envir=env)
  assign("ntest", 0L, envir=env)
  assign("prevtest", -1L, envir=env)
  assign("whichfail", NULL, envir=env)
  assign("started.at", proc.time(), envir=env)
  assign("lasttime", proc.time()[3L], envir=env)  # used by test() to attribute time inbetween tests to the next test
  assign("timings", data.table( ID = seq_len(9999L), time=0.0, nTest=0L ), envir=env)   # test timings aggregated to integer id
  assign("memtest", as.logical(Sys.getenv("TEST_DATA_TABLE_MEMTEST", "FALSE")), envir=env)
  assign("filename", fn, envir=env)
  assign("inittime", as.integer(Sys.time()), envir=env) # keep measures from various test.data.table runs
  assign("showProgress", showProgress, envir=env)

  err = try(sys.source(fn, envir=env), silent=silent)

  options(oldOptions)
  for (i in oldEnv) {
    if (is.na(oldEnv[i]))
      Sys.unsetenv(names(oldEnv)[i])
    else
      do.call("Sys.setenv", as.list(oldEnv[i])) # nocov
  }
  # Sys.setlocale("LC_CTYPE", oldlocale)
  suppressWarnings(do.call("RNGkind",as.list(oldRNG)))
  # suppressWarnings for the unlikely event that user selected sample='Rounding' themselves before calling test.data.table()

  # Now output platform trace before error (if any) to be sure to always show it; e.g. to confirm endianness in #4099.
  # As one long dense line for cases when 00check.log only shows the last 13 lines of log; to only use up one
  # of those 13 line and give a better chance of seeing more of the output before it. Having said that, CRAN
  # does show the full file output these days, so the 13 line limit no longer bites so much. It still bit recently
  # when receiving output of R CMD check sent over email, though.
  tz = Sys.getenv("TZ", unset=NA)
  cat("\n", date(),   # so we can tell exactly when these tests ran on CRAN to double-check the result is up to date
    "  endian==", .Platform$endian,
    ", sizeof(long double)==", .Machine$sizeof.longdouble,
    ", sizeof(pointer)==", .Machine$sizeof.pointer,
    ", TZ==", if (is.na(tz)) "unset" else paste0("'",tz,"'"),
    ", Sys.timezone()=='", suppressWarnings(Sys.timezone()), "'",
    ", Sys.getlocale()=='", Sys.getlocale(), "'",
    ", l10n_info()=='", paste0(names(l10n_info()), "=", l10n_info(), collapse="; "), "'",
    ", getDTthreads()=='", paste0(gsub("[ ][ ]+","==",gsub("^[ ]+","",capture.output(invisible(getDTthreads(verbose=TRUE))))), collapse="; "), "'",
    "\n", sep="")

  if (inherits(err,"try-error")) {
    # nocov start
    if (silent) return(FALSE)
    stop("Failed after test ", env$prevtest, " before the next test() call in ",fn)
    # the try() above with silent=FALSE will have already printed the error itself
    # nocov end
  }

  nfail = env$nfail
  ntest = env$ntest
  if (nfail > 0L) {
    # nocov start
    if (nfail > 1L) {s1="s";s2="s: "} else {s1="";s2=" "}
    stop(nfail," error",s1," out of ",ntest,". Search ",names(fn)," for test number",s2,paste(env$whichfail,collapse=", "),".")
    # important to stop() here, so that 'R CMD check' fails
    # nocov end
  }

  # There aren't any errors, so we can use up 11 lines for the timings table
  timings = env$timings
  DT = head(timings[-1L][order(-time)], 10L)   # exclude id 1 as in dev that includes JIT
  if ((x<-sum(timings[["nTest"]])) != ntest) {
    warning("Timings count mismatch:",x,"vs",ntest)  # nocov
  }
  cat("10 longest running tests took ", as.integer(tt<-DT[, sum(time)]), "s (", as.integer(100*tt/(ss<-timings[,sum(time)])), "% of ", as.integer(ss), "s)\n", sep="")
  print(DT, class=FALSE)

  cat("All ",ntest," tests in ",names(fn)," completed ok in ",timetaken(env$started.at),"\n",sep="")

  ## this chunk requires to include new suggested deps: graphics, grDevices
  #memtest.plot = function(.inittime) {
  #  if (!all(requireNamespace(c("graphics","grDevices"), quietly=TRUE))) return(invisible())
  #  inittime=PS_rss=GC_used=GC_max_used=NULL
  #  m = fread("memtest.csv")[inittime==.inittime]
  #  if (nrow(m)) {
  #    ps_na = allNA(m[["PS_rss"]]) # OS with no 'ps -o rss R' support
  #    grDevices::png("memtest.png")
  #    p = graphics::par(mfrow=c(if (ps_na) 2 else 3, 2))
  #    if (!ps_na) {
  #      m[, graphics::plot(test, PS_rss, pch=18, xlab="test num", ylab="mem MB", main="ps -o rss R")]
  #      m[, graphics::plot(timestamp, PS_rss, type="l", xlab="timestamp", ylab="mem MB", main="ps -o rss R")]
  #    }
  #    m[, graphics::plot(test, GC_used, pch=18, xlab="test num", ylab="mem MB", main="gc used")]
  #    m[, graphics::plot(timestamp, GC_used, type="l", xlab="timestamp", ylab="mem MB", main="gc used")]
  #    m[, graphics::plot(test, GC_max_used, pch=18, xlab="test num", ylab="mem MB", main="gc max used")]
  #    m[, graphics::plot(timestamp, GC_max_used, type="l", xlab="timestamp", ylab="mem MB", main="gc max used")]
  #    graphics::par(p)
  #    grDevices::dev.off()
  #  } else {
  #    warning("test.data.table runs with memory testing but did not collect any memory statistics.")
  #  }
  #}
  #if (memtest<-get("memtest", envir=env)) memtest.plot(get("inittime", envir=env))

  invisible(nfail==0L)
}

# nocov start
compactprint = function(DT, topn=2L) {
  tt = vapply_1c(DT,function(x)class(x)[1L])
  tt[tt=="integer64"] = "i64"
  tt = substring(tt, 1L, 3L)
  makeString = function(x) paste(x, collapse = ",")  # essentially toString.default
  cn = paste0(" [Key=",makeString(key(DT)),
             " Types=", makeString(substring(sapply(DT, typeof), 1L, 3L)),
             " Classes=", makeString(tt), "]")
  if (nrow(DT)) {
    print(copy(DT)[,(cn):="",verbose=FALSE], topn=topn, class=FALSE)
  } else {
    print(DT, class=FALSE)  # "Empty data.table (0 rows) of <ncol> columns ...
    if (ncol(DT)) cat(cn,"\n")
  }
  invisible()
}
# nocov end

INT = function(...) { as.integer(c(...)) }   # utility used in tests.Rraw

ps_mem = function() {
  # nocov start
  cmd = sprintf("ps -o rss %s | tail -1", Sys.getpid())
  ans = tryCatch(as.numeric(system(cmd, intern=TRUE, ignore.stderr=TRUE)), warning=function(w) NA_real_, error=function(e) NA_real_)
  stopifnot(length(ans)==1L) # extra check if other OSes would not handle 'tail -1' properly for some reason
  # returns RSS memory occupied by current R process in MB rounded to 1 decimal places (as in gc), ps already returns KB
  c("PS_rss"=round(ans / 1024, 1L))
  # nocov end
}

gc_mem = function() {
  # nocov start
  # gc reported memory in MB
  m = apply(gc()[, c(2L, 4L, 6L)], 2L, sum)
  names(m) = c("GC_used", "GC_gc_trigger", "GC_max_used")
  m
  # nocov end
}

test = function(num,x,y=TRUE,error=NULL,warning=NULL,message=NULL,output=NULL,notOutput=NULL,ignore.warning=NULL) {
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
  .test.data.table = exists("nfail", parent.frame()) # test() can be used inside functions defined in tests.Rraw, so inherits=TRUE (default) here
  numStr = sprintf("%.8g", num)
  if (.test.data.table) {
    prevtest = get("prevtest", parent.frame())
    nfail = get("nfail", parent.frame())   # to cater for both test.data.table() and stepping through tests in dev
    whichfail = get("whichfail", parent.frame())
    assign("ntest", get("ntest", parent.frame()) + 1L, parent.frame(), inherits=TRUE)   # bump number of tests run
    lasttime = get("lasttime", parent.frame())
    timings = get("timings", parent.frame())
    memtest = get("memtest", parent.frame())
    inittime = get("inittime", parent.frame())
    filename = get("filename", parent.frame())
    foreign = get("foreign", parent.frame())
    showProgress = get("showProgress", parent.frame())
    time = nTest = NULL  # to avoid 'no visible binding' note
    on.exit( {
       now = proc.time()[3L]
       took = now-lasttime  # so that prep time between tests is attributed to the following test
       assign("lasttime", now, parent.frame(), inherits=TRUE)
       timings[ as.integer(num), `:=`(time=time+took, nTest=nTest+1L), verbose=FALSE ]
    } )
    if (showProgress)
      cat("\rRunning test id", numStr, "     ")   # nocov.
    # See PR #4090 for comments about change here in Dec 2019.
    # If a segfault error occurs in future and we'd like to know after which test, then arrange for the
    # try(sys.source()) in test.data.table() to be run in a separate R process. That process could write out
    # prevtest to a temp file so we know where it got to from this R process. That should be more reliable
    # than what we were doing before which was for test() to always write its test number to output (which might
    # not be flushed to the output upon segfault, depending on OS).
  } else {
    # not `test.data.table` but developer running tests manually; i.e. `cc(F); test(...)`
    memtest = FALSE          # nocov
    filename = NA_character_ # nocov
    foreign = FALSE          # nocov ; assumes users of 'cc(F); test(...)' has LANGUAGE=en
    showProgress = FALSE     # nocov
  }
  if (!missing(error) && !missing(y))
    stop("Test ",numStr," is invalid: when error= is provided it does not make sense to pass y as well")  # nocov

  string_match = function(x, y, ignore.case=FALSE) {
    length(grep(x, y, fixed=TRUE)) ||  # try treating x as literal first; useful for most messages containing ()[]+ characters
    length(tryCatch(grep(x, y, ignore.case=ignore.case), error=function(e)NULL))  # otherwise try x as regexp
  }

  xsub = substitute(x)
  ysub = substitute(y)

  actual = list("warning"=NULL, "error"=NULL, "message"=NULL)
  wHandler = function(w) {
    # Thanks to: https://stackoverflow.com/a/4947528/403310
    actual$warning <<- c(actual$warning, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
  eHandler = function(e) {
    actual$error <<- conditionMessage(e)
    e
  }
  mHandler = function(m) {
    actual$message <<- c(actual$message, conditionMessage(m))
    m
  }
  if (memtest) {
    timestamp = as.numeric(Sys.time())   # nocov
  }
  if (is.null(output) && is.null(notOutput)) {
    x = suppressMessages(withCallingHandlers(tryCatch(x, error=eHandler), warning=wHandler, message=mHandler))
    # save the overhead of capture.output() since there are a lot of tests, often called in loops
    # Thanks to tryCatch2 by Jan here : https://github.com/jangorecki/logR/blob/master/R/logR.R#L21
  } else {
    out = capture.output(print(x <- suppressMessages(withCallingHandlers(tryCatch(x, error=eHandler), warning=wHandler, message=mHandler))))
  }
  if (memtest) {
    mem = as.list(c(inittime=inittime, filename=basename(filename), timestamp=timestamp, test=num, ps_mem(), gc_mem())) # nocov
    fwrite(mem, "memtest.csv", append=TRUE, verbose=FALSE)                                                                             # nocov
  }
  fail = FALSE
  if (.test.data.table) {
    if (num<prevtest+0.0000005) {
      # nocov start
      cat("Test id", numStr, "is not in increasing order\n")
      fail = TRUE
      # nocov end
    }
    assign("prevtest", num, parent.frame(), inherits=TRUE)
  }
  if (!fail) for (type in c("warning","error","message")) {
    observed = actual[[type]]
    expected = get(type)
    if (type=="warning" && length(observed) && !is.null(ignore.warning)) {
      # if a warning containing this string occurs, ignore it. First need for #4182 where warning about 'timedatectl' only
      # occurs in R 3.4, and maybe only on docker too not for users running test.data.table().
      stopifnot(length(ignore.warning)==1L, is.character(ignore.warning), !is.na(ignore.warning), nchar(ignore.warning)>=1L)
      observed = grep(ignore.warning, observed, value=TRUE, invert=TRUE)
    }
    if (length(expected) != length(observed)) {
      # nocov start
      cat("Test ",numStr," produced ",length(observed)," ",type,"s but expected ",length(expected),"\n",sep="")
      cat(paste("Expected:",expected), sep="\n")
      cat(paste("Observed:",observed), sep="\n")
      fail = TRUE
      # nocov end
    } else {
      # the expected type occurred and, if more than 1 of that type, in the expected order
      for (i in seq_along(expected)) {
        if (!foreign && !string_match(expected[i], observed[i])) {
          # nocov start
          cat("Test",numStr,"didn't produce the correct",type,":\n")
          cat("Expected:", expected[i], "\n")
          cat("Observed:", observed[i], "\n")
          fail = TRUE
          # nocov end
        }
      }
    }
  }
  if (fail && exists("out",inherits=FALSE)) {
    # nocov start
    cat("Output captured before unexpected warning/error/message:\n")
    cat(out,sep="\n")
    # nocov end
  }
  if (!fail && !length(error) && (length(output) || length(notOutput))) {
    if (out[length(out)] == "NULL") out = out[-length(out)]
    out = paste(out, collapse="\n")
    output = paste(output, collapse="\n")  # so that output= can be either a \n separated string, or a vector of strings.
    if (length(output) && !string_match(output, out)) {
      # nocov start
      cat("Test",numStr,"did not produce correct output:\n")
      cat("Expected: <<",gsub("\n","\\\\n",output),">>\n",sep="")  # \n printed as '\\n' so the two lines of output can be compared vertically
      cat("Observed: <<",gsub("\n","\\\\n",out),">>\n",sep="")
      fail = TRUE
      # nocov end
    }
    if (length(notOutput) && string_match(notOutput, out, ignore.case=TRUE)) {
      # nocov start
      cat("Test",numStr,"produced output but should not have:\n")
      cat("Expected absent (case insensitive): <<",gsub("\n","\\\\n",notOutput),">>\n",sep="")
      cat("Observed: <<",gsub("\n","\\\\n",out),">>\n",sep="")
      fail = TRUE
      # nocov end
    }
  }
  if (!fail && !length(error) && (!length(output) || !missing(y))) {   # TODO test y when output=, too
    y = try(y,TRUE)
    if (identical(x,y)) return(invisible(TRUE))
    all.equal.result = TRUE
    if (is.data.table(x) && is.data.table(y)) {
      if (!selfrefok(x) || !selfrefok(y)) {
        # nocov start
        cat("Test ",numStr," ran without errors but selfrefok(", if(!selfrefok(x))"x"else"y", ") is FALSE\n", sep="")
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
        if (identical(xc,yc) && identical(key(x),key(y))) return(invisible(TRUE))  # check key on original x and y because := above might have cleared it on xc or yc
        if (isTRUE(all.equal.result<-all.equal(xc,yc)) && identical(key(x),key(y)) &&
          identical(vapply_1c(xc,typeof), vapply_1c(yc,typeof))) return(invisible(TRUE))
      }
    }
    if (is.atomic(x) && is.atomic(y) && isTRUE(all.equal.result<-all.equal(x,y,check.names=!isTRUE(y))) && typeof(x)==typeof(y)) return(invisible(TRUE))
    # For test 617 on r-prerel-solaris-sparc on 7 Mar 2013
    # nocov start
    if (!fail) {
      cat("Test", numStr, "ran without errors but failed check that x equals y:\n")
      failPrint = function(x, xsub) {
        cat(">", substitute(x), "=", xsub, "\n")
        if (is.data.table(x)) compactprint(x) else {
          nn = length(x)
          cat(sprintf("First %d of %d (type '%s'): \n", min(nn, 6L), length(x), typeof(x)))
          # head.matrix doesn't restrict columns
          if (length(d <- dim(x))) do.call(`[`, c(list(x, drop = FALSE), lapply(pmin(d, 6L), seq_len)))
          else print(head(x))
        }
      }
      failPrint(x, deparse(xsub))
      failPrint(y, deparse(ysub))
      if (!isTRUE(all.equal.result)) cat(all.equal.result, sep="\n")
      fail = TRUE
    }
    # nocov end
  }
  if (fail && .test.data.table) {
    # nocov start
    assign("nfail", nfail+1L, parent.frame(), inherits=TRUE)
    assign("whichfail", c(whichfail, numStr), parent.frame(), inherits=TRUE)
    # nocov end
  }
  invisible(!fail)
}

