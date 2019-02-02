test.data.table <- function(verbose=FALSE, pkg="pkg", silent=FALSE, with.other.packages=FALSE, benchmark=FALSE, script=NULL) {
  if (exists("test.data.table", .GlobalEnv,inherits=FALSE)) {
    # package developer
    # nocov start
    if ("package:data.table" %chin% search()) stop("data.table package is loaded. Unload or start a fresh R session.")
    rootdir = if (pkg %chin% dir()) file.path(getwd(), pkg) else Sys.getenv("CC_DIR")
    subdir = file.path("inst","tests")
    # nocov end
  } else {
    # i) R CMD check and ii) user running test.data.table()
    rootdir = getNamespaceInfo("data.table","path")
    subdir = "tests"
  }
  fulldir = file.path(rootdir, subdir)

  if (!is.null(script)) {
    stopifnot(is.character(script), length(script)==1L, !is.na(script), nzchar(script))
    if (!identical(basename(script), script)) {
      subdir = dirname(script)
      fulldir = normalizePath(subdir, mustWork=FALSE)
      fn = basename(script)
    } else {
      fn = script
    }
  } else {
    stopifnot( !(with.other.packages && benchmark) )
    fn = if (with.other.packages) "other.Rraw"
         else if (benchmark) "benchmark.Rraw"
         else "tests.Rraw"
  }
  fn = setNames(file.path(fulldir, fn), file.path(subdir, fn))
  if (!file.exists(fn)) stop(fn," does not exist")

  # From R 3.6.0 onwards, we can check that && and || are using only length-1 logicals (in the test suite)
  # rather than relying on x && y being equivalent to x[[1L]] && y[[1L]]  silently.
  orig__R_CHECK_LENGTH_1_LOGIC2_ <- Sys.getenv("_R_CHECK_LENGTH_1_LOGIC2_", unset = NA_character_)
  Sys.setenv("_R_CHECK_LENGTH_1_LOGIC2_" = TRUE)
  # This environment variable is restored to its previous state (including not defined) after sourcing test script
  
  oldverbose = options(datatable.verbose=verbose)
  oldenc = options(encoding="UTF-8")[[1L]]  # just for tests 708-712 on Windows
  # TO DO: reinstate solution for C locale of CRAN's Mac (R-Forge's Mac is ok)
  # oldlocale = Sys.getlocale("LC_CTYPE")
  # Sys.setlocale("LC_CTYPE", "")   # just for CRAN's Mac to get it off C locale (post to r-devel on 16 Jul 2012)

  cat("Running", fn, "\n")
  env = new.env(parent=.GlobalEnv)
  assign("testDir", function(x) file.path(fulldir, x), envir=env)
  assign("nfail", 0L, envir=env)
  assign("ntest", 0L, envir=env)
  assign("whichfail", NULL, envir=env)
  setDTthreads(2) # explicitly limit to 2 so as not to breach CRAN policy (but tests are small so should not use more than 2 anyway)
  assign("started.at", proc.time(), envir=env)
  assign("lasttime", proc.time()[3L], envir=env)  # used by test() to attribute time inbetween tests to the next test
  assign("timings", data.table( ID = seq_len(9999L), time=0.0, nTest=0L ), envir=env)   # test timings aggregated to integer id
  assign("memtest", as.logical(Sys.getenv("TEST_DATA_TABLE_MEMTEST", "FALSE")), envir=env)
  assign("filename", fn, envir=env)
  assign("inittime", as.integer(Sys.time()), envir=env) # keep measures from various test.data.table runs
  # It doesn't matter that 3000L is far larger than needed for other and benchmark.
  if(isTRUE(silent)){
    try(sys.source(fn, envir=env), silent=silent)  # nocov
  } else {
    sys.source(fn, envir=env)
  }
  options(oldverbose)
  options(oldenc)
  # Sys.setlocale("LC_CTYPE", oldlocale)
  setDTthreads(0)
  ans = env$nfail==0
  
  if (is.na(orig__R_CHECK_LENGTH_1_LOGIC2_)) {
    Sys.unsetenv("_R_CHECK_LENGTH_1_LOGIC2_")
  } else {
    Sys.setenv("_R_CHECK_LENGTH_1_LOGIC2_" = orig__R_CHECK_LENGTH_1_LOGIC2_)
  }
  
  timings = get("timings", envir=env)
  ntest = get("ntest", envir=env)
  nfail = get("nfail", envir=env)
  started.at = get("started.at", envir=env)
  whichfail = get("whichfail", envir=env)
  
  # Summary. This code originally in tests.Rraw and moved up here in #3307
  plat = paste0("endian==", .Platform$endian,
                ", sizeof(long double)==", .Machine$sizeof.longdouble,
                ", sizeof(pointer)==", .Machine$sizeof.pointer,
                ", TZ=", suppressWarnings(Sys.timezone()),
                ", locale='", Sys.getlocale(), "'",
                ", l10n_info()='", paste0(names(l10n_info()), "=", l10n_info(), collapse="; "), "'")
  DT = head(timings[-1L][order(-time)],10)   # exclude id 1 as in dev that includes JIT
  if ((x<-sum(timings[["nTest"]])) != ntest) warning("Timings count mismatch:",x,"vs",ntest)
  cat("\n10 longest running tests took ", as.integer(tt<-DT[, sum(time)]), "s (", as.integer(100*tt/(ss<-timings[,sum(time)])), "% of ", as.integer(ss), "s)\n", sep="")
  print(DT, class=FALSE)
  
  ## this chunk requires to include new suggested deps: graphics, grDevices
  #memtest.plot = function(.inittime) {
  #  if (!all(requireNamespace(c("graphics","grDevices"), quietly=TRUE))) return(invisible())
  #  inittime=PS_rss=GC_used=GC_max_used=NULL
  #  m = fread("memtest.csv")[inittime==.inittime]
  #  if (nrow(m)) {
  #    ps_na = all(is.na(m[["PS_rss"]])) # OS with no 'ps -o rss R' support
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
  
  if (nfail > 0) {
    if (nfail>1) {s1="s";s2="s: "} else {s1="";s2=" "}
    cat("\r")
    stop(nfail," error",s1," out of ",ntest," in ",timetaken(started.at)," on ",date(),". [",plat,"].",
         " Search ",names(fn)," for test number",s2,paste(whichfail,collapse=", "),".")
    # important to stop() here, so that 'R CMD check' fails
  }
  cat(plat,"\n\nAll ",ntest," tests in ",names(fn)," completed ok in ",timetaken(started.at)," on ",date(),"\n",sep="")
  # date() is included so we can tell exactly when these tests ran on CRAN. Sometimes a CRAN log can show error but that can be just
  # stale due to not updating yet since a fix in R-devel, for example.
  
  #attr(ans, "details") <- env
  invisible(ans)
}

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
  c("PS_rss"=round(ans / 1024, 1))
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

test <- function(num,x,y=TRUE,error=NULL,warning=NULL,output=NULL,message=NULL) {
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
  if (.test.data.table) {
    nfail = get("nfail", parent.frame())   # to cater for both test.data.table() and stepping through tests in dev
    whichfail = get("whichfail", parent.frame())
    assign("ntest", get("ntest", parent.frame()) + 1L, parent.frame(), inherits=TRUE)   # bump number of tests run
    lasttime = get("lasttime", parent.frame())
    timings = get("timings", parent.frame())
    memtest = get("memtest", parent.frame())
    inittime = get("inittime", parent.frame())
    filename = get("filename", parent.frame())
    time = nTest = NULL  # to avoid 'no visible binding' note
    on.exit( {
       now = proc.time()[3]
       took = now-lasttime  # so that prep time between tests is attributed to the following test
       assign("lasttime", now, parent.frame(), inherits=TRUE)
       timings[ as.integer(num), `:=`(time=time+took, nTest=nTest+1L), verbose=FALSE ]
    } )
    cat("\rRunning test id", sprintf("%.8g", num), "     ")
    flush.console()
    # This flush is for Windows to make sure last test number is written to file in CRAN and win-builder output where
    # console output is captured. \r seems especially prone to not being auto flushed. The downside is that the last 13
    # lines output are filled with the last 13 "running test num" lines rather than the last error output, but that's
    # better than the dev-time-lost when it crashes and it actually crashed much later than the last test number visible.
  } else {
    memtest = FALSE          # nocov
    filename = NA_character_ # nocov
  }

  if (!missing(error) && !missing(y)) stop("Test ",num," is invalid: when error= is provided it does not make sense to pass y as well")

  string_match = function(x, y) {
    length(grep(x,y,fixed=TRUE)) ||                    # try treating x as literal first; useful for most messages containing ()[]+ characters
    length(tryCatch(grep(x,y), error=function(e)NULL)) # otherwise try x as regexp
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
  if (is.null(output)) {
    x = suppressMessages(withCallingHandlers(tryCatch(x, error=eHandler), warning=wHandler, message=mHandler))
    # save the overhead of capture.output() since there are a lot of tests, often called in loops
    # Thanks to tryCatch2 by Jan here : https://github.com/jangorecki/logR/blob/master/R/logR.R#L21
  } else {
    out = capture.output(print(x <- suppressMessages(withCallingHandlers(tryCatch(x, error=eHandler), warning=wHandler, message=mHandler))))
  }
  if (memtest) {
    mem = as.list(c(inittime=inittime, filename=basename(filename), timestamp=timestamp, test=num, ps_mem(), gc_mem())) # nocov
    fwrite(mem, "memtest.csv", append=TRUE)                                                                             # nocov
  }
  fail = FALSE
  for (type in c("warning","error","message")) {
    observed = actual[[type]]
    expected = get(type)
    if (length(expected) != length(observed)) {
      # nocov start
      cat("Test ",num," produced ",length(observed)," ",type,"s but expected ",length(expected),"\n",sep="")
      cat(paste("Expected:",expected), sep="\n")
      cat(paste("Observed:",observed), sep="\n")
      fail = TRUE
      # nocov end
    } else {
      # the expected type occurred and, if more than 1 of that type, in the expected order
      for (i in seq_along(expected)) {
        if (!string_match(expected[i], observed[i])) {
          # nocov start
          cat("Test",num,"didn't produce the correct",type,":\n")
          cat("Expected:", expected[i], "\n")
          cat("Observed:", observed[i], "\n")
          fail = TRUE
          # nocov end
        }
      }
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
        if (identical(xc,yc) && identical(key(x),key(y))) return(invisible(TRUE))  # check key on original x and y because := above might have cleared it on xc or yc
        if (isTRUE(all.equal.result<-all.equal(xc,yc)) && identical(key(x),key(y)) &&
          identical(vapply_1c(xc,typeof), vapply_1c(yc,typeof))) return(invisible(TRUE))
      }
    }
    if (is.atomic(x) && is.atomic(y) && isTRUE(all.equal.result<-all.equal(x,y,check.names=!isTRUE(y))) && typeof(x)==typeof(y)) return(invisible(TRUE))
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
  if (fail && .test.data.table) {
    # nocov start
    assign("nfail", nfail+1L, parent.frame(), inherits=TRUE)
    assign("whichfail", c(whichfail, num), parent.frame(), inherits=TRUE)
    # nocov end
  }
  invisible(!fail)
}

