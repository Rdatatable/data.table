test.data.table = function(script="tests.Rraw", verbose=FALSE, pkg=".", silent=FALSE, showProgress=interactive()&&!silent, testPattern=NULL,
                           memtest=Sys.getenv("TEST_DATA_TABLE_MEMTEST", 0), memtest.id=NULL) {
  stopifnot(isTRUEorFALSE(verbose), isTRUEorFALSE(silent), isTRUEorFALSE(showProgress))
  memtest = as.integer(memtest)
  stopifnot(length(memtest)==1L, memtest %in% 0:2)
  memtest.id = as.integer(memtest.id)
  if (length(memtest.id)) {
    if (length(memtest.id)==1L) memtest.id = rep(memtest.id, 2L)  # for convenience of supplying one id rather than always a range
    stopifnot(length(memtest.id)<=2L,  # conditions quoted to user when false so "<=2L" even though following conditions rely on ==2L
                     !anyNA(memtest.id), memtest.id[1L]<=memtest.id[2L])
    if (memtest==0L) memtest=1L  # using memtest.id implies memtest
  }
  if (exists("test.data.table", .GlobalEnv, inherits=FALSE)) {
    # package developer
    # nocov start
    dev = TRUE
    if ("package:data.table" %chin% search()) stopf("data.table package is loaded. Unload or start a fresh R session.")
    rootdir = if (pkg!="." && pkg %chin% dir()) file.path(getwd(), pkg) else Sys.getenv("PROJ_PATH", normalizePath("."))
    subdir = file.path("inst","tests")
    env = new.env(parent=.GlobalEnv)  # in dev cc() sources all functions in .GlobalEnv
    # nocov end
  } else {
    # i) R CMD check and ii) user running test.data.table()
    dev = FALSE
    rootdir = getNamespaceInfo("data.table","path")
    subdir = "tests"
    env = new.env(parent=parent.env(.GlobalEnv))  # when user runs test.data.table() we don't want their variables in .GlobalEnv affecting tests, #3705
  }
  fulldir = file.path(rootdir, subdir)

  stopifnot(is.character(script), length(script)==1L, !is.na(script), nzchar(script))
  if (!grepl(".Rraw$", script))
    stopf("script must end with '.Rraw'. If a file ending '.Rraw.bz2' exists, that will be found and used.") # nocov

  if (identical(script,"*.Rraw")) {
    # nocov start
    scripts = dir(fulldir, "*.Rraw.*")
    scripts = scripts[!grepl("bench|other", scripts)]
    scripts = gsub("[.]bz2$","",scripts)
    return(sapply(scripts, function(fn) {
      err = try(test.data.table(script=fn, verbose=verbose, pkg=pkg, silent=silent, showProgress=showProgress, testPattern=testPattern))
      cat("\n");
      isTRUE(err)
    }))
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
      stopf("Neither %s nor %s exist in %s",fn, fn2, fulldir)
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

  userNumericRounding = setNumericRounding(0) # Initialise to 0 in case the user has set it to a different value. Restore to user's value when finished.

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
    datatable.print.class = FALSE,  # output= tests were written when default was FALSE
    datatable.print.keys = FALSE,   # output= tests were written when default was FALSE
    datatable.print.trunc.cols = FALSE, #4552
    datatable.rbindlist.check = NULL,
    datatable.integer64 = "integer64",
    digits = 7L, # ensure printing rounds to the expected number of digits in all sessions, #5285
    warn = 0L,   # ensure signals are emitted as they are in the code, #5285
    warnPartialMatchArgs = base::getRversion()>="3.6.0", # ensure we don't rely on partial argument matching in internal code, #3664; >=3.6.0 for #3865
    warnPartialMatchAttr = TRUE,
    warnPartialMatchDollar = TRUE,
    width = max(getOption('width'), 80L), # some tests (e.g. 1066, 1293) rely on capturing output that will be garbled with small width
    datatable.old.fread.datetime.character = FALSE
  )

  cat("getDTthreads(verbose=TRUE):\n")         # for tracing on CRAN; output to log before anything is attempted
  getDTthreads(verbose=TRUE)                   # includes the returned value in the verbose output (rather than dangling '[1] 4'); e.g. "data.table is using 4 threads"
  catf("test.data.table() running: %s\n", fn)  # print fn to log before attempting anything on it (in case it is missing); on same line for slightly easier grep
  assign("testDir", function(x) file.path(fulldir, x), envir=env)

  # are R's messages being translated to a foreign language? #3039, #630
  foreign = gettext("object '%s' not found", domain="R") != "object '%s' not found"
  if (foreign) {
    # nocov start
    catf("\n**** This R session's language is not English. Each test will still check that the correct number of errors and/or\n**** warnings are produced. However, to test the text of each error/warning too, please restart R with LANGUAGE=en\n\n")
    # nocov end
  }
  assign("foreign", foreign, envir=env)
  assign("nfail", 0L, envir=env)
  assign("ntest", 0L, envir=env)
  assign("prevtest", -1L, envir=env)
  assign("whichfail", NULL, envir=env)
  assign("started.at", proc.time(), envir=env)
  assign("lasttime", proc.time()[3L], envir=env)  # used by test() to attribute time in between tests to the next test
  assign("timings", data.table( ID = seq_len(9999L), time=0.0, nTest=0L, RSS=0.0 ), envir=env)   # test timings aggregated to integer id
  assign("memtest", memtest, envir=env)
  assign("memtest.id", memtest.id, envir=env)
  assign("filename", fn, envir=env)
  assign("showProgress", showProgress, envir=env)

  owd = setwd(tempdir()) # ensure writeable directory; e.g. tests that plot may write .pdf here depending on device option and/or batch mode; #5190
  on.exit(setwd(owd))

  if (memtest) {
    catf("\n***\n*** memtest=%d. This should be the first call in a fresh R_GC_MEM_GROW=0 R session for best results. Ctrl-C now if not.\n***\n\n", memtest)
    if (is.na(rss())) stopf("memtest intended for Linux. Step through data.table:::rss() to see what went wrong.")
  }

  # nocov start: only used interactively -- "production" suites should always run in full
  if (!is.null(testPattern)) {
    # due to how non-hermetic our tests are, the simple approach (pass this to test(), return early if 'numStr' matches testPattern)
    #   does not work, or at least getting it to work is not much more efficient (see initial commit of #6040). so instead,
    #   here we parse the file, extract the tests that match the pattern to a new file, and include other setup lines likely required
    #   to run the tests successfully. two major drawbacks (1) we can only take a guess which lines are required, so this approach
    #   can't work (or at least, may need a lot of adjustment) for _every_ test, though not working is also a good sign that test
    #   should be refactored to be more hermetic (2) not all tests have literal test numbers, meaning we can't always match the
    #   runtime test number (i.e. 'numStr') since we're just doing a static check here, though we _are_ careful to match the
    #   full test expression string, i.e., not just limited to numeric literal test numbers.
    arg_line = call_id = col1 = col2 = i.line1 = id = line1 = parent = preceding_line = test_start_line = text = token = x.line1 = x.parent = NULL # R CMD check
    pd = setDT(utils::getParseData(parse(fn, keep.source=TRUE)))
    file_lines = readLines(fn)
    # NB: a call looks like (with id/parent tracking)
    # <expr>
    #   <expr "lhs"><SYMBOL_FUNCTION_CALL>name</SYMBOL_FUNCTION_CALL></expr>
    #   <LEFT_PAREN>(</LEFT_PAREN>
    #   <expr "arg1"> ... </expr>
    #   ...
    #   <RIGHT_PAREN>)</RIGHT_PAREN>
    # </expr>
    ## navigate up two steps from 'test' SYMBOL_FUNCTION_CALL to the overall 'expr' for the call
    test_calls = pd[
      pd[
        pd[token == 'SYMBOL_FUNCTION_CALL' & text == 'test'],
        list(call_lhs_id=id, call_id=x.parent),
        on=c(id='parent')],
      list(line1, id),
      on=c(id='call_id')]
    ## all the arguments for each call to test()
    test_call_args = test_calls[pd[token == 'expr'], list(call_id=parent, arg_line=i.line1, col1, col2), on=c(id='parent'), nomatch=NULL]
    ## 2nd argument is the num= argument
    test_num_expr = test_call_args[ , .SD[2L], by="call_id"]
    # NB: subtle assumption that 2nd arg to test() is all on one line, true as of 2024-Apr and likely to remain so
    keep_test_ids = test_num_expr[grepl(testPattern, substring(file_lines[arg_line], col1, col2)), call_id]
    # Now find all tests just previous to the keep tests; we want to keep non-test setup lines between them, e.g.
    # test(drop, ...)
    # setup_line1     # retain
    # setup_line2     # retain
    # test(keep, ...) # retain
    intertest_ranges = test_calls[!id %in% keep_test_ids][
      test_calls[id %in% keep_test_ids],
      list(preceding_line=x.line1, test_start_line=i.line1),
      on='line1',
      roll=TRUE]
    # TODO(michaelchirico): this doesn't do well with tests inside control statements.
    #   those could be included by looking for tests with parent!=0, i.e., not-top-level tests,
    #   and including the full parent for such tests. omitting for now until needed.
    keep_lines = intertest_ranges[, sort(unique(unlist(Map(function(l, u) l:u, preceding_line+1L, test_start_line))))]
    header_lines = seq_len(test_calls$line1[1L]-1L)

    tryCatch(error = function(c) warningf("Attempt to subset to %d tests matching '%s' failed, running full suite.", length(keep_test_ids), testPattern), {
      new_script = file_lines[c(header_lines, keep_lines)]
      parse(text = new_script) # as noted above the static approach is not fool-proof (yet?), so force the script to at least parse before continuing.
      fn = tempfile()
      on.exit(unlink(fn), add=TRUE)
      catf("Running %d of %d tests matching '%s'\n", length(keep_test_ids), nrow(test_calls), testPattern)
      writeLines(new_script, fn)
    })
  }
  # nocov end

  err = try(sys.source(fn, envir=env), silent=silent)

  options(oldOptions)
  for (i in oldEnv) {
    if (is.na(oldEnv[i]))
      Sys.unsetenv(names(oldEnv)[i])
    else
      do.call(Sys.setenv, as.list(oldEnv[i])) # nocov
  }
  # Sys.setlocale("LC_CTYPE", oldlocale)
  suppressWarnings(do.call(RNGkind,as.list(oldRNG)))
  # suppressWarnings for the unlikely event that user selected sample='Rounding' themselves before calling test.data.table()

  setNumericRounding(userNumericRounding)  # Restore the user's numeric rounding value

  # Now output platform trace before error (if any) to be sure to always show it; e.g. to confirm endianness in #4099.
  # As one long dense line for cases when 00check.log only shows the last 13 lines of log; to only use up one
  # of those 13 line and give a better chance of seeing more of the output before it. Having said that, CRAN
  # does show the full file output these days, so the 13 line limit no longer bites so much. It still bit recently
  # when receiving output of R CMD check sent over email, though.
  tz = Sys.getenv("TZ", unset=NA)
  cat("\n", date(),   # so we can tell exactly when these tests ran on CRAN to double-check the result is up to date
    "  endian==", .Platform$endian,
    ", sizeof(long double)==", .Machine$sizeof.longdouble,
    ", longdouble.digits==", .Machine$longdouble.digits, # 64 normally, 53 for example under valgrind where some high accuracy tests need turning off, #4639
    ", sizeof(pointer)==", .Machine$sizeof.pointer,
    ", TZ==", if (is.na(tz)) "unset" else paste0("'",tz,"'"),
    ", Sys.timezone()=='", suppressWarnings(Sys.timezone()), "'",
    ", Sys.getlocale()=='", Sys.getlocale(), "'",
    ", l10n_info()=='", paste0(names(l10n_info()), "=", l10n_info(), collapse="; "), "'",
    ", getDTthreads()=='", paste(gsub("[ ][ ]+","==",gsub("^[ ]+","",capture.output(invisible(getDTthreads(verbose=TRUE))))), collapse="; "), "'",
    ", .libPaths()==", paste0("'", .libPaths(), "'", collapse = ","),
    ", ", .Call(Cdt_zlib_version),
    "\n", sep="")

  if (inherits(err,"try-error")) {
    # nocov start
    if (silent) return(FALSE)
    stopf("Failed in %s after test %s before the next test() call in %s", timetaken(env$started.at), env$prevtest, fn)
    # the try() above with silent=FALSE will have already printed the error itself
    # nocov end
  }

  nfail = env$nfail
  ntest = env$ntest
  if (nfail > 0L) {
    # nocov start
    stopf(
      ngettext(nfail, "%d error out of %d. Search %s for test number %s. Duration: %s.", "%d errors out of %d. Search %s for test numbers %s. Duration: %s."),
      nfail, ntest, names(fn), toString(env$whichfail), timetaken(env$started.at), domain=NA
    )
    # important to stopf() here, so that 'R CMD check' fails
    # nocov end
  }

  # There aren't any errors, so we can use up 11 lines for the timings table
  time = nTest = RSS = NULL  # to avoid 'no visible binding' note
  timings = env$timings[nTest>0]
  if (!memtest) {
    ans = head(timings[if (dev) -1L else TRUE][order(-time)], 10L)[,RSS:=NULL]   # exclude id 1 in dev as that includes JIT
    if ((x<-sum(timings[["nTest"]])) != ntest) {
      warningf("Timings count mismatch: %d vs %d", x, ntest)  # nocov
    }
    catf("10 longest running tests took %ds (%d%% of %ds)\n", as.integer(tt<-ans[, sum(time)]), as.integer(100*tt/(ss<-timings[,sum(time)])), as.integer(ss))
    print(ans, class=FALSE)
  } else {
    y = head(order(-diff(timings$RSS)), 10L)
    ans = timings[, diff:=c(NA,round(diff(RSS),1))][y+1L][,time:=NULL]  # time is distracting and influenced by gc() calls; just focus on RAM usage here
    catf("10 largest RAM increases (MB); see plot for cumulative effect (if any)\n")
    print(ans, class=FALSE)
    get("dev.new")(width=14, height=7)
    get("par")(mfrow=c(1,2))
    get("plot")(timings$RSS, main=paste(basename(fn),"\nylim[0]=0 for context"), ylab="RSS (MB)", ylim=c(0,max(timings$RSS)))
    get("mtext")(lastRSS<-as.integer(ceiling(last(timings$RSS))), side=4, at=lastRSS, las=1, font=2)
    get("plot")(timings$RSS, main=paste(basename(fn),"\nylim=range for inspection"), ylab="RSS (MB)")
    get("mtext")(lastRSS, side=4, at=lastRSS, las=1, font=2)
  }

  catf("All %d tests (last %.8g) in %s completed ok in %s\n", ntest, env$prevtest, names(fn), timetaken(env$started.at))
  ans = nfail==0L
  attr(ans, "timings") = timings  # as attr to not upset callers who expect a TRUE/FALSE result
  invisible(ans)
}

# nocov start
compactprint = function(DT, topn=2L) {
  classes = classes1(DT)
  classes[classes == "integer64"] = "i64"
  classes = substr(classes, 1L, 3L)
  makeString = function(x) paste(x, collapse = ",")  # essentially toString.default
  cn = paste0(" [Key=", makeString(key(DT)),
             " Types=", makeString(substr(vapply_1c(DT, typeof), 1L, 3L)),
             " Classes=", makeString(classes), "]")
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

gc_mem = function() {
  # nocov start
  # gc reports memory in MB
  m = colSums(gc()[, c(2L, 4L, 6L)])
  names(m) = c("GC_used", "GC_gc_trigger", "GC_max_used")
  m
  # nocov end
}

test = function(num,x,y=TRUE,error=NULL,warning=NULL,message=NULL,output=NULL,notOutput=NULL,ignore.warning=NULL,options=NULL,env=NULL) {
  if (!is.null(env)) {
    old = Sys.getenv(names(env), names=TRUE, unset=NA)
    to_unset = !lengths(env)
    # NB: Sys.setenv() (no arguments) errors
    if (!all(to_unset)) do.call(Sys.setenv, as.list(env[!to_unset]))
    Sys.unsetenv(names(env)[to_unset])
    # TODO(R>=4.0.2): Use add=TRUE up-front in on.exit() once non-positional arguments are supported.
    on.exit({
      is_preset = !is.na(old)
      if (any(is_preset)) do.call(Sys.setenv, as.list(old[is_preset]))
      Sys.unsetenv(names(old)[!is_preset])
    }, add=TRUE)
  }
  if (!is.null(options)) {
    old_options <- do.call(base::options, as.list(options)) # as.list(): allow passing named character vector for convenience
    on.exit(base::options(old_options), add=TRUE)
  }
  # Usage:
  # i) tests that x equals y when both x and y are supplied, the most common usage
  # ii) tests that x is TRUE when y isn't supplied
  # iii) if error is supplied, y should be missing and x is tested to result in an error message matching the pattern
  # iv) if warning is supplied, y is checked to equal x, and x should result in a warning message matching the pattern
  # v) if output is supplied, x is evaluated and printed and the output is checked to match the pattern
  # num just needs to be numeric and unique. We normally increment integers at the end, but inserts can be made using decimals e.g. 10,11,11.1,11.2,12,13,...
  # num=0 to escape global failure tracking so we can test behaviour of test function itself: test(1.1, test(0, TRUE, FALSE), FALSE, output="1 element mismatch")
  # Motivations:
  # 1) we'd like to know all tests that fail not just stop at the first. This often helps by revealing a common feature across a set of
  #    failing tests
  # 2) test() tests more deeply than a diff on console output and uses a data.table appropriate definition of "equals" different
  #    from all.equal and different to identical related to row.names and unused factor levels
  # 3) each test has a unique id which we refer to in commit messages, emails etc.
  # 4) test that a query generates exactly 2 warnings, that they are both the correct warning messages, and that the result is the one expected
  nfail = get0("nfail", parent.frame()) # test() can be used inside functions defined in tests.Rraw, so inherits=TRUE (default) here
  .test.data.table = !is.null(nfail)
  numStr = sprintf("%.8g", num)
  if (.test.data.table) {
    prevtest = get("prevtest", parent.frame())
    whichfail = get("whichfail", parent.frame())
    assign("ntest", get("ntest", parent.frame()) + if (num>0) 1L else 0L, parent.frame(), inherits=TRUE)   # bump number of tests run
    lasttime = get("lasttime", parent.frame())
    timings = get("timings", parent.frame())
    memtest = get("memtest", parent.frame())
    memtest.id = get("memtest.id", parent.frame())
    filename = get("filename", parent.frame())
    foreign = get("foreign", parent.frame())
    showProgress = get("showProgress", parent.frame())
    time = nTest = RSS = NULL  # to avoid 'no visible binding' note
    # TODO(R>=4.0.2): Use add=TRUE up-front in on.exit() once non-positional arguments are supported.
    if (num>0) on.exit({
       took = proc.time()[3L]-lasttime  # so that prep time between tests is attributed to the following test
       timings[as.integer(num), `:=`(time=time+took, nTest=nTest+1L), verbose=FALSE]
       if (memtest) {
         if (memtest==1L) gc()  # see #5515 for before/after
         inum = as.integer(num)
         timings[inum, RSS:=max(rss(),RSS), verbose=FALSE]  # TODO prefix inum with .. for clarity when that works
         if (length(memtest.id) && memtest.id[1L]<=inum && inum<=memtest.id[2L]) cat(rss(),"\n") # after 'testing id ...' output; not using between() as it has verbose output when getOption(datatable.verbose)
         if (memtest==2L) gc()
       }
       assign("lasttime", proc.time()[3L], parent.frame(), inherits=TRUE)  # after gc() to exclude gc() time from next test when memtest
    }, add=TRUE )
    if (showProgress)
      # \r can't be in gettextf msg
      cat("\rRunning test id", numStr, "         ")   # nocov.
    # See PR #4090 for comments about change here in Dec 2019.
    # If a segfault error occurs in future and we'd like to know after which test, then arrange for the
    # try(sys.source()) in test.data.table() to be run in a separate R process. That process could write out
    # prevtest to a temp file so we know where it got to from this R process. That should be more reliable
    # than what we were doing before which was for test() to always write its test number to output (which might
    # not be flushed to the output upon segfault, depending on OS).
  } else {
    # not `test.data.table` but developer running tests manually; i.e. `cc(F); test(...)`
    memtest = 0L             # nocov
    filename = NA_character_ # nocov
    foreign = FALSE          # nocov ; assumes users of 'cc(F); test(...)' has LANGUAGE=en
    showProgress = FALSE     # nocov
  }
  if (!missing(error) && !missing(y))
    stopf("Test %s is invalid: when error= is provided it does not make sense to pass y as well", numStr)  # nocov

  string_match = function(x, y, ignore.case=FALSE) {
    length(grep(x, y, fixed=TRUE)) ||  # try treating x as literal first; useful for most messages containing ()[]+ characters
    length(tryCatch(grep(x, y, ignore.case=ignore.case), error=function(e)NULL))  # otherwise try x as regexp
  }

  xsub = substitute(x)
  ysub = substitute(y)

  actual = list2env(list(warning=NULL, error=NULL, message=NULL))
  wHandler = function(w) {
    # Thanks to: https://stackoverflow.com/a/4947528/403310
    actual$warning <- c(actual$warning, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
  eHandler = function(e) {
    actual$error <- conditionMessage(e)
    e
  }
  mHandler = function(m) {
    actual$message <- c(actual$message, conditionMessage(m))
    m
  }
  if (is.null(output) && is.null(notOutput)) {
    x = suppressMessages(withCallingHandlers(tryCatch(x, error=eHandler), warning=wHandler, message=mHandler))
    # save the overhead of capture.output() since there are a lot of tests, often called in loops
    # Thanks to tryCatch2 by Jan here : https://github.com/jangorecki/logR/blob/master/R/logR.R#L21
  } else {
    out = capture.output(print(x <- suppressMessages(withCallingHandlers(tryCatch(x, error=eHandler), warning=wHandler, message=mHandler))))
  }
  fail = FALSE
  if (.test.data.table && num>0) {
    if (num<prevtest+0.0000005) {
      # nocov start
      catf("Test id %s is not in increasing order\n", numStr)
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
      stopifnot(is.character(ignore.warning), !anyNA(ignore.warning), nchar(ignore.warning)>=1L)
      for (msg in ignore.warning) observed = grep(msg, observed, value=TRUE, invert=TRUE) # allow multiple for translated messages rather than relying on '|' to always work
    }
    if (length(expected) != length(observed)) {
      # nocov start
      catf("Test %s produced %d %ss but expected %d\n%s\n%s\n", numStr, length(observed), type, length(expected), paste("Expected:", expected), paste("Observed:", observed, collapse = "\n"))
      fail = TRUE
      # nocov end
    } else {
      # the expected type occurred and, if more than 1 of that type, in the expected order
      for (i in seq_along(expected)) {
        if (!foreign && !string_match(expected[i], observed[i])) {
          # nocov start
          catf("Test %s didn't produce the correct %s:\nExpected: %s\nObserved: %s\n", numStr, type, expected[i], observed[i])
          fail = TRUE
          # nocov end
        }
      }
    }
  }
  if (fail && exists("out",inherits=FALSE)) {
    # nocov start
    catf("Output captured before unexpected warning/error/message:\n")
    writeLines(out)
    # nocov end
  }
  if (!fail && !length(error) && (length(output) || length(notOutput))) {
    if (out[length(out)] == "NULL") out = out[-length(out)]
    out = paste(out, collapse="\n")
    output = paste(output, collapse="\n")  # so that output= can be either a \n separated string, or a vector of strings.
    if (length(output) && !string_match(output, out)) {
      # nocov start
      catf("Test %s did not produce correct output:\n", numStr)
      catf("Expected: <<%s>>\n", encodeString(output))  # \n printed as '\\n' so the two lines of output can be compared vertically
      catf("Observed: <<%s>>\n", encodeString(out))
      if (anyNonAscii(output) || anyNonAscii((out))) {
        catf("Expected (raw): <<%s>>\n", paste(charToRaw(output), collapse = " "))
        catf("Observed (raw): <<%s>>\n", paste(charToRaw(out), collapse = " "))
      }
      fail = TRUE
      # nocov end
    }
    if (length(notOutput) && string_match(notOutput, out, ignore.case=TRUE)) {
      # nocov start
      catf("Test %s produced output but should not have:\n", numStr)
      catf("Expected absent (case insensitive): <<%s>>\n", encodeString(notOutput))
      catf("Observed: <<%s>>\n", encodeString(out))
      if (anyNonAscii(notOutput) || anyNonAscii((out))) {
        catf("Expected absent (raw): <<%s>>\n", paste(charToRaw(notOutput), collapse = " "))
        catf("Observed (raw): <<%s>>\n", paste(charToRaw(out), collapse = " "))
      }
      fail = TRUE
      # nocov end
    }
  }
  if (!fail && !length(error) && (!length(output) || !missing(y))) {   # TODO test y when output=, too
    capture.output(y <- try(y, silent=TRUE)) # y might produce verbose output, just toss it
    if (identical(x,y)) return(invisible(TRUE))
    all.equal.result = TRUE
    if (is.data.frame(x) && is.data.frame(y)) {
      if ((is.data.table(x) && !selfrefok(x)) || (is.data.table(y) && !selfrefok(y))) {
        # nocov start
        catf("Test %s ran without errors but selfrefok(%s) is FALSE\n", numStr, if (selfrefok(x)) "y" else "x")
        fail = TRUE
        # nocov end
      } else {
        xc=copy(x)
        yc=copy(y)  # so we don't affect the original data which may be used in the next test
        # drop unused levels in factors
        if (length(x)) for (i in which(vapply_1b(x,is.factor))) {.xi=x[[i]];xc[[i]]<-factor(.xi)}
        if (length(y)) for (i in which(vapply_1b(y,is.factor))) {.yi=y[[i]];yc[[i]]<-factor(.yi)}
        if (is.data.table(xc)) setattr(xc,"row.names",NULL)  # for test 165+, i.e. x may have row names set from inheritance but y won't, consider these equal
        if (is.data.table(yc)) setattr(yc,"row.names",NULL)
        setattr(xc,"index",NULL)   # too onerous to create test RHS with the correct index as well, just check result
        setattr(yc,"index",NULL)
        setattr(xc,".internal.selfref",NULL)   # test 2212
        setattr(yc,".internal.selfref",NULL)
        if (identical(xc,yc) && identical(key(x),key(y))) return(invisible(TRUE))  # check key on original x and y because := above might have cleared it on xc or yc
        if (isTRUE(all.equal.result<-all.equal(xc,yc,check.environment=FALSE)) && identical(key(x),key(y)) &&
                                                     # ^^ to pass tests 2022.[1-4] in R-devel from 5 Dec 2020, #4835
          identical(vapply_1c(xc,typeof), vapply_1c(yc,typeof))) return(invisible(TRUE))
      }
    }
    if (is.atomic(x) && is.atomic(y) && isTRUE(all.equal.result<-all.equal(x,y,check.names=!isTRUE(y))) && typeof(x)==typeof(y)) return(invisible(TRUE))
    # For test 617 on r-prerel-solaris-sparc on 7 Mar 2013
    # nocov start
    if (!fail) {
      catf("Test %s ran without errors but failed check that x equals y:\n", numStr)
      failPrint = function(x, xsub) {
        cat(">", substitute(x), "=", xsub, "\n")
        if (is.data.table(x)) compactprint(x) else {
          nn = length(x)
          catf("First %d of %d (type '%s'): \n", min(nn, 6L), length(x), typeof(x))
          # head.matrix doesn't restrict columns
          if (length(d <- dim(x))) do.call(`[`, c(list(x, drop = FALSE), lapply(pmin(d, 6L), seq_len)))
          else print(head(x))
          if (typeof(x) == 'character' && anyNonAscii(x)) {
            cat("Non-ASCII string detected, raw representation:\n")
            print(lapply(head(x), charToRaw))
          }
        }
      }
      failPrint(x, deparse(xsub))
      failPrint(y, deparse(ysub))
      if (!isTRUE(all.equal.result)) cat(all.equal.result, sep="\n")
      fail = TRUE
    }
    # nocov end
  }
  if (fail && .test.data.table && num>0) {
    # nocov start
    assign("nfail", nfail+1L, parent.frame(), inherits=TRUE)
    assign("whichfail", c(whichfail, numStr), parent.frame(), inherits=TRUE)
    # nocov end
  }
  invisible(!fail)
}

anyNonAscii = function(x) anyNA(iconv(x, to="ASCII")) # nocov
