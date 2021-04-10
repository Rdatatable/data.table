# nocov start

# used to raise message (write to STDERR but not raise warning) once per session only
# in future this will be upgraded to warning, then error, until eventually removed after several years
.pkg.store = new.env()
.pkg.store$.unsafe.done = FALSE
.unsafe.opt = function() {
  if (.pkg.store$.unsafe.done) return(invisible())
  val = getOption("datatable.nomatch")
  if (is.null(val)) return(invisible())  # not set is ideal (it's no longer set in .onLoad)
  if (identical(val, NA) || identical(val, NA_integer_)) return(invisible())  # set to default NA is ok for now; in future possible message/warning asking to remove
  message("The option 'datatable.nomatch' is being used and is not set to the default NA. This option is still honored for now but will be deprecated in future. Please see NEWS for 1.12.4 for detailed information and motivation. To specify inner join, please specify `nomatch=NULL` explicitly in your calls rather than changing the default using this option.")
  .pkg.store$.unsafe.done = TRUE
}

.Last.updated = vector("integer", 1L) # exported variable; number of rows updated by the last := or set(), #1885

.onLoad = function(libname, pkgname) {
  # Runs when loaded but not attached to search() path; e.g., when a package just Imports (not Depends on) data.table
  if (!exists("test.data.table", .GlobalEnv, inherits=FALSE)) {
    # check when installed package is loaded but skip when developing the package with cc()
    dllV = if (is.loaded("CdllVersion",PACKAGE="datatable")) .Call(CdllVersion) else "before 1.12.0"
    #                                              ^^ no dot as this is the name of the dll file, #3282
    RV = packageVersion("data.table")
    if (dllV != RV) {
      dll = if (.Platform$OS.type=="windows") "dll" else "so"
      # https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17478
      stop("The datatable.",dll," version (",dllV,") does not match the package (",RV,"). Please close all R sessions to release the old ",toupper(dll)," and reinstall data.table in a fresh R session. The root cause is that R's package installer can in some unconfirmed circumstances leave a package in a state that is apparently functional but where new R code is calling old C code silently: https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17478. Once a package is in this mismatch state it may produce wrong results silently until you next upgrade the package. Please help by adding precise circumstances to 17478 to move the status to confirmed. This mismatch between R and C code can happen with any package not just data.table. It is just that data.table has added this check.")
    }
    builtUsing = readRDS(system.file("Meta/package.rds",package="data.table"))$Built$R
    if (!identical(base::getRversion()>="4.0.0", builtUsing>="4.0.0")) {
      stop("This is R ", base::getRversion(), " but data.table has been installed using R ",builtUsing,". The major version must match. Please reinstall data.table.")
      # the if(R>=4.0.0) in NAMESPACE when registering S3 methods rbind.data.table and cbind.data.table happens on install; #3968
    }
  }

  # c|rbind S3 dispatch now works in R-devel from Sep 2019; #3948 and FAQ 2.24, and R-devel is 4.0.0 as of now. I wanted to test
  # for the presence of the R fix here rather than hard code a version number. But in this case the S3method registration needs to
  # be conditional too: registering the S3 methods in R before 4.0.0 causes this workaround to no longer work. However, the R
  # syntax available to use in NAMESPACE is very limited (can't call data.table() in it in a capability test, for example).
  # This version number ("4.0.0") must be precisely the same as used in NAMESPACE; see PR for #3948.
  if (base::getRversion() < "4.0.0") {
    # continue to support R<4.0.0
    # If R 3.6.2 (not yet released) includes the c|rbind S3 dispatch fix, then this workaround still works.
    tt = base::cbind.data.frame
    ss = body(tt)
    if (class(ss)[1L]!="{") ss = as.call(c(as.name("{"), ss))
    prefix = if (!missing(pkgname)) "data.table::" else ""  # R provides the arguments when it calls .onLoad, I don't in dev/test
    if (!length(grep("data.table", ss[[2L]], fixed = TRUE))) {
      ss = ss[c(1L, NA, 2L:length(ss))]
      ss[[2L]] = parse(text=paste0("if (!identical(class(..1),'data.frame')) for (x in list(...)) { if (inherits(x,'data.table')) return(",prefix,"data.table(...)) }"))[[1L]]
      body(tt)=ss
      (unlockBinding)("cbind.data.frame",baseenv())
      assign("cbind.data.frame",tt,envir=asNamespace("base"),inherits=FALSE)
      lockBinding("cbind.data.frame",baseenv())
    }
    tt = base::rbind.data.frame
    ss = body(tt)
    if (class(ss)[1L]!="{") ss = as.call(c(as.name("{"), ss))
    if (!length(grep("data.table", ss[[2L]], fixed = TRUE))) {
      ss = ss[c(1L, NA, 2L:length(ss))]
      ss[[2L]] = parse(text=paste0("for (x in list(...)) { if (inherits(x,'data.table')) return(",prefix,".rbind.data.table(...)) }"))[[1L]] # fix for #89
      body(tt)=ss
      (unlockBinding)("rbind.data.frame",baseenv())
      assign("rbind.data.frame",tt,envir=asNamespace("base"),inherits=FALSE)
      lockBinding("rbind.data.frame",baseenv())
    }
  }

  # Set options for the speed boost in v1.8.0 by avoiding 'default' arg of getOption(,default=)
  # In fread and fwrite we have moved back to using getOption's default argument since it is unlikely fread and fread will be called in a loop many times, plus they
  # are relatively heavy functions where the overhead in getOption() would not be noticed.  It's only really [.data.table where getOption default bit.
  # Improvement to base::getOption() now submitted (100x; 5s down to 0.05s):  https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17394
  opts = c("datatable.verbose"="FALSE",        # datatable.<argument name>
       "datatable.optimize"="Inf",             # datatable.<argument name>
       "datatable.print.nrows"="100L",         # datatable.<argument name>
       "datatable.print.topn"="5L",            # datatable.<argument name>
       "datatable.print.class"="FALSE",        # for print.data.table
       "datatable.print.rownames"="TRUE",      # for print.data.table
       "datatable.print.colnames"="'auto'",    # for print.data.table
       "datatable.print.keys"="FALSE",         # for print.data.table
       "datatable.print.trunc.cols"="FALSE",   # for print.data.table
       "datatable.allow.cartesian"="FALSE",    # datatable.<argument name>
       "datatable.dfdispatchwarn"="TRUE",                   # not a function argument
       "datatable.warnredundantby"="TRUE",                  # not a function argument
       "datatable.alloccol"="1024L",           # argument 'n' of alloc.col. Over-allocate 1024 spare column slots
       "datatable.auto.index"="TRUE",          # DT[col=="val"] to auto add index so 2nd time faster
       "datatable.use.index"="TRUE",           # global switch to address #1422
       "datatable.prettyprint.char" = NULL     # FR #1091
       )
  for (i in setdiff(names(opts),names(options()))) {
    eval(parse(text=paste0("options(",i,"=",opts[i],")")))
  }

  if (!is.null(getOption("datatable.old.bywithoutby")))
    warning("Option 'datatable.old.bywithoutby' has been removed as warned for 2 years. It is now ignored. Please use by=.EACHI instead and stop using this option.")
  if (!is.null(getOption("datatable.old.unique.by.key")))
    warning("Option 'datatable.old.unique.by.key' has been removed as warned for 4 years. It is now ignored. Please use by=key(DT) instead and stop using this option.")

  # Test R behaviour that changed in v3.1 and is now depended on
  x = 1L:3L
  y = list(x)
  if (address(x) != address(y[[1L]])) stop("Unexpected base R behaviour: list(x) has copied x")

  DF = data.frame(a=1:3, b=4:6)
  add1 = address(DF$a)
  add2 = address(DF$b)
  names(DF) = c("A","B")
  add3 = address(DF$A)
  add4 = address(DF$B)
  if (add1!=add3 || add2!=add4) stop("Unexpected base R behaviour: names<- has copied column contents")

  DF = data.frame(a=1:3, b=4:6)
  add1 = address(DF$a)
  add2 = address(DF$b)
  add3 = address(DF)
  DF[2L, "b"] = 7  # changed b but not a
  add4 = address(DF$a)
  add5 = address(DF$b)
  add6 = address(DF)
  if (add2==add5) stop("Unexpected base R behaviour: DF[2,2]<- did not copy column 2 which was assigned to")
  if (add1!=add4) stop("Unexpected base R behaviour: DF[2,2]<- copied the first column which was not assigned to, too")

  if (add3==add6) warning("Unexpected base R behaviour: DF[2,2]<- has not copied address(DF)")
  # R could feasibly in future not copy DF's vecsxp in this case. If that changes in R, we'd like to know via the warning
  # because tests will likely break too. The warning will quickly tell R-core and us why, so we can then update.

  .Call(CinitLastUpdated, .Last.updated)  #1885

  invisible()
}

getRversion = function(...) stop("Reminder to data.table developers: don't use getRversion() internally. Add a behaviour test to .onLoad instead.")
# 1) using getRversion() wasted time when R3.0.3beta was released without the changes we expected in getRversion()>"3.0.2".
# 2) R-devel and ourselves may wish to tinker with R-devel, turning on and off features in the same version number. So it's better if data.table doesn't hard code expectations into the version number.
# 3) The discipline of adding a feature test here helps fully understand the change.
# 4) Defining getRversion with a stop() here helps prevent new switches on getRversion() being added in future. Easily circumvented but the point is to issue the message above.

.onUnload = function(libpath) {
  # fix for #474. the shared object name is different from package name
  # So 'detach' doesn't find datatable.so, as it looks by default for data.table.so
  library.dynam.unload("datatable", libpath)
}

# nocov end
