# nocov start

.pkg.store = new.env()
.pkg.store$.unsafe.done = FALSE
.unsafe.opt = function() {
  if (.pkg.store$.unsafe.done) return(invisible())
  val = getOption("datatable.nomatch")
  if (is.null(val)) return(invisible())  # not defined (it hasn't been defined in .onLoad since v1.12.4)
  warningf("Option 'datatable.nomatch' is defined but is now ignored. Please see note 11 in v1.12.4 NEWS (Oct 2019), and note 14 in v1.14.2.")
  # leave this as warning for a long time
  .pkg.store$.unsafe.done = TRUE
  invisible()
}

.Last.updated = vector("integer", 1L) # exported variable; number of rows updated by the last := or set(), #1885

.onLoad = function(libname, pkgname) {
  session_r_version = base::getRversion()
  # Runs when loaded but not attached to search() path; e.g., when a package just Imports (not Depends on) data.table
  if (!exists("test.data.table", .GlobalEnv, inherits=FALSE)) {
    # check when installed package is loaded but skip when developing the package with cc()
    dllV = if (is.loaded("CdllVersion",PACKAGE="data_table")) .Call(CdllVersion) else "before 1.12.0"
    RV = as.character(packageVersion("data.table"))
    if (dllV != RV) {
      dll = if (.Platform$OS.type=="windows") "dll" else "so"
      # https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17478
      # TODO(R>=4.0.0): Remove or adjust this message once we're sure all users are unaffected
      stopf("The data_table.%s version (%s) does not match the package (%s). Please close all R sessions to release the old %s and reinstall data.table in a fresh R session. Prior to R version 3.6.0 patched, R's package installer could leave a package in an apparently functional state where new R code was calling old C code silently: https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17478. Once a package is in this mismatch state it may produce wrong results silently until you next upgrade the package. This mismatch between R and C code can happen with any package not just data.table. It is just that data.table has added this check.", dll, dllV, RV, toupper(dll))
    }
    builtPath = system.file("Meta", "package.rds", package="data.table")
    if (builtPath != "" && !identical(session_r_version>="4.0.0", (build_r_version <- readRDS(builtPath)$Built$R)>="4.0.0")) {
      stopf("This is R %s but data.table has been installed using R %s. The major version must match. Please reinstall data.table.", session_r_version, build_r_version)
      # the if(R>=4.0.0) in NAMESPACE when registering S3 methods rbind.data.table and cbind.data.table happens on install; #3968
    }
  }

  # c|rbind S3 dispatch now works in R-devel from Sep 2019; #3948 and FAQ 2.24, and R-devel is 4.0.0 as of now. I wanted to test
  # for the presence of the R fix here rather than hard code a version number. But in this case the S3method registration needs to
  # be conditional too: registering the S3 methods in R before 4.0.0 causes this workaround to no longer work. However, the R
  # syntax available to use in NAMESPACE is very limited (can't call data.table() in it in a capability test, for example).
  # This version number ("4.0.0") must be precisely the same as used in NAMESPACE; see PR for #3948.
  if (session_r_version < "4.0.0") {
    # continue to support R<4.0.0
    # If R 3.6.2 (not yet released) includes the c|rbind S3 dispatch fix, then this workaround still works.
    tt = base::cbind.data.frame
    ss = body(tt)
    if (class1(ss) != "{") ss = as.call(c(as.name("{"), ss))
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
    if (class1(ss) != "{") ss = as.call(c(as.name("{"), ss))
    if (!length(grep("data.table", ss[[2L]], fixed = TRUE))) {
      ss = ss[c(1L, NA, 2L:length(ss))]
      ss[[2L]] = parse(text=paste0("for (x in list(...)) { if (inherits(x,'data.table')) return(",prefix,".rbind.data.table(...)) }"))[[1L]] # fix for #89
      body(tt)=ss
      (unlockBinding)("rbind.data.frame",baseenv())
      assign("rbind.data.frame",tt,envir=asNamespace("base"),inherits=FALSE)
      lockBinding("rbind.data.frame",baseenv())
    }
  }
  if (session_r_version < "3.6.0") { # corresponds to S3method() directive in NAMESPACE
    # no delayed registration support for NAMESPACE; perform it manually
    if (isNamespaceLoaded("knitr")) {
      registerS3method("knit_print", "data.table", knit_print.data.table, envir = asNamespace("knitr"))
    }
    setHook(packageEvent("knitr", "onLoad"), function(...) {
      registerS3method("knit_print", "data.table", knit_print.data.table, envir = asNamespace("knitr"))
    })
  }

  # Set options for the speed boost in v1.8.0 by avoiding 'default' arg of getOption(,default=)
  # In fread and fwrite we have moved back to using getOption's default argument since it is unlikely fread and fread will be called in a loop many times, plus they
  # are relatively heavy functions where the overhead in getOption() would not be noticed.  It's only really [.data.table where getOption default bit.
  # Improvement to base::getOption() now submitted (100x; 5s down to 0.05s):  https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17394
  opts = c("datatable.verbose"="FALSE",        # datatable.<argument name>
       "datatable.optimize"="Inf",             # datatable.<argument name>
       "datatable.print.nrows"="100L",         # datatable.<argument name>
       "datatable.print.topn"="5L",            # datatable.<argument name>
       "datatable.print.class"="TRUE",         # for print.data.table
       "datatable.print.rownames"="TRUE",      # for print.data.table
       "datatable.print.colnames"="'auto'",    # for print.data.table
       "datatable.print.keys"="TRUE",          # for print.data.table
       "datatable.print.trunc.cols"="FALSE",   # for print.data.table
       "datatable.show.indices"="FALSE",       # for print.data.table
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

  # Test R behaviour that changed in v3.1 and is now depended on
  x = 1L:3L
  y = list(x)
  if (address(x) != address(y[[1L]])) stopf("Unexpected base R behaviour: list(x) has copied x")

  DF = data.frame(a=1:3, b=4:6)
  add1 = address(DF$a)
  add2 = address(DF$b)
  names(DF) = c("A","B")
  add3 = address(DF$A)
  add4 = address(DF$B)
  if (add1!=add3 || add2!=add4) stopf("Unexpected base R behaviour: names<- has copied column contents")

  DF = data.frame(a=1:3, b=4:6)
  add1 = address(DF$a)
  add2 = address(DF$b)
  add3 = address(DF)
  DF[2L, "b"] = 7  # changed b but not a
  add4 = address(DF$a)
  add5 = address(DF$b)
  add6 = address(DF)
  if (add2==add5) stopf("Unexpected base R behaviour: DF[2,2]<- did not copy column 2 which was assigned to")
  if (add1!=add4) stopf("Unexpected base R behaviour: DF[2,2]<- copied the first column which was not assigned to, too")

  if (add3==add6) warningf("Unexpected base R behaviour: DF[2,2]<- has not copied address(DF)")
  # R could feasibly in future not copy DF's vecsxp in this case. If that changes in R, we'd like to know via the warning
  # because tests will likely break too. The warning will quickly tell R-core and us why, so we can then update.

  .Call(CinitLastUpdated, .Last.updated)  #1885

  invisible()
}

getRversion = function(...) stopf("Reminder to data.table developers: don't use getRversion() internally. Add a behaviour test to .onLoad instead.") # notranslate
# 1) using getRversion() wasted time when R3.0.3beta was released without the changes we expected in getRversion()>"3.0.2".
# 2) R-devel and ourselves may wish to tinker with R-devel, turning on and off features in the same version number. So it's better if data.table doesn't hard code expectations into the version number.
# 3) The discipline of adding a feature test here helps fully understand the change.
# 4) Defining getRversion with a stopf() here helps prevent new switches on getRversion() being added in future. Easily circumvented but the point is to issue the message above.

.onUnload = function(libpath) {
  library.dynam.unload("data_table", libpath)
}

# nocov end
