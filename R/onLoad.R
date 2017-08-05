.onLoad <- function(libname, pkgname) {
    # Runs when loaded but not attached to search() path; e.g., when a package just Imports (not Depends on) data.table
    
    "Please read FAQ 2.23 (vignette('datatable-faq')) which explains in detail why data.table adds one for loop to the start of base::cbind.data.frame and base::rbind.data.frame. If there is a better solution we will gladly change it."
    # Commented as a character string so this message is retained and seen by anyone who types data.table:::.onLoad
    tt = base::cbind.data.frame
    ss = body(tt)
    if (class(ss)!="{") ss = as.call(c(as.name("{"), ss))
    prefix = if (!missing(pkgname)) "data.table::" else ""  # R provides the arguments when it calls .onLoad, I don't in dev/test
    if (!length(grep("data.table",ss[[2]]))) {
        ss = ss[c(1,NA,2:length(ss))]
        ss[[2]] = parse(text=paste("if (!identical(class(..1),'data.frame')) for (x in list(...)) { if (inherits(x,'data.table')) return(",prefix,"data.table(...)) }",sep=""))[[1]]
        body(tt)=ss
        (unlockBinding)("cbind.data.frame",baseenv())
        assign("cbind.data.frame",tt,envir=asNamespace("base"),inherits=FALSE)
        lockBinding("cbind.data.frame",baseenv())
    }
    tt = base::rbind.data.frame
    ss = body(tt)
    if (class(ss)!="{") ss = as.call(c(as.name("{"), ss))
    if (!length(grep("data.table",ss[[2]]))) {
        ss = ss[c(1,NA,2:length(ss))]
        ss[[2]] = parse(text=paste("for (x in list(...)) { if (inherits(x,'data.table')) return(",prefix,".rbind.data.table(...)) }",sep=""))[[1]] # fix for #4995
        body(tt)=ss
        (unlockBinding)("rbind.data.frame",baseenv())
        assign("rbind.data.frame",tt,envir=asNamespace("base"),inherits=FALSE)
        lockBinding("rbind.data.frame",baseenv())
    }
    # Set options for the speed boost in v1.8.0 by avoiding 'default' arg of getOption(,default=)
    opts = c("datatable.verbose"="FALSE",            # datatable.<argument name>
             "datatable.nomatch"="NA_integer_",      # datatable.<argument name>
             "datatable.optimize"="Inf",             # datatable.<argument name>
             "datatable.print.nrows"="100L",         # datatable.<argument name>
             "datatable.print.topn"="5L",            # datatable.<argument name>
             "datatable.print.class"="FALSE",        # for print.data.table
             "datatable.print.rownames"="TRUE",      # for print.data.table
             "datatable.allow.cartesian"="FALSE",    # datatable.<argument name>
             "datatable.dfdispatchwarn"="TRUE",                   # not a function argument
             "datatable.warnredundantby"="TRUE",                  # not a function argument
             "datatable.alloccol"="1024L",           # argument 'n' of alloc.col. Over-allocate 1024 spare column slots
             "datatable.integer64"="'integer64'",    # datatable.<argument name>    integer64|double|character
             "datatable.auto.index"="TRUE",          # DT[col=="val"] to auto add index so 2nd time faster
             "datatable.use.index"="TRUE",           # global switch to address #1422
             "datatable.fread.datatable"="TRUE",
             "datatable.prettyprint.char" = NULL,     # FR #1091
             "datatable.old.unique.by.key" = "FALSE"  # TODO: warn 1 year, remove after 2 years
             )
    for (i in setdiff(names(opts),names(options()))) {
        eval(parse(text=paste("options(",i,"=",opts[i],")",sep="")))
    }
    
    if (!is.null(getOption("datatable.old.bywithoutby")))
        warning("Option 'datatable.old.bywithoutby' has been removed as warned for 2 years. It is now ignored. Please use by=.EACHI instead and stop using this option.")
    
    # reshape2
    # Tried this :
    # if (!"package:reshape2" %in% search()) {
    #   # temporary until reshape2 pull request to make generic is on CRAN ...
    #   try(library(reshape2, pos="package:base", quietly=TRUE, warn.conflicts=FALSE), silent=TRUE)
    # }
    # which works. But then when melt in data.table is loaded, _that's_ what generates the mask message.
    # There's also a NOTE: Package startup functions should not change the search path.
    # Therefore, removed. Users will need to make sure reshape2 isn't loaded, or loaded behind data.table on search()
    
    # Test R behaviour ...
    
    x = 1:3
    y = list(x)
    .R.listCopiesNamed <<- (address(x) != address(y[[1]]))   # FALSE from R 3.1
    
    DF = data.frame(a=1:3, b=4:6)
    add1 = address(DF$a)
    add2 = address(DF$b)
    names(DF) = c("A","B")
    add3 = address(DF$A)
    add4 = address(DF$B)
    .R.assignNamesCopiesAll <<- add1 != add3                 # FALSE from R 3.1
    if ((add1 == add3) != (add2 == add4)) stop("If one column is copied surely the other should be as well, when checking .R.assignNamesCopiesAll")
    
    DF = data.frame(a=1:3, b=4:6)
    add1 = address(DF$a)
    add2 = address(DF)
    DF[2,"b"] = 7  # changed b but not a
    add3 = address(DF$a)
    add4 = address(DF)
    .R.subassignCopiesOthers <<- add1 != add3                # FALSE from R 3.1
    .R.subassignCopiesVecsxp <<- add2 != add4                # currently TRUE in R 3.1, but could feasibly change
    
    invisible()
}

# Switch on these variables instead of getRversion(). Set to TRUE just to create them as a single logical. They are set by .onLoad() above.
.R.listCopiesNamed = TRUE
.R.assignNamesCopiesAll = TRUE
.R.subassignCopiesOthers = TRUE
.R.subassignCopiesVecsxp = TRUE

getRversion <- function(...) stop("Reminder to data.table developers: don't use getRversion() internally. Add a behaviour test to .onLoad instead.")
# 1) using getRversion() wasted time when R3.0.3beta was released without the changes we expected in getRversion()>"3.0.2".
# 2) R-devel and ourselves may wish to tinker with R-devel, turning on and off features in the same version number. So it's better if data.table doesn't hard code expectations into the version number.
# 3) The discipline of adding a feaure test here helps fully understand the change.
# 4) Defining getRversion with a stop() here helps prevent new switches on getRversion() being added in future. Easily circumvented but the point is to issue the message above.

.onUnload <- function(libpath) {
    # fix for #474. the shared object name is different from package name
    # So 'detach' doesn't find datatable.so, as it looks by default for data.table.so
    library.dynam.unload("datatable", libpath)
}
