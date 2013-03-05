
test.data.table = function(verbose=FALSE) {
    if (exists("test.data.table",.GlobalEnv,inherits=FALSE)) {
        # package developer
        if ("package:data.table" %in% search()) stop("data.table package loaded")
        if (.Platform$OS.type == "unix")
            d = path.expand("~/R/datatable/pkg/inst/tests")
        else {
            if (!"pkg" %in% dir()) stop("'pkg' not in dir()") 
            d = paste0(getwd(),"/pkg/inst/tests")
        }
    } else {
        # user
        d = paste(getNamespaceInfo("data.table","path"),"/tests",sep="")
    }
    # for (fn in dir(d,"*.[rR]$",full=TRUE)) {  # testthat runs those
    oldenc = options(encoding="UTF-8")[[1L]]  # just for tests 708-712 on Windows
    # TO DO: reinstate solution for C locale of CRAN's Mac (R-Forge's Mac is ok)
    # oldlocale = Sys.getlocale("LC_CTYPE")
    # Sys.setlocale("LC_CTYPE", "")   # just for CRAN's Mac to get it off C locale (post to r-devel on 16 Jul 2012)
    olddir = setwd(d)
    on.exit(setwd(olddir))
    for (fn in file.path(d, 'tests.Rraw')) {    # not testthat
        cat("Running",fn,"\n")
        oldverbose = getOption("datatable.verbose")
        if (verbose) options(datatable.verbose=TRUE)
        sys.source(fn,envir=new.env(parent=.GlobalEnv))  # using source(local=) would break 2.12 compatibility
        options(data.table.verbose=oldverbose)
        # the new.env() is required for when a *user* runs test.data.table() because
        # the context of this function is sealed in the namespace w.r.t S4.
        # It is also tidier to protect the tests from the variable 'd' above.
        
        # As from v1.7.2, testthat doesn't run the tests.Rraw (hence file name change to .Rraw).
        # There were environment issues with system.time() (when run by test_package) that only
        # showed up when CRAN maintainers tested on 64bit. Matthew spent a long time including
        # testing on 64bit in Amazon EC2. Solution was simply to not run the tests.R from
        # testthat, which probably makes sense anyway to speed it up a bit (was running twice
        # before).
    }
    options(encoding=oldenc)
    # Sys.setlocale("LC_CTYPE", oldlocale)
    invisible()
}


## Tests that two data.tables (`target` and `current`) are equivalent.
## This method is used primarily to make life easy with a testing harness
## built around test_that. A call to test_that::{expect_equal|equal} will
## ultimately dispatch to this method when making an "equality" call.
all.equal.data.table <- function(target, current, trim.levels=TRUE, ...) {
    target = copy(target)
    current = copy(current)
    if (trim.levels) {
        ## drop unused levels
        if (length(target)) {
            for (i in which(sapply(target, is.factor))) {
                .xi = factor(target[[i]])
                target[,i:=.xi,with=FALSE]
            }
        }
        if (length(current)) {
            for (i in which(sapply(current, is.factor))) {
                .xi = factor(current[[i]])
                current[,i:=.xi,with=FALSE]
            }
        }
    }

    ## Trim any extra row.names attributes that came from some inheritence
    setattr(target, "row.names", NULL)
    setattr(current, "row.names", NULL)
    
    # all.equal uses unclass which doesn't know about external pointers; there
    # doesn't seem to be all.equal.externalptr method in base.
    setattr(target, ".internal.selfref", NULL)
    setattr(current, ".internal.selfref", NULL)
    
    all.equal.list(target, current, ...)
}



