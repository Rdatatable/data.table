
test.data.table = function() {
    if (exists("test.data.table",.GlobalEnv,inherits=FALSE)) {
        # package developer
        if ("package:data.table" %in% search()) stop("data.table package loaded")
        d = path.expand("~/R/datatable/pkg/inst/tests")
        assign(".devtesting",TRUE,envir=.GlobalEnv)
    } else {
        # user
        d = paste(getNamespaceInfo("data.table","path"),"/tests",sep="")
    }
    # for (fn in dir(d,"*.[rR]$",full=TRUE)) {
    for (fn in file.path(d, 'tests.R')) {
        cat("Running",fn,"\n")
        sys.source(fn,envir=new.env(parent=.GlobalEnv))
        # the new.env() is required for when a *user* runs test.data.table() because
        # the context of this function is sealed in the namespace w.r.t S4.
        # It is also tidier to protect the tests from the variable 'd' above.
    }
    invisible()
}


## Tests that two data.tables (`target` and `current`) are equivalent.
## This method is used primarily to make life easy with a testing harness
## built around test_that. A call to test_that::{expect_equal|equal} will
## ultimately dispatch to this method when making an "equality" call.
all.equal.data.table <- function(target, current, trim.levels=TRUE, ...) {
    force(target)
    force(current)
    if (trim.levels) {
        ## drop unused levels
        if (length(target)) {
            for (i in which(sapply(target, is.factor))) {
                target[[i]] <- factor(target[[i]])
            }
        }
        if (length(current)) {
            for (i in which(sapply(current, is.factor))) {
                current[[i]] <- factor(current[[i]])
            }
        }
    }

    ## Trim any extra row.names attributes that came from some inheritence
    if (length(attr(target, "row.names"))) {
        setattr(target, "row.names", NULL)
    }
    if (length(attr(current, "row.names"))) {
        setattr(current, "row.names", NULL)
    }
    
    all.equal.list(target, current, ...)
}
