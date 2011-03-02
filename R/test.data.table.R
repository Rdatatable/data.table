
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
    for (fn in dir(d,"*.[rR]$",full=TRUE)) {
        cat("Running",fn,"\n")
        sys.source(fn,envir=new.env(parent=.GlobalEnv))
        # the new.env() is required for when a *user* runs test.data.table() because
        # the context of this function is sealed in the namespace w.r.t S4.
        # It is also tidier to protect the tests from the variable 'd' above.
    }
    invisible()
}


