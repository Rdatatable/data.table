
test.data.table = function() {
    if (exists("test.data.table",.GlobalEnv)) {
        # package developer
        d = path.expand("~/R/datatable/pkg/tests")
        assign(".devtesting",TRUE,envir=.GlobalEnv)
    } else {
        # user
        d = getNamespaceInfo("data.table","path")
    }
    for (fn in dir(d,"*.[rR]$",full=TRUE)) {
        cat("Running",fn,"\n")
        source(fn,local=TRUE)
    }
    invisible()
}


