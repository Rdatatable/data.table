
test.data.table = function() {
    if (exists("test.data.table",.GlobalEnv)) {
        # package developer
        d = path.expand("~/R/datatable/pkg/tests")
        assign(".devtesting",TRUE,envir=.GlobalEnv)
        # ggplot takes over 10 seconds for the two 2 simple tests and slows me down
        # since I run test.data.table from my cc() funcion and run it a lot. The
        # ggplot tests will be run by R CMD check only.
    } else {
        # user
        d = getNamespaceInfo("data.table","path")
    }
    for (fn in dir(d,"*.[rR]$",full=TRUE)) {
        cat("Running",fn,"\n")
        source(fn)
    }
    invisible()
}


