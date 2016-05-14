.onAttach <- function(libname, pkgname) {
    # Runs when attached to search() path such as by library() or require()
    if (interactive()) {
        v = packageVersion("data.table")
        d = read.dcf(system.file("DESCRIPTION", package="data.table"))[,"Packaged"]
        dev = as.integer(v[1,3])%%2 == 1  # version number odd
        packageStartupMessage("data.table ", v, if(dev) paste0(" IN DEVELOPMENT built ", d))
        if (dev && (Sys.Date() - as.Date(d))>28) packageStartupMessage("**********\nThis development version of data.table was built more than 4 weeks ago. Please update.\n**********")
        packageStartupMessage('For help type ?data.table or https://github.com/Rdatatable/data.table/wiki')
        packageStartupMessage('The fastest way to learn (by data.table authors): https://www.datacamp.com/courses/data-analysis-the-data-table-way')
    }
}

