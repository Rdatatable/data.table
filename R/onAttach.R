.onAttach <- function(libname, pkgname) {
    # Runs when attached to search() path such as by library() or require()
    if (interactive()) {
        packageStartupMessage('data.table ',as.character(packageVersion("data.table")),'  For help type ?data.table or https://github.com/Rdatatable/data.table/wiki')
        packageStartupMessage('The fastest way to learn (by data.table authors): https://www.datacamp.com/courses/data-analysis-the-data-table-way')
    }
}

