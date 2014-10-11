.onAttach <- function(libname, pkgname) {
    # Runs when attached to search() path such as by library() or require()
    if (interactive()) {
        packageStartupMessage('data.table ',as.character(packageVersion("data.table")),'  For help type: ?data.table')
        packageStartupMessage('*** NB: by=.EACHI is now explicit. See README to restore previous behaviour.')
    }
}

