".onAttach"=function(libname, pkgname) {
    if (interactive()) {
        packageStartupMessage('data.table ',as.character(packageVersion("data.table")),'  For help type: help("data.table")')
    }
    assignInNamespace("cbind.data.frame",.cbind.data.frame,"base")
}

