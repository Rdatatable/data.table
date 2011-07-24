".onAttach"=function(libname, pkgname) {
    if (interactive()) {
        packageStartupMessage('data.table ',as.character(packageVersion("data.table")),' New user? Type help("data.table")')
    }
}

