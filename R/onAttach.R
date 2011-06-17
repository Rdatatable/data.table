".onAttach"=function(libname, pkgname) {
    if (interactive()) {
        packageStartupMessage("data.table ",as.character(packageVersion("data.table")))
        packageStartupMessage("Quick start guide : vignette(\"datatable-intro\")")
        packageStartupMessage("Homepage : http://datatable.r-forge.r-project.org/")
        packageStartupMessage('Help : help("data.table") or ?data.table (includes fast start examples)')
    }
}

