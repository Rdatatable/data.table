".onAttach"=function(libname, pkgname) {
    cat("data.table",as.character(packageVersion("data.table")),"\n")
    cat("Quick start guide : vignette(\"datatable-intro\")\n")
    cat("Homepage : http://datatable.r-forge.r-project.org/\n")
    cat('Help : help("data.table") or ?data.table (includes fast start examples)\n')
}

