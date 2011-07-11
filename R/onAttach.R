".onAttach"=function(libname, pkgname) {
    if (interactive()) {
        packageStartupMessage("data.table ",as.character(packageVersion("data.table")))
        packageStartupMessage("Quick start guide : vignette(\"datatable-intro\")")
        packageStartupMessage("Homepage : http://datatable.r-forge.r-project.org/")
        packageStartupMessage('Help : help("data.table") or ?data.table (includes fast start examples)')
        setTimeLimit(elapsed=1,transient=TRUE)
        try({
            # Check if later version is available
            suppressWarnings(download.file("http://c.statcounter.com/7025334/0/7603caf6/0/",tt<-tempfile(),quiet=TRUE,cacheOK=FALSE))
            latestv = readBin(tt,1L,47,size=1,signed=FALSE)[42:47]
            unlink(tt)
            if (!identical(latestv[1:3],latestv[4:6])) return() else latestv=paste(latestv[1:3],collapse=".")
            if (packageVersion("data.table") < latestv) {
                packageStartupMessage("*** Version ",latestv," is released. Please upgrade by typing update.packages() ***")
            }
        },TRUE)
        setTimeLimit()  # transient=TRUE should mean this isn't necessary, but no harm in calling anyway
    }
}

