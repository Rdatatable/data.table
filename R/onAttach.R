# nocov start

.onAttach = function(libname, pkgname) {
  # Runs when attached to search() path such as by library() or require()
  if (!interactive()) return()
  v = packageVersion("data.table")
  d = read.dcf(system.file("DESCRIPTION", package="data.table"), fields = c("Packaged", "Built", "Revision"))
  if (is.na(d[1L])) {
    if (is.na(d[2L])) {
      return() # Packaged, Built fields does not exists
    } else {
      d = unlist(strsplit(d[2L], split="; "))[3L]
      g = if (is.na(d[3L])) "" else paste0(" (",d[3L],")")
    }
  } else {
      d = d[1L]
      g = ""
  }
  dev = as.integer(v[1L, 3L]) %% 2L == 1L  # version number odd => dev
  if (!isTRUE(getOption("datatable.quiet"))) {   # new option in v1.12.4, #3489
    packageStartupMessage("data.table ", v, if(dev)paste0(" IN DEVELOPMENT built ",d,g),
                          " using ", getDTthreads(verbose=FALSE), " threads (see ?getDTthreads).  Latest news: r-datatable.com")
    if (dev && (Sys.Date() - as.Date(d))>28L)
      packageStartupMessage("**********\nThis development version of data.table was built more than 4 weeks ago. Please update: data.table::update.dev.pkg()\n**********")
    if (!.Call(ChasOpenMP))
      packageStartupMessage("**********\nThis installation of data.table has not detected OpenMP support. It should still work but in single-threaded mode.",
        " If this is a Mac, please ensure you are using R>=3.4.0 and have followed our Mac instructions here: https://github.com/Rdatatable/data.table/wiki/Installation.",
        " This warning message should not occur on Windows or Linux. If it does, please file a GitHub issue.\n**********")
  }
}

# nocov end
