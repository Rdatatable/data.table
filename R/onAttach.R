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
    if (gettext("TRANSLATION CHECK", domain='R-data.table') != "TRANSLATION CHECK")
      packageStartupMessage(gettext("**********\nRunning data.table in English; package support is available in English only. When searching for online help, be sure to also check for the English error message. This can be obtained by looking at the po/R-<locale>.po and po/<locale>.po files in the package source, where the native language and English error messages can be found side-by-side\n**********", domain="R-data.table"))
    if (dev && (Sys.Date() - as.Date(d))>28L)
      packageStartupMessage("**********\nThis development version of data.table was built more than 4 weeks ago. Please update: data.table::update.dev.pkg()\n**********")
    if (!.Call(ChasOpenMP))
      packageStartupMessage("**********\n",
        "This installation of data.table has not detected OpenMP support. It should still work but in single-threaded mode.\n",
        if (Sys.info()["sysname"]=="Darwin")
          "This is a Mac. Please read https://mac.r-project.org/openmp/. Please engage with Apple and ask them for support. Check r-datatable.com for updates, and our Mac instructions here: https://github.com/Rdatatable/data.table/wiki/Installation. After several years of many reports of installation problems on Mac, it's time to gingerly point out that there have been no similar problems on Windows or Linux."
        else
          paste0("This is ", Sys.info()["sysname"], ". This warning should not normally occur on Windows or Linux where OpenMP is turned on by data.table's configure script by passing -fopenmp to the compiler. If you see this warning on Windows or Linux, please file a GitHub issue."),
        "\n**********")
  }
}

# nocov end
