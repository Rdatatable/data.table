.onAttach <- function(libname, pkgname) {
  # Runs when attached to search() path such as by library() or require()
  if (interactive()) {
  v = packageVersion("data.table")
  d = read.dcf(system.file("DESCRIPTION", package="data.table"), fields = c("Packaged", "Built"))
  if (is.na(d[1])) {
    if (is.na(d[2])) {
      return() #neither field exists
    } else {
      d = unlist(strsplit(d[2], split="; "))[3]
    }
  } else {
    d = d[1]
  }
  dev = as.integer(v[1,3])%%2 == 1  # version number odd => dev
  packageStartupMessage("data.table ", v, if(dev) paste0(" IN DEVELOPMENT built ", d))
  if (dev && (Sys.Date() - as.Date(d))>28)
    packageStartupMessage("**********\nThis development version of data.table was built more than 4 weeks ago. Please update.\n**********")
  if (!.Call(ChasOpenMP))
    packageStartupMessage("**********\nThis installation of data.table has not detected OpenMP support. It should still work but in single-threaded mode. If this is a Mac, please ensure you are using R>=3.4.0 and have installed the MacOS binary package from CRAN: see ?install.packages, the 'type=' argument and the 'Binary packages' section. If you compiled from source, please reinstall and precisely follow the installation instructions on the data.table homepage. This warning message should not occur on Windows or Linux. If it does and you've followed the installation instructions on the data.table homepage, please file a GitHub issue.\n**********")
  packageStartupMessage('  The fastest way to learn (by data.table authors): https://www.datacamp.com/courses/data-analysis-the-data-table-way')
  packageStartupMessage('  Documentation: ?data.table, example(data.table) and browseVignettes("data.table")')
  packageStartupMessage('  Release notes, videos and slides: http://r-datatable.com')
  }
}


