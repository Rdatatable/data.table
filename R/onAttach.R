.onAttach <- function(libname, pkgname) {
  # Runs when attached to search() path such as by library() or require()
  if (interactive()) {
    v = packageVersion("data.table")
    d = read.dcf(system.file("DESCRIPTION", package="data.table"), fields = c("Packaged", "Built"))
    if(is.na(d[1])){
      if(is.na(d[2])){
        return() #neither field exists
      } else{
        d = unlist(strsplit(d[2], split="; "))[3]
      }
    } else {
      d = d[1]
    }
      
    dev = as.integer(v[1,3])%%2 == 1  # version number odd
    packageStartupMessage("data.table ", v, if(dev) paste0(" IN DEVELOPMENT built ", d))
    if (dev && (Sys.Date() - as.Date(d))>28) packageStartupMessage("**********\nThis development version of data.table was built more than 4 weeks ago. Please update.\n**********")
    packageStartupMessage('For help type ?data.table and browse sidebar on http://r-datatable.com')
    packageStartupMessage('The fastest way to learn (by data.table authors): https://www.datacamp.com/courses/data-analysis-the-data-table-way')
    packageStartupMessage('View documentation: browseVignettes("data.table")')
    packageStartupMessage('Test your data.table installation: test.data.table()')
    packageStartupMessage("** By default, unique(), duplicated() and uniqueN() now consider all columns. To restore old default: options(datatable.old.unique.by.key=TRUE)")
    packageStartupMessage("** Subsetting, fwrite() and fsort() are now automatically parallel. To revert to single threaded: setDTthreads(1)")
  }
}

