# Matt's ~/.Rprofile is a link to this file at ~/GitHub/data.table/.dev/.Rprofile

# options(repos = c(CRAN="http://cran.stat.ucla.edu"))
# options(repos = c(CRAN=c("http://cran.stat.ucla.edu", "http://cloud.r-project.org")))  # both needed for revdep checks sometimes
options(repos = c(CRAN="http://cloud.r-project.org"))

options(help_type="html")
options(error=quote(dump.frames()))
options(width=200)
options(digits.secs=3)  # for POSIXct to print milliseconds
suppressWarnings(RNGversion("3.5.0"))  # so when I create tests in dev there isn't a mismatch when run by cc()

Sys.setenv(PROJ_PATH=path.expand("~/GitHub/data.table"))
source(paste0(Sys.getenv("PROJ_PATH"),"/.dev/cc.R"))
