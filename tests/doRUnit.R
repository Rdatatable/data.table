#### doRUnit.R --- Run RUnit tests
####------------------------------------------------------------------------

### Origianlly follows Gregor Gojanc's example in CRAN package  'gdata'
### and the corresponding section in the R Wiki:
###  http://wiki.r-project.org/rwiki/doku.php?id=developers:runit

### MM: Vastly changed:  This should also be "runnable" for *installed*
##              package which has no ./tests/
## ----> put the bulk of the code e.g. in  ../inst/unitTests/runTests.R :

if(require("RUnit", quietly = TRUE)) {

  ## --- Setup ---

  wd <- getwd()
  pkg <- "data.table" # sub("\\.Rcheck$", '', basename(dirname(wd)))

  library(package=pkg, character.only = TRUE)

  path <- system.file("unitTests", package = pkg)

  stopifnot(file.exists(path), file.info(path.expand(path))$isdir)

  source(file.path(path, "runTests.R"), echo = TRUE)
}
