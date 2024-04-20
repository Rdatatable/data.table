if (!suppressPackageStartupMessages(requireNamespace("knitr", quietly=TRUE))) {
  cat(readLines("knitr.Rout.mock", warn=FALSE), sep="\n")
  q('no')
}

library(knitr)
invisible(knit("knitr.Rmd", quiet=TRUE))
cat(readLines("knitr.md"), sep="\n")
invisible(file.remove("knitr.md"))
