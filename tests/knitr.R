if (!suppressPackageStartupMessages(requireNamespace("knitr", quietly=TRUE))) {
  cat(readLines("knitr.Rout.mock", warn=FALSE), sep="\n")
  q('no')
}

require(knitr)
knit("knitr.Rmd", quiet=TRUE)
cat(readLines("knitr.md"), sep="\n")
invisible(file.remove("knitr.md"))
