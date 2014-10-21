require(knitr)
knit("knitr.Rmd", quiet=TRUE)
cat(readLines("knitr.md"),sep="\n")

