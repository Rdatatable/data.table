if (suppressPackageStartupMessages(requireNamespace("knitr", quietly = TRUE))) {
    require(knitr)
    knit("knitr.Rmd", quiet=TRUE)
    cat(readLines("knitr.md"), sep="\n")
} else {
    cat(readLines("knitr.Rout.mock", warn = FALSE), sep="\n")
}

