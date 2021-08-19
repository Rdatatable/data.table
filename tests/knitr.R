if (suppressPackageStartupMessages(requireNamespace("knitr", quietly = TRUE))) {
    require(knitr)
    knit("knitr.Rmd", quiet=TRUE)
    cat(readLines("knitr.md"), sep="\n")
    invisible(file.remove("knitr.md"))
} else {
    cat(readLines("knitr.Rout.mock", warn = FALSE), sep="\n")
}

