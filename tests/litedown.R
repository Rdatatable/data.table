if (!suppressPackageStartupMessages(requireNamespace("litedown", quietly = TRUE))) {
  cat(readLines("litedown.Rout.mock", warn = FALSE), sep = "\n")
  q("no")
}

# Load litedown and render the Rmd file
library(litedown)
litedown::reactor(comment="# ", print=xfun::record_print)
invisible(litedown::fuse("litedown.Rmd", output = "litedown.md"))

# Print the rendered markdown content
cat(readLines("litedown.md"), sep = "\n")

# Clean up
invisible(file.remove("litedown.md"))