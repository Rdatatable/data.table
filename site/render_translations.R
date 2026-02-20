files <- list.files("vignettes", "\\.Rmd$", recursive = TRUE, full.names = TRUE)
translated <- files[dirname(files) != "vignettes"]

if (length(translated) == 0L) {
  cat("No translated vignettes found\n")
  quit("no")
}

cat(sprintf("Found %d translated vignettes\n", length(translated)))

for (f in translated) {
  out <- file.path("website/articles", sub("\\.Rmd$", ".html", sub("^vignettes/", "", f)))
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  cat(sprintf("  Rendering: %s -> %s\n", f, out))
  src = litedown::fuse(f)
  file.rename(src, out)
}

cat("Done\n")
