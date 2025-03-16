cocci_linter = if (!nzchar(Sys.which("spatch"))) function(...) {} else function(c_obj) {
  bad <- FALSE
  for (spfile in list.files(".ci/linters/cocci", full.names = TRUE)) {
    # Coccinelle parser gets confused sometimes, so ignore stderr and the exit code
    out = suppressWarnings(system2(
      "spatch",
      shQuote(c(
        "--sp-file", spfile, c_obj$path, "--recursive-includes",
        "-I", R.home("include"), "-I", "src"
      )),
      stdout = TRUE, stderr = FALSE
    ))
    if (length(out) > 0) {
      cat(sprintf("In file '%s', Coccinelle patch '%s' recommends the following changes:\n", c_obj$path, spfile))
      writeLines(out)
      bad <- TRUE
    }
  }
  if (bad) stop("Please apply the changes above or fix the linter")
}
