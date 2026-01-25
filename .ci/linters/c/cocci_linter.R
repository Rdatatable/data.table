cocci_linter = if (!nzchar(Sys.which("spatch"))) function(...) {} else function(c_obj) {
  bad = FALSE
  tmp = tempfile(fileext = '.c')
  on.exit(unlink(tmp))
  writeLines(c_obj$preprocessed, tmp)
  for (spfile in list.files(".ci/linters/cocci", full.names = TRUE)) {
    out = system2(
      "spatch",
      shQuote(c("--sp-file", spfile, tmp)),
      stdout = TRUE, stderr = FALSE
    )
    if (length(out) > 0) {
      cat(sprintf("In file '%s', Coccinelle linter '%s' located the following problems:\n", c_obj$path, spfile))
      writeLines(out)
      bad = TRUE
    }
    if (!is.null(status <- attr(out, 'status'))) {
      cat(sprintf("While working on file '%s', Coccinelle linter '%s' failed with exit code %d:\n", c_obj$path, spfile, status))
      bad = TRUE
    }
  }
  if (bad) stop("Please investigate the problems above.")
}
