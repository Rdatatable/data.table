incomplete_translation_linter <- function(po_file) {
  res = system2("msgfmt", c("--statistics", po_file, "-o", tempfile()), stdout=TRUE, stderr=TRUE)
  if (any(grepl("untranslated message|fuzzy translation", res))) {
    cat(sprintf("In %s, found incomplete translations:\n%s\n", po_file, paste(res, collapse="\n")))
    stop("Please fix.")
  }
}
