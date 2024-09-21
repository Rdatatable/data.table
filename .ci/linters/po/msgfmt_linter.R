# Use msgfmt to check for untranslated/fuzzy messages, and for whether
#   the implied .mo compiled form matches that which is already checked in
msgfmt_linter <- function(po_file) {
  mo_tmp <- tempfile()
  on.exit(unlink(mo_tmp))

  res = system2("msgfmt", c("--statistics", po_file, "-o", mo_tmp), stdout=TRUE, stderr=TRUE)
  if (any(grepl("untranslated message|fuzzy translation", res))) {
    cat(sprintf("In %s, found incomplete translations:\n%s\n", po_file, paste(res, collapse="\n")))
    stop("Please fix.")
  }

  mo_ref = sprintf(
    "inst/%s/LC_MESSAGES/%sdata.table.mo",
    gsub("^R-|[.]po$", "", po_file),
    if (startsWith(basename(po_file), "R-")) "R-" else ""
  )

  if (!file.exists(mo_ref)) {
    stop(po_file, " has not been compiled as ", mo_ref, ". Please fix.")
  }
  if (tools::md5sum(mo_ref) == tools::md5sum(mo_tmp)) return(invisible())
  # TODO(#6517): Re-activate this part of the check to ensure .mo is up to date.
  cat(sprintf("Note: MD5 sum of msgfmt output for %s does not match %s.\n", po_file, mo_ref))
  return(invisible())

  # NB: file.mtime() will probably be wrong, it will reflect the check-out time of the git repo.
  last_edit_time = system2("git",
    c("log", "-1", '--format="%ad"', "--date=format:'%Y-%m-%d %H:%M:%S'", "--", mo_ref),
    stdout=TRUE
  )
  cat(sprintf(
    ".mo compilation %s of .po translation %s appears out of date! It was last updated %s\n",
    mo_ref, po_file, last_edit_time
  ))

  unmo_tmp = tempfile()
  unmo_ref = tempfile()
  on.exit(unlink(c(unmo_tmp, unmo_ref)), add=TRUE)
  system2("msgunfmt", c(mo_tmp, "-o", unmo_tmp))
  system2("msgunfmt", c(mo_ref, "-o", unmo_ref))
  cat("Here are the observed differences after converting back to .po:\n\n")
  system2("diff", c(unmo_tmp, unmo_ref))
  stop("Please fix.")
}
