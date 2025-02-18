any_mismatch = FALSE

# ensure that ids are limited to alphanumerics and dashes
# (in particular, dots and underscores break the links)
check_header_ids = function(md) {
  # A bit surprisingly, some headings don't start with a letter.
  # We're interested in those that set an id to link to, i.e., end with {#id}.
  heading_captures = regmatches(md, regexec("^#+ \\S.*[{]#([^}]*)[}]$", md))
  lines_with_id = which(lengths(heading_captures) > 0)
  ids = vapply(heading_captures[lines_with_id], `[`, '', 2)
  # ids must start with a letter and consist of alphanumerics or dashes.
  good_ids = grepl('^[A-Za-z][A-Za-z0-9-]*$', ids)
  for (line in lines_with_id[!good_ids]) cat(sprintf(
    "On line %d, bad heading id '%s':\n%s\n",
    line, heading_captures[[line]][2], heading_captures[[line]][1]
  ))
  !all(good_ids)
}

any_error = FALSE
for (vignette in list.files('vignettes', pattern = "[.]Rmd$", recursive = TRUE, full.name = TRUE)) {
  cat(sprintf("Checking vignette file %s...\n", vignette))
  rmd_lines = readLines(vignette)
  any_error = check_header_ids(rmd_lines) || any_error
}
if (any_error) stop("Please fix the vignette issues above.")
