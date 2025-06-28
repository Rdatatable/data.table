# ensure that ids are limited to alphanumerics and dashes
# (in particular, dots and underscores break the links)
check_header_ids = function(md) {
  if (!grepl('[.]Rmd$', md)) return(invisible())
  md = readLines(md)
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
  stopifnot('Please fix the vignette issues above' = all(good_ids))
}
