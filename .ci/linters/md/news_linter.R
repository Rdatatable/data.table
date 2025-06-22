# ensure that numbered list in each section is in sequence
check_section_numbering = function(news) {
  if (!grepl("NEWS", news)) return(invisible())
  news = readLines(news)
  # plain '#' catches some examples; 'd' for 'data.table'
  sections = grep("^#+ [A-Zd]", news)
  entries = grep("^[0-9]+[.]", news)
  entry_value = as.integer(gsub("^([0-9]+)[.].*", "\\1", news[entries]))
  section_id = findInterval(entries, sections)

  any_mismatch = FALSE
  for (id in unique(section_id)) {
    section_entries = entry_value[section_id == id]
    intended_value = seq_along(section_entries)
    matched = section_entries == intended_value
    if (all(matched)) next
    any_mismatch = TRUE
    section_header = news[sections[id]]
    cat(sprintf(
      "In section '%s' (line %d), bad numbering:\n%s\n",
      section_header, sections[id],
      paste0("  [", section_entries[!matched], " --> ", intended_value[!matched], "]", collapse="\n")
    ))
  }
  stopifnot("Please fix the NEWS issues above" = !any_mismatch)
}

# ensure that GitHub link text & URL actually agree
check_gh_links = function(news) {
  news = readLines(news)
  gh_links_info = gregexpr(
    "\\[#(?<md_number>[0-9]+)\\]\\(https://github.com/Rdatatable/data.table/(?<link_type>[^/]+)/(?<link_number>[0-9]+)\\)",
    news,
    perl=TRUE # required for within-group indices
  )
  gh_link_metadata = do.call(rbind, lapply(seq_along(gh_links_info), function(idx) {
    x = gh_links_info[[idx]]
    if (x[1L] <= 0L) return(NULL)
    match_mat = attr(x, "capture.start") # matrix seeded with the correct dimensions
    match_mat[] = substring(news[idx], match_mat, match_mat + attr(x, "capture.length") - 1L)
    match_df = data.frame(match_mat)
    match_df$line_number = idx
    match_df
  }))
  matched = gh_link_metadata$md_number == gh_link_metadata$link_number
  if (all(matched)) return(FALSE)

  cat(sep = "", with(gh_link_metadata[!matched, ], sprintf(
    "In line %d, link pointing to %s %s is written #%s\n",
    line_number, link_type, link_number, md_number
  )))
  stop("Please fix the NEWS issues above.")
}
