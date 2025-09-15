# ensure that numbered list in each section is in sequence
news_section_numbering_linter = function(news) {
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
