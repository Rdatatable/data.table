any_mismatch = FALSE

# ensure that numbered list in each section is in sequence
check_section_numbering = function(news) {
  # plain '#' catches some examples
  sections = grep("^#+ [A-Z]", news)
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
  return(any_mismatch)
}

any_error = FALSE
for (news in list.files(pattern = "NEWS")) {
  cat(sprintf("Checking NEWS file %s...\n", news))
  news_lines = readLines("NEWS.md")
  any_error = any_error || check_section_numbering(news_lines)
}
if (any_error) stop("Please fix the NEWS issues above.")
