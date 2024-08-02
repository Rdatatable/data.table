any_mismatch = FALSE
for (news in list.files("NEWS")) {
  cat(sprintf("Checking NEWS file %s...\n", news))
  news = readLines("NEWS.md")
  sections = grep("^#+ [A-Z]", news)
  entries = grep("^[0-9]+[.]", news)
  entry_value = as.integer(gsub("^([0-9]+)[.].*", "\\1", news[entries]))
  section_id = findInterval(entries, sections)
  
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
}
if (any_mismatch) stop("Please fix the section numbering issues above.")
