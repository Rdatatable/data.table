# ensure that GitHub link text & URL actually agree
news_github_link_mismatch_linter = function(news) {
  if (!grepl("NEWS", news)) return(invisible())
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
