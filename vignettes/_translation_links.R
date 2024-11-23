# build a link list of alternative languages (may be character(0))
.write.translation.links <- function(fmt) {
    url = "https://rdatatable.gitlab.io/data.table/articles"
    path = dirname(knitr::current_input(TRUE))
    if (basename(path) == "vignettes") {
      lang = "en"
    } else {
      lang = basename(path)
      path = dirname(path)
    }
    translation = dir(path,
      recursive = TRUE,
      pattern = glob2rx(knitr::current_input(FALSE))
    )
    transl_lang = ifelse(dirname(translation) == ".", "en", dirname(translation))
    block = if (length(which(transl_lang != lang))) {
      sprintf(
        fmt, paste(collapse = " | ", sprintf(
          "[%s](%s)",
          transl_lang[transl_lang != lang],
          file.path(url, sub("(?i)\\.Rmd$", ".html", translation[transl_lang != lang]))
      )))
    } else ""
    knitr::asis_output(block)
}
