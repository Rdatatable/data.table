# build a link list of alternative languages (may be character(0))
# idea is to look like 'Other languages: en | fr | de'
.write.translation.links <- function(fmt) {
    # Check if we're in a litedown context
    input_file = litedown::get_context("input")
    if (is.null(input_file) || !is.character(input_file) || length(input_file) == 0) {
      # Not in litedown context (e.g., being rendered by rmarkdown/pkgdown)
      return("")
    }

    url = "https://rdatatable.gitlab.io/data.table/articles"
    path = dirname(input_file)
    if (basename(path) == "vignettes") {
      lang = "en"
    } else {
      lang = basename(path)
      path = dirname(path)
    }
    translation = dir(path,
      recursive = TRUE,
      pattern = glob2rx(input_file)
    )
    transl_lang = ifelse(dirname(translation) == ".", "en", dirname(translation))
    block = if (!all(transl_lang == lang)) {
      linked_transl = sprintf("[%s](%s)", transl_lang, file.path(url, sub("(?i)\\.Rmd$", ".html", translation)))
      linked_transl[transl_lang == lang] = lang
      sprintf(fmt, paste(linked_transl, collapse = " | "))
    } else ""
    litedown::raw_text(block)
}
