# build a link list of alternative languages (may be character(0))
# idea is to look like 'Other languages: en | fr | de'
.write.translation.links = function(fmt) {
    url = "https://rdatatable.gitlab.io/data.table/articles"
    context = litedown::get_context("input")
    path = dirname(context)
    if (basename(path) %in% c(".", "vignettes")) {
      lang = "en"
    } else {
      lang = basename(path)
      path = ".."
    }
    translation = dir(path = path, recursive = TRUE, pattern = basename(context))
    transl_lang = ifelse(dirname(translation) == ".", "en", dirname(translation))
    block = if (!all(transl_lang == lang)) {
      linked_transl = sprintf("[%s](%s)", transl_lang, file.path(url, sub("(?i)\\.Rmd$", ".html", translation)))
      linked_transl[transl_lang == lang] = lang
      sprintf(fmt, paste(linked_transl, collapse = " | "))
    } else ""
    litedown::raw_text(block)
}
