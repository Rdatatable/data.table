# build a link list of alternative languages (may be character(0))
# idea is to look like 'Other languages: en | fr | de'
.write.translation.links <- function() {
  url = "https://rdatatable.gitlab.io/data.table/articles"
  # this guard is needed because pkgdown::build_article does not use litedown 
  # https://github.com/yihui/knitr/issues/926#issuecomment-68503962
  knitr = isTRUE(getOption('knitr.in.progress'))
  if (knitr) {
    path = knitr::current_input(TRUE)
    i18n_msg = knitr::opts_current$get("i18n_msg")
  } else {
    path = litedown::get_context("input")
    i18n_msg = litedown::reactor("i18n_msg")
  }
  path_dir = dirname(path)
  if (basename(path_dir) == "vignettes") {
    lang = "en"
  } else {
    lang = basename(path_dir)
    path_dir = dirname(path_dir)   # might be "vignettes"
  }
  translation = dir(path_dir, recursive = TRUE, pattern = glob2rx(path))
  transl_lang = dirname(translation)
  transl_lang[transl_lang == "."] = "en"
  if (any(transl_lang != lang)) {
    link_path = file.path(url, sub("(?i)\\.Rmd$", ".html", translation))
    linked_transl = sprintf("[%s](%s)", transl_lang, link_path)
    linked_transl[transl_lang == lang] = lang
    block = sprintf(i18n_msg, paste(linked_transl, collapse = " | "))
  } else {
    block = ""
  }
   #if (knitr) block else litedown::raw_text(block)
  cat(block)
}
.write.translation.links()
