library(data.table)
translated.Rmd <- Sys.glob("*/*.Rmd")
rm_suffix <- function(x)sub("[.][^.]+$", "", x)
trans.dt <- data.table(
  lang=dirname(translated.Rmd),
  trans_file=rm_suffix(basename(translated.Rmd)))
en.dt <- data.table(en.Rmd=Sys.glob("*.Rmd"))[
, en_file := rm_suffix(en.Rmd)
][]
link.dt <- en.dt[, nc::capture_all_str(
  en.Rmd,
  "https://rdatatable.gitlab.io/data.table/articles/",
  lang=".*?",
  "/",
  trans_file=".*?html", rm_suffix
), by=en_file]
maybe_err <- function(dt,msg){
  if(nrow(dt)){
    cat(msg,":\n")
    print(dt)
  }
}
maybe_err(link.dt[en_file != trans_file],"typos in links")
files.join <- trans.dt[en.dt, .(
  langs=paste(lang[!is.na(lang)],collapse=",")
), on=.(trans_file=en_file), by=.EACHI]
maybe_err(files.join[nchar(langs)<max(nchar(langs))], "missing translation files")
