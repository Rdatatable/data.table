# templated warning/error functions to smooth translation & development

catf = function(fmt, ..., sep=" ", domain="R-data.table") {
  cat(gettextf(fmt, ..., domain=domain), sep=sep)
}

stopf = function(fmt, ..., domain="R-data.table") {
  stop(gettextf(fmt, ..., domain=domain), domain=NA, call. = FALSE)
}

warningf = function(fmt, ..., immediate.=FALSE, noBreaks.=FALSE, domain="R-data.table") {
  warning(gettextf(fmt, ..., domain=domain), domain=NA, call.=FALSE, immediate.=immediate., noBreaks.=noBreaks.)
}

messagef = function(fmt, ..., appendLF=TRUE, domain="R-data.table") {
  message(gettextf(fmt, ..., domain=domain), domain=NA, appendLF=appendLF)
}

packageStartupMessagef = function(fmt, ..., appendLF=TRUE, domain="R-data.table") {
  packageStartupMessage(gettextf(fmt, ..., domain=domain), domain=NA, appendLF=appendLF)
}
