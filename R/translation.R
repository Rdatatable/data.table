# templated warning/error functions to smooth translation & development

catf = function(fmt, ..., sep=" ", domain=NULL) {
  cat(gettextf(fmt, ..., domain=domain), sep=sep)
}

stopf = function(fmt, ..., domain=NULL) {
  stop(gettextf(fmt, ..., domain=domain), domain=NA, call. = FALSE)
}

warningf = function(fmt, ..., immediate.=FALSE, noBreaks.=FALSE, domain=NULL) {
  warning(gettextf(fmt, ..., domain=domain), domain=NA, call.=FALSE, immediate.=immediate., noBreaks.=noBreaks.)
}

messagef = function(fmt, ..., appendLF=TRUE, domain=NULL) {
  message(gettextf(fmt, ..., domain=domain), domain=NA, appendLF=appendLF)
}

packageStartupMessagef = function(fmt, ..., appendLF=TRUE, domain=NULL) {
  packageStartupMessage(gettextf(fmt, ..., domain=domain), domain=NA, appendLF=appendLF)
}
