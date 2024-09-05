# templated warning/error functions to smooth translation & development

catf = function(fmt, ..., sep=" ", domain="R-data.table") {
  cat(gettextf(fmt, ..., domain=domain), sep=sep)
}

raise_condition = function(signal, message, classes, immediate=FALSE, appendLF=FALSE) {
  obj = list(message=message, call=sys.call(sys.nframe()-2L))
  # NB: append _after_ translation
  if (appendLF) obj$message = paste0(obj$message, "\n")
  setattr(obj, "class", classes)
  # cannot set immediate.=TRUE through warning(), so use the description in ?warning to replicate this behavior ourselves. tested manually.
  if (immediate) {
    old = options(warn=1)
    on.exit(options(old))
  }
  signal(obj)
}

stopf = function(fmt, ..., class=NULL, domain="R-data.table") {
  raise_condition(stop, gettextf(fmt, ..., domain=domain), c(class, "simpleError", "error", "condition"))
}

warningf = function(fmt, ..., immediate.=FALSE, class=NULL, domain="R-data.table") {
  raise_condition(warning, gettextf(fmt, ..., domain=domain), c(class, "simpleWarning", "warning", "condition"), immediate=immediate.)
}

messagef = function(fmt, ..., appendLF=TRUE, class=NULL, domain="R-data.table") {
  raise_condition(message, gettextf(fmt, ..., domain=domain), c(class, "simpleMessage", "message", "condition"), appendLF=appendLF)
}

packageStartupMessagef = function(fmt, ..., appendLF=TRUE, class=NULL, domain="R-data.table") {
  # NB: packageStartupMessage() itself calls message(.packageStartupMessage(...))
  messagef(fmt, ..., appendLF=appendLF, class=c(class, "packageStartupMessage"), domain=domain)
}
