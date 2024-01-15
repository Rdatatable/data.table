# templated warning/error functions to smooth translation & development

catf = function(fmt, ..., sep=" ", domain="R-data.table") {
  cat(gettextf(fmt, ..., domain=domain), sep=sep)
}

stopf = function(fmt, ..., class=NULL, domain="R-data.table") {
  obj = list(
    message=gettextf(fmt, ..., domain=domain),
    call=sys.call()
  )
  setattr(obj, "class", c(class, "simpleError", "error", "condition"))
  stop(obj)
}

warningf = function(fmt, ..., immediate.=FALSE, class=NULL, domain="R-data.table") {
  # cannot set immediate.=TRUE through warning(), so use the description in ?warning to replicate this behavior ourselves
  if (immediate.) {
    old = options(warn = 1)
    on.exit(options(old))
  }
  obj = list(
    message=gettextf(fmt, ..., domain=domain),
    call=sys.call()
  )
  setattr(obj, "class", c(class, "simpleWarning", "warning", "condition"))
  warning(obj)
}

messagef = function(fmt, ..., appendLF=TRUE, class=NULL, domain="R-data.table") {
  obj = list(
    message=gettextf(fmt, ..., domain=domain),
    call=sys.call()
  )
  # NB: append _after_ translation
  if (appendLF) obj$message <- paste0(obj$message, "\n")
  setattr(obj, "class", c(class, "simpleMessage", "message", "condition"))
  message(obj)
}

packageStartupMessagef = function(fmt, ..., appendLF=TRUE, class=NULL, domain="R-data.table") {
  obj = list(
    message=gettextf(fmt, ..., domain=domain),
    call=sys.call()
  )
  # NB: append _after_ translation
  if (appendLF) obj$message <- paste0(obj$message, "\n")
  setattr(obj, "class", c(class, "packageStartupMessage", "simpleMessage", "message", "condition"))
  # NB: packageStartupMessage() itself calls message(.packageStartupMessage(...))
  message(obj)
}
