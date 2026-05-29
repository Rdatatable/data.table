# Moved here out from data.table.R on 10 Aug 2017. See data.table.R for history prior to that.

print.data.table = function(x, topn=getOption("datatable.print.topn"),
               nrows=getOption("datatable.print.nrows"),
               class=getOption("datatable.print.class"),
               row.names=getOption("datatable.print.rownames"),
               col.names=getOption("datatable.print.colnames"),
               print.keys=getOption("datatable.print.keys"),
               trunc.cols=getOption("datatable.print.trunc.cols"),
               show.indices=getOption("datatable.show.indices"),
               quote=FALSE,
               na.print=NULL,
               timezone=FALSE, ...) {
  # topn  - print the top topn and bottom topn rows with '---' in between (5)
  # nrows - under this the whole (small) table is printed, unless topn is provided (100)
  # class - should column class be printed underneath column name? (FALSE)
  # trunc.cols - should only the columns be printed that can fit in the console? (FALSE)
  if (!col.names %chin% c("auto", "top", "none"))
    stopf("Valid options for col.names are 'auto', 'top', and 'none'")
  if (length(trunc.cols) != 1L || !is.logical(trunc.cols) || is.na(trunc.cols))
    stopf("Valid options for trunc.cols are TRUE and FALSE")
  stopifnot(isTRUEorFALSE(class))
  if (col.names == "none" && class)
    warningf("Column classes will be suppressed when col.names is 'none'")
  if (!shouldPrint(x)) {
    SYS = sys.calls()
    if (identical(SYS[[1L]][[1L]], print) || 
        ( length(SYS) >= 3L && is.symbol(thisSYS <- SYS[[length(SYS)-2L]][[1L]]) &&
          as.character(thisSYS) == 'source') ) { 
      return(invisible(x))
    }
  }
  if (!is.numeric(nrows)) nrows = 100L
  if (!is.infinite(nrows)) nrows = as.integer(nrows)
  if (nrows <= 0L) return(invisible(x))
  if (!is.numeric(topn)) topn = 5L
  topnmiss = missing(topn)
  topn = max(as.integer(topn),1L)
  if (print.keys) {
    if (!is.null(ky <- key(x)))
    catf("Key: <%s>\n", toString(ky))
    if (!is.null(ixs <- indices(x)))
    cat(sprintf(
      ngettext(length(ixs), "Index: %s\n", "Indices: %s\n"),
      paste0("<", ixs, ">", collapse = ", ")
    ))
  }
  if (any(dim(x)==0L)) {
    x_class = if (is.data.table(x)) "data.table" else "data.frame"
    if (all(dim(x)==0L)) {
      catf("Null %s (0 rows and 0 cols)\n", x_class)
    } else {
      catf("Empty %s (%d rows and %d cols)", x_class, NROW(x), NCOL(x))
      if (length(x)>0L) cat(": ",paste(head(names(x),6L),collapse=","),if(length(x)>6L)"...",sep="")
      cat("\n")
    }
    return(invisible(x))
  }
  if (show.indices) {
    if (is.null(indices(x))) {
      show.indices = FALSE
    } else {
      index_dt = as.data.table(attributes(attr(x, 'index')))
      print_names = paste0("index", if (ncol(index_dt) > 1L) seq_len(ncol(index_dt)) else "", ":", sub("^__", "", names(index_dt)))
      setnames(index_dt, print_names)
    }
  }
  n_x = nrow(x)
  if ((topn*2L+1L)<n_x && (n_x>nrows || !topnmiss)) {
    rn = c(seq_len(topn), seq.int(to=n_x, length.out=topn))
    printdots = TRUE
    idx = c(seq_len(topn), seq(to=nrow(x), length.out=topn))
    toprint = x[idx, ]
    if (show.indices) toprint = cbind(toprint, index_dt[idx, ])
  } else {
    toprint = x
    rn = seq_len(n_x)
    printdots = FALSE
    if (show.indices) toprint = cbind(toprint, index_dt)
  }
  require_bit64_if_needed(x)
  classes = classes1(toprint)

  # PRECISE STEP 1: Calculate the exact width of the row labels prefix (e.g., "100: " is 5 chars)
  rw = if (isTRUE(row.names)) as.integer(ceiling(log10(n_x)) + 2L) else 0L

  # PRECISE STEP 2: Pass 'rownum_width' into the formatters
  toprint=format.data.table(toprint, na.encode=FALSE, timezone = timezone, rownum_width = rw, ...)

  if (isTRUE(row.names)) rownames(toprint)=paste0(format(rn,right=TRUE,scientific=FALSE),":") else rownames(toprint)=rep.int("", nrow(toprint))
  if (is.null(names(x)) || !any(nzchar(names(x), keepNA=TRUE)))
    colnames(toprint)=rep("", ncol(toprint))
  if (class && col.names != "none") {
    class_abb = c(list = "<list>", integer = "<int>", numeric = "<num>",
      character = "<char>", Date = "<Date>", complex = "<cplx>",
      factor = "<fctr>", POSIXct = "<POSc>", logical = "<lgcl>",
      IDate = "<IDat>", integer64 = "<i64>", raw = "<raw>",
      expression = "<expr>", ordered = "<ord>")
    abbs = unname(class_abb[classes])
    if ( length(idx <- which(is.na(abbs))) ) abbs[idx] = paste0("<", classes[idx], ">")
    toprint = rbind(abbs, toprint)
    rownames(toprint)[1L] = ""
  } else {
    abbs = ""
  }
  if (quote) colnames(toprint) <- paste0('"', old <- colnames(toprint), '"')
  if (isTRUE(trunc.cols)) {
    widths = dt_width(toprint, n_x, class, row.names, col.names)
    cons_width = getOption("width")
    cols_to_print = widths < cons_width
    not_printed = colnames(toprint)[!cols_to_print]
    if (!any(cols_to_print)) {
      trunc_cols_message(not_printed, abbs, class, col.names)
      return(invisible(x))
    }
    toprint = toprint_subset(toprint, cols_to_print)
    trunc.cols = length(not_printed) > 0L
  }
  print_default = function(x) {
    if (col.names != "none") cut_colnames = identity
    cut_colnames(print(x, right=TRUE, quote=quote, na.print=na.print))
    if (trunc.cols) trunc_cols_message(not_printed, abbs, class, col.names)
  }
  if (printdots) {
    if (isFALSE(row.names)) {
      toprint = rbind(head(toprint, topn + class), "---", tail(toprint, topn))
    } else {
      toprint = rbind(head(toprint, topn + class), "---"="", tail(toprint, topn))
    }
    rownames(toprint) = format(rownames(toprint), justify="right")
    print_default(toprint)
    return(invisible(x))
  }
  if (col.names == "none")
    colnames(toprint) = rep.int("", ncol(toprint))
  if (nrow(toprint)>20L && col.names == "auto")
    toprint = rbind(
      toprint,
      matrix(if (quote) old else colnames(toprint), nrow=1L),
      if (class) matrix(if (trunc.cols) abbs[cols_to_print] else abbs, nrow=1L)
    )
  print_default(toprint)
  invisible(x)
}

format.data.table = function(x, ..., justify="none") {
  if (is.atomic(x) && !is.null(x)) {
    stopf("Internal structure doesn't seem to be a list. Possibly corrupt data.table.")
  }
  # ... contains rownum_width
  do.call(cbind, lapply(x, format_col, ..., justify=justify))
}

shouldPrint = function(x) {
  ret = (identical(.global$print, "") || address(x)!=.global$print)
  .global$print = ""
  ret
}

cut_colnames = function(x) writeLines(grepv("^\\s*(?:[0-9]+:|---)", capture.output(x)))

paste_dims = function(x) {
  dims = if (isS4(x)) length(slotNames(x)) else dim(x) %||% length(x)
  paste0("[", paste(dims,collapse="x"), "]")
}

format_col = function(x, ...) UseMethod("format_col")

format_list_item = function(x, ...) UseMethod("format_list_item")

has_format_method = function(x) {
  f = function(y) !is.null(getS3method("format", class=y, optional=TRUE))
  any(vapply_1b(class(x), f))
}

format_col.default = function(x, ..., rownum_width = 0L) {
  if (!is.null(dim(x))) return("<multi-column>")
  if (is.list(x)) x = vapply_1c(x, format_list_item, ...)
  # Pass metadata to truncator
  format(char.trunc(x, rownum_width = rownum_width), ...)
}

format_col.POSIXct = function(x, ..., timezone=FALSE) {
  if (timezone) {
    tz = attr(x,'tzone',exact=TRUE)
    nas = is.na(x)
    x = paste0(as.character(x)," ",tz)
    is.na(x) = nas
  } else {
    x = format(x, usetz=FALSE)
  }
  x
}

format_col.expression = function(x, ..., rownum_width = 0L) {
  format(char.trunc(as.character(x), rownum_width = rownum_width), ...)
}

format_list_item.default = function(x, ...) {
  if (is.null(x)) "[NULL]"
  else if (is.atomic(x) || inherits(x, "formula")) 
    paste(c(format(head(x, 6L), ...), if (length(x) > 6L) sprintf("...[%d]", length(x))), collapse=",") 
  else if (has_format_method(x) && length(formatted<-format(x, ...))==1L) formatted
  else paste0("<", class1(x), paste_dims(x), ">")
}

format_list_item.data.frame = function(x, ...) paste0("<", class1(x), paste_dims(x), ">")

char.trunc = function(x, trunc.char = getOption("datatable.prettyprint.char"), rownum_width = 0L) {
  if (is.null(trunc.char)) {
    # PRECISE STEP 3: The strict formula
    # options(width) - rownum_width - 1 (padding space) - 3 (ellipsis) = width - rownum_width - 4
    trunc.char = getOption("width") - rownum_width - 4L
  }
  trunc.char = max(0L, suppressWarnings(as.integer(trunc.char[1L])), na.rm=TRUE)
  if (!is.character(x) || trunc.char <= 0L) return(x)
  
  nchar_width = nchar(x, 'width', allowNA=TRUE)
  nchar_chars = nchar(x, 'char', allowNA=TRUE)
  
  idx = which(!is.na(x) & !is.na(nchar_width) & pmin(nchar_width, nchar_chars) > trunc.char)
  
  if (length(idx)) {
    is_full_width = nchar_width[idx] > nchar_chars[idx]
    width_mult = fifelse(is_full_width, 2L, 1L, na = 1L)
    x[idx] = paste0(strtrim(x[idx], trunc.char * width_mult), "...")
  }
  x
}

dt_width = function(x, nrow, class, row.names, col.names) {
  widths = apply(nchar(x, type='width'), 2L, max)
  if (class) widths = pmax(widths, 6L)
  if (col.names != "none") names = sapply(colnames(x), nchar, type="width") else names = 0L
  dt_widths = pmax(widths, names)
  rownum_width = if (row.names) as.integer(ceiling(log10(nrow))+2.0) else 0L
  cumsum(dt_widths + 1L) + rownum_width
}

toprint_subset = function(x, cols_to_print) {
  if (nrow(x) == 1L){
    atts = attributes(x)
    atts$dim = c(1L, sum(cols_to_print))
    atts$dimnames[[2L]] = atts$dimnames[[2L]][cols_to_print]
    x = x[, cols_to_print, drop=FALSE]; attributes(x) = atts; x
  } else x[, cols_to_print, drop=FALSE]
}

trunc_cols_message = function(not_printed, abbs, class, col.names){
  n = length(not_printed)
  if (class && col.names != "none") classes = paste0(" ", tail(abbs, n)) else classes = ""
  catf(ngettext(n, "%d variable not shown: %s\n", "%d variables not shown: %s\n"), n, brackify(paste0(not_printed, classes)), domain=NA)
}

# nocov start
knit_print.data.table = function(x, ...) { if (!shouldPrint(x)) return(invisible(x)); NextMethod() }
record_print.data.table = function(x, ...) { if (!shouldPrint(x)) return(character()); NextMethod() }
# nocov end
