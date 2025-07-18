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
    #  := in [.data.table sets .global$print=address(x) to suppress the next print i.e., like <- does. See FAQ 2.22 and README item in v1.9.5
    # The issue is distinguishing "> DT" (after a previous := in a function) from "> DT[,foo:=1]". To print.data.table(), there
    # is no difference. Now from R 3.2.0 a side effect of the very welcome and requested change to avoid silent deep copy is that
    # there is now no longer a difference between > DT and > print(DT). So decided that DT[] is now needed to guarantee print; simpler.
    # This applies just at the prompt. Inside functions, print(DT) will of course print.
    # Other options investigated (could revisit): Cstack_info(), .Last.value gets set first before autoprint, history(), sys.status(),
    #   topenv(), inspecting next statement in caller, using clock() at C level to timeout suppression after some number of cycles
    SYS = sys.calls()
    if (identical(SYS[[1L]][[1L]], print) || # this is what auto-print looks like, i.e. '> DT' and '> DT[, a:=b]' in the terminal; see #3029.
        ( length(SYS) >= 3L && is.symbol(thisSYS <- SYS[[length(SYS)-2L]][[1L]]) &&
          as.character(thisSYS) == 'source') ) { # suppress printing from source(echo = TRUE) calls, #2369
      return(invisible(x))
    }
  }
  if (!is.numeric(nrows)) nrows = 100L
  if (!is.infinite(nrows)) nrows = as.integer(nrows)
  if (nrows <= 0L) return(invisible(x))   # ability to turn off printing
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
    x_class = if (is.data.table(x)) "data.table" else "data.frame"  # a data.frame could be passed to print.data.table() directly, #3363
    if (all(dim(x)==0L)) {
      catf("Null %s (0 rows and 0 cols)\n", x_class)  # See FAQ 2.5 and NEWS item in v1.8.9
    } else {
      catf("Empty %s (%d rows and %d cols)", x_class, NROW(x), NCOL(x))
      if (length(x)>0L) cat(": ",paste(head(names(x),6L),collapse=","),if(length(x)>6L)"...",sep="") # notranslate
      cat("\n") # notranslate
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
    toprint = rbindlist(list(head(x, topn), tail(x, topn)), use.names=FALSE)  # no need to match names because head and tail of same x, and #3306
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
  toprint=format.data.table(toprint, na.encode=FALSE, timezone = timezone, ...)  # na.encode=FALSE so that NA in character cols print as <NA>

  # FR #353 - add row.names = logical argument to print.data.table
  if (isTRUE(row.names)) rownames(toprint)=paste0(format(rn,right=TRUE,scientific=FALSE),":") else rownames(toprint)=rep.int("", nrow(toprint))
  if (is.null(names(x)) || !any(nzchar(names(x), keepNA=TRUE)))
    # fixes bug #97 and #545
    colnames(toprint)=rep("", ncol(toprint))
  if (class && col.names != "none") {
    #Matching table for most common types & their abbreviations
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
    # allow truncation of columns to print only what will fit in console PR #4074
    widths = dt_width(toprint, n_x, class, row.names, col.names)
    cons_width = getOption("width")
    cols_to_print = widths < cons_width
    not_printed = colnames(toprint)[!cols_to_print]
    if (!any(cols_to_print)) {
      trunc_cols_message(not_printed, abbs, class, col.names)
      return(invisible(x))
    }
    # When nrow(toprint) = 1, attributes get lost in the subset,
    #   function below adds those back when necessary
    toprint = toprint_subset(toprint, cols_to_print)
    trunc.cols = length(not_printed) > 0L
  }
  print_default = function(x) {
    if (col.names != "none") cut_colnames = identity
    cut_colnames(print(x, right=TRUE, quote=quote, na.print=na.print))
    # prints names of variables not shown in the print
    if (trunc.cols) trunc_cols_message(not_printed, abbs, class, col.names)
  }
  if (printdots) {
    if (isFALSE(row.names)) {
      toprint = rbind(head(toprint, topn + class), "---", tail(toprint, topn)) # 4083
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
    # repeat colnames at the bottom if over 20 rows so you don't have to scroll up to see them
    #   option to shut this off per request of Oleg Bondar on SO, #1482
    toprint = rbind(
      toprint,
      matrix(if (quote) old else colnames(toprint), nrow=1L), # see #97
      if (class) matrix(if (trunc.cols) abbs[cols_to_print] else abbs, nrow=1L) # #6902
    )
  print_default(toprint)
  invisible(x)
}

format.data.table = function(x, ..., justify="none") {
  if (is.atomic(x) && !is.null(x)) { ## future R can use  if (is.atomic(x))

    stopf("Internal structure doesn't seem to be a list. Possibly corrupt data.table.")
  }
  do.call(cbind, lapply(x, format_col, ..., justify=justify))
}

shouldPrint = function(x) {
  ret = (identical(.global$print, "") ||   # to save address() calls and adding lots of address strings to R's global cache
     address(x)!=.global$print)
  .global$print = ""
  ret
}

# for removing the head (column names) of matrix output entirely,
#   as opposed to printing a blank line, for excluding col.names per PR #1483
# be sure to remove colnames from any row where they exist, #4270
cut_colnames = function(x) writeLines(grepv("^\\s*(?:[0-9]+:|---)", capture.output(x)))

# for printing the dims for list columns #3671; used by format.data.table()
paste_dims = function(x) {
  dims = if (isS4(x)) {
    length(slotNames(x))
  } else {
    dim(x) %||% length(x)
  }
  paste0("[", paste(dims,collapse="x"), "]")
}

format_col = function(x, ...) {
  UseMethod("format_col")
}

format_list_item = function(x, ...) {
  UseMethod("format_list_item")
}

has_format_method = function(x) {
  f = function(y) !is.null(getS3method("format", class=y, optional=TRUE))
  any(vapply_1b(class(x), f))
}

format_col.default = function(x, ...) {
  if (!is.null(dim(x)))
    "<multi-column>"
  else if (is.list(x))
    vapply_1c(x, format_list_item, ...)
  else
    format(char.trunc(x), ...) # relevant to #37
}

# #2842 -- different columns can have different tzone, so force usage in output
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

# #3011 -- expression columns can wrap to newlines which breaks printing
format_col.expression = function(x, ...) format(char.trunc(as.character(x)), ...)

format_list_item.default = function(x, ...) {
  if (is.null(x))  # NULL item in a list column
    "[NULL]" # not '' or 'NULL' to distinguish from those "common" string values in data
  else if (is.atomic(x) || inherits(x, "formula")) # FR #2591 - format.data.table issue with columns of class "formula"
    paste(c(format(head(x, 6L), ...), if (length(x) > 6L) sprintf("...[%d]", length(x))), collapse=",") # fix for #5435, #37, and #605 - format has to be added here...
  else if (has_format_method(x) && length(formatted<-format(x, ...))==1L) {
    # the column's class does not have a format method (otherwise it would have been used by format_col and this
    # format_list_item would not be reached) but this particular list item does have a format method so use it
    formatted
  } else {
    paste0("<", class1(x), paste_dims(x), ">")
  }
}

# #6592 -- nested 1-column frames breaks printing
format_list_item.data.frame = function(x, ...) {
  paste0("<", class1(x), paste_dims(x), ">")
}

# FR #1091 for pretty printing of character
# TODO: maybe instead of doing "this is...", we could do "this ... test"?
# Current implementation may have issues when dealing with strings that have combinations of full-width and half-width characters,
# if this becomes a problem in the future, we could consider string traversal instead.
char.trunc = function(x, trunc.char = getOption("datatable.prettyprint.char")) {
  trunc.char = max(0L, suppressWarnings(as.integer(trunc.char[1L])), na.rm=TRUE)
  if (!is.character(x) || trunc.char <= 0L) return(x)
  nchar_width = nchar(x, 'width') # Check whether string is full-width or half-width, #5096
  nchar_chars = nchar(x, 'char')
  is_full_width = nchar_width > nchar_chars
  idx = !is.na(x) & pmin(nchar_width, nchar_chars) > trunc.char
  x[idx] = paste0(strtrim(x[idx], trunc.char * fifelse(is_full_width[idx], 2L, 1L)), "...")
  x
}

# to calculate widths of data.table for PR #4074
# gets the width of the data.table at each column
#   and compares it to the console width
# pass nrow because x is the head/tail only so nrow(x) is wrong, #4266
dt_width = function(x, nrow, class, row.names, col.names) {
  widths = apply(nchar(x, type='width'), 2L, max)
  if (class) widths = pmax(widths, 6L)
  if (col.names != "none") names = sapply(colnames(x), nchar, type="width") else names = 0L
  dt_widths = pmax(widths, names)
  rownum_width = if (row.names) as.integer(ceiling(log10(nrow))+2.0) else 0L
  cumsum(dt_widths + 1L) + rownum_width
}
# keeps the dim and dimnames attributes
toprint_subset = function(x, cols_to_print) {
  if (nrow(x) == 1L){
    atts = attributes(x)
    atts$dim = c(1L, sum(cols_to_print))
    atts$dimnames[[2L]] = atts$dimnames[[2L]][cols_to_print]
    x = x[, cols_to_print, drop=FALSE]
    attributes(x) = atts
    x
  } else {
    x[, cols_to_print, drop=FALSE]
  }
}
# message for when trunc.cols=TRUE and some columns are not printed
trunc_cols_message = function(not_printed, abbs, class, col.names){
  n = length(not_printed)
  if (class && col.names != "none") classes = paste0(" ", tail(abbs, n)) else classes = ""
  catf(
    ngettext(n, "%d variable not shown: %s\n", "%d variables not shown: %s\n"),
    n, brackify(paste0(not_printed, classes)),
    domain=NA
  )
}

# Maybe add a method for repr::repr_text.  See https://github.com/Rdatatable/data.table/issues/933#issuecomment-220237965
knit_print.data.table = function(x, ...) {
  if (!shouldPrint(x)) return(invisible(x))
  NextMethod()
}
