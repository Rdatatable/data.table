# Moved here out from data.table.R on 10 Aug 2017. See data.table.R for history prior to that.

print.data.table = function(x, topn=getOption("datatable.print.topn"),
               nrows=getOption("datatable.print.nrows"),
               class=getOption("datatable.print.class"),
               row.names=getOption("datatable.print.rownames"),
               col.names=getOption("datatable.print.colnames"),
               print.keys=getOption("datatable.print.keys"),
               trunc.cols=getOption("datatable.print.trunc.cols"),
               quote=FALSE,
               timezone=FALSE, ...) {
  # topn  - print the top topn and bottom topn rows with '---' inbetween (5)
  # nrows - under this the whole (small) table is printed, unless topn is provided (100)
  # class - should column class be printed underneath column name? (FALSE)
  # trunc.cols - should only the columns be printed that can fit in the console? (FALSE)
  if (!col.names %chin% c("auto", "top", "none"))
    stopf("Valid options for col.names are 'auto', 'top', and 'none'")
  if (length(trunc.cols) != 1L || !is.logical(trunc.cols) || is.na(trunc.cols))
    stopf("Valid options for trunc.cols are TRUE and FALSE")
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
    if (length(SYS) <= 2L ||  # "> DT" auto-print or "> print(DT)" explicit print (cannot distinguish from R 3.2.0 but that's ok)
        ( length(SYS) >= 3L && is.symbol(thisSYS <- SYS[[length(SYS)-2L]][[1L]]) &&
          as.character(thisSYS) == 'source') || # suppress printing from source(echo = TRUE) calls, #2369
        ( length(SYS) > 3L && is.symbol(thisSYS <- SYS[[length(SYS)-3L]][[1L]]) &&
          as.character(thisSYS) %chin% mimicsAutoPrint ) )  {
      return(invisible(x))
      # is.symbol() temp fix for #1758.
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
    class = if (is.data.table(x)) "table" else "frame"  # a data.frame could be passed to print.data.table() directly, #3363
    if (all(dim(x)==0L)) {
      catf("Null data.%s (0 rows and 0 cols)\n", class)  # See FAQ 2.5 and NEWS item in v1.8.9
    } else {
      catf("Empty data.%s (%d rows and %d cols)", class, NROW(x), NCOL(x))
      if (length(x)>0L) cat(": ",paste(head(names(x),6L),collapse=","),if(length(x)>6L)"...",sep="")
      cat("\n")
    }
    return(invisible(x))
  }
  n_x = nrow(x)
  if ((topn*2L+1L)<n_x && (n_x>nrows || !topnmiss)) {
    toprint = rbindlist(list(head(x, topn), tail(x, topn)), use.names=FALSE)  # no need to match names because head and tail of same x, and #3306
    rn = c(seq_len(topn), seq.int(to=n_x, length.out=topn))
    printdots = TRUE
  } else {
    toprint = x
    rn = seq_len(n_x)
    printdots = FALSE
  }
  toprint=format.data.table(toprint, na.encode=FALSE, timezone = timezone, ...)  # na.encode=FALSE so that NA in character cols print as <NA>
  require_bit64_if_needed(x)

  # FR #353 - add row.names = logical argument to print.data.table
  if (isTRUE(row.names)) rownames(toprint)=paste0(format(rn,right=TRUE,scientific=FALSE),":") else rownames(toprint)=rep.int("", nrow(toprint))
  if (is.null(names(x)) || all(names(x) == ""))
    # fixes bug #97 and #545
    colnames(toprint)=rep("", ncol(toprint))
  if (isTRUE(class) && col.names != "none") {
    #Matching table for most common types & their abbreviations
    class_abb = c(list = "<list>", integer = "<int>", numeric = "<num>",
      character = "<char>", Date = "<Date>", complex = "<cplx>",
      factor = "<fctr>", POSIXct = "<POSc>", logical = "<lgcl>",
      IDate = "<IDat>", integer64 = "<i64>", raw = "<raw>",
      expression = "<expr>", ordered = "<ord>")
    classes = vapply_1c(x, function(col) class(col)[1L], use.names=FALSE)
    abbs = unname(class_abb[classes])
    if ( length(idx <- which(is.na(abbs))) ) abbs[idx] = paste0("<", classes[idx], ">")
    toprint = rbind(abbs, toprint)
    rownames(toprint)[1L] = ""
  }
  if (isFALSE(class) || (isTRUE(class) && col.names == "none")) abbs = ""
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
  }
  if (printdots) {
    toprint = rbind(head(toprint, topn + isTRUE(class)), "---"="", tail(toprint, topn))
    rownames(toprint) = format(rownames(toprint), justify="right")
    if (col.names == "none") {
      cut_colnames(print(toprint, right=TRUE, quote=quote))
    } else {
      print(toprint, right=TRUE, quote=quote)
    }
    if (trunc.cols && length(not_printed) > 0L)
      # prints names of variables not shown in the print
      trunc_cols_message(not_printed, abbs, class, col.names)

    return(invisible(x))
  }
  if (nrow(toprint)>20L && col.names == "auto")
    # repeat colnames at the bottom if over 20 rows so you don't have to scroll up to see them
    #   option to shut this off per request of Oleg Bondar on SO, #1482
    toprint=rbind(toprint, matrix(if (quote) old else colnames(toprint), nrow=1L)) # fixes bug #97
  if (col.names == "none") {
    cut_colnames(print(toprint, right=TRUE, quote=quote))
  } else {
    print(toprint, right=TRUE, quote=quote)
  }
  if (trunc.cols && length(not_printed) > 0L)
    # prints names of variables not shown in the print
    trunc_cols_message(not_printed, abbs, class, col.names)

  invisible(x)
}

format.data.table = function (x, ..., justify="none") {
  if (is.atomic(x) && !is.null(x)) {
    stopf("Internal structure doesn't seem to be a list. Possibly corrupt data.table.")
  }
  do.call("cbind", lapply(x, format_col, ..., justify=justify))
}

mimicsAutoPrint = c("knit_print.default")
# add maybe repr_text.default.  See https://github.com/Rdatatable/data.table/issues/933#issuecomment-220237965

shouldPrint = function(x) {
  ret = (.global$print=="" ||   # to save address() calls and adding lots of address strings to R's global cache
     address(x)!=.global$print)
  .global$print = ""
  ret
}

# for removing the head (column names) of matrix output entirely,
#   as opposed to printing a blank line, for excluding col.names per PR #1483
# be sure to remove colnames from any row where they exist, #4270
cut_colnames = function(x) writeLines(grep("^\\s*(?:[0-9]+:|---)", capture.output(x), value=TRUE))

# for printing the dims for list columns #3671; used by format.data.table()
paste_dims = function(x) {
  dims = if (isS4(x)) {
    length(slotNames(x))
  } else {
    if (is.null(dim(x))) length(x) else dim(x)
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
  any(sapply(class(x), f))
}

format_col.default = function(x, ...) {
  if (!is.null(dim(x)))
    "<multi-column>"
  else if (has_format_method(x) && length(formatted<-format(x, ...))==length(x))
    formatted  #PR5224 motivated by package sf where column class is c("sfc_MULTIPOLYGON","sfc") and sf:::format.sfc exists
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
    ""
  else if (is.atomic(x) || inherits(x, "formula")) # FR #2591 - format.data.table issue with columns of class "formula"
    paste(c(format(head(x, 6L), ...), if (length(x) > 6L) "..."), collapse=",") # fix for #5435 and #37 - format has to be added here...
  else if (has_format_method(x) && length(formatted<-format(x, ...))==1L) {
    # the column's class does not have a format method (otherwise it would have been used by format_col and this
    # format_list_item would not be reached) but this particular list item does have a format method so use it
    formatted
  } else {
    paste0("<", class(x)[1L], paste_dims(x), ">")
  }
}

# FR #1091 for pretty printing of character
# TODO: maybe instead of doing "this is...", we could do "this ... test"?
char.trunc = function(x, trunc.char = getOption("datatable.prettyprint.char")) {
  trunc.char = max(0L, suppressWarnings(as.integer(trunc.char[1L])), na.rm=TRUE)
  if (!is.character(x) || trunc.char <= 0L) return(x)
  idx = which(nchar(x) > trunc.char)
  x[idx] = paste0(substr(x[idx], 1L, as.integer(trunc.char)), "...")
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
  rownum_width = if (row.names) as.integer(ceiling(log10(nrow))+2) else 0L
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
    "%d variable(s) not shown: %s\n",
    n, brackify(paste0(not_printed, classes))
  )
}

