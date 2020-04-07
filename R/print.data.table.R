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
    stop("Valid options for col.names are 'auto', 'top', and 'none'")
  if (col.names == "none" && class)
    warning("Column classes will be suppressed when col.names is 'none'")
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
  if (print.keys){
    if (!is.null(ky <- key(x)))
    cat("Key: <", paste(ky, collapse=", "), ">\n", sep="")
    if (!is.null(ixs <- indices(x)))
    cat("Ind", if (length(ixs) > 1L) "ices" else "ex", ": <",
      paste(ixs, collapse=">, <"), ">\n", sep="")
  }
  if (any(dim(x)==0L)) {
    class = if (is.data.table(x)) "table" else "frame"  # a data.frame could be passed to print.data.table() directly, #3363
    if (all(dim(x)==0L)) {
      cat("Null data.",class," (0 rows and 0 cols)\n", sep="")  # See FAQ 2.5 and NEWS item in v1.8.9
    } else {
      cat("Empty data.",class," (", dim(x)[1L], " rows and ",length(x)," cols)", sep="")
      if (length(x)>0L) cat(": ",paste(head(names(x),6L),collapse=","),if(length(x)>6L)"...",sep="")
      cat("\n")
    }
    return(invisible(x))
  }
  if ((topn*2L+1L)<nrow(x) && (nrow(x)>nrows || !topnmiss)) {
    toprint = rbindlist(list(head(x, topn), tail(x, topn)), use.names=FALSE)  # no need to match names because head and tail of same x, and #3306
    rn = c(seq_len(topn), seq.int(to=nrow(x), length.out=topn))
    printdots = TRUE
  } else {
    toprint = x
    rn = seq_len(nrow(x))
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
    widths = dt_width(toprint, class, row.names, col.names)
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
      cut_top(print(toprint, right=TRUE, quote=quote))
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
    cut_top(print(toprint, right=TRUE, quote=quote))
  } else {
    print(toprint, right=TRUE, quote=quote)
  }
  if (trunc.cols && length(not_printed) > 0L)
    # prints names of variables not shown in the print
    trunc_cols_message(not_printed, abbs, class, col.names)

  invisible(x)
}

format.data.table = function (x, ..., justify="none", timezone = FALSE) {
  if (is.atomic(x) && !is.null(x)) {
    stop("Internal structure doesn't seem to be a list. Possibly corrupt data.table.")
  }
  format.item = function(x) {
    if (is.null(x))  # NULL item in a list column
      ""
    else if (is.atomic(x) || inherits(x,"formula")) # FR #2591 - format.data.table issue with columns of class "formula"
      paste(c(format(head(x, 6L), justify=justify, ...), if (length(x) > 6L) "..."), collapse=",")  # fix for #37 - format has to be added here...
    else
      paste0("<", class(x)[1L], paste_dims(x), ">")
  }
  # FR #2842 add timezone for posix timestamps
  format.timezone = function(col) { # paste timezone to a time object
    tz = attr(col,'tzone', exact=TRUE)
    if (!is.null(tz)) { # date object with tz
      nas = is.na(col)
      col = paste0(as.character(col)," ",tz) # parse to character
      col[nas] = NA_character_
    }
    return(col)
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
  do.call("cbind",lapply(x,function(col,...) {
    if (!is.null(dim(col))) return("<multi-column>")
    if(timezone) col = format.timezone(col)
    if (is.list(col)) col = vapply_1c(col, format.item)
    else col = format(char.trunc(col), justify=justify, ...) # added an else here to fix #37
    col
  },...))
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
cut_top = function(x) cat(capture.output(x)[-1L], sep = '\n')

# for printing the dims for list columns #3671; used by format.data.table()
paste_dims = function(x) {
  dims = if (isS4(x)) {
    length(slotNames(x))
  } else {
    if (is.null(dim(x))) length(x) else dim(x)
  }
  paste0("[", paste(dims,collapse="x"), "]")
}

# to calculate widths of data.table for PR #4074
# gets the width of the data.table at each column
#   and compares it to the console width
dt_width = function(x, class, row.names, col.names) {
  widths = apply(nchar(x, type='width'), 2L, max)
  if (class) widths = pmax(widths, 6L)
  if (col.names != "none") names = sapply(colnames(x), nchar, type = "width") else names = 0L
  dt_widths = pmax(widths, names)
  rownum_width = if (row.names) as.integer(ceiling(log10(nrow(x)))+2) else 0L
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
  cat(sprintf(
    ngettext(n,
             "%d variable not shown: %s\n",
             "%d variables not shown: %s\n"),
    n, brackify(paste0(not_printed, classes))
  ))
}

