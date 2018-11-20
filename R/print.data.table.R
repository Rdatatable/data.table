# Moved here out from data.table.R on 10 Aug 2017. See data.table.R for history prior to that.

print.data.table <- function(x, topn=getOption("datatable.print.topn"),
               nrows=getOption("datatable.print.nrows"),
               class=getOption("datatable.print.class"),
               row.names=getOption("datatable.print.rownames"),
               col.names=getOption("datatable.print.colnames"),
               print.keys=getOption("datatable.print.keys"),
               quote=FALSE, ...) {    # topn  - print the top topn and bottom topn rows with '---' inbetween (5)
  # nrows - under this the whole (small) table is printed, unless topn is provided (100)
  # class - should column class be printed underneath column name? (FALSE)
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
    SYS <- sys.calls()
    if (length(SYS) <= 2L ||  # "> DT" auto-print or "> print(DT)" explicit print (cannot distinguish from R 3.2.0 but that's ok)
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
  if (nrow(x) == 0L) {
    if (length(x)==0L)
       cat("Null data.table (0 rows and 0 cols)\n")  # See FAQ 2.5 and NEWS item in v1.8.9
    else
       cat("Empty data.table (0 rows) of ",length(x)," col",if(length(x)>1L)"s",": ",paste(head(names(x),6L),collapse=","),if(ncol(x)>6L)"...","\n",sep="")
    return(invisible(x))
  }
  if ((topn*2+1)<nrow(x) && (nrow(x)>nrows || !topnmiss)) {
    toprint = rbind(head(x, topn), tail(x, topn))
    rn = c(seq_len(topn), seq.int(to=nrow(x), length.out=topn))
    printdots = TRUE
  } else {
    toprint = x
    rn = seq_len(nrow(x))
    printdots = FALSE
  }
  toprint=format.data.table(toprint, na.encode=FALSE, ...)  # na.encode=FALSE so that NA in character cols print as <NA>

  if ((!"bit64" %chin% loadedNamespaces()) && any(sapply(x,inherits,"integer64"))) require_bit64()
  # When we depend on R 3.2.0 (Apr 2015) we can use isNamespaceLoaded() added then, instead of %chin% above

  # FR #5020 - add row.names = logical argument to print.data.table
  if (isTRUE(row.names)) rownames(toprint)=paste0(format(rn,right=TRUE,scientific=FALSE),":") else rownames(toprint)=rep.int("", nrow(toprint))
  if (is.null(names(x)) || all(names(x) == ""))
    # fixes bug #97 (RF#4934) and #545 (RF#5253)
    colnames(toprint)=rep("", ncol(toprint))
  if (isTRUE(class) && col.names != "none") {
    #Matching table for most common types & their abbreviations
    class_abb = c(list = "<list>", integer = "<int>", numeric = "<num>",
      character = "<char>", Date = "<Date>", complex = "<cplx>",
      factor = "<fctr>", POSIXct = "<POSc>", logical = "<lgcl>",
      IDate = "<IDat>", integer64 = "<i64>", raw = "<raw>",
      expression = "<expr>", ordered = "<ord>")
    classes = vapply(x, function(col) class(col)[1L], "", USE.NAMES=FALSE)
    abbs = unname(class_abb[classes])
    if ( length(idx <- which(is.na(abbs))) )
    abbs[idx] = paste0("<", classes[idx], ">")
    toprint = rbind(abbs, toprint)
    rownames(toprint)[1L] = ""
  }
  if (quote) colnames(toprint) <- paste0('"', old <- colnames(toprint), '"')
  if (printdots) {
    toprint = rbind(head(toprint, topn + isTRUE(class)), "---"="", tail(toprint, topn))
    rownames(toprint) = format(rownames(toprint), justify="right")
    if (col.names == "none") {
      cut_top(print(toprint, right=TRUE, quote=quote))
    } else {
      print(toprint, right=TRUE, quote=quote)
    }
    return(invisible(x))
  }
  if (nrow(toprint)>20L && col.names == "auto")
    # repeat colnames at the bottom if over 20 rows so you don't have to scroll up to see them
    #   option to shut this off per request of Oleg Bondar on SO, #1482
    toprint=rbind(toprint, matrix(if (quote) old else colnames(toprint), nrow=1L)) # fixes bug #4934
  if (col.names == "none") {
    cut_top(print(toprint, right=TRUE, quote=quote))
  } else {
    print(toprint, right=TRUE, quote=quote)
  }
  invisible(x)
}

format.data.table <- function (x, ..., justify="none") {
  if (is.atomic(x) && !is.null(x)) {
    stop("Internal structure doesn't seem to be a list. Possibly corrupt data.table.")
  }
  format.item <- function(x) {
    if (is.null(x))  # NULL item in a list column
      ""
    else if (is.atomic(x) || inherits(x,"formula")) # FR #2591 - format.data.table issue with columns of class "formula"
      paste(c(format(head(x, 6L), justify=justify, ...), if (length(x) > 6L) "..."), collapse=",")  # fix for #5435 - format has to be added here...
    else
      paste0("<", class(x)[1L], ">")
  }
  # FR #1091 for pretty printing of character
  # TODO: maybe instead of doing "this is...", we could do "this ... test"?
  char.trunc <- function(x, trunc.char = getOption("datatable.prettyprint.char")) {
    trunc.char = max(0L, suppressWarnings(as.integer(trunc.char[1L])), na.rm=TRUE)
    if (!is.character(x) || trunc.char <= 0L) return(x)
    idx = which(nchar(x) > trunc.char)
    x[idx] = paste0(substr(x[idx], 1L, as.integer(trunc.char)), "...")
    x
  }
  do.call("cbind",lapply(x,function(col,...){
    if (!is.null(dim(col))) stop("Invalid column: it has dimensions. Can't format it. If it's the result of data.table(table()), use as.data.table(table()) instead.")
    if (is.list(col)) col = vapply_1c(col, format.item)
    else col = format(char.trunc(col), justify=justify, ...) # added an else here to fix #5435
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

