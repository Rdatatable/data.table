if (!exists("trimws", "package:base")) {
  # trimws was new in R 3.2.0. Backport it for internal data.table use in R 3.1.0
  trimws = function(x) {
    mysub = function(re, x) sub(re, "", x, perl = TRUE)
    mysub("[ \t\r\n]+$", mysub("^[ \t\r\n]+", x))
  }
}

dim.data.table = function(x)
{
  .Call(Cdim, x)
}

.global = new.env()  # thanks to: http://stackoverflow.com/a/12605694/403310
setPackageName("data.table",.global)
.global$print = ""

# NB: if adding to/editing this list, be sure to do the following:
#   (1) add to man/special-symbols.Rd
#   (2) export() in NAMESPACE
#   (3) add to vignettes/datatable-importing.Rmd#globals section
.SD = .N = .I = .GRP = .NGRP = .BY = .EACHI = NULL
# These are exported to prevent NOTEs from R CMD check, and checkUsage via compiler.
# But also exporting them makes it clear (to users and other packages) that data.table uses these as symbols.
# And NULL makes it clear (to the R's mask check on loading) that they're variables not functions.
# utils::globalVariables(c(".SD",".N")) was tried as well, but exporting seems better.
# So even though .BY doesn't appear in this file, it should still be NULL here and exported because it's
# defined in SDenv and can be used by users.

is.data.table = function(x) inherits(x, "data.table")
is.ff = function(x) inherits(x, "ff")  # define this in data.table so that we don't have to require(ff), but if user is using ff we'd like it to work

#NCOL = function(x) {
#    # copied from base, but additionally covers data.table via is.list()
#    # because NCOL in base explicitly tests using is.data.frame()
#    if (is.list(x) && !is.ff(x)) return(length(x))
#    if (is.array(x) && length(dim(x)) > 1L) ncol(x) else as.integer(1L)
#}
#NROW = function(x) {
#    if (is.data.frame(x) || is.data.table(x)) return(nrow(x))
#    if (is.list(x) && !is.ff(x)) stop("List is not a data.frame or data.table. Convert first before using NROW")   # list may have different length elements, which data.table and data.frame's resolve.
#    if (is.array(x)) nrow(x) else length(x)
#}

null.data.table = function() {
  ans = list()
  setattr(ans,"class",c("data.table","data.frame"))
  setattr(ans,"row.names",.set_row_names(0L))
  setalloccol(ans)
}

data.table = function(..., keep.rownames=FALSE, check.names=FALSE, key=NULL, stringsAsFactors=FALSE)
{
  # NOTE: It may be faster in some circumstances for users to create a data.table by creating a list l
  #       first, and then setattr(l,"class",c("data.table","data.frame")) and forgo checking.
  x = list(...)   # list() doesn't copy named inputs as from R >= 3.1.0 (a very welcome change)
  nd = name_dots(...)
  names(x) = nd$vnames
  if (length(x)==0L) return( null.data.table() )
  if (length(x)==1L && (is.null(x[[1L]]) || (is.list(x[[1L]]) && length(x[[1L]])==0L))) return( null.data.table() ) #48
  ans = as.data.table.list(x, keep.rownames=keep.rownames, check.names=check.names, .named=nd$.named)  # see comments inside as.data.table.list re copies
  if (!is.null(key)) {
    if (!is.character(key)) stop("key argument of data.table() must be character")
    if (length(key)==1L) {
      key = strsplit(key,split=",")[[1L]]
      # eg key="A,B"; a syntax only useful in key argument to data.table(), really.
    }
    setkeyv(ans,key)
  } else {
    # retain key of cbind(DT1, DT2, DT3) where DT2 is keyed but not DT1. cbind calls data.table().
    # If DT inputs with keys have been recycled then can't retain key
    ckey = NULL
    for (i in seq_along(x)) {
      xi = x[[i]]
      if (is.data.table(xi) && haskey(xi) && nrow(xi)==nrow(ans)) ckey=c(ckey, key(xi))
    }
    if (length(ckey) &&
        !anyDuplicated(ckey) &&
        identical(is.na(chmatchdup(c(ckey,ckey), names(ans))), rep(c(FALSE,TRUE),each=length(ckey)))) {
      setattr(ans, "sorted", ckey)
    }
  }
  if (isTRUE(stringsAsFactors)) {
    for (j in which(vapply_1b(ans, is.character))) set(ans, NULL, j, as_factor(.subset2(ans, j)))
    # as_factor is internal function in fread.R currently
  }
  setalloccol(ans)  # returns a NAMED==0 object, unlike data.frame()
}

replace_dot_alias = function(e) {
  # we don't just simply alias .=list because i) list is a primitive (faster to iterate) and ii) we test for use
  # of "list" in several places so it saves having to remember to write "." || "list" in those places
  if (is.call(e) && !is.function(e[[1L]])) {
    # . alias also used within bquote, #1912
    if (e[[1L]] == 'bquote') return(e)
    if (e[[1L]] == ".") e[[1L]] = quote(list)
    for (i in seq_along(e)[-1L]) if (!is.null(e[[i]])) e[[i]] = replace_dot_alias(e[[i]])
  }
  e
}

.massagei = function(x) {
  # J alias for list as well in i, just if the first symbol
  # if x = substitute(base::order) then as.character(x[[1L]]) == c("::", "base", "order")
  if (x %iscall% c("J","."))
    x[[1L]] = quote(list)
  x
}

.checkTypos = function(err, ref) {
  if (grepl('object.*not found', err$message)) {
    used = gsub(".*object '([^']+)'.*", "\\1", err$message)
    found = agrep(used, ref, value=TRUE, ignore.case=TRUE, fixed=TRUE)
    if (length(found)) {
      stop("Object '", used, "' not found. Perhaps you intended ",
           paste(head(found, 5L), collapse=", "),
           if (length(found)<=5L) "" else paste(" or",length(found)-5L, "more"))
    } else {
      stop("Object '", used, "' not found amongst ",
           paste(head(ref, 5L), collapse=', '),
           if (length(ref)<=5L) "" else paste(" and", length(ref)-5L, "more"))
    }
  } else {
    stop(err$message, call.=FALSE)
  }
}

"[.data.table" = function (x, i, j, by, keyby, with=TRUE, nomatch=getOption("datatable.nomatch", NA), mult="all", roll=FALSE, rollends=if (roll=="nearest") c(TRUE,TRUE) else if (roll>=0) c(FALSE,TRUE) else c(TRUE,FALSE), which=FALSE, .SDcols, verbose=getOption("datatable.verbose"), allow.cartesian=getOption("datatable.allow.cartesian"), drop=NULL, on=NULL)
{
  # ..selfcount <<- ..selfcount+1  # in dev, we check no self calls, each of which doubles overhead, or could
  # test explicitly if the caller is [.data.table (even stronger test. TO DO.)
  # the drop=NULL is to sink drop argument when dispatching to [.data.frame; using '...' stops test 147
  if (!cedta()) {
    # Fix for #500 (to do)
    Nargs = nargs() - (!missing(drop))
    ans = if (Nargs<3L) { `[.data.frame`(x,i) }  # drop ignored anyway by DF[i]
        else if (missing(drop)) `[.data.frame`(x,i,j)
        else `[.data.frame`(x,i,j,drop)
    # added is.data.table(ans) check to fix bug #81
    if (!missing(i) & is.data.table(ans)) setkey(ans,NULL)  # See test 304
    return(ans)
  }
  if (!missing(verbose)) {
    stopifnot(isTRUEorFALSE(verbose))
    # set the global verbose option because that is fetched from C code without having to pass it through
    oldverbose = options(datatable.verbose=verbose)
    on.exit(options(oldverbose))
  }
  .global$print=""
  missingby = missing(by) && missing(keyby)  # for tests 359 & 590 where passing by=NULL results in data.table not vector
  if (!missing(keyby)) {
    if (!missing(by)) stop("Provide either by= or keyby= but not both")
    if (missing(j)) { warning("Ignoring keyby= because j= is not supplied"); keyby=NULL; }
    by=bysub=substitute(keyby)
    keyby=TRUE
    # Assign to 'by' so that by is no longer missing and we can proceed as if there were one by
  } else {
    if (!missing(by) && missing(j)) { warning("Ignoring by= because j= is not supplied"); by=NULL; }
    by=bysub= if (missing(by)) NULL else substitute(by)
    keyby=FALSE
  }
  bynull = !missingby && is.null(by) #3530
  byjoin = !is.null(by) && is.symbol(bysub) && bysub==".EACHI"
  naturaljoin = FALSE
  names_x = names(x)
  if (missing(i) && !missing(on)) {
    tt = eval.parent(.massagei(substitute(on)))
    if (!is.list(tt) || !length(names(tt))) {
      warning("When on= is provided but not i=, on= must be a named list or data.table|frame, and a natural join (i.e. join on common names) is invoked. Ignoring on= which is '",class(tt)[1L],"'.")
      on = NULL
    } else {
      i = tt
      naturaljoin = TRUE
    }
  }
  if (missing(i) && missing(j)) {
    tt_isub = substitute(i)
    tt_jsub = substitute(j)
    if (!is.null(names(sys.call())) &&  # not relying on nargs() as it considers DT[,] to have 3 arguments, #3163
        tryCatch(!is.symbol(tt_isub), error=function(e)TRUE) &&   # a symbol that inherits missingness from caller isn't missing for our purpose; test 1974
        tryCatch(!is.symbol(tt_jsub), error=function(e)TRUE)) {
      warning("i and j are both missing so ignoring the other arguments. This warning will be upgraded to error in future.")
    }
    return(x)
  }
  if (!mult %chin% c("first","last","all")) stop("mult argument can only be 'first', 'last' or 'all'")
  missingroll = missing(roll)
  if (length(roll)!=1L || is.na(roll)) stop("roll must be a single TRUE, FALSE, positive/negative integer/double including +Inf and -Inf or 'nearest'")
  if (is.character(roll)) {
    if (roll!="nearest") stop("roll is '",roll,"' (type character). Only valid character value is 'nearest'.")
  } else {
    roll = if (isTRUE(roll)) +Inf else as.double(roll)
  }
  force(rollends)
  if (!is.logical(rollends)) stop("rollends must be a logical vector")
  if (length(rollends)>2L) stop("rollends must be length 1 or 2")
  if (length(rollends)==1L) rollends=rep.int(rollends,2L)
  # TO DO (document/faq/example). Removed for now ... if ((roll || rolltolast) && missing(mult)) mult="last" # for when there is exact match to mult. This does not control cases where the roll is mult, that is always the last one.
  .unsafe.opt() #3585
  missingnomatch = missing(nomatch)
  if (is.null(nomatch)) nomatch = 0L # allow nomatch=NULL API already now, part of: https://github.com/Rdatatable/data.table/issues/857
  if (!is.na(nomatch) && nomatch!=0L) stop("nomatch= must be either NA or NULL (or 0 for backwards compatibility which is the same as NULL)")
  nomatch = as.integer(nomatch)
  if (!is.logical(which) || length(which)>1L) stop("which= must be a logical vector length 1. Either FALSE, TRUE or NA.")
  if ((isTRUE(which)||is.na(which)) && !missing(j)) stop("which==",which," (meaning return row numbers) but j is also supplied. Either you need row numbers or the result of j, but only one type of result can be returned.")
  if (!is.na(nomatch) && is.na(which)) stop("which=NA with nomatch=0 would always return an empty vector. Please change or remove either which or nomatch.")
  if (!with && missing(j)) stop("j must be provided when with=FALSE")
  irows = NULL  # Meaning all rows. We avoid creating 1:nrow(x) for efficiency.
  notjoin = FALSE
  rightcols = leftcols = integer()
  optimizedSubset = FALSE ## flag: tells whether a normal query was optimized into a join.
  ..syms = NULL
  av = NULL
  jsub = NULL
  if (!missing(j)) {
    jsub = replace_dot_alias(substitute(j))
    root = if (is.call(jsub)) as.character(jsub[[1L]])[1L] else ""
    if (root == ":" ||
        (root %chin% c("-","!") && jsub[[2L]] %iscall% '(' && jsub[[2L]][[2L]] %iscall% ':') ||
        ( (!length(av<-all.vars(jsub)) || all(substring(av,1L,2L)=="..")) &&
          root %chin% c("","c","paste","paste0","-","!") &&
          missingby )) {   # test 763. TODO: likely that !missingby iff with==TRUE (so, with can be removed)
      # When no variable names (i.e. symbols) occur in j, scope doesn't matter because there are no symbols to find.
      # If variable names do occur, but they are all prefixed with .., then that means look up in calling scope.
      # Automatically set with=FALSE in this case so that DT[,1], DT[,2:3], DT[,"someCol"] and DT[,c("colB","colD")]
      # work as expected.  As before, a vector will never be returned, but a single column data.table
      # for type consistency with >1 cases. To return a single vector use DT[["someCol"]] or DT[[3]].
      # The root==":" is to allow DT[,colC:colH] even though that contains two variable names.
      # root == "-" or "!" is for tests 1504.11 and 1504.13 (a : with a ! or - modifier root)
      # We don't want to evaluate j at all in making this decision because i) evaluating could itself
      # increment some variable and not intended to be evaluated a 2nd time later on and ii) we don't
      # want decisions like this to depend on the data or vector lengths since that can introduce
      # inconsistency reminiscent of drop=TRUE in [.data.frame that we seek to avoid.
      with=FALSE
      if (length(av)) {
        for (..name in av) {
          name = substring(..name, 3L)
          if (name=="") stop("The symbol .. is invalid. The .. prefix must be followed by at least one character.")
          if (!exists(name, where=parent.frame())) {
            stop("Variable '",name,"' is not found in calling scope. Looking in calling scope because you used the .. prefix.",
              if (exists(..name, where=parent.frame()))
                paste0(" Variable '..",name,"' does exist in calling scope though, so please just removed the .. prefix from that variable name in calling scope.")
                # We have recommended 'manual' .. prefix in the past, so try to be helpful
              else
                ""
            )
          } else if (exists(..name, where=parent.frame())) {
            warning("Both '",name,"' and '..", name, "' exist in calling scope. Please remove the '..", name,"' variable in calling scope for clarity.")
          }
        }
        ..syms = av
      }
    } else if (is.name(jsub)) {
      if (substring(jsub, 1L, 2L) == "..") stop("Internal error:  DT[, ..var] should be dealt with by the branch above now.") # nocov
      if (!with && !exists(as.character(jsub), where=parent.frame()))
        stop("Variable '",jsub,"' is not found in calling scope. Looking in calling scope because you set with=FALSE. Also, please use .. symbol prefix and remove with=FALSE.")
    }
    if (root=="{") {
      if (length(jsub) == 2L) {
        jsub = jsub[[2L]]  # to allow {} wrapping of := e.g. [,{`:=`(...)},] [#376]
        root = if (is.call(jsub)) as.character(jsub[[1L]])[1L] else ""
      } else if (length(jsub) > 2L && jsub[[2L]] %iscall% ":=") {
        #2142 -- j can be {} and have length 1
        stop("You have wrapped := with {} which is ok but then := must be the only thing inside {}. You have something else inside {} as well. Consider placing the {} on the RHS of := instead; e.g. DT[,someCol:={tmpVar1<-...;tmpVar2<-...;tmpVar1*tmpVar2}")
      }
    }
    if (root=="eval" && !any(all.vars(jsub[[2L]]) %chin% names_x)) {
      # TODO: this && !any depends on data. Can we remove it?
      # Grab the dynamic expression from calling scope now to give the optimizer a chance to optimize it
      # Only when top level is eval call.  Not nested like x:=eval(...) or `:=`(x=eval(...), y=eval(...))
      jsub = eval(jsub[[2L]], parent.frame(), parent.frame())  # this evals the symbol to return the dynamic expression
      if (is.expression(jsub)) jsub = jsub[[1L]]    # if expression, convert it to call
      # Note that the dynamic expression could now be := (new in v1.9.7)
      root = if (is.call(jsub)) {
        jsub = replace_dot_alias(jsub)
        as.character(jsub[[1L]])[1L]
      } else ""
    }
    if (root == ":=") {
      allow.cartesian=TRUE   # (see #800)
      if (!missing(i) && keyby)
        stop(":= with keyby is only possible when i is not supplied since you can't setkey on a subset of rows. Either change keyby to by or remove i")
      if (!missingnomatch) {
        warning("nomatch isn't relevant together with :=, ignoring nomatch")
        nomatch=0L
      }
    }
  }

  # setdiff removes duplicate entries, which'll create issues with duplicated names. Use %chin% instead.
  dupdiff = function(x, y) x[!x %chin% y]

  if (!missing(i)) {
    xo = NULL
    isub = substitute(i)
    if (identical(isub, NA)) {
      # only possibility *isub* can be NA (logical) is the symbol NA itself; i.e. DT[NA]
      # replace NA in this case with NA_integer_ as that's almost surely what user intended to
      # return a single row with NA in all columns. (DT[0] returns an empty table, with correct types.)
      # Any expression (including length 1 vectors) that evaluates to a single NA logical will
      # however be left as NA logical since that's important for consistency to return empty in that
      # case; e.g. DT[Col==3] where DT is 1 row and Col contains NA.
      # Replacing the NA symbol makes DT[NA] and DT[c(1,NA)] consistent and provides
      # an easy way to achieve a single row of NA as users expect rather than requiring them
      # to know and change to DT[NA_integer_].
      isub=NA_integer_
    }
    isnull_inames = FALSE
    # Fixes 4994: a case where quoted expression with a "!", ex: expr = quote(!dt1); dt[eval(expr)] requires
    # the "eval" to be checked before `as.name("!")`. Therefore interchanged.
    restore.N = remove.N = FALSE
    if (exists(".N", envir=parent.frame(), inherits=FALSE)) {
      old.N = get(".N", envir=parent.frame(), inherits=FALSE)
      locked.N = bindingIsLocked(".N", parent.frame())
      if (locked.N) eval(call("unlockBinding", ".N", parent.frame()))  # eval call to pass R CMD check NOTE until we find cleaner way
      assign(".N", nrow(x), envir=parent.frame(), inherits=FALSE)
      restore.N = TRUE
      # the comment below is invalid hereafter (due to fix for #1145)
      # binding locked when .SD[.N] but that's ok as that's the .N we want anyway

      # TO DO: change isub at C level s/.N/nrow(x); changing a symbol to a constant should be ok
    } else {
       assign(".N", nrow(x), envir=parent.frame(), inherits=FALSE)
       remove.N = TRUE
    }
    if (isub %iscall% "eval") {  # TO DO: or ..()
      isub = eval(.massagei(isub[[2L]]), parent.frame(), parent.frame())
      if (is.expression(isub)) isub=isub[[1L]]
    }
    if (isub %iscall% "!") {
      notjoin = TRUE
      if (!missingnomatch) stop("not-join '!' prefix is present on i but nomatch is provided. Please remove nomatch.");
      nomatch = 0L
      isub = isub[[2L]]
      # #932 related so that !(v1 == 1) becomes v1 == 1 instead of (v1 == 1) after removing "!"
      if (isub %iscall% "(" && !is.name(isub[[2L]]))
        isub = isub[[2L]]
    }

    if (is.null(isub)) return( null.data.table() )

    if (length(o <- .prepareFastSubset(isub = isub, x = x,
                                              enclos =  parent.frame(),
                                              notjoin = notjoin, verbose = verbose))){
      ## redirect to the is.data.table(x) == TRUE branch.
      ## Additional flag to adapt things after bmerge:
      optimizedSubset = TRUE
      notjoin = o$notjoin
      i = o$i
      on = o$on
      ## the following two are ignored if i is not a data.table.
      ## Since we are converting i to data.table, it is important to set them properly.
      nomatch = 0L
      mult = "all"
    }
    else if (!is.name(isub)) {
      ienv = new.env(parent=parent.frame())
      if (getOption("datatable.optimize")>=1L) assign("order", forder, ienv)
      i = tryCatch(eval(.massagei(isub), x, ienv), error=function(e) {
        if (grepl(":=.*defined for use in j.*only", e$message))
          stop("Operator := detected in i, the first argument inside DT[...], but is only valid in the second argument, j. Most often, this happens when forgetting the first comma (e.g. DT[newvar := 5] instead of DT[ , new_var := 5]). Please double-check the syntax. Run traceback(), and debugger() to get a line number.")
        else
          .checkTypos(e, names_x)
      })
    } else {
      # isub is a single symbol name such as B in DT[B]
      i = try(eval(isub, parent.frame(), parent.frame()), silent=TRUE)
      if (inherits(i,"try-error")) {
        # must be "not found" since isub is a mere symbol
        col = try(eval(isub, x), silent=TRUE)  # is it a column name?
        msg = if (inherits(col,"try-error")) " and it is not a column name either."
        else paste0(" but it is a column of type ", typeof(col),". If you wish to select rows where that column contains TRUE",
                    ", or perhaps that column contains row numbers of itself to select, try DT[(col)], DT[DT$col], or DT[col==TRUE] is particularly clear and is optimized.")
        stop(as.character(isub), " is not found in calling scope", msg,
             " When the first argument inside DT[...] is a single symbol (e.g. DT[var]), data.table looks for var in calling scope.")
      }
    }
    if (restore.N) {
      assign(".N", old.N, envir=parent.frame())
      if (locked.N) lockBinding(".N", parent.frame())
    }
    if (remove.N) rm(list=".N", envir=parent.frame())
    if (is.matrix(i)) {
      if (is.numeric(i) && ncol(i)==1L) { # #826 - subset DT on single integer vector stored as matrix
        i = as.integer(i)
      } else {
        stop("i is invalid type (matrix). Perhaps in future a 2 column matrix could return a list of elements of DT (in the spirit of A[B] in FAQ 2.14). Please report to data.table issue tracker if you'd like this, or add your comments to FR #657.")
      }
    }
    if (is.logical(i)) {
      if (notjoin) {
        notjoin = FALSE
        i = !i
      }
    }
    if (is.null(i)) return( null.data.table() )
    if (is.character(i)) {
      isnull_inames = TRUE
      i = data.table(V1=i)   # for user convenience; e.g. DT["foo"] without needing DT[.("foo")]
    } else if (identical(class(i),"list") && length(i)==1L && is.data.frame(i[[1L]])) { i = as.data.table(i[[1L]]) }
    else if (identical(class(i),"data.frame")) { i = as.data.table(i) }   # TO DO: avoid these as.data.table() and use a flag instead
    else if (identical(class(i),"list")) {
      isnull_inames = is.null(names(i))
      i = as.data.table(i)
    }

    if (is.data.table(i)) {
      if (missing(on)) {
        if (!haskey(x)) {
          stop("When i is a data.table (or character vector), the columns to join by must be specified using 'on=' argument (see ?data.table), by keying x (i.e. sorted, and, marked as sorted, see ?setkey), or by sharing column names between x and i (i.e., a natural join). Keyed joins might have further speed benefits on very large data due to x being sorted in RAM.")
        }
      } else if (identical(substitute(on), as.name(".NATURAL"))) {
        naturaljoin = TRUE
      }
      if (naturaljoin) { # natural join #629
        common_names = intersect(names_x, names(i))
        len_common_names = length(common_names)
        if (!len_common_names) stop("Attempting to do natural join but no common columns in provided tables")
        if (verbose) {
          which_cols_msg = if (len_common_names == length(x)) " all 'x' columns"
          else paste(":", brackify(common_names))
          cat("Joining but 'x' has no key, natural join using", which_cols_msg, "\n", sep = "")
        }
        on = common_names
      }
      if (!missing(on)) {
        # on = .() is now possible, #1257
        on_ops = .parse_on(substitute(on), isnull_inames)
        on = on_ops[[1L]]
        ops = on_ops[[2L]]
        if (any(ops > 1L)) { ## fix for #4489;  ops = c("==", "<=", "<", ">=", ">", "!=")
          allow.cartesian = TRUE
        }
        # TODO: collect all '==' ops first to speeden up Cnestedid
        rightcols = colnamesInt(x, names(on), check_dups=FALSE)
        leftcols  = colnamesInt(i, unname(on), check_dups=FALSE)
      } else {
        ## missing on
        rightcols = chmatch(key(x), names_x)   # NAs here (i.e. invalid data.table) checked in bmerge()
        leftcols = if (haskey(i))
          chmatch(head(key(i), length(rightcols)), names(i))
        else
          seq_len(min(length(i),length(rightcols)))
        rightcols = head(rightcols,length(leftcols))
        ops = rep(1L, length(leftcols))
      }
      # Implementation for not-join along with by=.EACHI, #604
      if (notjoin && (byjoin || mult != "all")) { # mult != "all" needed for #1571
        notjoin = FALSE
        if (verbose) {last.started.at=proc.time();cat("not-join called with 'by=.EACHI'; Replacing !i with i=setdiff_(x,i) ...");flush.console()}
        orignames = copy(names(i))
        i = setdiff_(x, i, rightcols, leftcols) # part of #547
        if (verbose) {cat("done in",timetaken(last.started.at),"\n"); flush.console()}
        setnames(i, orignames[leftcols])
        setattr(i, 'sorted', names(i)) # since 'x' has key set, this'll always be sorted
      }
      i = .shallow(i, retain.key = TRUE)
      ans = bmerge(i, x, leftcols, rightcols, roll, rollends, nomatch, mult, ops, verbose=verbose)
      xo = ans$xo ## to make it available for further use.
      # temp fix for issue spotted by Jan, test #1653.1. TODO: avoid this
      # 'setorder', as there's another 'setorder' in generating 'irows' below...
      if (length(ans$indices)) setorder(setDT(ans[1L:3L]), indices)
      allLen1 = ans$allLen1
      f__ = ans$starts
      len__ = ans$lens
      allGrp1 = all(ops==1L) # was previously 'ans$allGrp1'. Fixing #1991. TODO: Revisit about allGrp1 possibility for speedups in certain cases when I find some time.
      indices__ = if (length(ans$indices)) ans$indices else seq_along(f__) # also for #1991 fix
      # length of input nomatch (single 0 or NA) is 1 in both cases.
      # When no match, len__ is 0 for nomatch=0 and 1 for nomatch=NA, so len__ isn't .N
      # If using secondary key of x, f__ will refer to xo
      if (is.na(which)) {
        w = if (notjoin) f__!=0L else is.na(f__)
        return( if (length(xo)) fsort(xo[w], internal=TRUE) else which(w) )
      }
      if (mult=="all") {
        # is by=.EACHI along with non-equi join?
        nqbyjoin = byjoin && length(ans$indices) && !allGrp1
        if (!byjoin || nqbyjoin) {
          # Really, `anyDuplicated` in base is AWESOME!
          # allow.cartesian shouldn't error if a) not-join, b) 'i' has no duplicates
          if (verbose) {last.started.at=proc.time();cat("Constructing irows for '!byjoin || nqbyjoin' ... ");flush.console()}
          irows = if (allLen1) f__ else vecseq(f__,len__,
            if (allow.cartesian ||
                notjoin || # #698. When notjoin=TRUE, ignore allow.cartesian. Rows in answer will never be > nrow(x).
                !anyDuplicated(f__, incomparables = c(0L, NA_integer_))) {
              NULL # #742. If 'i' has no duplicates, ignore
            } else as.double(nrow(x)+nrow(i))) # rows in i might not match to x so old max(nrow(x),nrow(i)) wasn't enough. But this limit now only applies when there are duplicates present so the reason now for nrow(x)+nrow(i) is just to nail it down and be bigger than max(nrow(x),nrow(i)).
          if (verbose) {cat(timetaken(last.started.at),"\n"); flush.console()}
          # Fix for #1092 and #1074
          # TODO: implement better version of "any"/"all"/"which" to avoid
          # unnecessary construction of logical vectors
          if (identical(nomatch, 0L) && allLen1) irows = irows[irows != 0L]
        } else {
          if (length(xo) && missing(on))
            stop("Internal error. Cannot by=.EACHI when joining to a secondary key, yet") # nocov
          # since f__ refers to xo later in grouping, so xo needs to be passed through to dogroups too.
          if (length(irows))
            stop("Internal error. irows has length in by=.EACHI") # nocov
        }
        if (nqbyjoin) {
          irows = if (length(xo)) xo[irows] else irows
          xo = setorder(setDT(list(indices=rep.int(indices__, len__), irows=irows)))[["irows"]]
          ans = .Call(CnqRecreateIndices, xo, len__, indices__, max(indices__), nomatch) # issue#4388 fix
          f__ = ans[[1L]]; len__ = ans[[2L]]
          allLen1 = FALSE # TODO; should this always be FALSE?
          irows = NULL # important to reset
          if (any_na(as_list(xo))) xo = xo[!is.na(xo)]
        }
      } else {
        if (!byjoin) { #1287 and #1271
          irows = f__ # len__ is set to 1 as well, no need for 'pmin' logic
          if (identical(nomatch,0L)) irows = irows[len__>0L]  # 0s are len 0, so this removes -1 irows
        }
        # TODO: when nomatch=NA, len__ need not be allocated / set at all for mult="first"/"last"?
        # TODO: how about when nomatch=0L, can we avoid allocating then as well?
      }
      if (length(xo) && length(irows)) {
        irows = xo[irows]   # TO DO: fsort here?
        if (mult=="all" && !allGrp1) { # following #1991 fix, !allGrp1 will always be TRUE. TODO: revisit.
          if (verbose) {last.started.at=proc.time();cat("Reorder irows for 'mult==\"all\" && !allGrp1' ... ");flush.console()}
          irows = setorder(setDT(list(indices=rep.int(indices__, len__), irows=irows)))[["irows"]]
          if (verbose) {cat(timetaken(last.started.at),"\n"); flush.console()}
        }
      }
      if (optimizedSubset){
        ## special treatment for calls like DT[x == 3] that are transformed into DT[J(x=3), on = "x==x"]

        if(!.Call(CisOrderedSubset, irows, nrow(x))){
          ## restore original order. This is a very expensive operation.
          ## benchmarks have shown that starting with 1e6 irows, a tweak can significantly reduce time
          ## (see #2366)
          if (verbose) {last.started.at=proc.time()[3L];cat("Reordering", length(irows), "rows after bmerge done in ... ");flush.console()}
          if(length(irows) < 1e6){
            irows = fsort(irows, internal=TRUE) ## internally, fsort on integer falls back to forderv
            } else {
              irows = as.integer(fsort(as.numeric(irows))) ## nocov; parallelized for numeric, but overhead of type conversion
            }
          if (verbose) {cat(round(proc.time()[3L]-last.started.at,3L),"secs\n");flush.console()}
        }
        ## make sure, all columns are taken from x and not from i.
        ## This is done by simply telling data.table to continue as if there was a simple subset
        leftcols  = integer(0L)
        rightcols = integer(0L)
        i = irows ## important to make i not a data.table because otherwise Gforce doesn't kick in
      }
    }
    else {
      if (!missing(on)) {
        stop("logical error. i is not a data.table, but 'on' argument is provided.")
      }
      # TO DO: TODO: Incorporate which_ here on DT[!i] where i is logical. Should avoid i = !i (above) - inefficient.
      # i is not a data.table
      if (!is.logical(i) && !is.numeric(i)) stop("i has evaluated to type ", typeof(i), ". Expecting logical, integer or double.")
      if (is.logical(i)) {
        if (length(i)==1L  # to avoid unname copy when length(i)==nrow (normal case we don't want to slow down)
          && isTRUE(unname(i))) { irows=i=NULL }  # unname() for #2152 - length 1 named logical vector.
        # NULL is efficient signal to avoid creating 1:nrow(x) but still return all rows, fixes #1249

        else if (length(i)<=1L) { irows=i=integer(0L) }
        # FALSE, NA and empty. All should return empty data.table. The NA here will be result of expression,
        # where for consistency of edge case #1252 all NA to be removed. If NA is a single NA symbol, it
        # was auto converted to NA_integer_ higher up for ease of use and convenience. We definitely
        # don't want base R behaviour where DF[NA,] returns an entire copy filled with NA everywhere.

        else if (length(i)==nrow(x)) { irows=i=which(i) }
        # The which() here auto removes NA for convenience so user doesn't need to remember "!is.na() & ..."
        # Also this which() is for consistency of DT[colA>3,which=TRUE] and which(DT[,colA>3])
        # Assigning to 'i' here as well to save memory, #926.

        else stop("i evaluates to a logical vector length ", length(i), " but there are ", nrow(x), " rows. Recycling of logical i is no longer allowed as it hides more bugs than is worth the rare convenience. Explicitly use rep(...,length=.N) if you really need to recycle.")
      } else {
        irows = as.integer(i)  # e.g. DT[c(1,3)] and DT[c(-1,-3)] ok but not DT[c(1,-3)] (caught as error)
        irows = .Call(CconvertNegAndZeroIdx, irows, nrow(x), is.null(jsub) || root!=":=")  # last argument is allowOverMax (NA when selecting, error when assigning)
        # simplifies logic from here on: can assume positive subscripts (no zeros)
        # maintains Arun's fix for #2697 (test 1042)
        # efficient in C with more detailed helpful messages when user mixes positives and negatives
        # falls through quickly (no R level allocs) if all items are within range [1,max] with no zeros or negatives
        # minor TO DO: can we merge this with check_idx in fcast.c/subset ?
      }
    }
    if (notjoin) {
      if (byjoin || !is.integer(irows) || is.na(nomatch)) stop("Internal error: notjoin but byjoin or !integer or nomatch==NA") # nocov
      irows = irows[irows!=0L]
      if (verbose) {last.started.at=proc.time()[3L];cat("Inverting irows for notjoin done in ... ");flush.console()}
      i = irows = if (length(irows)) seq_len(nrow(x))[-irows] else NULL  # NULL meaning all rows i.e. seq_len(nrow(x))
      if (verbose) cat(round(proc.time()[3L]-last.started.at, 3L), "sec\n")
      leftcols = integer()  # proceed as if row subset from now on, length(leftcols) is switched on later
      rightcols = integer()
      # Doing this once here, helps speed later when repeatedly subsetting each column. R's [irows] would do this for each
      # column when irows contains negatives.
    }
    if (which) return( if (is.null(irows)) seq_len(nrow(x)) else irows )
  } else {  # missing(i)
    i = NULL
  }
  names_i = names(i) # value is now stable
  byval = NULL
  xnrow = nrow(x)
  xcols = xcolsAns = icols = icolsAns = integer()
  xdotcols = FALSE
  # track which columns appear in j through ansvars, and those
  #   that belong in .SD through sdvars. Mostly these will be the same,
  #   except in cases like DT[ , lapply(.SD, function(x) x/V1), .SDcols = !'V1'],
  #   see e.g. #484, #495, #1744, #1965.
  non_sdvars = character(0L)
  if (missing(j)) {
    # missingby was already checked above before dealing with i
    if (!length(x)) return(null.data.table())
    if (!length(leftcols)) {
      # basic x[i] subset, #2951
      if (is.null(irows)) return(shallow(x))   # e.g. DT[TRUE] (#3214); otherwise CsubsetDT would materialize a deep copy
      else                return(.Call(CsubsetDT, x, irows, seq_along(x)) )
    } else {
      jisvars = names_i[-leftcols]
      tt = jisvars %chin% names_x
      if (length(tt)) jisvars[tt] = paste0("i.",jisvars[tt])
      if (length(duprightcols <- rightcols[duplicated(rightcols)])) {
        nx = c(names_x, names_x[duprightcols])
        rightcols = chmatchdup(names_x[rightcols], nx)
        nx = make.unique(nx)
      } else nx = names_x
      ansvars = make.unique(c(nx, jisvars))
      icols = c(leftcols, seq_along(i)[-leftcols])
      icolsAns = c(rightcols, seq.int(length(nx)+1L, length.out=ncol(i)-length(unique(leftcols))))
      xcols = xcolsAns = seq_along(x)[-rightcols]
    }
    ansvals = chmatch(ansvars, nx)
  }
  else {
    if (is.data.table(i)) {
      idotprefix = paste0("i.", names_i)
      xdotprefix = paste0("x.", names_x)
    } else idotprefix = xdotprefix = character(0L)

    # j was substituted before dealing with i so that := can set allow.cartesian=FALSE (#800) (used above in i logic)
    if (is.null(jsub)) return(NULL)

    if (!with && jsub %iscall% ":=") {
      # TODO: make these both errors (or single long error in both cases) in next release.
      # i.e. using with=FALSE together with := at all will become an error. Eventually with will be removed.
      if (is.null(names(jsub)) && is.name(jsub[[2L]])) {
        warning("with=FALSE together with := was deprecated in v1.9.4 released Oct 2014. Please wrap the LHS of := with parentheses; e.g., DT[,(myVar):=sum(b),by=a] to assign to column name(s) held in variable myVar. See ?':=' for other examples. As warned in 2014, this is now a warning.")
        jsub[[2L]] = eval(jsub[[2L]], parent.frame(), parent.frame())
      } else {
        warning("with=FALSE ignored, it isn't needed when using :=. See ?':=' for examples.")
      }
      with = TRUE
    }

    if (!with) {
      # missingby was already checked above before dealing with i
      if (jsub %iscall% c("!", "-") && length(jsub)==2L) {  # length 2 to only match unary, #2109
        notj = TRUE
        jsub = jsub[[2L]]
      } else notj = FALSE
      # fix for #1216, make sure the parentheses are peeled from expr of the form (((1:4)))
      while (jsub %iscall% "(") jsub = as.list(jsub)[[-1L]]
      if (jsub %iscall% ":" && length(jsub)==3L) {
        j = eval(jsub, setattr(as.list(seq_along(x)), 'names', names_x), parent.frame()) # else j will be evaluated for the first time on next line
      } else {
        names(..syms) = ..syms
        j = eval(jsub, lapply(substring(..syms,3L), get, pos=parent.frame()), parent.frame())
      }
      if (is.logical(j)) j <- which(j)
      if (!length(j) && !notj) return( null.data.table() )
      if (is.factor(j)) j = as.character(j)  # fix for FR: #358
      if (is.character(j)) {
        if (notj) {
          if (anyNA(idx <- chmatch(j, names_x))) warning("column(s) not removed because not found: ", brackify(j[is.na(idx)]))
          # all duplicates of the name in names(x) must be removed; e.g. data.table(x=1, y=2, x=3)[, !"x"] should just output 'y'.
          w = !names_x %chin% j
          ansvars = names_x[w]
          ansvals = which(w)
        } else {
          # if DT[, c("x","x")] and "x" is duplicated in names(DT), we still subset only the first. Because dups are unusual and
          # it's more common to select the same column a few times. A syntax would be needed to distinguish these intents.
          ansvars = j   # x. and i. prefixes may be in here, they'll result in NA and will be dealt with further below if length(leftcols)
          ansvals = chmatch(ansvars, names_x)   # not chmatchdup()
        }
        if (!length(ansvals)) return(null.data.table())
        if (!length(leftcols)) {
          if (!anyNA(ansvals)) return(.Call(CsubsetDT, x, irows, ansvals))
          else stop("column(s) not found: ", paste(ansvars[is.na(ansvals)],collapse=", "))
        }
        # else the NA in ansvals are for join inherited scope (test 1973), and NA could be in irows from join and data in i should be returned (test 1977)
        #   in both cases leave to the R-level subsetting of i and x together further below
      } else if (is.numeric(j)) {
        j = as.integer(j)
        if (any(w<-(j>ncol(x)))) stop("Item ",which.first(w)," of j is ",j[which.first(w)]," which is outside the column number range [1,ncol=", ncol(x),"]")
        j = j[j!=0L]
        if (any(j<0L)) {
          if (any(j>0L)) stop("j mixes positives and negatives")
          j = seq_along(x)[j]  # all j are <0 here
        }
        # 3013 -- handle !FALSE in column subset in j via logical+with
        if (notj) j = seq_along(x)[if (length(j)) -j else TRUE]
        if (!length(j)) return(null.data.table())
        return(.Call(CsubsetDT, x, irows, j))
      } else {
        stop("When with=FALSE, j-argument should be of type logical/character/integer indicating the columns to select.") # fix for #1440.
      }
    } else {   # with=TRUE and byjoin could be TRUE
      bynames = NULL
      allbyvars = NULL
      if (byjoin) {
        bynames = names_x[rightcols]
      } else if (!missingby) {
        # deal with by before j because we need byvars when j contains .SD
        # may evaluate to NULL | character() | "" | list(), likely a result of a user expression where no-grouping is one case being loop'd through
        bysubl = as.list.default(bysub)
        bysuborig = bysub
        if (is.name(bysub) && !(bysub %chin% names_x)) {  # TO DO: names(x),names(i),and i. and x. prefixes
          bysub = eval(bysub, parent.frame(), parent.frame())
          # fix for # 5106 - http://stackoverflow.com/questions/19983423/why-by-on-a-vector-not-from-a-data-table-column-is-very-slow
          # case where by=y where y is not a column name, and not a call/symbol/expression, but an atomic vector outside of DT.
          # note that if y is a list, this'll return an error (not sure if it should).
          if (is.atomic(bysub)) bysubl = list(bysuborig) else bysubl = as.list.default(bysub)
        }
        if (length(bysubl) && identical(bysubl[[1L]],quote(eval))) {    # TO DO: or by=..()
          bysub = eval(bysubl[[2L]], parent.frame(), parent.frame())
          bysub = replace_dot_alias(bysub) # fix for #1298
          if (is.expression(bysub)) bysub=bysub[[1L]]
          bysubl = as.list.default(bysub)
        } else if (bysub %iscall% c("c","key","names", "intersect", "setdiff")) {
          # catch common cases, so we don't have to copy x[irows] for all columns
          # *** TO DO ***: try() this eval first (as long as not list() or .()) and see if it evaluates to column names
          # to avoid the explicit c,key,names which already misses paste("V",1:10) for example
          #        tried before but since not wrapped in try() it failed on some tests
          # or look for column names used in this by (since if none it wouldn't find column names anyway
          # when evaled within full x[irows]).  Trouble is that colA%%2L is a call and should be within frame.
          tt = eval(bysub, parent.frame(), parent.frame())
          if (!is.character(tt)) stop("by=c(...), key(...) or names(...) must evaluate to 'character'")
          bysub=tt
        } else if (is.call(bysub) && !(bysub[[1L]] %chin% c("list", "as.list", "{", ".", ":"))) {
          # potential use of function, ex: by=month(date). catch it and wrap with "(", because we need to set "bysameorder" to FALSE as we don't know if the function will return ordered results just because "date" is ordered. Fixes #2670.
          bysub = as.call(c(as.name('('), list(bysub)))
          bysubl = as.list.default(bysub)
        } else if (bysub %iscall% ".") bysub[[1L]] = quote(list)

        if (mode(bysub) == "character") {
          if (length(grep(",", bysub, fixed = TRUE))) {
            if (length(bysub)>1L) stop("'by' is a character vector length ",length(bysub)," but one or more items include a comma. Either pass a vector of column names (which can contain spaces, but no commas), or pass a vector length 1 containing comma separated column names. See ?data.table for other possibilities.")
            bysub = strsplit(bysub,split=",")[[1L]]
          }
          backtick_idx = grep("^[^`]+$",bysub)
          if (length(backtick_idx)) bysub[backtick_idx] = paste0("`",bysub[backtick_idx],"`")
          backslash_idx = grep("\\", bysub, fixed = TRUE)
          if (length(backslash_idx)) bysub[backslash_idx] = gsub('\\', '\\\\', bysub[backslash_idx], fixed = TRUE)
          bysub = parse(text=paste0("list(",paste(bysub,collapse=","),")"))[[1L]]
          bysubl = as.list.default(bysub)
        }
        allbyvars = intersect(all.vars(bysub), names_x)
        orderedirows = .Call(CisOrderedSubset, irows, nrow(x))  # TRUE when irows is NULL (i.e. no i clause). Similar but better than is.sorted(f__)
        bysameorder = byindex = FALSE
        if (!bysub %iscall% ":" && ##Fix #4285
            all(vapply_1b(bysubl, is.name))) {
          bysameorder = orderedirows && haskey(x) && length(allbyvars) && identical(allbyvars,head(key(x),length(allbyvars)))
          # either bysameorder or byindex can be true but not both. TODO: better name for bysameorder might be bykeyx
          if (!bysameorder && keyby && !length(irows) && isTRUE(getOption("datatable.use.index"))) {
            # TODO: could be allowed if length(irows)>1 but then the index would need to be squashed for use by uniqlist, #3062
            # find if allbyvars is leading subset of any of the indices; add a trailing "__" to fix #3498 where a longer column name starts with a shorter column name
            tt = paste0(c(allbyvars,""), collapse="__")
            w = which.first(substring(paste0(indices(x),"__"),1L,nchar(tt)) == tt)
            if (!is.na(w)) {
              byindex = indices(x)[w]
              if (!length(getindex(x, byindex))) {
                if (verbose) cat("by index '", byindex, "' but that index has 0 length. Ignoring.\n", sep="")
                byindex=FALSE
              }
            }
          }
        }

        if (is.null(irows)) {
          if (bysub %iscall% ':' && length(bysub)==3L && is.name(bysub[[2L]]) && is.name(bysub[[3L]])) {
            byval = eval(bysub, setattr(as.list(seq_along(x)), 'names', names_x), parent.frame())
            byval = as.list(x)[byval]
          } else byval = eval(bysub, x, parent.frame())
        } else {
          # length 0 when i returns no rows
          if (!is.integer(irows)) stop("Internal error: irows isn't integer") # nocov
          # Passing irows as i to x[] below has been troublesome in a rare edge case.
          # irows may contain NA, 0, negatives and >nrow(x) here. That's all ok.
          # But we may need i join column values to be retained (where those rows have no match), hence we tried eval(isub)
          # in 1.8.3, but this failed test 876.
          # TO DO: Add a test like X[i,sum(v),by=i.x2], or where by includes a join column (both where some i don't match).
          # TO DO: Make xss directly, rather than recursive call.
          if (!is.na(nomatch)) irows = irows[irows!=0L]   # TO DO: can be removed now we have CisSortedSubset
          if (length(allbyvars)) {    ###############  TO DO  TO DO  TO DO  ###############
            if (verbose) cat("i clause present and columns used in by detected, only these subset:",paste(allbyvars,collapse=","),"\n")
            xss = x[irows,allbyvars,with=FALSE,nomatch=nomatch,mult=mult,roll=roll,rollends=rollends]
          } else {
            if (verbose) cat("i clause present but columns used in by not detected. Having to subset all columns before evaluating 'by': '",deparse(by),"'\n",sep="")
            xss = x[irows,nomatch=nomatch,mult=mult,roll=roll,rollends=rollends]
          }
          if (bysub %iscall% ':' && length(bysub)==3L) {
            byval = eval(bysub, setattr(as.list(seq_along(xss)), 'names', names(xss)), parent.frame())
            byval = as.list(xss)[byval]
          } else byval = eval(bysub, xss, parent.frame())
          xnrow = nrow(xss)
          # TO DO: pass xss (x subset) through into dogroups. Still need irows there (for :=), but more condense
          # and contiguous to use xss to form .SD in dogroups than going via irows
        }
        if (!length(byval) && xnrow>0L) {
          # see missingby up above for comments
          # by could be NULL or character(0L) for example (e.g. passed in as argument in a loop of different bys)
          bysameorder = FALSE  # 1st and only group is the entire table, so could be TRUE, but FALSE to avoid
                     # a key of empty character()
          byval = list()
          bynames = allbyvars = NULL
          # the rest now fall through
        } else bynames = names(byval)
        if (is.atomic(byval)) {
          if (is.character(byval) && length(byval)<=ncol(x) && !(is.name(bysub) && bysub %chin% names_x) ) {
            stop("'by' appears to evaluate to column names but isn't c() or key(). Use by=list(...) if you can. Otherwise, by=eval",deparse(bysub)," should work. This is for efficiency so data.table can detect which columns are needed.")
          } else {
            # by may be a single unquoted column name but it must evaluate to list so this is a convenience to users. Could also be a single expression here such as DT[,sum(v),by=colA%%2]
            byval = list(byval)
            bysubl = c(as.name("list"),bysuborig)  # for guessing the column name below
            if (is.name(bysuborig))
              bynames = as.character(bysuborig)
            else
              bynames = names(byval)
          }
        }
        if (!is.list(byval)) stop("'by' or 'keyby' must evaluate to a vector or a list of vectors (where 'list' includes data.table and data.frame which are lists, too)")
        if (length(byval)==1L && is.null(byval[[1L]])) bynull=TRUE #3530 when by=(function()NULL)()
        if (!bynull) for (jj in seq_len(length(byval))) {
          if (!typeof(byval[[jj]]) %chin% ORDERING_TYPES) stop("column or expression ",jj," of 'by' or 'keyby' is type ",typeof(byval[[jj]]),". Do not quote column names. Usage: DT[,sum(colC),by=list(colA,month(colB))]")
        }
        tt = vapply_1i(byval,length)
        if (any(tt!=xnrow)) stop(gettextf("The items in the 'by' or 'keyby' list are length(s) (%s). Each must be length %d; the same length as there are rows in x (after subsetting if i is provided).", paste(tt, collapse=","), xnrow, domain='R-data.table'))
        if (is.null(bynames)) bynames = rep.int("",length(byval))
        if (length(idx <- which(!nzchar(bynames))) && !bynull) {
          # TODO: improve this and unify auto-naming of jsub and bysub
          if (is.name(bysubl[[1L]]) && bysubl[[1L]] == '{') bysubl = bysubl[[length(bysubl)]] # fix for #3156
          for (jj in idx) {
            # Best guess. Use "month" in the case of by=month(date), use "a" in the case of by=a%%2
            byvars = all.vars(bysubl[[jj+1L]], functions = TRUE)
            if (length(byvars) == 1L) tt = byvars
            else {
              # take the first variable that is (1) not eval (#3758) and (2) starts with a character that can't start a variable name
              tt = grep("^eval$|^[^[:alpha:]. ]", byvars, invert=TRUE, value=TRUE)
              # byvars but exclude functions or `0`+`1` becomes `+`
              tt = if (length(tt)) tt[1L] else all.vars(bysubl[[jj+1L]])[1L]
            }
            # fix for #497
            if (length(byvars) > 1L && tt %chin% all.vars(jsub, FALSE)) {
              bynames[jj] = deparse(bysubl[[jj+1L]])
              if (verbose)
                cat("by-expression '", bynames[jj], "' is not named, and the auto-generated name '", tt,
                    "' clashed with variable(s) in j. Therefore assigning the entire by-expression as name.\n", sep="")
            }
            else bynames[jj] = tt
            # if user doesn't like this inferred name, user has to use by=list() to name the column
          }
          # Fix for #1334
          if (any(duplicated(bynames))) {
            bynames = make.unique(bynames)
          }
        }
        setattr(byval, "names", bynames)  # byval is just a list not a data.table hence setattr not setnames
      }

      jvnames = NULL
      drop_dot = function(x) {
        if (length(x)!=1L) stop("Internal error: drop_dot passed ",length(x)," items")  # nocov
        if (identical(substring(x<-as.character(x), 1L, 1L), ".") && x %chin% c(".N", ".I", ".GRP", ".NGRP", ".BY"))
          substring(x, 2L)
        else
          x
      }
      # handle auto-naming of last item of j (e.g. within {} or if/else, #2478)
      #   e.g. DT[, .(a=sum(v), v, .N), by=] should create columns named a, v, N
      do_j_names = function(q) {
        if (!is.call(q) || !is.name(q[[1L]])) return(q)
        if (q[[1L]] %chin% c('list', '.')) {
          q[[1L]] = quote(list)
          qlen = length(q)
          if (qlen>1L) {
            nm = names(q[-1L])   # check list(a=sum(v),v)
            if (is.null(nm)) nm = rep.int("", qlen-1L)
            # attempt to auto-name unnamed columns
            for (jj in which(nm=="")) {
              thisq = q[[jj + 1L]]
              if (missing(thisq)) stop(gettextf("Item %d of the .() or list() passed to j is missing", jj, domain="R-data.table")) #3507
              if (is.name(thisq)) nm[jj] = drop_dot(thisq)
              # TO DO: if call to a[1] for example, then call it 'a' too
            }
            if (!is.null(jvnames) && any(idx <- nm != jvnames))
              warning("Different branches of j expression produced different auto-named columns: ", brackify(sprintf('%s!=%s', nm[idx], jvnames[idx])), '; using the most "last" names', call. = FALSE)
            jvnames <<- nm # TODO: handle if() list(a, b) else list(b, a) better
            setattr(q, "names", NULL)  # drops the names from the list so it's faster to eval the j for each group; reinstated at the end on the result.
          }
          return(q) # else empty list is needed for test 468: adding an empty list column
        }
        if (q[[1L]] == '{') {
          if (!is.null(q[[qlen<-length(q)]])) q[[qlen]] = do_j_names(q[[qlen]])
          return(q)
        }
        if (q[[1L]] == 'if') {
          #explicit NULL would return NULL, assigning NULL would delete that from the expression
          if (!is.null(q[[3L]])) q[[3L]] = do_j_names(q[[3L]])
          if (length(q) == 4L && !is.null(q[[4L]])) q[[4L]] = do_j_names(q[[4L]])
          return(q)
        }
        return(q)
      }
      if (is.name(jsub)) {
        # j is a single unquoted column name
        if (jsub!=".SD") jvnames = drop_dot(jsub)
        # jsub is list()ed after it's eval'd inside dogroups.
      } else jsub = do_j_names(jsub) # else maybe a call to transform or something which returns a list.
      av = all.vars(jsub,TRUE)  # TRUE fixes bug #1294 which didn't see b in j=fns[[b]](c)
      use.I = ".I" %chin% av
      if (any(c(".SD","eval","get","mget") %chin% av)) {
        if (missing(.SDcols)) {
          # here we need to use 'dupdiff' instead of 'setdiff'. Ex: setdiff(c("x", "x"), NULL) will give 'x'.
          # slight memory efficiency boost if we only store sdvars if it differs from ansvars, but a bit tricky to examine all the uses here
          ansvars = sdvars = dupdiff(names_x, union(bynames, allbyvars))   # TO DO: allbyvars here for vars used by 'by'. Document.
          # just using .SD in j triggers all non-by columns in the subset even if some of
          # those columns are not used. It would be tricky to detect whether the j expression
          # really does use all of the .SD columns or not, hence .SDcols for grouping
          # over a subset of columns

          # all duplicate columns must be matched, because nothing is provided
          ansvals = chmatchdup(ansvars, names_x)
        } else {
          # FR #355 - negative numeric and character indices for SDcols
          colsub = substitute(.SDcols)
          # fix for R-Forge #5190. colsub[[1L]] gave error when it's a symbol.
          if (colsub %iscall% c("!", "-")) {
            negate_sdcols = TRUE
            colsub = colsub[[2L]]
          } else negate_sdcols = FALSE
          # fix for #1216, make sure the parentheses are peeled from expr of the form (((1:4)))
          while(colsub %iscall% "(") colsub = as.list(colsub)[[-1L]]
          if (colsub %iscall% ':' && length(colsub)==3L) {
            # .SDcols is of the format a:b
            .SDcols = eval(colsub, setattr(as.list(seq_along(x)), 'names', names_x), parent.frame())
          } else {
            if (colsub %iscall% 'patterns') {
              # each pattern gives a new filter condition, intersect the end result
              .SDcols = Reduce(intersect, do_patterns(colsub, names_x))
            } else {
              .SDcols = eval(colsub, parent.frame(), parent.frame())
              # allow filtering via function in .SDcols, #3950
              if (is.function(.SDcols)) {
                .SDcols = lapply(x, .SDcols)
                if (any(idx <- vapply_1i(.SDcols, length) > 1L | vapply_1c(.SDcols, typeof) != 'logical' | vapply_1b(.SDcols, anyNA)))
                  stop("When .SDcols is a function, it is applied to each column; the output of this function must be a non-missing boolean scalar signalling inclusion/exclusion of the column. However, these conditions were not met for: ", brackify(names(x)[idx]))
                .SDcols = unlist(.SDcols, use.names = FALSE)
              }
            }
          }
          if (anyNA(.SDcols))
            stop(".SDcols missing at the following indices: ", brackify(which(is.na(.SDcols))))
          if (is.logical(.SDcols)) {
            ansvals = which_(rep(.SDcols, length.out=length(x)), !negate_sdcols)
            ansvars = sdvars = names_x[ansvals]
          } else if (is.numeric(.SDcols)) {
            .SDcols = as.integer(.SDcols)
            # if .SDcols is numeric, use 'dupdiff' instead of 'setdiff'
            if (length(unique(sign(.SDcols))) > 1L) stop(".SDcols is numeric but has both +ve and -ve indices")
            if (any(idx <- abs(.SDcols)>ncol(x) | abs(.SDcols)<1L))
              stop(".SDcols is numeric but out of bounds [1, ", ncol(x), "] at: ", brackify(which(idx)))
            ansvars = sdvars = if (negate_sdcols) dupdiff(names_x[-.SDcols], bynames) else names_x[.SDcols]
            ansvals = if (negate_sdcols) setdiff(seq_along(names(x)), c(.SDcols, which(names(x) %chin% bynames))) else .SDcols
          } else {
            if (!is.character(.SDcols)) stop(".SDcols should be column numbers or names")
            if (!all(idx <- .SDcols %chin% names_x))
              stop("Some items of .SDcols are not column names: ", brackify(.SDcols[!idx]))
            ansvars = sdvars = if (negate_sdcols) setdiff(names_x, c(.SDcols, bynames)) else .SDcols
            # dups = FALSE here. DT[, .SD, .SDcols=c("x", "x")] again doesn't really help with which 'x' to keep (and if '-' which x to remove)
            ansvals = chmatch(ansvars, names_x)
          }
        }
        # fix for long standing FR/bug, #495 and #484
        allcols = c(names_x, xdotprefix, names_i, idotprefix)
        non_sdvars = setdiff(intersect(av, allcols), c(bynames, ansvars))

        # added 'mget' - fix for #994
        if (any(c("get", "mget") %chin% av)){
          if (verbose)
            cat(gettextf("'(m)get' found in j. ansvars being set to all columns. Use .SDcols or a single j=eval(macro) instead. Both will detect the columns used which is important for efficiency.\nOld ansvars: %s \n", brackify(ansvars), domain = "R-data.table"))
            # get('varname') is too difficult to detect which columns are used in general
            # eval(macro) column names are detected via the  if jsub[[1]]==eval switch earlier above.

          # Do not include z in .SD when dt[, z := {.SD; get("x")}, .SDcols = "y"] (#2326, #2338)
          if (jsub %iscall% ":=" && is.symbol(jsub[[2L]])) {
            jsub_lhs_symbol = as.character(jsub[[2L]])
            if (jsub_lhs_symbol %chin% non_sdvars) {
              sdvars = setdiff(sdvars, jsub_lhs_symbol)
            }
          }

          if (missing(.SDcols)) {
            ansvars = setdiff(allcols, bynames) # fix for bug #34
          } else {
            # fixes #4089 - if .SDcols was already evaluated, we do not want the order of the columns to change.
            ansvars = union(ansvars, setdiff(setdiff(allcols, ansvars), bynames))
          }
          non_sdvars = setdiff(ansvars, sdvars)
          ansvals = chmatch(ansvars, names_x)
          if (verbose) cat(gettextf("New ansvars: %s \n", brackify(ansvars), domain = "R-data.table"))
        } else if (length(non_sdvars)) {
          # we've a situation like DT[, c(sum(V1), lapply(.SD, mean)), by=., .SDcols=...] or
          # DT[, lapply(.SD, function(x) x *v1), by=, .SDcols=...] etc.,
          ansvars = union(ansvars, non_sdvars)
          ansvals = chmatch(ansvars, names_x)
        }
        # .SDcols might include grouping columns if users wants that, but normally we expect user not to include them in .SDcols
      } else {
        if (!missing(.SDcols)) warning("This j doesn't use .SD but .SDcols has been supplied. Ignoring .SDcols. See ?data.table.")
        allcols = c(names_x, xdotprefix, names_i, idotprefix)
        ansvars = sdvars = setdiff(intersect(av, allcols), bynames)
        if (verbose) cat("Detected that j uses these columns:",if (!length(ansvars)) "<none>" else paste(ansvars,collapse=","),"\n")
        # using a few named columns will be faster
        # Consider:   DT[,max(diff(date)),by=list(month=month(date))]
        # and:        DT[,lapply(.SD,sum),by=month(date)]
        # We don't want date in .SD in the latter, but we do in the former; hence the union() above.
        ansvals = chmatch(ansvars, names_x)
      }
      # if (!length(ansvars)) Leave ansvars empty. Important for test 607.

      lhs = NULL
      newnames = NULL
      suppPrint = identity
      if (length(av) && av[1L] == ":=") {
        if (.Call(C_islocked, x)) stop(".SD is locked. Using := in .SD's j is reserved for possible future use; a tortuously flexible way to modify by group. Use := in j directly to modify by group by reference.")
        suppPrint = function(x) { .global$print=address(x); x }
        # Suppress print when returns ok not on error, bug #2376. Thanks to: http://stackoverflow.com/a/13606880/403310
        # All appropriate returns following this point are wrapped; i.e. return(suppPrint(x)).

        if (is.null(names(jsub))) {
          # regular LHS:=RHS usage, or `:=`(...) with no named arguments (an error)
          # `:=`(LHS,RHS) is valid though, but more because can't see how to detect that, than desire
          if (length(jsub)!=3L) stop("In `:=`(col1=val1, col2=val2, ...) form, all arguments must be named.")
          lhs = jsub[[2L]]
          jsub = jsub[[3L]]
          if (is.name(lhs)) {
            lhs = as.character(lhs)
          } else {
            # e.g. (MyVar):= or get("MyVar"):=
            lhs = eval(lhs, parent.frame(), parent.frame())
          }
        } else {
          # `:=`(c2=1L,c3=2L,...)
          lhs = names(jsub)[-1L]
          if (any(lhs=="")) stop("In `:=`(col1=val1, col2=val2, ...) form, all arguments must be named.")
          names(jsub)=""
          jsub[[1L]]=as.name("list")
        }
        av = all.vars(jsub,TRUE)
        if (!is.atomic(lhs)) stop("LHS of := must be a symbol, or an atomic vector (column names or positions).")
        if (is.character(lhs)) {
          m = chmatch(lhs, names_x)
        } else if (is.numeric(lhs)) {
          m = as.integer(lhs)
          if (any(m<1L | ncol(x)<m)) stop("LHS of := appears to be column positions but are outside [1,ncol] range. New columns can only be added by name.")
          lhs = names_x[m]
        } else
          stop("LHS of := isn't column names ('character') or positions ('integer' or 'numeric')")
        if (all(!is.na(m))) {
          # updates by reference to existing columns
          cols = as.integer(m)
          newnames=NULL
          if (identical(irows, integer())) {
            # Empty integer() means no rows e.g. logical i with only FALSE and NA
            # got converted to empty integer() by the which() above
            # Short circuit and do-nothing since columns already exist. If some don't
            # exist then for consistency with cases where irows is non-empty, we need to create
            # them of the right type and populate with NA.  Which will happen via the regular
            # alternative branches below, to cover #759.
            # We need this short circuit at all just for convenience. Otherwise users may need to
            # fix errors in their RHS when called on empty edge cases, even when the result won't be
            # used anyway (so it would be annoying to have to fix it.)
            if (verbose) {
              cat("No rows match i. No new columns to add so not evaluating RHS of :=\n")
              cat("Assigning to 0 row subset of",nrow(x),"rows\n")
            }
            .Call(Cassign, x, irows, NULL, NULL, NULL) # only purpose is to write 0 to .Last.updated
            .global$print = address(x)
            return(invisible(x))
          }
        } else {
          # Adding new column(s). TO DO: move after the first eval in case the jsub has an error.
          newnames=setdiff(lhs, names_x)
          m[is.na(m)] = ncol(x)+seq_len(length(newnames))
          cols = as.integer(m)
          # don't pass verbose to selfrefok here -- only activated when
          #   ok=-1 which will trigger setalloccol with verbose in the next
          #   branch, which again calls _selfrefok and returns the message then
          if ((ok<-selfrefok(x, verbose=FALSE))==0L)   # ok==0 so no warning when loaded from disk (-1) [-1 considered TRUE by R]
            warning("Invalid .internal.selfref detected and fixed by taking a (shallow) copy of the data.table so that := can add this new column by reference. At an earlier point, this data.table has been copied by R (or was created manually using structure() or similar). Avoid names<- and attr<- which in R currently (and oddly) may copy the whole data.table. Use set* syntax instead to avoid copying: ?set, ?setnames and ?setattr. If this message doesn't help, please report your use case to the data.table issue tracker so the root cause can be fixed or this message improved.")
          if ((ok<1L) || (truelength(x) < ncol(x)+length(newnames))) {
            DT = x  # in case getOption contains "ncol(DT)" as it used to.  TODO: warn and then remove
            n = length(newnames) + eval(getOption("datatable.alloccol"))  # TODO: warn about expressions and then drop the eval()
            # i.e. reallocate at the size as if the new columns were added followed by setalloccol().
            name = substitute(x)
            if (is.name(name) && ok && verbose) { # && NAMED(x)>0 (TO DO)    # ok here includes -1 (loaded from disk)
              cat("Growing vector of column pointers from truelength ", truelength(x), " to ", n, ". A shallow copy has been taken, see ?setalloccol. Only a potential issue if two variables point to the same data (we can't yet detect that well) and if not you can safely ignore this. To avoid this message you could setalloccol() first, deep copy first using copy(), wrap with suppressWarnings() or increase the 'datatable.alloccol' option.\n")
              # #1729 -- copying to the wrong environment here can cause some confusion
              if (ok == -1L) cat("Note that the shallow copy will assign to the environment from which := was called. That means for example that if := was called within a function, the original table may be unaffected.\n")

              # Verbosity should not issue warnings, so cat rather than warning.
              # TO DO: Add option 'datatable.pedantic' to turn on warnings like this.

              # TO DO ... comments moved up from C ...
              # Note that the NAMED(dt)>1 doesn't work because .Call
              # always sets to 2 (see R-ints), it seems. Work around
              # may be possible but not yet working. When the NAMED test works, we can drop allocwarn argument too
              # because that's just passed in as FALSE from [<- where we know `*tmp*` isn't really NAMED=2.
              # Note also that this growing will happen for missing columns assigned NULL, too. But so rare, we
              # don't mind.
            }
            setalloccol(x, n, verbose=verbose)   # always assigns to calling scope; i.e. this scope
            if (is.name(name)) {
              assign(as.character(name),x,parent.frame(),inherits=TRUE)
            } else if (name %iscall% c('$', '[[') && is.name(name[[2L]])) {
              k = eval(name[[2L]], parent.frame(), parent.frame())
              if (is.list(k)) {
                origj = j = if (name[[1L]] == "$") as.character(name[[3L]]) else eval(name[[3L]], parent.frame(), parent.frame())
                if (is.character(j)) {
                  if (length(j)!=1L) stop("Cannot assign to an under-allocated recursively indexed list -- L[[i]][,:=] syntax is only valid when i is length 1, but it's length ", length(j))
                  j = match(j, names(k))
                  if (is.na(j)) stop("Internal error -- item '", origj, "' not found in names of list") # nocov
                }
                .Call(Csetlistelt,k,as.integer(j), x)
              } else if (is.environment(k) && exists(as.character(name[[3L]]), k)) {
                assign(as.character(name[[3L]]), x, k, inherits=FALSE)
              }
            } # TO DO: else if env$<- or list$<-
          }
        }
      }
    }

    if (length(ansvars)) {
      w = ansvals
      if (length(rightcols) && missingby) {
        w[ w %in% rightcols ] = NA
      }
      # patch for #1615. Allow 'x.' syntax. Only useful during join op when x's join col needs to be used.
      # Note that I specifically have not implemented x[y, aa, on=c(aa="bb")] to refer to x's join column
      # as well because x[i, col] == x[i][, col] will not be TRUE anymore..
      if ( any(xdotprefixvals <- ansvars %chin% xdotprefix)) {
        w[xdotprefixvals] = chmatch(ansvars[xdotprefixvals], xdotprefix)
        xdotcols = TRUE
      }
      if (!any(wna <- is.na(w))) {
        xcols = w
        xcolsAns = seq_along(ansvars)
        icols = icolsAns = integer()
      } else {
        if (!length(leftcols)) stop("Internal error -- column(s) not found: ", paste(ansvars[wna],collapse=", ")) # nocov
        xcols = w[!wna]
        xcolsAns = which(!wna)
        map = c(seq_along(i), leftcols)   # this map is to handle dups in leftcols, #3635
        names(map) = c(names_i, names_x[rightcols])
        w2 = map[ansvars[wna]]
        if (any(w2na <- is.na(w2))) {
          ivars = paste0("i.", names_i)   # ivars is only used in this branch
          ivars[leftcols] = names_i[leftcols]
          w2[w2na] = chmatch(ansvars[wna][w2na], ivars)
          if (any(w2na <- is.na(w2))) {
            ivars[leftcols] = paste0("i.",ivars[leftcols])
            w2[w2na] = chmatch(ansvars[wna][w2na], ivars)
            if (any(w2na <- is.na(w2))) stop("Internal error -- column(s) not found: ", paste(ansvars[wna][w2na],sep=", ")) # nocov
          }
        }
        icols = w2
        icolsAns = which(wna)
      }
    }
  }  # end of  if !missing(j)

  SDenv = new.env(parent=parent.frame())
  # taking care of warnings for posixlt type, #646
  SDenv$strptime = function(x, ...) {
    warning("strptime() usage detected and wrapped with as.POSIXct(). This is to minimize the chance of assigning POSIXlt columns, which use 40+ bytes to store one date (versus 8 for POSIXct). Use as.POSIXct() (which will call strptime() as needed internally) to avoid this warning.")
    as.POSIXct(base::strptime(x, ...))
  }

  syms = all.vars(jsub)
  syms = syms[ substring(syms,1L,2L)==".." ]
  syms = syms[ substring(syms,3L,3L)!="." ]  # exclude ellipsis
  for (sym in syms) {
    if (sym %chin% names_x) {
      # if "..x" exists as column name, use column, for backwards compatibility; e.g. package socialmixr in rev dep checks #2779
      next
      # TODO in future, as warned in NEWS item for v1.11.0 :
      # warning(sym," in j is looking for ",getName," in calling scope, but a column '", sym, "' exists. Column names should not start with ..")
    }
    getName = substring(sym, 3L)
    if (!exists(getName, parent.frame())) {
      if (exists(sym, parent.frame())) next  # user did 'manual' prefix; i.e. variable in calling scope has .. prefix
      stop("Variable '",getName,"' is not found in calling scope. Looking in calling scope because this symbol was prefixed with .. in the j= parameter.")
    }
    assign(sym, get(getName, parent.frame()), SDenv)
  }
  # hash=TRUE (the default) does seem better as expected using e.g. test 645.  TO DO experiment with 'size' argument
  if (missingby || bynull || (!byjoin && !length(byval))) {
    # No grouping: 'by' = missing | NULL | character() | "" | list()
    # Considered passing a one-group to dogroups but it doesn't do the recycling of i within group, that's done here
    if (length(ansvars)) {
      if (!(length(i) && length(icols))) {
        # new in v1.12.0 to redirect to CsubsetDT in this case
        if (!identical(xcolsAns, seq_along(xcolsAns)) || length(xcols)!=length(xcolsAns) || length(ansvars)!=length(xcolsAns)) {
          stop("Internal error: xcolAns does not pass checks: ", length(xcolsAns), length(ansvars), length(xcols), paste(xcolsAns,collapse=","))   # nocov
        }
        # Retained from old R way below (test 1542.01 checks shallow at this point)
        # ' Temp fix for #921 - skip COPY until after evaluating 'jval' (scroll down).
        # ' Unless 'with=FALSE' - can not be expressions but just column names.
        ans = if (with && is.null(irows)) shallow(x, xcols) else .Call(CsubsetDT, x, irows, xcols)
        setattr(ans, "names", ansvars)
      } else {
        # length(i) && length(icols)
        if (is.null(irows)) {
          stop("Internal error: irows is NULL when making join result at R level. Should no longer happen now we use CsubsetDT earlier.")  # nocov
          # TODO: Make subsetDT do a shallow copy when irows is NULL (it currently copies). Then copy only when user uses := or set* on the result
          # by using NAMED/REFCNT on columns, with warning if they copy. Since then, even foo = DT$b would cause the next set or := to copy that
          # column (so the warning is needed). To tackle that, we could have our own DT.NAMED attribute, perhaps.
          # Or keep the rule that [.data.table always returns new memory, and create view() or view= as well, maybe cleaner.
        }
        ans = vector("list", length(ansvars))
        ii = rep.int(indices__, len__) # following #1991 fix
        # TODO: if (allLen1 && allGrp1 && (is.na(nomatch) || !any(f__==0L))) then ii will be 1:nrow(i)  [nomatch=0 should drop rows in i that have no match]
        #       But rather than that complex logic here at R level to catch that and do a shallow copy for efficiency, just do the check inside CsubsetDT
        #       to see if it passed 1:nrow(x) and then CsubsetDT should do the shallow copy safely and centrally.
        #       That R level branch was taken out in PR #3213

        # TO DO: use CsubsetDT twice here and then remove this entire R level branch
        for (s in seq_along(icols)) {
          target = icolsAns[s]
          source = icols[s]
          ans[[target]] = .Call(CsubsetVector,i[[source]],ii)  # i.e. i[[source]][ii]
        }
        for (s in seq_along(xcols)) {
          target = xcolsAns[s]
          source = xcols[s]
          ans[[target]] = .Call(CsubsetVector,x[[source]],irows)   # i.e. x[[source]][irows], but guaranteed new memory even for singleton logicals from R 3.1.0
        }
        setattr(ans, "names", ansvars)
        if (haskey(x)) {
          keylen = which.first(!key(x) %chin% ansvars)-1L
          if (is.na(keylen)) keylen = length(key(x))
          len = length(rightcols)
          # fix for #1268, #1704, #1766 and #1823
          chk = if (len && !missing(on)) !identical(head(key(x), len), names(on)) else FALSE
          if ( (keylen>len || chk) && !.Call(CisOrderedSubset, irows, nrow(x))) {
            keylen = if (!chk) len else 0L # fix for #1268
          }
          ## check key on i as well!
          ichk = is.data.table(i) && haskey(i) &&
                 identical(head(key(i), length(leftcols)), names_i[leftcols]) # i has the correct key, #3061
          if (keylen && (ichk || is.logical(i) || (.Call(CisOrderedSubset, irows, nrow(x)) && ((roll == FALSE) || length(irows) == 1L)))) # see #1010. don't set key when i has no key, but irows is ordered and roll != FALSE
            setattr(ans,"sorted",head(key(x),keylen))
        }
        setattr(ans, "class", class(x)) # fix for #64
        setattr(ans, "row.names", .set_row_names(nrow(ans)))
        setalloccol(ans)
      }

      if (!with || missing(j)) return(ans)

      SDenv$.SDall = ans
      SDenv$.SD = if (length(non_sdvars)) shallow(SDenv$.SDall, sdvars) else SDenv$.SDall
      SDenv$.N = nrow(ans)

    } else {
      SDenv$.SDall = SDenv$.SD = null.data.table()   # no columns used by j so .SD can be empty. Only needs to exist so that we can rely on it being there when locking it below for example. If .SD were used by j, of course then xvars would be the columns and we wouldn't be in this leaf.
      SDenv$.N = if (is.null(irows)) nrow(x) else if(!length(irows) || identical(max(irows), 0L)) 0L else length(irows)
      # Fix for #963.
      # When irows is integer(0L), length(irows) = 0 will result in 0 (as expected).
      # Binary search can return all 0 irows when none of the input matches. Instead of doing all(irows==0L) (previous method), which has to allocate a logical vector the size of irows, we can make use of 'max'. If max is 0, we return 0. The condition where only some irows > 0 won't occur.
    }
    # Temp fix for #921. Allocate `.I` only if j-expression uses it.
    SDenv$.I = if (!missing(j) && use.I) seq_len(SDenv$.N) else 0L
    SDenv$.GRP = 1L
    SDenv$.NGRP = 1L
    .Call(C_lock, SDenv$.SD) # used to stop := modifying .SD via j=f(.SD), bug#1727. The more common case of j=.SD[,subcol:=1] was already caught when jsub is inspected for :=.
    .Call(C_lock, SDenv$.SDall)
    lockBinding(".SD",SDenv)
    lockBinding(".SDall",SDenv)
    lockBinding(".N",SDenv)
    lockBinding(".I",SDenv)
    lockBinding(".GRP",SDenv)
    lockBinding(".NGRP", SDenv)
    for (ii in ansvars) assign(ii, SDenv$.SDall[[ii]], SDenv)
    # Since .SD is inside SDenv, alongside its columns as variables, R finds .SD symbol more quickly, if used.
    # There isn't a copy of the columns here, the xvar symbols point to the SD columns (copy-on-write).

    if (is.name(jsub) && is.null(lhs) && !exists(jsubChar<-as.character(jsub), SDenv, inherits=FALSE)) {
      stop("j (the 2nd argument inside [...]) is a single symbol but column name '",jsubChar,"' is not found. Perhaps you intended DT[, ..",jsubChar,"]. This difference to data.frame is deliberate and explained in FAQ 1.1.")
    }

    jval = eval(jsub, SDenv, parent.frame())
    .Call(C_unlock, jval) # in case jval inherits .SD's lock, #1341 #2245. .Call directly (not via an R function like setattr or unlock) to avoid bumping jval's MAYBE_SHARED.

    # copy 'jval' when required
    # More speedup - only check + copy if irows is NULL
    # Temp fix for #921 - check address and copy *after* evaluating 'jval'.  #75 also related.
    if (is.null(irows)) {
      if (!is.list(jval)) { # performance improvement when i-arg is S4, but not list, #1438, Thanks @DCEmilberg.
        jcpy = address(jval) %in% vapply_1c(SDenv$.SD, address) # %chin% errors when RHS is list()
        if (jcpy) jval = copy(jval)
      } else if (address(jval) == address(SDenv$.SD)) {
        jval = copy(jval)
      } else if ( length(jcpy <- which(vapply_1c(jval, address) %chin% vapply_1c(SDenv, address))) ) {
        for (jidx in jcpy) jval[[jidx]] = copy(jval[[jidx]])
      } else if (jsub %iscall% 'get') {
        jval = copy(jval) # fix for #1212
      }
    }

    if (!is.null(lhs)) {
      # TODO?: use set() here now that it can add new columns. Then remove newnames and alloc logic above.
      .Call(Cassign,x,irows,cols,newnames,jval)
      return(suppPrint(x))
    }
    if ((is.call(jsub) && jsub[[1L]] != "get" && is.list(jval) && !is.object(jval)) || !missingby) {
      # is.call: selecting from a list column should return list
      # is.object: for test 168 and 168.1 (S4 object result from ggplot2::qplot). Just plain list results should result in data.table

      # Fix for #813 and #758. Ex: DT[c(FALSE, FALSE), list(integer(0L), y)]
      # where DT = data.table(x=1:2, y=3:4) should return an empty data.table!!
      if (!is.null(irows) && `||`(
          identical(irows, integer(0L)) && !bynull,
          length(irows) && !anyNA(irows) && all(irows==0L) ## anyNA() because all() returns NA (not FALSE) when irows is all-NA. TODO: any way to not check all 'irows' values?
          ))
        jval = lapply(jval, `[`, 0L)
      if (is.atomic(jval)) {
        setattr(jval,"names",NULL)  # discard names of named vectors otherwise each cell in the column would have a name
        jval = list(jval)
      }
      if (!is.null(jvnames) && !all(jvnames=="")) setattr(jval, 'names', jvnames)  # e.g. jvnames=="N" for DT[,.N,]
      jval = as.data.table.list(jval, .named=NULL)
    }

    if (is.data.table(jval)) {
      setattr(jval, 'class', class(x)) # fix for #64
      if (haskey(x) && all(key(x) %chin% names(jval)) && is.sorted(jval, by=key(x)))
        setattr(jval, 'sorted', key(x))
      if (any(sapply(jval, is.null))) stop("Internal error: j has created a data.table result containing a NULL column") # nocov
    }
    return(jval)
  }

  ###########################################################################
  # Grouping ...
  ###########################################################################

  o__ = integer()
  if (".N" %chin% ansvars) stop("The column '.N' can't be grouped because it conflicts with the special .N variable. Try setnames(DT,'.N','N') first.")
  if (".I" %chin% ansvars) stop("The column '.I' can't be grouped because it conflicts with the special .I variable. Try setnames(DT,'.I','I') first.")
  SDenv$.iSD = NULL  # null.data.table()
  SDenv$.xSD = NULL  # null.data.table() - introducing for FR #2693 and Gabor's post on fixing for FAQ 2.8

  SDenv$print = function(x, ...){ base::print(x,...); NULL}
  # Now ggplot2 returns data from print, we need a way to throw it away otherwise j accumulates the result

  SDenv$.SDall = SDenv$.SD = null.data.table()  # e.g. test 607. Grouping still proceeds even though no .SD e.g. grouping key only tables, or where j consists of .N only
  SDenv$.N = vector("integer", 1L)    # explicit new vector (not 0L or as.integer() which might return R's internal small-integer global)
  SDenv$.GRP = vector("integer", 1L)  #   because written to by reference at C level (one write per group). TODO: move this alloc to C level

  # #3694/#761 common gotcha -- doing t1-t0 by group, but -.POSIXt uses units='auto'
  #   independently by group & attr mismatch among groups is ignored. The latter
  #   is a more general issue but the former can be fixed by forcing units='secs'
  SDenv$`-.POSIXt` = function(e1, e2) {
    if (inherits(e2, 'POSIXt')) {
      if (verbose && !exists('done_units_report', parent.frame())) {
        cat('\nNote: forcing units="secs" on implicit difftime by group; call difftime explicitly to choose custom units')
        assign('done_units_report', TRUE, parent.frame())
      }
      return(difftime(e1, e2, units='secs'))
    } else return(base::`-.POSIXt`(e1, e2))
  }

  if (byjoin) {
    # The groupings come instead from each row of the i data.table.
    # Much faster for a few known groups vs a 'by' for all followed by a subset
    if (!is.data.table(i)) stop("logical error. i is not data.table, but mult='all' and 'by'=.EACHI")
    byval = i
    bynames = if (missing(on)) head(key(x),length(leftcols)) else names(on)
    allbyvars = NULL
    bysameorder = haskey(i) || (is.sorted(f__) && ((roll == FALSE) || length(f__) == 1L)) # Fix for #1010
    ##  'av' correct here ??  *** TO DO ***
    xjisvars = intersect(av, names_x[rightcols])  # no "x." for xvars.
    # if 'get' is in 'av' use all cols in 'i', fix for bug #34
    # added 'mget' - fix for #994
    jisvars = if (any(c("get", "mget") %chin% av)) names_i else intersect(gsub("^i[.]","", setdiff(av, xjisvars)), names_i)
    # JIS (non join cols) but includes join columns too (as there are named in i)
    if (length(jisvars)) {
      tt = min(nrow(i),1L)  # min() is here for when nrow(i)==0
      SDenv$.iSD = i[tt,jisvars,with=FALSE]
      for (ii in jisvars) {
        assign(ii, SDenv$.iSD[[ii]], SDenv)
        assign(paste0("i.",ii), SDenv$.iSD[[ii]], SDenv)
      }
    }

  } else {
    # Find the groups, using 'byval' ...
    if (missingby) stop("Internal error: by= is missing")   # nocov

    if (length(byval) && length(byval[[1L]])) {
      if (!bysameorder && isFALSE(byindex)) {
        if (verbose) {last.started.at=proc.time();cat("Finding groups using forderv ... ");flush.console()}
        o__ = forderv(byval, sort=keyby, retGrp=TRUE)
        # The sort= argument is called sortGroups at C level. It's primarily for saving the sort of unique strings at
        # C level for efficiency when by= not keyby=. Other types also retain appearance order, but at byte level to
        # minimize data movement and benefit from skipping subgroups which happen to be grouped but not sorted. This byte
        # appearance order is not the same as the order of group values within by= columns, so the 2nd forder below is
        # still needed to get the group appearance order. Always passing sort=TRUE above won't change any result at all
        # (tested and confirmed), it'll just make by= slower. It must be TRUE when keyby= though since the key is just
        # marked afterwards.
        # forderv() returns empty integer() if already ordered to save allocating 1:xnrow
        bysameorder = orderedirows && !length(o__)
        if (verbose) {
          cat(timetaken(last.started.at),"\n")
          last.started.at=proc.time()
          cat("Finding group sizes from the positions (can be avoided to save RAM) ... ")
          flush.console()  # for windows
        }
        f__ = attr(o__, "starts", exact=TRUE)
        len__ = uniqlengths(f__, xnrow)
        if (verbose) {cat(timetaken(last.started.at),"\n"); flush.console()}
        if (!bysameorder && !keyby) {
          # TO DO: lower this into forder.c
          if (verbose) {last.started.at=proc.time();cat("Getting back original order ... ");flush.console()}
          firstofeachgroup = o__[f__]
          if (length(origorder <- forderv(firstofeachgroup))) {
            f__ = f__[origorder]
            len__ = len__[origorder]
          }
          if (verbose) {cat(timetaken(last.started.at),"\n"); flush.console()}
        }
        if (!orderedirows && !length(o__)) o__ = seq_len(xnrow)  # temp fix.  TODO: revist orderedirows
      } else {
        if (verbose) last.started.at=proc.time();
        if (bysameorder) {
          if (verbose) {cat("Finding groups using uniqlist on key ... ");flush.console()}
          f__ = uniqlist(byval)
        } else {
          if (!is.character(byindex) || length(byindex)!=1L) stop("Internal error: byindex not the index name")  # nocov
          if (verbose) {cat("Finding groups using uniqlist on index '", byindex, "' ... ", sep="");flush.console()}
          o__ = getindex(x, byindex)
          if (is.null(o__)) stop("Internal error: byindex not found")  # nocov
          f__ = uniqlist(byval, order=o__)
        }
        if (verbose) {
          cat(timetaken(last.started.at),"\n")
          last.started.at=proc.time()
          cat("Finding group sizes from the positions (can be avoided to save RAM) ... ")
          flush.console()  # for windows
        }
        len__ = uniqlengths(f__, xnrow)
        # TO DO: combine uniqlist and uniquelengths into one call.  Or, just set len__ to NULL when dogroups infers that.
        if (verbose) { cat(timetaken(last.started.at),"\n"); flush.console() }
      }
    } else {
      f__=NULL
      len__=0L
      bysameorder=TRUE   # for test 724
    }
    # TO DO: allow secondary keys to be stored, then we see if our by matches one, if so use it, and no need to sort again. TO DO: document multiple keys.
  }
  if (length(xcols)) {
    #  TODO add: if (max(len__)==nrow) stop("There is no need to deep copy x in this case")
    #  TODO move down to dogroup.c, too.
    SDenv$.SDall = .Call(CsubsetDT, x, if (length(len__)) seq_len(max(len__)) else 0L, xcols)  # must be deep copy when largest group is a subset
    if (xdotcols) setattr(SDenv$.SDall, 'names', ansvars[xcolsAns]) # now that we allow 'x.' prefix in 'j', #2313 bug fix - [xcolsAns]
    SDenv$.SD = if (length(non_sdvars)) shallow(SDenv$.SDall, sdvars) else SDenv$.SDall
  }
  if (nrow(SDenv$.SDall)==0L) {
    setattr(SDenv$.SDall,"row.names",c(NA_integer_,0L))
    setattr(SDenv$.SD,"row.names",c(NA_integer_,0L))
  }
  # .set_row_names() basically other than not integer() for 0 length, otherwise dogroups has no [1] to modify to -.N
  .Call(C_lock, SDenv$.SD)  # stops := modifying .SD via j=f(.SD), bug#1727. The more common case of j=.SD[,subcol:=1] was already caught when jsub is inspected for :=.
  .Call(C_lock, SDenv$.SDall)
  lockBinding(".SD",SDenv)
  lockBinding(".SDall",SDenv)
  lockBinding(".N",SDenv)
  lockBinding(".GRP",SDenv)
  lockBinding(".iSD",SDenv)

  SDenv$.NGRP = length(f__)
  lockBinding(".NGRP", SDenv)

  GForce = FALSE
  if ( getOption("datatable.optimize")>=1L && (is.call(jsub) || (is.name(jsub) && jsub %chin% c(".SD", ".N"))) ) {  # Ability to turn off if problems or to benchmark the benefit
    # Optimization to reduce overhead of calling lapply over and over for each group
    oldjsub = jsub
    funi = 1L # Fix for #985
    # converted the lapply(.SD, ...) to a function and used below, easier to implement FR #2722 then.
    .massageSD = function(jsub) {
      txt = as.list(jsub)[-1L]
      if (length(names(txt))>1L) .Call(Csetcharvec, names(txt), 2L, "")  # fixes bug #110
      fun = txt[[2L]]
      if (fun %iscall% "function") {
        # Fix for #2381: added SDenv$.SD to 'eval' to take care of cases like: lapply(.SD, function(x) weighted.mean(x, bla)) where "bla" is a column in DT
        # http://stackoverflow.com/questions/13441868/data-table-and-stratified-means
        # adding this does not compromise in speed (that is, not any lesser than without SDenv$.SD)
        # replaced SDenv$.SD to SDenv to deal with Bug #87 reported by Ricardo (Nice catch!)
        thisfun = paste0("..FUN", funi) # Fix for #985
        assign(thisfun,eval(fun, SDenv, SDenv), SDenv)  # to avoid creating function() for each column of .SD
        lockBinding(thisfun,SDenv)
        txt[[1L]] = as.name(thisfun)
      } else {
        if (is.character(fun)) fun = as.name(fun)
        txt[[1L]] = fun
      }
      ans = vector("list", length(sdvars)+1L)
      ans[[1L]] = as.name("list")
      for (ii in seq_along(sdvars)) {
        txt[[2L]] = as.name(sdvars[ii])
        ans[[ii+1L]] = as.call(txt)
      }
      jsub = as.call(ans)  # important no names here
      jvnames = sdvars      # but here instead
      list(jsub, jvnames)
      # It may seem inefficient to construct a potentially long expression. But, consider calling
      # lapply 100000 times. The C code inside lapply does the LCONS stuff anyway, every time it
      # is called, involving small memory allocations.
      # The R level lapply calls as.list which needs a shallow copy.
      # lapply also does a setAttib of names (duplicating the same names over and over again
      # for each group) which is terrible for our needs. We replace all that with a
      # (ok, long, but not huge in memory terms) list() which is primitive (so avoids symbol
      # lookup), and the eval() inside dogroups hardly has to do anything. All this results in
      # overhead minimised. We don't need to worry about the env passed to the eval in a possible
      # lapply replacement, or how to pass ... efficiently to it.
      # Plus we optimize lapply first, so that mean() can be optimized too as well, next.
    }
    if (is.name(jsub)) {
      if (jsub == ".SD") {
        jsub = as.call(c(quote(list), lapply(sdvars, as.name)))
        jvnames = sdvars
      }
    } else if (length(as.character(jsub[[1L]])) == 1L) {  # Else expect problems with <jsub[[1L]] == >
      # g[[ only applies to atomic input, for now, was causing #4159
      subopt = length(jsub) == 3L &&
        (jsub[[1L]] == "[" ||
           (jsub[[1L]] == "[[" && is.name(jsub[[2L]]) && eval(call('is.atomic', jsub[[2L]]), envir = x))) &&
        (is.numeric(jsub[[3L]]) || jsub[[3L]] == ".N")
      headopt = jsub[[1L]] == "head" || jsub[[1L]] == "tail"
      firstopt = jsub[[1L]] == "first" || jsub[[1L]] == "last" # fix for #2030
      if ((length(jsub) >= 2L && jsub[[2L]] == ".SD") &&
          (subopt || headopt || firstopt)) {
        if (headopt && length(jsub)==2L) jsub[["n"]] = 6L # head-tail n=6 when missing #3462
        # optimise .SD[1] or .SD[2L]. Not sure how to test .SD[a] as to whether a is numeric/integer or a data.table, yet.
        jsub = as.call(c(quote(list), lapply(sdvars, function(x) { jsub[[2L]] = as.name(x); jsub })))
        jvnames = sdvars
      } else if (jsub[[1L]]=="lapply" && jsub[[2L]]==".SD" && length(xcols)) {
        deparse_ans = .massageSD(jsub)
        jsub = deparse_ans[[1L]]
        jvnames = deparse_ans[[2L]]
      } else if (jsub[[1L]] == "c" && length(jsub) > 1L) {
        # TODO, TO DO: raise the checks for 'jvnames' earlier (where jvnames is set by checking 'jsub') and set 'jvnames' already.
        # FR #2722 is just about optimisation of j=c(.N, lapply(.SD, .)) that is taken care of here.
        # FR #735 tries to optimise j-expressions of the form c(...) as long as ... contains
        # 1) lapply(.SD, ...), 2) simply .SD or .SD[..], 3) .N, 4) list(...) and 5) functions that normally return a single value*
        # On 5)* the IMPORTANT point to note is that things that are not wrapped within "list(...)" should *always*
        # return length 1 output for us to optimise. Else, there's no equivalent to optimising c(...) to list(...) AFAICT.
        # One issue could be that these functions (e.g., mean) can be "re-defined" by the OP to produce a length > 1 output
        # Of course this is worrying too much though. If the issue comes up, we'll just remove the relevant optimisations.
        # For now, we optimise all functions mentioned in 'optfuns' below.
        optfuns = c("max", "min", "mean", "length", "sum", "median", "sd", "var")
        is_valid = TRUE
        any_SD = FALSE
        jsubl = as.list.default(jsub)
        oldjvnames = jvnames
        jvnames = NULL           # TODO: not let jvnames grow, maybe use (number of lapply(.SD, .))*length(sdvars) + other jvars ?? not straightforward.
        # Fix for #744. Don't use 'i' in for-loops. It masks the 'i' from the input!!
        for (i_ in 2L:length(jsubl)) {
          this = jsub[[i_]]
          if (is.name(this)) {  # no need to check length(this)==1L; is.name() returns single TRUE or FALSE (documented); can't have a vector of names
            if (this == ".SD") { # optimise '.SD' alone
              any_SD = TRUE
              jsubl[[i_]] = lapply(sdvars, as.name)
              jvnames = c(jvnames, sdvars)
            } else if (this == ".N") {
              # don't optimise .I in c(.SD, .I), it's length can be > 1
              # only c(.SD, list(.I)) should be optimised!! .N is always length 1.
              jvnames = c(jvnames, gsub("^[.]([N])$", "\\1", this))
            } else {
              # jvnames = c(jvnames, if (is.null(names(jsubl))) "" else names(jsubl)[i_])
              is_valid=FALSE
              break
            }
          } else if (is.call(this)) {
            if (this[[1L]] == "lapply" && this[[2L]] == ".SD" && length(xcols)) {
              any_SD = TRUE
              deparse_ans = .massageSD(this)
              funi = funi + 1L # Fix for #985
              jsubl[[i_]] = as.list(deparse_ans[[1L]][-1L]) # just keep the '.' from list(.)
              jvnames = c(jvnames, deparse_ans[[2L]])
            } else if (this[[1L]] == "list") {
              # also handle c(lapply(.SD, sum), list()) - silly, yes, but can happen
              if (length(this) > 1L) {
                jl__ = as.list(jsubl[[i_]])[-1L] # just keep the '.' from list(.)
                jn__ = if (is.null(names(jl__))) rep("", length(jl__)) else names(jl__)
                idx  = unlist(lapply(jl__, function(x) is.name(x) && x == ".I"))
                if (any(idx)) jn__[idx & (jn__ == "")] = "I"
                jvnames = c(jvnames, jn__)
                jsubl[[i_]] = jl__
              }
            } else if (this %iscall% optfuns && length(this)>1L) {
              jvnames = c(jvnames, if (is.null(names(jsubl))) "" else names(jsubl)[i_])
            } else if ( length(this) == 3L && (this[[1L]] == "[" || this[[1L]] == "head") &&
                    this[[2L]] == ".SD" && (is.numeric(this[[3L]]) || this[[3L]] == ".N") ) {
              # optimise .SD[1] or .SD[2L]. Not sure how to test .SD[a] as to whether a is numeric/integer or a data.table, yet.
              any_SD = TRUE
              jsubl[[i_]] = lapply(sdvars, function(x) { this[[2L]] = as.name(x); this })
              jvnames = c(jvnames, sdvars)
            } else if (any(all.vars(this) == ".SD")) {
              # TODO, TO DO: revisit complex cases (as illustrated below)
              # complex cases like DT[, c(.SD[x>1], .SD[J(.)], c(.SD), a + .SD, lapply(.SD, sum)), by=grp]
              # hard to optimise such cases (+ difficulty in counting exact columns and therefore names). revert back to no optimisation.
              is_valid=FALSE
              break
            } else { # just to be sure that any other case (I've overlooked) runs smoothly, without optimisation
              # TO DO, TODO: maybe a message/warning here so that we can catch the overlooked cases, if any?
              is_valid=FALSE
              break
            }
          } else {
            is_valid = FALSE
            break
          }
        }
        if (!is_valid || !any_SD) { # restore if c(...) doesn't contain lapply(.SD, ..) or if it's just invalid
          jvnames = oldjvnames           # reset jvnames
          jsub = oldjsub                 # reset jsub
          jsubl = as.list.default(jsubl) # reset jsubl
        } else {
          setattr(jsubl, 'names', NULL)
          jsub = as.call(unlist(jsubl, use.names=FALSE))
          jsub[[1L]] = quote(list)
        }
      }
    }
    if (verbose) {
      if (!identical(oldjsub, jsub))
        cat("lapply optimization changed j from '",deparse(oldjsub),"' to '",deparse(jsub,width.cutoff=200L, nlines=1L),"'\n",sep="")
      else
        cat("lapply optimization is on, j unchanged as '",deparse(jsub,width.cutoff=200L, nlines=1L),"'\n",sep="")
    }
    dotN = function(x) is.name(x) && x==".N" # For #334. TODO: Rprof() showed dotN() may be the culprit if iterated (#1470)?; avoid the == which converts each x to character?
    # FR #971, GForce kicks in on all subsets, no joins yet. Although joins could work with
    # nomatch=0L even now.. but not switching it on yet, will deal it separately.
    if (getOption("datatable.optimize")>=2L && !is.data.table(i) && !byjoin && length(f__) && !length(lhs)) {
      if (!length(ansvars) && !use.I) {
        GForce = FALSE
        if ( (is.name(jsub) && jsub==".N") || (jsub %iscall% 'list' && length(jsub)==2L && jsub[[2L]]==".N") ) {
          GForce = TRUE
          if (verbose) cat("GForce optimized j to '",deparse(jsub, width.cutoff=200L, nlines=1L),"'\n",sep="")
        }
      } else {
        # Apply GForce
        .gforce_ok = function(q) {
          if (dotN(q)) return(TRUE) # For #334
          # run GForce for simple f(x) calls and f(x, na.rm = TRUE)-like calls where x is a column of .SD
          # is.symbol() is for #1369, #1974 and #2949
          if (!(is.call(q) && is.symbol(q[[1L]]) && is.symbol(q[[2L]]) && (q1 <- q[[1L]]) %chin% gfuns)) return(FALSE)
          if (!(q2 <- q[[2L]]) %chin% names(SDenv$.SDall) && q2 != ".I") return(FALSE)  # 875
          if ((length(q)==2L || identical("na",substring(names(q)[3L], 1L, 2L))) && (!q1 %chin% c("head","tail"))) return(TRUE)
          # ... head-tail uses default value n=6 which as of now should not go gforce ^^
          # otherwise there must be three arguments, and only in two cases:
          #   1) head/tail(x, 1) or 2) x[n], n>0
          length(q)==3L && length(q3 <- q[[3L]])==1L && is.numeric(q3) &&
            ( (q1 %chin% c("head", "tail") && q3==1L) || ((q1 == "[" || (q1 == "[[" && eval(call('is.atomic', q[[2L]]), envir=x))) && q3>0L) )
        }
        if (jsub[[1L]]=="list") {
          GForce = TRUE
          for (ii in seq.int(from=2L, length.out=length(jsub)-1L)) {
            if (!.gforce_ok(jsub[[ii]])) {GForce = FALSE; break}
          }
        } else GForce = .gforce_ok(jsub)
        if (GForce) {
          if (jsub[[1L]]=="list")
            for (ii in seq_along(jsub)[-1L]) {
              if (dotN(jsub[[ii]])) next; # For #334
              jsub[[ii]][[1L]] = as.name(paste0("g", jsub[[ii]][[1L]]))
              if (length(jsub[[ii]])==3L) jsub[[ii]][[3L]] = eval(jsub[[ii]][[3L]], parent.frame())  # tests 1187.2 & 1187.4
            }
          else {
            jsub[[1L]] = as.name(paste0("g", jsub[[1L]]))
            if (length(jsub)==3L) jsub[[3L]] = eval(jsub[[3L]], parent.frame())   # tests 1187.3 & 1187.5
          }
          if (verbose) cat("GForce optimized j to '",deparse(jsub, width.cutoff=200L, nlines=1L),"'\n",sep="")
        } else if (verbose) cat("GForce is on, left j unchanged\n");
      }
    }
    if (!GForce && !is.name(jsub)) {
      # Still do the old speedup for mean, for now
      nomeanopt=FALSE  # to be set by .optmean() using <<- inside it
      oldjsub = jsub
      if (jsub[[1L]]=="list") {
        # Addressing #1369, #2949 and #1974. This used to be 30s (vs 0.5s) with 30K elements items in j, #1470. Could have been dotN() and/or the for-looped if()
        # jsub[[1]]=="list" so the first item of todo will always be FALSE
        todo = sapply(jsub, `%iscall%`, 'mean')
        if (any(todo)) {
          w = which(todo)
          jsub[w] = lapply(jsub[w], .optmean)
        }
      } else if (jsub[[1L]]=="mean") {
        jsub = .optmean(jsub)
      }
      if (nomeanopt) {
        warning("Unable to optimize call to mean() and could be very slow. You must name 'na.rm' like that otherwise if you do mean(x,TRUE) the TRUE is taken to mean 'trim' which is the 2nd argument of mean. 'trim' is not yet optimized.",immediate.=TRUE)
      }
      if (verbose) {
        if (!identical(oldjsub, jsub))
          cat("Old mean optimization changed j from '",deparse(oldjsub),"' to '",deparse(jsub, width.cutoff=200L, nlines=1L),"'\n",sep="")
        else
          cat("Old mean optimization is on, left j unchanged.\n")
      }
      assign("Cfastmean", Cfastmean, SDenv)
      # Old comments still here for now ...
      # Here in case nomeanopt=TRUE or some calls to mean weren't detected somehow. Better but still slow.
      # Maybe change to :
      #     assign("mean", fastmean, SDenv)  # neater than the hard work above, but slower
      # when fastmean can do trim.
    }
  } else if (verbose) {
    if (getOption("datatable.optimize")<1L) cat("All optimizations are turned off\n")
    else cat("Optimization is on but left j unchanged (single plain symbol): '",deparse(jsub, width.cutoff=200L, nlines=1L),"'\n",sep="")
  }
  if (byjoin) {
    groups = i
    grpcols = leftcols # 'leftcols' are the columns in i involved in the join (either head of key(i) or head along i)
    jiscols = chmatch(jisvars, names_i)  # integer() if there are no jisvars (usually there aren't, advanced feature)
    xjiscols = chmatch(xjisvars, names_x)
    SDenv$.xSD = x[min(nrow(i), 1L), xjisvars, with=FALSE]
    if (!missing(on)) o__ = xo else o__ = integer(0L)
  } else {
    groups = byval
    grpcols = seq_along(byval)
    jiscols = NULL   # NULL rather than integer() is used in C to know when using by
    xjiscols = NULL
  }
  lockBinding(".xSD", SDenv)
  grporder = o__
  # for #971, added !GForce. if (GForce) we do it much more (memory) efficiently than subset of order vector below.
  if (length(irows) && !isTRUE(irows) && !GForce) {
    # any zeros in irows were removed by convertNegAndZeroIdx earlier above; no need to check for zeros again. Test 1058-1061 check case #2758.
    if (length(o__) && length(irows)!=length(o__)) stop("Internal error: length(irows)!=length(o__)") # nocov
    o__ = if (length(o__)) irows[o__]  # better do this once up front (even though another alloc) than deep repeated branch in dogroups.c
          else irows
  } # else grporder is left bound to same o__ memory (no cost of copy)
  if (is.null(lhs)) cols=NULL
  if (!length(f__)) {
    # for consistency of empty case in test 184
    f__=len__=0L
  }
  if (verbose) {last.started.at=proc.time();cat("Making each group and running j (GForce ",GForce,") ... ",sep="");flush.console()}
  if (GForce) {
    thisEnv = new.env()  # not parent=parent.frame() so that gsum is found
    for (ii in ansvars) assign(ii, x[[ii]], thisEnv)
    assign(".N", len__, thisEnv) # For #334
    #fix for #1683
    if (use.I) assign(".I", seq_len(nrow(x)), thisEnv)
    ans = gforce(thisEnv, jsub, o__, f__, len__, irows) # irows needed for #971.
    gi = if (length(o__)) o__[f__] else f__
    g = lapply(grpcols, function(i) groups[[i]][gi])
    ans = c(g, ans)
  } else {
    ans = .Call(Cdogroups, x, xcols, groups, grpcols, jiscols, xjiscols, grporder, o__, f__, len__, jsub, SDenv, cols, newnames, !missing(on), verbose)
  }
  # unlock any locked data.table components of the answer, #4159
  # MAX_DEPTH prevents possible infinite recursion from truly recursive object, #4173
  #   TODO: is there an efficient way to get around this MAX_DEPTH limit?
  MAX_DEPTH = 5L
  runlock = function(x, current_depth = 1L) {
    if (is.recursive(x) && current_depth <= MAX_DEPTH) {
      if (inherits(x, 'data.table')) .Call(C_unlock, x)
      else return(lapply(x, runlock, current_depth = current_depth + 1L))
    }
    return(invisible())
  }
  runlock(ans)
  if (verbose) {cat(timetaken(last.started.at),"\n"); flush.console()}
  # TO DO: xrows would be a better name for irows: irows means the rows of x that i joins to
  # Grouping by i: icols the joins columns (might not need), isdcols (the non join i and used by j), all __ are length x
  # Grouping by by: i is by val, icols NULL, o__ may be subset of x, f__ points to o__ (or x if !length o__)
  # TO DO: setkey could mark the key whether it is unique or not.
  if (!is.null(lhs)) {
    if (any(names_x[cols] %chin% key(x)))
      setkey(x,NULL)
    # fixes #1479. Take care of secondary indices, TODO: cleaner way of doing this
    attrs = attr(x, 'index', exact=TRUE)
    skeys = names(attributes(attrs))
    if (!is.null(skeys)) {
      hits  = unlist(lapply(paste0("__", names_x[cols]), function(x) grep(x, skeys, fixed = TRUE)))
      hits  = skeys[unique(hits)]
      for (i in seq_along(hits)) setattr(attrs, hits[i], NULL) # does by reference
    }
    if (keyby) {
      cnames = as.character(bysubl)[-1L]
      cnames = gsub('^`|`$', '', cnames)  # the wrapping backticks that were added above can be removed now, #3378
      if (all(cnames %chin% names_x)) {
        if (verbose) {last.started.at=proc.time();cat("setkey() after the := with keyby= ... ");flush.console()}
        setkeyv(x,cnames)  # TO DO: setkey before grouping to get memcpy benefit.
        if (verbose) {cat(timetaken(last.started.at),"\n"); flush.console()}
      }
      else warning("The setkey() normally performed by keyby= has been skipped (as if by= was used) because := is being used together with keyby= but the keyby= contains some expressions. To avoid this warning, use by= instead, or provide existing column names to keyby=.\n")
    }
    return(suppPrint(x))
  }
  if (is.null(ans)) {
    ans = as.data.table.list(lapply(groups,"[",0L))  # side-effects only such as test 168
    setnames(ans,seq_along(bynames),bynames)   # TO DO: why doesn't groups have bynames in the first place?
    return(ans)
  }
  setattr(ans,"row.names",.set_row_names(length(ans[[1L]])))
  setattr(ans,"class",class(x)) # fix for #64
  if (is.null(names(ans))) {
    # Efficiency gain of dropping names has been successful. Ordinarily this will run.
    if (is.null(jvnames)) jvnames = character(length(ans)-length(bynames))
    if (length(bynames)+length(jvnames)!=length(ans))
      stop("Internal error: jvnames is length ",length(jvnames), " but ans is ",length(ans)," and bynames is ", length(bynames)) # nocov
    ww = which(jvnames=="")
    if (any(ww)) jvnames[ww] = paste0("V",ww)
    setattr(ans, "names", c(bynames, jvnames))
  } else {
    setnames(ans,seq_along(bynames),bynames)   # TO DO: reinvestigate bynames flowing from dogroups here and simplify
  }
  if (byjoin && keyby && !bysameorder) {
    if (verbose) {last.started.at=proc.time();cat("setkey() afterwards for keyby=.EACHI ... ");flush.console()}
    setkeyv(ans,names(ans)[seq_along(byval)])
    if (verbose) {cat(timetaken(last.started.at),"\n"); flush.console()}
  } else if (keyby || (haskey(x) && bysameorder && (byjoin || (length(allbyvars) && identical(allbyvars,head(key(x),length(allbyvars))))))) {
    setattr(ans,"sorted",names(ans)[seq_along(grpcols)])
  }
  setalloccol(ans)   # TODO: overallocate in dogroups in the first place and remove this line
}

.optmean = function(expr) {   # called by optimization of j inside [.data.table only. Outside for a small speed advantage.
  if (length(expr)==2L)  # no parameters passed to mean, so defaults of trim=0 and na.rm=FALSE
    return(call(".External",quote(Cfastmean),expr[[2L]], FALSE))
    # return(call(".Internal",expr))  # slightly faster than .External, but R now blocks .Internal in coerce.c from apx Sep 2012
  if (length(expr)==3L && identical("na",substring(names(expr)[3L], 1L, 2L)))   # one parameter passed to mean()
    return(call(".External",quote(Cfastmean),expr[[2L]], expr[[3L]]))  # faster than .Call
  assign("nomeanopt",TRUE,parent.frame())
  expr  # e.g. trim is not optimized, just na.rm
}

#  [[.data.frame is now dispatched due to inheritance.
#  The code below tried to avoid that but made things
#  very slow (462 times faster down to 1 in the timings test).
#  TO DO. Reintroduce velow but dispatch straight to
#  .C("do_subset2") or better. Tests 604-608 test
#  that this doesn't regress.

#"[[.data.table" = function(x,...) {
#    if (!cedta()) return(`[[.data.frame`(x,...))
#    .subset2(x,...)
#    #class(x)=NULL  # awful, copy
#    #x[[...]]
#}

#"[[<-.data.table" = function(x,i,j,value) {
#    if (!cedta()) return(`[[<-.data.frame`(x,i,j,value))
#    if (!missing(j)) stop("[[i,j]] assignment not available in data.table, put assignment(s) in [i,{...}] instead, more powerful")
#    cl = oldClass(x)  # [[<-.data.frame uses oldClass rather than class, don't know why but we'll follow suit
#    class(x) = NULL
#    x[[i]] = value
#    class(x) = cl
#    x
#}

as.matrix.data.table = function(x, rownames=NULL, rownames.value=NULL, ...) {
  # rownames = the rownames column (most common usage)
  if (!is.null(rownames)) {
    if (!is.null(rownames.value)) stop("rownames and rownames.value cannot both be used at the same time")
    if (length(rownames)>1L) {
      # TODO in future as warned in NEWS for 1.11.6:
      #   warning("length(rownames)>1 is deprecated. Please use rownames.value= instead")
      if (length(rownames)!=nrow(x))
        stop("length(rownames)==", length(rownames), " but nrow(DT)==", nrow(x),
             ". The rownames argument specifies a single column name or number. Consider rownames.value= instead.")
      rownames.value = rownames
      rownames = NULL
    } else if (length(rownames)==0L) {
      stop("length(rownames)==0 but should be a single column name or number, or NULL")
    } else {
      if (isTRUE(rownames)) {
        if (length(key(x))>1L) {
          warning("rownames is TRUE but key has multiple columns ",
                  brackify(key(x)), "; taking first column x[,1] as rownames")
        }
        rownames = if (length(key(x))==1L) chmatch(key(x),names(x)) else 1L
      }
      else if (is.logical(rownames) || is.na(rownames)) {
        # FALSE, NA, NA_character_ all mean the same as NULL
        rownames = NULL
      }
      else if (is.character(rownames)) {
        w = chmatch(rownames, names(x))
        if (is.na(w)) stop("'", rownames, "' is not a column of x")
        rownames = w
      }
      else { # rownames is a column number already
        rownames = as.integer(rownames)
        if (is.na(rownames) || rownames<1L || rownames>ncol(x))
          stop("as.integer(rownames)==", rownames,
               " which is outside the column number range [1,ncol=", ncol(x), "].")
      }
    }
  } else if (!is.null(rownames.value)) {
    if (length(rownames.value)!=nrow(x))
      stop("length(rownames.value)==", length(rownames.value),
           " but should be nrow(x)==", nrow(x))
  }
  if (!is.null(rownames)) {
    # extract that column and drop it.
    rownames.value = x[[rownames]]
    dm = dim(x) - 0:1
    cn = names(x)[-rownames]
    X = x[, .SD, .SDcols = cn]
  } else {
    dm = dim(x)
    cn = names(x)
    X = x
  }
  if (any(dm == 0L))
    return(array(NA, dim = dm, dimnames = list(rownames.value, cn)))
  p = dm[2L]
  n = dm[1L]
  collabs = as.list(cn)
  class(X) = NULL
  non.numeric = non.atomic = FALSE
  all.logical = TRUE
  for (j in seq_len(p)) {
    if (is.ff(X[[j]])) X[[j]] = X[[j]][]   # nocov to bring the ff into memory, since we need to create a matrix in memory
    xj = X[[j]]
    if (length(dj <- dim(xj)) == 2L && dj[2L] > 1L) {
      if (inherits(xj, "data.table"))
        xj = X[[j]] = as.matrix(X[[j]])
      dnj = dimnames(xj)[[2L]]
      collabs[[j]] = paste(collabs[[j]], if (length(dnj) >
        0L)
        dnj
      else seq_len(dj[2L]), sep = ".")
    }
    if (!is.logical(xj))
      all.logical = FALSE
    if (length(levels(xj)) > 0L || !(is.numeric(xj) || is.complex(xj) || is.logical(xj)) ||
        (!is.null(cl <- attr(xj, "class", exact=TRUE)) && any(cl %chin%
        c("Date", "POSIXct", "POSIXlt"))))
      non.numeric = TRUE
    if (!is.atomic(xj))
      non.atomic = TRUE
  }
  if (non.atomic) {
    for (j in seq_len(p)) {
      xj = X[[j]]
      if (is.recursive(xj)) { }
      else X[[j]] = as.list(as.vector(xj))
    }
  }
  else if (all.logical) { }
  else if (non.numeric) {
    for (j in seq_len(p)) {
      if (is.character(X[[j]])) next
      xj = X[[j]]
      miss = is.na(xj)
      xj = if (length(levels(xj))) as.vector(xj) else format(xj)
      is.na(xj) = miss
      X[[j]] = xj
    }
  }
  X = unlist(X, recursive = FALSE, use.names = FALSE)
  dim(X) <- c(n, length(X)/n)
  dimnames(X) <- list(rownames.value, unlist(collabs, use.names = FALSE))
  X
}

# bug #2375. fixed. same as head.data.frame and tail.data.frame to deal with negative indices
head.data.table = function(x, n=6L, ...) {
  if (!cedta()) return(NextMethod()) # nocov
  stopifnot(length(n) == 1L)
  i = seq_len(if (n<0L) max(nrow(x)+n, 0L) else min(n,nrow(x)))
  x[i, , ]
}
tail.data.table = function(x, n=6L, ...) {
  if (!cedta()) return(NextMethod()) # nocov
  stopifnot(length(n) == 1L)
  n = if (n<0L) max(nrow(x) + n, 0L) else min(n, nrow(x))
  i = seq.int(to=nrow(x), length.out=n)
  x[i]
}

"[<-.data.table" = function (x, i, j, value) {
  # [<- is provided for consistency, but := is preferred as it allows by group and by reference to subsets of columns
  # with no copy of the (very large, say 10GB) columns at all. := is like an UPDATE in SQL and we like and want two symbols to change.
  if (!cedta()) {
    x = if (nargs()<4L) `[<-.data.frame`(x, i, value=value)
        else `[<-.data.frame`(x, i, j, value)
    return(setalloccol(x))    # over-allocate (again).   Avoid all this by using :=.
  }
  # TO DO: warning("Please use DT[i,j:=value] syntax instead of DT[i,j]<-value, for efficiency. See ?':='")
  if (!missing(i)) {
    isub=substitute(i)
    i = eval(.massagei(isub), x, parent.frame())
    if (is.matrix(i)) {
      if (!missing(j)) stop("When i is a matrix in DT[i]<-value syntax, it doesn't make sense to provide j")
      x = `[<-.data.frame`(x, i, value=value)
      return(setalloccol(x))
    }
    i = x[i, which=TRUE]
    # Tried adding ... after value above, and passing ... in here (e.g. for mult="first") but R CMD check
    # then gives "The argument of a replacement function which corresponds to the right hand side must be
    # named 'value'".  So, users have to use := for that.
  } else i = NULL          # meaning (to C code) all rows, without allocating 1L:nrow(x) vector
  if (missing(j)) j=names(x)
  if (!is.atomic(j)) stop("j must be an atomic vector, see ?is.atomic")
  if (anyNA(j)) stop("NA in j")
  if (is.character(j)) {
    newnames = setdiff(j,names(x))
    cols = as.integer(chmatch(j, c(names(x),newnames)))
    # We can now mix existing columns and new columns
  } else {
    if (!is.numeric(j)) stop("j must be vector of column name or positions")
    if (any(j>ncol(x))) stop("Attempt to assign to column position greater than ncol(x). Create the column by name, instead. This logic intends to catch (most likely) user errors.")
    cols = as.integer(j)  # for convenience e.g. to convert 1 to 1L
    newnames = NULL
  }
  reinstatekey=NULL
  if (haskey(x) && identical(key(x),key(value)) &&
    identical(names(x),names(value)) &&
    is.sorted(i) &&
    identical(substitute(x),quote(`*tmp*`))) {
    # DT["a",]$y = 1.1  winds up creating `*tmp*` subset of rows and assigning _all_ the columns into x and
    # over-writing the key columns with the same value (not just the single 'y' column).
    # That isn't good for speed; it's an R thing. Solution is to use := instead to avoid all this, but user
    # expects key to be retained in this case because _he_ didn't assign to a key column (the internal base R
    # code did).
    reinstatekey=key(x)
  }
  if (!selfrefok(x) || truelength(x) < ncol(x)+length(newnames)) {
    x = setalloccol(x, length(x)+length(newnames)) # because [<- copies via *tmp* and main/duplicate.c copies at length but copies truelength over too
    # search for one other .Call to assign in [.data.table to see how it differs
  }
  x = .Call(Cassign,copy(x),i,cols,newnames,value) # From 3.1.0, DF[2,"b"] = 7 no longer copies DF$a (so in this [<-.data.table method we need to copy)
  setalloccol(x)  #  can maybe avoid this realloc, but this is (slow) [<- anyway, so just be safe.
  if (length(reinstatekey)) setkeyv(x,reinstatekey)
  invisible(x)
  # no copy at all if user calls directly; i.e. `[<-.data.table`(x,i,j,value)
  # or uses data.table := syntax; i.e. DT[i,j:=value]
  # but, there is one copy by R in [<- dispatch to `*tmp*`; i.e. DT[i,j]=value. *Update: not from R > 3.0.2, yay*
  # That copy is via main/duplicate.c which preserves truelength but copies length amount. Hence setalloccol(x,length(x)).
  # No warn passed to assign here because we know it'll be copied via *tmp*.
  # := allows subassign to a column with no copy of the column at all,  and by group, etc.
}

"$<-.data.table" = function(x, name, value) {
  if (!cedta()) {
    ans = `$<-.data.frame`(x, name, value)
    return(setalloccol(ans))           # over-allocate (again)
  }
  x = copy(x)
  set(x,j=name,value=value)  # important i is missing here
}

as.data.frame.data.table = function(x, ...)
{
  ans = copy(x)
  setattr(ans,"row.names",.set_row_names(nrow(x)))   # since R 2.4.0, data.frames can have non-character row names
  setattr(ans,"class","data.frame")
  setattr(ans,"sorted",NULL)  # remove so if you convert to df, do something, and convert back, it is not sorted
  setattr(ans,".internal.selfref",NULL)
  # leave tl intact, no harm,
  ans
}

as.list.data.table = function(x, ...) {
  # Similar to as.list.data.frame in base. Although a data.table/frame is a list, too, it may be
  # being coerced to raw list type (by calling code) so that "[" and "[[" work in their raw list form,
  # such as lapply does for data.frame. So we do have to remove the class attributes (and thus shallow
  # copy is almost instant way to achieve that, without risking compatibility).
  #if (sys.call(-2L)[[1L]]=="lapply")
  #    return(x)
  ans = shallow(x)
  setattr(ans, "class", NULL)
  setattr(ans, "row.names", NULL)
  setattr(ans, "sorted", NULL)
  setattr(ans,".internal.selfref", NULL)   # needed to pass S4 tests for example
  ans
}


dimnames.data.table = function(x) {
  if (!cedta()) {
    if (!inherits(x, "data.frame"))
      stop("data.table inherits from data.frame (from v1.5), but this data.table does not. Has it been created manually (e.g. by using 'structure' rather than 'data.table') or saved to disk using a prior version of data.table?")
    return(`dimnames.data.frame`(x))
  }
  list(NULL, names(x))
}

"dimnames<-.data.table" = function (x, value)   # so that can do  colnames(dt)=<..>  as well as names(dt)=<..>
{
  if (!cedta()) return(`dimnames<-.data.frame`(x,value))  # nocov ; will drop key but names<-.data.table (below) is more common usage and does retain the key
  if (!is.list(value) || length(value) != 2L) stop("attempting to assign invalid object to dimnames of a data.table")
  if (!is.null(value[[1L]])) stop("data.tables do not have rownames")
  if (ncol(x) != length(value[[2L]])) stop("Can't assign ", length(value[[2L]]), " colnames to a ", ncol(x), "-column data.table")
  setnames(x,as.character(value[[2L]]))
  x  # this returned value is now shallow copied by R 3.1.0 via *tmp*. A very welcome change.
}

"names<-.data.table" = function(x,value)
{
  # When non data.table aware packages change names, we'd like to maintain the key.
  # If call is names(DT)[2]="newname", R will call this names<-.data.table function (notice no i) with 'value' already prepared to be same length as ncol
  x = shallow(x) # `names<-` should not modify by reference. Related to #1015, #476 and #825. Needed for R v3.1.0+.  TO DO: revisit
  if (is.null(value))
    setattr(x,"names",NULL)   # e.g. plyr::melt() calls base::unname()
  else
    setnames(x,value)
  x   # this returned value is now shallow copied by R 3.1.0 via *tmp*. A very welcome change.
}

within.data.table = function (data, expr, ...)
# basically within.list but retains key (if any)
# will be slower than using := or a regular query (see ?within for further info).
{
  if (!cedta()) return(NextMethod()) # nocov
  parent = parent.frame()
  e = evalq(environment(), data, parent)
  eval(substitute(expr), e)  # might (and it's known that some user code does) contain rm()
  l = as.list(e)
  l = l[!vapply_1b(l, is.null)]
  nD = length(del <- setdiff(names(data), (nl <- names(l))))
  ans = copy(data)
  if (length(nl)) ans[,nl] = l
  if (nD) ans[,del] = NULL
  if (haskey(data) && all(key(data) %chin% names(ans))) {
    x = TRUE
    for (i in key(data)) {
      x = identical(data[[i]],ans[[i]])
      if (!x) break
    }
    if (x) setattr(ans,"sorted",key(data))
  }
  ans
}

transform.data.table = function (`_data`, ...)
# basically transform.data.frame with data.table instead of data.frame, and retains key
{
  if (!cedta()) return(NextMethod()) # nocov
  e = eval(substitute(list(...)), `_data`, parent.frame())
  tags = names(e)
  inx = chmatch(tags, names(`_data`))
  matched = !is.na(inx)
  if (any(matched)) {
    .Call(C_unlock, `_data`) # fix for #1641, now covered by test 104.2
    `_data`[,inx[matched]] = e[matched]
    `_data` = as.data.table(`_data`)
  }
  if (!all(matched)) {
    ans = do.call("data.table", c(list(`_data`), e[!matched]))
  } else {
    ans = `_data`
  }
  key.cols = key(`_data`)
  if (!any(tags %chin% key.cols)) {
    setattr(ans, "sorted", key.cols)
  }
  ans
}

subset.data.table = function (x, subset, select, ...)
{
  key.cols = key(x)

  if (missing(subset)) {
    r = TRUE
  } else {
    e = substitute(subset)
    r = eval(e, x, parent.frame())
    if (!is.logical(r))
      stop("'subset' must evaluate to logical")
    r = r & !is.na(r)
  }

  if (missing(select)) {
    vars = seq_len(ncol(x))
  } else {
    nl = as.list(seq_len(ncol(x)))
    setattr(nl,"names",names(x))
    vars = eval(substitute(select), nl, parent.frame())  # e.g.  select=colF:colP
    # #891 fix - don't convert numeric vars to column names - will break when there are duplicate columns
    key.cols = intersect(key.cols, names(x)[vars]) ## Only keep key.columns found in the select clause
  }

  ans = x[r, vars, with = FALSE]

  if (nrow(ans) > 0L) {
    if (!missing(select) && length(key.cols)) {
      ## Set the key on the returned data.table as long as the key
      ## columns that "remain" are the same as the original, or a
      ## prefix of it.
      is.prefix = all(key(x)[seq_len(length(key.cols))] == key.cols)
      if (is.prefix) {
        setattr(ans, "sorted", key.cols)
      }
    }
  } else {
    setkey(ans,NULL)
  }
  ans
}

# Equivalent of 'rowSums(is.na(dt) > 0L)' but much faster and memory efficient.
# Also called "complete.cases" in base. Unfortunately it's not a S3 generic.
# Also handles bit64::integer64. TODO: export this?
# For internal use only. 'by' requires integer input. No argument checks here yet.
is_na = function(x, by=seq_along(x)) .Call(Cdt_na, x, by)
any_na = function(x, by=seq_along(x)) .Call(CanyNA, x, by)

na.omit.data.table = function (object, cols = seq_along(object), invert = FALSE, ...) {
  # compare to stats:::na.omit.data.frame
  if (!cedta()) return(NextMethod()) # nocov
  if ( !missing(invert) && is.na(as.logical(invert)) )
    stop("Argument 'invert' must be logical TRUE/FALSE")
  cols = colnamesInt(object, cols, check_dups=FALSE)
  ix = .Call(Cdt_na, object, cols)
  # forgot about invert with no NA case, #2660
  if (invert) {
    if (all(ix))
      object
    else
      .Call(CsubsetDT, object, which_(ix, bool = TRUE), seq_along(object))
  } else {
    if (any(ix))
      .Call(CsubsetDT, object, which_(ix, bool = FALSE), seq_along(object))
    else
      object
  }
}

which_ = function(x, bool = TRUE) {
  # fix for #1467, quotes result in "not resolved in current namespace" error
  .Call(Cwhichwrapper, x, bool)
}

is.na.data.table = function (x) {
  if (!cedta()) return(`is.na.data.frame`(x))
  do.call("cbind", lapply(x, "is.na"))
}

# not longer needed as inherits ...
#    t.data.table = t.data.frame
#    Math.data.table = Math.data.frame
#    summary.data.table = summary.data.frame

Ops.data.table = function(e1, e2 = NULL)
{
  ans = NextMethod()
  if (cedta() && is.data.frame(ans)) ans = as.data.table(ans) 
  else if (is.matrix(ans)) colnames(ans) = copy(colnames(ans))
  ans
}

split.data.table = function(x, f, drop = FALSE, by, sorted = FALSE, keep.by = TRUE, flatten = TRUE, ..., verbose = getOption("datatable.verbose")) {
  if (!is.data.table(x)) stop("x argument must be a data.table")
  stopifnot(is.logical(drop), is.logical(sorted), is.logical(keep.by),  is.logical(flatten))
  # split data.frame way, using `f` and not `by` argument
  if (!missing(f)) {
    if (!length(f) && nrow(x))
      stop("group length is 0 but data nrow > 0")
    if (!missing(by))
      stop("passing 'f' argument together with 'by' is not allowed, use 'by' when split by column in data.table and 'f' when split by external factor")
    # same as split.data.frame - handling all exceptions, factor orders etc, in a single stream of processing was a nightmare in factor and drop consistency
    return(lapply(split(x = seq_len(nrow(x)), f = f, drop = drop, ...), function(ind) x[ind]))
  }
  if (missing(by)) stop("Either 'by' or 'f' argument must be supplied")
  # check reserved column names during processing
  if (".ll.tech.split" %chin% names(x)) stop("Column '.ll.tech.split' is reserved for split.data.table processing")
  if (".nm.tech.split" %chin% by) stop("Column '.nm.tech.split' is reserved for split.data.table processing")
  if (!all(by %chin% names(x))) stop("Argument 'by' must refer to column names in x")
  if (!all(by.atomic <- vapply_1b(by, function(.by) is.atomic(x[[.by]])))) stop("Argument 'by' must refer only to atomic-type columns, but the following columns are non-atomic: ", brackify(by[!by.atomic]))
  # list of data.tables (flatten) or list of lists of ... data.tables
  make.levels = function(x, cols, sorted) {
    by.order = if (!sorted) x[, funique(.SD), .SDcols=cols] # remember order of data, only when not sorted=FALSE
    ul = lapply(setNames(cols, nm=cols), function(col) {
      if (!is.factor(x[[col]])) unique(x[[col]]) else {
      .x_lev = levels(x[[col]])
      #need to keep as a factor or order will be lost, #2082
      factor(.x_lev, levels = .x_lev)
      }
    })
    r = do.call("CJ", c(ul, sorted=sorted, unique=TRUE))
    if (!sorted && nrow(by.order)) {
      ii = r[by.order, on=cols, which=TRUE]
      r = rbindlist(list(
        r[ii], # original order from data
        r[-ii] # empty levels at the end
      ))
    }
    r
  }
  .by = by[1L]
  # this builds data.table call - is much more cleaner than handling each case one by one
  dtq = as.list(call("[", as.name("x")))
  join = FALSE
  flatten_any = flatten && any(vapply_1b(by, function(col) is.factor(x[[col]])))
  nested_current = !flatten && is.factor(x[[.by]])
  if (!drop && (flatten_any || nested_current)) {
    # create 'levs' here to avoid lexical scoping glitches, see #3151
    levs = make.levels(x=x, cols=if (flatten) by else .by, sorted=sorted)
    dtq[["i"]] = quote(levs)
    join = TRUE
  }
  dtq[["j"]] = substitute(
    list(.ll.tech.split=list(.expr)),
    list(.expr = if (join) quote(if(.N == 0L) .SD[0L] else .SD) else as.name(".SD")) # simplify when `nomatch` accept NULL #857 ?
  )
  by.or.keyby = if (join) "by" else c("by"[!sorted], "keyby"[sorted])[1L]
  dtq[[by.or.keyby]] = substitute( # retain order, for `join` and `sorted` it will use order of `i` data.table instead of `keyby`.
    .expr,
    list(.expr = if(join) {as.name(".EACHI")} else if (flatten) by else .by)
  )
  dtq[[".SDcols"]] = if (keep.by) names(x) else setdiff(names(x), if (flatten) by else .by)
  if (join) dtq[["on"]] = if (flatten) by else .by
  dtq = as.call(dtq)
  if (isTRUE(verbose)) cat("Processing split.data.table with: ", deparse(dtq, width.cutoff=500L), "\n", sep="")
  tmp = eval(dtq)
  # add names on list
  setattr(ll <- tmp$.ll.tech.split,
      "names",
      as.character(
        if (!flatten) tmp[[.by]] else tmp[, list(.nm.tech.split=paste(unlist(lapply(.SD, as.character)), collapse = ".")), by=by, .SDcols=by]$.nm.tech.split
      ))
  # handle nested split
  if (flatten || length(by) == 1L) {
    for (x in ll) .Call(C_unlock, x)
    lapply(ll, setDT)
    # alloc.col could handle DT in list as done in: c9c4ff80bdd4c600b0c4eff23b207d53677176bd
  } else if (length(by) > 1L) {
    lapply(ll, split.data.table, drop=drop, by=by[-1L], sorted=sorted, keep.by=keep.by, flatten=flatten)
  }
}

# TO DO, add more warnings e.g. for by.data.table(), telling user what the data.table syntax is but letting them dispatch to data.frame if they want

copy = function(x) {
  newx = .Call(Ccopy,x)  # copies at length but R's duplicate() also copies truelength over.
                         # TO DO: inside Ccopy it could reset tl to 0 or length, but no matter as selfrefok detects it
                         # TO DO: revisit duplicate.c in R 3.0.3 and see where it's at

  reallocate = function(y) {
    if (is.data.table(y)) {
      .Call(C_unlock, y)
      setalloccol(y)
    } else if (is.list(y)) {
      y[] = lapply(y, reallocate)
    }
    y
  }

  reallocate(newx)
}

.shallow = function(x, cols = NULL, retain.key = FALSE, unlock = FALSE) {
  wasnull = is.null(cols)
  cols = colnamesInt(x, cols, check_dups=FALSE)
  ans = .Call(Cshallowwrapper, x, cols)  # copies VECSXP only

  if(retain.key){
    if (wasnull) return(ans) # handle most frequent case first
    ## get correct key if cols are present
    cols = names(x)[cols]
    keylength = which.first(!key(ans) %chin% cols) - 1L
    if (is.na(keylength)) keylength <- length(key(ans))
    if (!keylength) {
      setattr(ans, "sorted", NULL) ## no key remaining
    } else {
      setattr(ans, "sorted", head(key(ans), keylength)) ## keep what can be kept
    }
    ## take care of attributes.
    indices = names(attributes(attr(ans, "index", exact=TRUE)))
    for(index in indices) {
      indexcols = strsplit(index, split = "__")[[1L]][-1L]
      indexlength = which.first(!indexcols %chin% cols) - 1L
      if (is.na(indexlength)) next ## all columns are present, nothing to be done
      reducedindex = paste0("__", indexcols[seq_len(indexlength)], collapse="") ## the columns until the first missing from the new index
      if (reducedindex %chin% indices || !indexlength) {
        ## Either reduced index already present or no columns of the original index remain.
        ## Drop the original index completely
        setattr(attr(ans, "index", exact=TRUE), index, NULL)
      } else if(length(attr(attr(ans, "index", exact=TRUE), index, exact=TRUE))) {
        ## index is not length 0. Drop it since shortening could lead to spurious reordering in discarded columns (#2336)
        setattr(attr(ans, "index", exact=TRUE), index, NULL)
      } else {
        ## rename index to reducedindex
        names(attributes(attr(ans, "index")))[names(attributes(attr(ans, "index", exact=TRUE))) == index] = reducedindex
      }
    }
  } else { # retain.key == FALSE
    setattr(ans, "sorted", NULL)
    setattr(ans, "index", NULL)
  }
  if (unlock) .Call(C_unlock, ans)
  ans
}

shallow = function(x, cols=NULL) {
  if (!is.data.table(x))
    stop("x is not a data.table. Shallow copy is a copy of the vector of column pointers (only), so is only meaningful for data.table")
  ans = .shallow(x, cols=cols, retain.key = TRUE)
  ans
}

setalloccol = alloc.col = function(DT, n=getOption("datatable.alloccol"), verbose=getOption("datatable.verbose"))
{
  name = substitute(DT)
  if (identical(name, quote(`*tmp*`))) stop("setalloccol attempting to modify `*tmp*`")
  ans = .Call(Calloccolwrapper, DT, eval(n), verbose)
  if (is.name(name)) {
    name = as.character(name)
    assign(name,ans,parent.frame(),inherits=TRUE)
  }
  ans
}

selfrefok = function(DT,verbose=getOption("datatable.verbose")) {
  .Call(Cselfrefokwrapper,DT,verbose)
}

truelength = function(x) .Call(Ctruelength,x)
# deliberately no "truelength<-" method.  setalloccol is the mechanism for that.
# settruelength() no longer need (and so removed) now that data.table depends on R 2.14.0
# which initializes tl to zero rather than leaving uninitialized.

setattr = function(x,name,value) {
  # Wrapper for setAttrib internal R function
  # Sets attribute by reference (no copy)
  # Named setattr (rather than setattrib) at R level to more closely resemble attr<-
  # And as from 1.7.8 is made exported in NAMESPACE for use in user attributes.
  # User can also call `attr<-` function directly, but that copies (maybe just when NAMED>0, which is always for data.frame, I think).  See "Confused by NAMED" thread on r-devel 24 Nov 2011.
  # We tend to use setattr() internally in data.table.R because often we construct a data.table and it hasn't
  # got names yet. setnames() is the user interface which checks integrity and doesn't let you drop names for example.
  if (name=="names" && is.data.table(x) && length(attr(x, "names", exact=TRUE)) && !is.null(value))
    setnames(x,value)
    # Using setnames here so that truelength of names can be retained, to carry out integrity checks such as not
    # creating names longer than the number of columns of x, and to change the key, too
    # For convenience so that setattr(DT,"names",allnames) works as expected without requiring a switch to setnames.
  else {
    ans = .Call(Csetattrib, x, name, value)
    # If name=="names" and this is the first time names are assigned (e.g. in data.table()), this will be grown by setalloccol very shortly afterwards in the caller.
    if (!is.null(ans)) {
      warning("Input is a length=1 logical that points to the same address as R's global value. Therefore the attribute has not been set by reference, rather on a copy. You will need to assign the result back to a variable. See issue #1281.")
      x = ans
    }
  }
  # fix for #1142 - duplicated levels for factors
  if (name == "levels" && is.factor(x) && anyDuplicated(value))
    .Call(Csetlevels, x, (value <- as.character(value)), unique(value))
  invisible(x)
}

setnames = function(x,old,new,skip_absent=FALSE) {
  # Sets by reference, maintains truelength, no copy of table at all.
  # But also more convenient than names(DT)[i]="newname"  because we can also do setnames(DT,"oldname","newname")
  # without an onerous match() ourselves. old can be positions, too, but we encourage by name for robustness.
  # duplicates are permitted to be created without warning; e.g. in revdeps and for example, and setting spacer columns all with ""
  if (!is.data.frame(x)) stop("x is not a data.table or data.frame")
  ncol = length(x)
  if (length(names(x)) != ncol) stop("x has ",ncol," columns but its names are length ",length(names(x)))
  stopifnot(isTRUEorFALSE(skip_absent))
  if (missing(new) || missing(old)) {
    # usage: setnames(DT, new = letters[1:n])
    if (missing(old)) { old = new; new = NULL }
    # for setnames(DT,new); e.g., setnames(DT,c("A","B")) where ncol(DT)==2
    if (is.function(old)) old = old(names(x))
    if (!is.character(old)) stop("Passed a vector of type '",typeof(old),"'. Needs to be type 'character'.")
    if (length(old) != ncol) stop("Can't assign ",length(old)," names to a ",ncol," column data.table")
    if (anyNA(names(x))) {
      # if x somehow has some NA names, which() needs help to return them, #2475
      w = which((names(x) != old) | (Encoding(names(x)) != Encoding(old)) | (is.na(names(x)) & !is.na(old)))
    } else {
      w = which(names(x) != old | (Encoding(names(x)) != Encoding(old)))
    }
    if (!length(w)) return(invisible(x))  # no changes
    new = old[w]
    i = w
  } else {
    if (is.function(new)) new = if (is.numeric(old)) new(names(x)[old]) else new(old)
    if (!is.character(new)) stop("'new' is not a character vector or a function")
    #  if (anyDuplicated(new)) warning("Some duplicates exist in 'new': ", brackify(new[duplicated(new)]))  # dups allowed without warning; warn if and when the dup causes an ambiguity
    if (anyNA(new)) stop("NA in 'new' at positions ", brackify(which(is.na(new))))
    if (anyDuplicated(old)) stop("Some duplicates exist in 'old': ", brackify(old[duplicated(old)]))
    if (is.numeric(old)) i = old = seq_along(x)[old]  # leave it to standard R to manipulate bounds and negative numbers
    else if (!is.character(old)) stop("'old' is type ",typeof(old)," but should be integer, double or character")
    if (length(new)!=length(old)) stop("'old' is length ",length(old)," but 'new' is length ",length(new))
    if (anyNA(old)) stop("NA (or out of bounds) in 'old' at positions ", brackify(which(is.na(old))))
    if (is.character(old)) {
      i = chmatchdup(c(old,old), names(x))  # chmatchdup returns the second of any duplicates matched to in names(x) (if any)
      if (!all(tt<-is.na(tail(i,length(old))))) warning("Item ",w<-which.first(!tt)," of 'old' is '", old[w],"' which appears several times in column names. Just the first will be changed. There are ", sum(!tt)-1L," other items in old that are also duplicated in column names.")
      i = head(i,length(old))
      if (anyNA(i)) {
        if (isTRUE(skip_absent)) {
          w = !is.na(i)
          new = new[w]
          i = i[w]
        } else {
          stop("Items of 'old' not found in column names: ", brackify(old[is.na(i)]), ". Consider skip_absent=TRUE.")
        }
      }
    }
    if (any(w <- new==names(x)[i] & Encoding(new)==Encoding(names(x)[i]))) {
      w = which(!w)
      new = new[w]
      i = i[w]
    }
    if (!length(new)) return(invisible(x)) # no changes
    if (length(i) != length(new)) stop("Internal error: length(i)!=length(new)") # nocov
  }
  # update the key if the column name being change is in the key
  m = chmatch(names(x)[i], key(x))
  w = which(!is.na(m))
  if (length(w))
    .Call(Csetcharvec, attr(x, "sorted", exact=TRUE), m[w], new[w])

  # update secondary keys
  idx = attr(x, "index", exact=TRUE)
  for (k in names(attributes(idx))) {
    tt = strsplit(k,split="__")[[1L]][-1L]
    m = chmatch(names(x)[i], tt)
    w = which(!is.na(m))
    if (length(w)) {
      tt[m[w]] = new[w]
      newk = paste0("__",tt,collapse="")
      setattr(idx, newk, attr(idx, k, exact=TRUE))
      setattr(idx, k, NULL)
    }
  }

  .Call(Csetcharvec, attr(x, "names", exact=TRUE), as.integer(i), new)
  invisible(x)
}

setcolorder = function(x, neworder=key(x))
{
  if (is.character(neworder) && anyDuplicated(names(x)))
    stop("x has some duplicated column name(s): ", paste(names(x)[duplicated(names(x))], collapse=","), ". Please remove or rename the duplicate(s) and try again.")
  # if (!is.data.table(x)) stop("x is not a data.table")
  neworder = colnamesInt(x, neworder, check_dups=FALSE)  # dups are now checked inside Csetcolorder below
  if (length(neworder) != length(x)) {
    #if shorter than length(x), pad by the missing
    #  elements (checks below will catch other mistakes)
    neworder = c(neworder, setdiff(seq_along(x), neworder))
  }
  .Call(Csetcolorder, x, neworder)
  invisible(x)
}

set = function(x,i=NULL,j,value)  # low overhead, loopable
{
  .Call(Cassign,x,i,j,NULL,value)
  invisible(x)
}

chmatch = function(x, table, nomatch=NA_integer_)
  .Call(Cchmatch, x, table, as.integer(nomatch[1L])) # [1L] to fix #1672

# chmatchdup() behaves like 'pmatch' but only the 'exact' matching part; i.e. a value in
# 'x' is matched to 'table' only once. No index will be present more than once. For example:
# chmatchdup(c("a", "a"), c("a", "a")) # 1,2 - the second 'a' in 'x' has a 2nd match in 'table'
# chmatchdup(c("a", "a"), c("a", "b")) # 1,NA - the second one doesn't 'see' the first 'a'
# chmatchdup(c("a", "a"), c("a", "a.1")) # 1,NA - this is where it differs from pmatch - we don't need the partial match.
chmatchdup = function(x, table, nomatch=NA_integer_)
  .Call(Cchmatchdup, x, table, as.integer(nomatch[1L]))

"%chin%" = function(x, table)
  .Call(Cchin, x, table)  # TO DO  if table has 'ul' then match to that

chorder = function(x) {
  o = forderv(x, sort=TRUE, retGrp=FALSE)
  if (length(o)) o else seq_along(x)
}

chgroup = function(x) {
  # TO DO: deprecate and remove this. It's exported but doubt anyone uses it. Think the plan was to use it internally, but forderv superceded.
  o = forderv(x, sort=FALSE, retGrp=TRUE)
  if (length(o)) as.vector(o) else seq_along(x)  # as.vector removes the attributes
}

# plain rbind and cbind methods are registered using S3method() in NAMESPACE only from R>=4.0.0; #3948
rbind.data.table = function(..., use.names=TRUE, fill=FALSE, idcol=NULL) {
  l = lapply(list(...), function(x) if (is.list(x)) x else as.data.table(x))  #1626; e.g. psych binds a data.frame|table with a matrix
  rbindlist(l, use.names, fill, idcol)
}
cbind.data.table = data.table
.rbind.data.table = rbind.data.table  # the workaround using this in FAQ 2.24 is still applied to support R < 4.0.0

rbindlist = function(l, use.names="check", fill=FALSE, idcol=NULL) {
  if (is.null(l)) return(null.data.table())
  if (!is.list(l) || is.data.frame(l)) stop("Input is ", class(l)[1L]," but should be a plain list of items to be stacked")
  if (isFALSE(idcol)) { idcol = NULL }
  else if (!is.null(idcol)) {
    if (isTRUE(idcol)) idcol = ".id"
    if (!is.character(idcol)) stop("idcol must be a logical or character vector of length 1. If logical TRUE the id column will named '.id'.")
    idcol = idcol[1L]
  }
  miss = missing(use.names)
  # more checking of use.names happens at C level; this is just minimal to massage 'check' to NA
  if (identical(use.names, NA)) stop("use.names=NA invalid")  # otherwise use.names=NA could creep in an usage equivalent to use.names='check'
  if (identical(use.names,"check")) {
    if (!miss) stop("use.names='check' cannot be used explicitly because the value 'check' is new in v1.12.2 and subject to change. It is just meant to convey default behavior. See ?rbindlist.")
    use.names = NA
  }
  ans = .Call(Crbindlist, l, use.names, fill, idcol)
  if (!length(ans)) return(null.data.table())
  setDT(ans)[]
}

vecseq = function(x,y,clamp) .Call(Cvecseq,x,y,clamp)

# .Call(Caddress, x) increments NAM() when x is vector with NAM(1). Referring object within non-primitive function is enough to increment reference.
address = function(x) .Call(Caddress, eval(substitute(x), parent.frame()))

":=" = function(...) {
  # this error is detected when eval'ing isub and replaced with a more helpful one when using := in i due to forgetting a comma, #4227
  stop('Check that is.data.table(DT) == TRUE. Otherwise, := and `:=`(...) are defined for use in j, once only and in particular ways. See help(":=").')
}

setDF = function(x, rownames=NULL) {
  if (!is.list(x)) stop("setDF only accepts data.table, data.frame or list of equal length as input")
  if (anyDuplicated(rownames)) stop("rownames contains duplicates")
  if (is.data.table(x)) {
    # copied from as.data.frame.data.table
    if (is.null(rownames)) {
      rn = .set_row_names(nrow(x))
    } else {
      if (length(rownames) != nrow(x))
        stop("rownames incorrect length; expected ", nrow(x), " names, got ", length(rownames))
      rn = rownames
    }
    setattr(x, "row.names", rn)
    setattr(x, "class", "data.frame")
    setattr(x, "sorted", NULL)
    setattr(x, ".internal.selfref", NULL)
  } else if (is.data.frame(x)) {
    if (!is.null(rownames)) {
      if (length(rownames) != nrow(x))
        stop("rownames incorrect length; expected ", nrow(x), " names, got ", length(rownames))
      setattr(x, "row.names", rownames)
    }
    x
  } else {
    n = vapply_1i(x, length)
    mn = max(n)
    if (any(n<mn))
      stop("All elements in argument 'x' to 'setDF' must be of same length")
    xn = names(x)
    if (is.null(xn)) {
      setattr(x, "names", paste0("V",seq_len(length(x))))
    } else {
      idx = xn %chin% ""
      if (any(idx)) {
        xn[idx] = paste0("V", seq_along(which(idx)))
        setattr(x, "names", xn)
      }
    }
    if (is.null(rownames)) {
      rn = .set_row_names(mn)
    } else {
      if (length(rownames) != mn)
      stop("rownames incorrect length; expected ", mn, " names, got ", length(rownames))
      rn = rownames
    }
    setattr(x,"row.names", rn)
    setattr(x,"class","data.frame")
  }
  invisible(x)
}

setDT = function(x, keep.rownames=FALSE, key=NULL, check.names=FALSE) {
  name = substitute(x)
  if (is.name(name)) {
    home = function(x, env) {
      if (identical(env, emptyenv()))
        stop("Cannot find symbol ", cname, call. = FALSE)
      else if (exists(x, env, inherits=FALSE)) env
      else home(x, parent.env(env))
    }
    cname = as.character(name)
    envir = home(cname, parent.frame())
    if (bindingIsLocked(cname, envir)) {
      stop("Cannot convert '", cname, "' to data.table by reference because binding is locked. It is very likely that '", cname, "' resides within a package (or an environment) that is locked to prevent modifying its variable bindings. Try copying the object to your current environment, ex: var <- copy(var) and then using setDT again.")
    }
  }
  # check no matrix-like columns, #3760. Other than a single list(matrix) is unambiguous and depended on by some revdeps, #3581
  if (length(x)>1L) {
    idx = vapply_1i(x, function(xi) length(dim(xi)))>1L
    if (any(idx))
      warning("Some columns are a multi-column type (such as a matrix column): ", brackify(which(idx)),". setDT will retain these columns as-is but subsequent operations like grouping and joining may fail. Please consider as.data.table() instead which will create a new column for each embedded column.")
  }
  if (is.data.table(x)) {
    # fix for #1078 and #1128, see .resetclass() for explanation.
    setattr(x, 'class', .resetclass(x, 'data.table'))
    if (!missing(key)) setkeyv(x, key) # fix for #1169
    if (check.names) setattr(x, "names", make.names(names(x), unique=TRUE))
    if (selfrefok(x) > 0L) return(invisible(x)) else setalloccol(x)
  } else if (is.data.frame(x)) {
    rn = if (!identical(keep.rownames, FALSE)) rownames(x) else NULL
    setattr(x, "row.names", .set_row_names(nrow(x)))
    if (check.names) setattr(x, "names", make.names(names(x), unique=TRUE))
    # fix for #1078 and #1128, see .resetclass() for explanation.
    setattr(x, "class", .resetclass(x, 'data.frame'))
    setalloccol(x)
    if (!is.null(rn)) {
      nm = c(if (is.character(keep.rownames)) keep.rownames[1L] else "rn", names(x))
      x[, (nm[1L]) := rn]
      setcolorder(x, nm)
    }
  } else if (is.list(x) && length(x)==1L && is.matrix(x[[1L]])) {
    # a single list(matrix) is unambiguous and depended on by some revdeps, #3581
    x = as.data.table.matrix(x[[1L]])
  } else if (is.null(x) || (is.list(x) && !length(x))) {
    x = null.data.table()
  } else if (is.list(x)) {
    # copied from as.data.table.list - except removed the copy
    for (i in seq_along(x)) {
      if (is.null(x[[i]])) next   # allow NULL columns to be created by setDT(list) even though they are not really allowed
                                  # many operations still work in the presence of NULL columns and it might be convenient
                                  # e.g. in package eplusr which calls setDT on a list when parsing JSON. Operations which
                                  # fail for NULL columns will give helpful error at that point, #3480 and #3471
      if (inherits(x[[i]], "POSIXlt")) stop("Column ", i, " is of POSIXlt type. Please convert it to POSIXct using as.POSIXct and run setDT again. We do not recommend use of POSIXlt at all because it uses 40 bytes to store one date.")
    }
    n = vapply_1i(x, length)
    n_range = range(n)
    if (n_range[1L] != n_range[2L]) {
      tbl = sort(table(n))
      stop("All elements in argument 'x' to 'setDT' must be of same length, but the profile of input lengths (length:frequency) is: ",
           brackify(sprintf('%s:%d', names(tbl), tbl)), "\nThe first entry with fewer than ", n_range[2L], " entries is ", which.max(n<n_range[2L]))
    }
    xn = names(x)
    if (is.null(xn)) {
      setattr(x, "names", paste0("V",seq_len(length(x))))
    } else {
      idx = xn %chin% "" # names can be NA - test 1006 caught that!
      if (any(idx)) {
        xn[idx] = paste0("V", seq_along(which(idx)))
        setattr(x, "names", xn)
      }
      if (check.names) setattr(x, "names", make.names(xn, unique=TRUE))
    }
    setattr(x,"row.names",.set_row_names(n_range[2L]))
    setattr(x,"class",c("data.table","data.frame"))
    setalloccol(x)
  } else {
    stop("Argument 'x' to 'setDT' should be a 'list', 'data.frame' or 'data.table'")
  }
  if (!is.null(key)) setkeyv(x, key)
  if (is.name(name)) {
    name = as.character(name)
    assign(name, x, parent.frame(), inherits=TRUE)
  } else if (name %iscall% c('$', '[[') && is.name(name[[2L]])) {
    # common case is call from 'lapply()'
    k = eval(name[[2L]], parent.frame(), parent.frame())
    if (is.list(k)) {
      origj = j = if (name[[1L]] == "$") as.character(name[[3L]]) else eval(name[[3L]], parent.frame(), parent.frame())
      if (length(j) == 1L) {
        if (is.character(j)) {
          j = match(j, names(k))
          if (is.na(j))
            stop("Item '", origj, "' not found in names of input list")
        }
      }
      .Call(Csetlistelt,k,as.integer(j), x)
    } else if (is.environment(k) && exists(as.character(name[[3L]]), k)) {
      assign(as.character(name[[3L]]), x, k, inherits=FALSE)
    }
  }
  .Call(CexpandAltRep, x)  # issue#2866 and PR#2882
  invisible(x)
}

as_list = function(x) {
  lx = vector("list", 1L)
  .Call(Csetlistelt, lx, 1L, x)
  lx
}

# FR #1353
rowid = function(..., prefix=NULL) {
  rowidv(list(...), prefix=prefix)
}

rowidv = function(x, cols=seq_along(x), prefix=NULL) {
  if (!is.null(prefix) && (!is.character(prefix) || length(prefix) != 1L))
    stop("'prefix' must be NULL or a character vector of length 1.")
  if (is.atomic(x)) {
    if (!missing(cols) && !is.null(cols))
      stop("x is a single vector, non-NULL 'cols' doesn't make sense.")
    cols = 1L
    x = as_list(x)
  } else if (!length(cols)) {
    stop("x is a list, 'cols' cannot be 0-length.")
  }
  xorder = forderv(x, by=cols, sort=FALSE, retGrp=TRUE) # speedup on char with sort=FALSE
  xstart = attr(xorder, 'starts', exact=TRUE)
  if (!length(xorder)) xorder = seq_along(x[[1L]])
  ids = .Call(Cfrank, xorder, xstart, uniqlengths(xstart, length(xorder)), "sequence")
  if (!is.null(prefix))
    ids = paste0(prefix, ids)
  ids
}

# FR #686
rleid = function(..., prefix=NULL) {
  rleidv(list(...), prefix=prefix)
}

rleidv = function(x, cols=seq_along(x), prefix=NULL) {
  if (!is.null(prefix) && (!is.character(prefix) || length(prefix) != 1L))
    stop("'prefix' must be NULL or a character vector of length 1.")
  if (is.atomic(x)) {
    if (!missing(cols) && !is.null(cols))
      stop("x is a single vector, non-NULL 'cols' doesn't make sense.")
    cols = 1L
    x = as_list(x)
  } else if (!length(cols)) {
    stop("x is a list, 'cols' cannot be 0-length.")
  }
  cols = colnamesInt(x, cols, check_dups=FALSE)
  ids = .Call(Crleid, x, cols)
  if (!is.null(prefix)) ids = paste0(prefix, ids)
  ids
}

# GForce functions
#   to add a new function to GForce (from the R side -- the easy part!):
#     (1) add it to gfuns
#     (2) edit .gforce_ok (defined within `[`) to catch which j will apply the new function
#     (3) define the gfun = function() R wrapper
gfuns = c("[", "[[", "head", "tail", "first", "last", "sum", "mean", "prod",
          "median", "min", "max", "var", "sd", ".N") # added .N for #334
`g[` = `g[[` = function(x, n) .Call(Cgnthvalue, x, as.integer(n)) # n is of length=1 here.
ghead = function(x, n) .Call(Cghead, x, as.integer(n)) # n is not used at the moment
gtail = function(x, n) .Call(Cgtail, x, as.integer(n)) # n is not used at the moment
gfirst = function(x) .Call(Cgfirst, x)
glast = function(x) .Call(Cglast, x)
gsum = function(x, na.rm=FALSE) .Call(Cgsum, x, na.rm, TRUE)  # warnOverflow=TRUE, #986
gmean = function(x, na.rm=FALSE) .Call(Cgmean, x, na.rm)
gprod = function(x, na.rm=FALSE) .Call(Cgprod, x, na.rm)
gmedian = function(x, na.rm=FALSE) .Call(Cgmedian, x, na.rm)
gmin = function(x, na.rm=FALSE) .Call(Cgmin, x, na.rm)
gmax = function(x, na.rm=FALSE) .Call(Cgmax, x, na.rm)
gvar = function(x, na.rm=FALSE) .Call(Cgvar, x, na.rm)
gsd = function(x, na.rm=FALSE) .Call(Cgsd, x, na.rm)
gforce = function(env, jsub, o, f, l, rows) .Call(Cgforce, env, jsub, o, f, l, rows)

isReallyReal = function(x) {
  .Call(CisReallyReal, x)
}

.prepareFastSubset = function(isub, x, enclos, notjoin, verbose = FALSE){
  ## helper that decides, whether a fast binary search can be performed, if i is a call
  ## For details on the supported queries, see \code{\link{datatable-optimize}}
  ## Additional restrictions are imposed if x is .SD, or if options indicate that no optimization
  ## is to be performed
  #' @param isub the substituted i
  #' @param x the data.table
  #' @param enclos The environment where to evaluate when RHS is not a column of x
  #' @param notjoin boolean that is set before, indicating whether i started with '!'.
  #' @param verbose TRUE for detailed output
  #' @return If i is not fast subsettable, NULL. Else, a list with entries:
  #'        out$i: a data.table that will be used as i with proper column names and key.
  #'        out$on: the correct 'on' statement that will be used for x[i, on =...]
  #'        out$notjoin Bool. In some cases, notjoin is updated within the function.
  #'        ATTENTION: If nothing else helps, an auto-index is created on x unless options prevent this.
  if(getOption("datatable.optimize") < 3L) return(NULL) ## at least level three optimization required.
  if (!is.call(isub)) return(NULL)
  if (.Call(C_islocked, x)) return(NULL)  # fix for #958, don't create auto index on '.SD'.
  ## a list of all possible operators with their translations into the 'on' clause
  validOps = list(op = c("==", "%in%", "%chin%"),
                   on = c("==", "==",   "=="))

  ## Determine, whether the nature of isub in general supports fast binary search
  remainingIsub = isub
  i = list()
  on = character(0L)
  nonEqui = FALSE
  while(length(remainingIsub)){
    if(is.call(remainingIsub)){
      if (length(remainingIsub[[1L]]) != 1L) return(NULL) ## only single symbol, either '&' or one of validOps allowed.
      if (remainingIsub[[1L]] != "&"){ ## only a single expression present or a different connection.
        stub = remainingIsub
        remainingIsub = NULL ## there is no remainder to be evaluated after stub.
      } else {
        ## multiple expressions with & connection.
        if (notjoin) return(NULL) ## expressions of type DT[!(a==1 & b==2)] currently not supported
        stub = remainingIsub[[3L]] ## the single column expression like col == 4
        remainingIsub = remainingIsub[[2L]] ## the potentially longer expression with potential additional '&'
      }
    } else { ## single symbol present
      stub = remainingIsub
      remainingIsub = NULL
    }
    ## check the stub if it is fastSubsettable
    if(is.symbol(stub)){
      ## something like DT[x & y]. If x and y are logical columns, we can optimize.
      col = as.character(stub)
      if(!col %chin% names(x)) return(NULL)
      if(!is.logical(x[[col]])) return(NULL)
      ## redirect to normal DT[x == TRUE]
      stub = call("==", as.symbol(col), TRUE)
    }
    if (length(stub[[1L]]) != 1) return(NULL) # nocov Whatever it is, definitely not one of the valid operators
    operator = as.character(stub[[1L]])
    if (!operator %chin% validOps$op) return(NULL) ## operator not supported
    if (!is.name(stub[[2L]])) return(NULL)
    col = as.character(stub[[2L]])
    if (!col %chin% names(x)) return(NULL) ## any non-column name prevents fast subsetting
    if(col %chin% names(i)) return(NULL) ## repeated appearance of the same column not supported (e.g. DT[x < 3 & x < 5])
    ## now check the RHS of stub
    RHS = eval(stub[[3L]], x, enclos)
    if (is.list(RHS)) RHS = as.character(RHS)  # fix for #961
    if (length(RHS) != 1L && !operator %chin% c("%in%", "%chin%")){
      if (length(RHS) != nrow(x)) stop("RHS of ", operator, " is length ",length(RHS)," which is not 1 or nrow (",nrow(x),"). For robustness, no recycling is allowed (other than of length 1 RHS). Consider %in% instead.")
      return(NULL) # DT[colA == colB] regular element-wise vector scan
    }
    if ( mode(x[[col]]) != mode(RHS) ||                # mode() so that doubleLHS/integerRHS and integerLHS/doubleRHS!isReallyReal are optimized (both sides mode 'numeric')
         is.factor(x[[col]])+is.factor(RHS) == 1L ||   # but factor is also mode 'numeric' so treat that separately
         is.integer(x[[col]]) && isReallyReal(RHS) ) { # and if RHS contains fractions then don't optimize that as bmerge truncates the fractions to match to the target integer type
      # re-direct non-matching type cases to base R, as data.table's binary
      # search based join is strict in types. #957, #961 and #1361
      # the mode() checks also deals with NULL since mode(NULL)=="NULL" and causes this return, as one CRAN package (eplusr 0.9.1) relies on
      return(NULL)
    }
    if (!operator %chin% c("%in%", "%chin%")) {
      # additional requirements for notjoin and NA values. Behaviour is different for %in%, %chin% compared to other operators
      # RHS is of length=1 or n
      if (any_na(as_list(RHS))) {
        ## dt[x == NA] or dt[x <= NA] will always return empty
        notjoin = FALSE
        RHS = RHS[0L]
      } else if (notjoin) {
        ## dt[!x == 3] must not return rows where x is NA
        RHS = c(RHS, if (is.double(RHS) && is.double(x[[col]])) c(NA, NaN) else NA)
      }
    }
    ## if it passed until here, fast subset can be done for this stub
    i = c(i, setNames(list(RHS), col))
    on = c(on, setNames(paste0(col, validOps$on[validOps$op == operator], col), col))
    ## loop continues with remainingIsub
  }
  if (length(i) == 0L) stop("Internal error in .isFastSubsettable. Please report to data.table developers") # nocov
  ## convert i to data.table with all combinations in rows.
  if(length(i) > 1L && prod(vapply_1i(i, length)) > 1e4){
    ## CJ would result in more than 1e4 rows. This would be inefficient, especially memory-wise #2635
    if (verbose) {cat("Subsetting optimization disabled because the cross-product of RHS values exceeds 1e4, causing memory problems.\n");flush.console()}
    return(NULL)
  }
  ## Care is needed with names as we construct i
  ## with 'CJ' and 'do.call' and this would cause problems if colNames were 'sorted' or 'unique'
  ## as these two would be interpreted as args for CJ
  colNames = names(i)
  names(i) = NULL
  i$sorted = FALSE
  i$unique = TRUE
  i = do.call(CJ, i)
  setnames(i, colNames)
  idx = NULL
  if(is.null(idx)){
      ## check whether key fits the columns in i.
      ## order of key columns makes no difference, as long as they are all upfront in the key, I believe.
      if (all(names(i) %chin% head(key(x), length(i)))){
          if (verbose) {cat("Optimized subsetting with key '", paste0( head(key(x), length(i)), collapse = ", "),"'\n",sep="");flush.console()}
          idx = integer(0L) ## integer(0L) not NULL! Indicates that x is ordered correctly.
          idxCols = head(key(x), length(i)) ## in correct order!
      }
  }
  if (is.null(idx)){
    if (!getOption("datatable.use.index")) return(NULL) # #1422
    ## check whether an existing index can be used
    ## An index can be used if it corresponds exactly to the columns in i (similar to the key above)
    candidates = indices(x, vectors = TRUE)
    idx = NULL
    for (cand in candidates){
      if (all(names(i) %chin% cand) && length(cand) == length(i)){
        idx = attr(attr(x, "index", exact=TRUE), paste0("__", cand, collapse = ""), exact = TRUE)
        idxCols = cand
        break
      }
    }
    if (!is.null(idx)){
      if (verbose) {cat("Optimized subsetting with index '", paste0( idxCols, collapse = "__"),"'\n",sep="");flush.console()}
    }
  }
  if (is.null(idx)){
    ## if nothing else helped, auto create a new index that can be used
    if (!getOption("datatable.auto.index")) return(NULL)
    if (verbose) {cat("Creating new index '", paste0(names(i), collapse = "__"),"'\n",sep="");flush.console()}
    if (verbose) {last.started.at=proc.time();cat("Creating index", paste0(names(i), collapse = "__"), "done in ... ");flush.console()}
    setindexv(x, names(i))
    if (verbose) {cat(timetaken(last.started.at),"\n");flush.console()}
    if (verbose) {cat("Optimized subsetting with index '", paste0(names(i), collapse = "__"),"'\n",sep="");flush.console()}
    idx = attr(attr(x, "index", exact=TRUE), paste0("__", names(i), collapse = ""), exact=TRUE)
    idxCols = names(i)
  }
  if(!is.null(idxCols)){
    setkeyv(i, idxCols)
    on = on[idxCols] ## make sure 'on' is in the correct order. Otherwise the logic won't recognise that a key / index already exists.
  }
  return(list(i  = i,
              on = on,
              notjoin = notjoin
              )
         )
}


.parse_on = function(onsub, isnull_inames) {
  ## helper that takes the 'on' string(s) and extracts comparison operators and column names from it.
  #' @param onsub the substituted on
  #' @param isnull_inames bool; TRUE if i has no names.
  #' @return List with two entries:
  #'         'on' : character vector providing the column names for the join.
  #'                Names correspond to columns in x, entries correspond to columns in i
  #'         'ops': integer vector. Gives the indices of the operators that connect the columns in x and i.
  ops = c("==", "<=", "<", ">=", ">", "!=")
  pat = paste0("(", ops, ")", collapse="|")
  if (onsub %iscall% 'eval') {
    onsub = eval(onsub[[2L]], parent.frame(2L), parent.frame(2L))
  }
  if (onsub %iscall% c('list', '.')) {
    spat = paste0("[ ]+(", pat, ")[ ]+")
    onsub = lapply(as.list(onsub)[-1L], function(x) gsub(spat, "\\1", deparse(x, width.cutoff=500L)))
    onsub = as.call(c(quote(c), onsub))
  }
  on = eval(onsub, parent.frame(2L), parent.frame(2L))
  if (length(on) == 0L || !is.character(on))
    stop("'on' argument should be a named atomic vector of column names indicating which columns in 'i' should be joined with which columns in 'x'.")
  ## extract the operators and potential variable names from 'on'.
  ## split at backticks to take care about variable names like `col1<=`.
  pieces = strsplit(on, "(?=[`])", perl = TRUE)
  xCols  = character(length(on))
  ## if 'on' is named, the names are the xCols for sure
  if(!is.null(names(on))){
    xCols = names(on)
  }
  iCols     = character(length(on))
  operators = character(length(on))
  ## loop over the elements and extract operators and column names.
  for(i in seq_along(pieces)){
    thisCols      = character(0L)
    thisOperators = character(0L)
    j = 1L
    while(j <= length(pieces[[i]])){
      if(pieces[[i]][j] == "`"){
        ## start of a variable name with backtick.
        thisCols = c(thisCols, pieces[[i]][j+1L])
        j = j+3L # +1 is the column name, +2 is delimiting "`", +3 is next relevant entry.`
      } else {
        ## no backtick
        ## search for operators
        thisOperators = c(thisOperators,
                           unlist(regmatches(pieces[[i]][j], gregexpr(pat, pieces[[i]][j])),
                                  use.names = FALSE))
        ## search for column names
        thisCols = c(thisCols, trimws(strsplit(pieces[[i]][j], pat)[[1L]]))
        ## there can be empty string column names because of trimws, remove them
        thisCols = thisCols[thisCols != ""]
        j = j+1L
      }
    }
    if (length(thisOperators) == 0L) {
      ## if no operator is given, it must be ==
      operators[i] = "=="
    } else if (length(thisOperators) == 1L) {
      operators[i] = thisOperators
    } else {
      ## multiple operators found in one 'on' part. Something is wrong.
      stop("Found more than one operator in one 'on' statement: ", on[i], ". Please specify a single operator.")
    }
    if (length(thisCols) == 2L){
      ## two column names found, first is xCol, second is iCol for sure
      xCols[i] = thisCols[1L]
      iCols[i] = thisCols[2L]
    } else if (length(thisCols) == 1L){
      ## a single column name found. Can mean different things
      if(xCols[i] != ""){
        ## xCol is given by names(on). thisCols must be iCol
        iCols[i] = thisCols[1L]
      } else if (isnull_inames){
        ## i has no names. It will be given the names V1, V2, ... automatically.
        ## The single column name is the x column. It will match to the ith column in i.
        xCols[i] = thisCols[1L]
        iCols[i] = paste0("V", i)
      } else {
        ## i has names and one single column name is given by on.
        ## This means that xCol and iCol have the same name.
        xCols[i] = thisCols[1L]
        iCols[i] = thisCols[1L]
      }
    } else if (length(thisCols) == 0L){
      stop("'on' contains no column name: ", on[i], ". Each 'on' clause must contain one or two column names.")
    } else {
      stop("'on' contains more than 2 column names: ", on[i], ". Each 'on' clause must contain one or two column names.")
    }
  }
  idx_op = match(operators, ops, nomatch=0L)
  if (any(idx_op %in% c(0L, 6L)))
    stop("Invalid operators ", paste(operators[idx_op %in% c(0L, 6L)], collapse=","), ". Only allowed operators are ", paste(ops[1:5], collapse=""), ".")
  ## the final on will contain the xCol as name, the iCol as value
  on = iCols
  names(on) = xCols
  return(list(on = on, ops = idx_op))
}
