deconstruct_and_eval = function(expr, envir = parent.frame(), enclos = parent.frame()) {
    if (!mode(expr) %in% c("call", "expression", "(")) return(expr)

    if (length(expr) == 1L) {
        if (is.expression(expr)) return (deconstruct_and_eval(expr[[1L]]))
        else if (is.call(expr[[1L]])) return (list(deconstruct_and_eval(expr[[1L]])))
        else return(expr)
    }
    
    # Fix for #2496. the `{` in `DT[, {var := bla}, by=x]` is caught and removed from `j`.
    if (expr[[1L]] == "{" & is.call(expr[[2L]])) {
        if (identical(expr[[2L]][[1L]], quote(`:=`))) {
            warning('Caught and removed `{` wrapped around := in j. := and `:=`(...) are defined for use in j, once only and in particular ways. See help(":=").')
            return(deconstruct_and_eval(expr[[2L]], envir, enclos))
        }
    }

    # don't evaluate eval's if the environment is specified
    if (expr[[1L]] == quote(eval) && length(expr) < 3L) {
        return(deconstruct_and_eval(eval(expr[[2L]], envir, enclos), envir, enclos))
    }

    lapply(expr, function(m) {
        if (is.call(m)) {
            if (m[[1L]] == quote(eval)) 
                if (is.call(m[[2L]]) && m[[2L]][[1L]] == quote(parse)) eval(m, envir, enclos) else eval(m[[2L]], envir, enclos)
            else deconstruct_and_eval(m, envir, enclos)
        } else {
            m
        }
    })
}

construct = function(l) {
    if (length(l) == 0L) return(NULL)
    if (is.name(l)) return(l) # fix for error in cases as reported in Bug #5007: DT[, (cols) := lapply(.SD, function(x) MyValueIsTen), by=ID]
                              # construct(l[[3L]] would give an error when l[[3L]] is MyValueIsTen if not for this line)
    if (length(l) == 1L) {
        if (length(l[[1L]]) == 1L & !is.call(l)) return(l[[1L]]) # so that DT[, test()] does not return just the function definition
        else return(as.call(list(construct(l[[1L]]))))
    }

    if (identical(l[[1L]], quote(`function`))) return(as.call(list(l[[1L]], l[[2L]], construct(l[[3L]]))))

    if (!is.list(l)) return(l)

    as.call(setNames(lapply(l, function(m) {
        if (length(m) == 1L) m
        else construct(m)
    }), names(l)))
}

dim.data.table <- function(x) {
    if (length(x)) c(length(x[[1L]]), length(x))
    else c(0L,0L)
    # TO DO: consider placing "dim" as an attibute updated on inserts. Saves this 'if'.
}

.global = new.env()  # thanks to: http://stackoverflow.com/a/12605694/403310
setPackageName("data.table",.global)
.global$print = TRUE
.global$depthtrigger =
    if (exists(".global",.GlobalEnv)) {
        9L # this is development
    } else {
        3L # normal value when package is loaded in 2.14+ (where functions are compiled in namespace). data.table depends on R >= 2.14.0
    }

.SD = .N = .I = .GRP = .BY = NULL
# These are exported to prevent NOTEs from R CMD check, and checkUsage via compiler.
# But also exporting them makes it clear (to users and other packages) that data.table uses these as symbols.
# And NULL makes it clear (to the R's mask check on loading) that they're variables not functions.
# utils::globalVariables(c(".SD",".N")) was tried as well, but exporting seems better.
# So even though, .BY doesn't appear in this file, it should still be NULL here and exported because it's
# defined in SDenv and can be used by users.

print.data.table = function(x,
    topn=getOption("datatable.print.topn"),   # (5) print the top topn and bottom topn rows with '---' inbetween
    nrows=getOption("datatable.print.nrows"), # (100) under this the whole (small) table is printed, unless topn is provided
    row.names = TRUE, ...)
{
    if (!.global$print) {
        #  := in [.data.table sets print=FALSE, when appropriate, to suppress := autoprinting at the console
        .global$print = TRUE
        return(invisible())
    }
    if (!is.numeric(nrows)) nrows = 100L
    if (!is.infinite(nrows)) nrows = as.integer(nrows)
    if (nrows <= 0L) return(invisible())   # ability to turn off printing
    if (!is.numeric(topn)) topn = 5L
    topnmiss = missing(topn)
    topn = max(as.integer(topn),1L)
    if (nrow(x) == 0L) {
        if (length(x)==0L)
           cat("Null data.table (0 rows and 0 cols)\n")  # See FAQ 2.5 and NEWS item in v1.8.9
        else
           cat("Empty data.table (0 rows) of ",length(x)," col",if(length(x)>1L)"s",": ",paste(head(names(x),6),collapse=","),if(ncol(x)>6)"...","\n",sep="")
        return()
    }
    if (topn*2<nrow(x) && (nrow(x)>nrows || !topnmiss)) {
        toprint = rbind(head(x, topn), tail(x, topn))
        rn = c(seq_len(topn), seq.int(to=nrow(x), length.out=topn))
        printdots = TRUE
    } else {
        toprint = x
        rn = seq_len(nrow(x))
        printdots = FALSE
    }
    toprint=format.data.table(toprint, ...)
    # FR #5020 - add row.names = logical argument to print.data.table
    if (isTRUE(row.names)) rownames(toprint)=paste(format(rn,right=TRUE),":",sep="") else rownames(toprint)=rep.int("", nrow(x))
    if (is.null(names(x))) colnames(toprint)=rep("NA", ncol(toprint)) # fixes bug #4934
    if (printdots) {
        toprint = rbind(head(toprint,topn),"---"="",tail(toprint,topn))
        rownames(toprint) = format(rownames(toprint),justify="right")
        print(toprint,right=TRUE,quote=FALSE)
        return(invisible())
    }
    if (nrow(toprint)>20L)
        # repeat colnames at the bottom if over 20 rows so you don't have to scroll up to see them
        toprint=rbind(toprint,matrix(colnames(toprint),nrow=1)) # fixes bug #4934
    print(toprint,right=TRUE,quote=FALSE)
    invisible()
}

# FR #2591 - format.data.table issue with columns of class "formula"
is.formula <- function(x) class(x) == "formula"

format.data.table <- function (x, ..., justify="none") {
    format.item = function(x) {
        if (is.atomic(x) || is.formula(x)) # FR #2591 - format.data.table issue with columns of class "formula"
            paste(c(head(x,6),if(length(x)>6)""),collapse=",")
        else
            paste("<",class(x)[1L],">",sep="")
    }
    do.call("cbind",lapply(x,function(col,...){
        if (!is.null(dim(col))) stop("Invalid column: it has dimensions. Can't format it. If it's the result of data.table(table()), use as.data.table(table()) instead.")
        if (is.list(col))
            col = sapply(col,format.item)
        format(col, justify=justify, ...)
    },...))
}

is.data.table = function(x) inherits(x, "data.table")
is.ff = function(x) inherits(x, "ff")  # define this in data.table so that we don't have to require(ff), but if user is using ff we'd like it to work

#NCOL = function(x) {
#    # copied from base, but additionally covers data.table via is.list()
#    # because NCOL in base explicity tests using is.data.frame()
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
    alloc.col(ans)
}

data.table = function(..., keep.rownames=FALSE, check.names=FALSE, key=NULL)
{
    # NOTE: It may be faster in some circumstances to create a data.table by creating a list l first, and then setattr(l,"class",c("data.table","data.frame")) at the expense of checking.
    # TO DO: rewrite data.table(), one of the oldest functions here. Many people use data.table() to convert data.frame rather than
    # as.data.table which is faster; speed could be better.  Revisit how many copies are taken in for example data.table(DT1,DT2) which
    # cbind directs to.  And the nested loops for recycling lend themselves to being C level.
    
    x <- list(...)   # doesn't copy named inputs as from R >= 3.1 (a very welcome change)
    if (!.R.listCopiesNamed) .Call(CcopyNamedInList,x)   # to maintain the old behaviour going forwards, for now. See test 548.2.
    # **TO DO** Something strange with NAMED on components of `...`. To investigate. Or just port data.table() to C. This is why
    # it's switched, because extra copies would be introduced in R <= 3.1, iiuc.
    
    # fix for #5377 - data.table(null list, data.frame and data.table) should return null data.table. Simple fix: check all scenarios here at the top.
    if (identical(x, list(NULL)) || identical(x, list(list())) || 
           identical(x, list(data.frame(NULL))) || identical(x, list(data.table(NULL)))) return( null.data.table() )
    tt <- as.list(substitute(list(...)))[-1L]  # Intention here is that data.table(X,Y) will automatically put X and Y as the column names.  For longer expressions, name the arguments to data.table(). But in a call to [.data.table, wrap in list() e.g. DT[,list(a=mean(v),b=foobarzoo(zang))] will get the col names
    vnames = names(tt)
    if (is.null(vnames)) vnames = rep.int("",length(x))
    vnames[is.na(vnames)] = ""
    novname = vnames==""
    if (any(!novname)) {
        if (any(vnames[!novname] == ".SD")) stop("A column may not be called .SD. That has special meaning.")
    }
    for (i in which(novname)) {
        # if (ncol(as.data.table(x[[i]])) <= 1) { # cbind call in test 230 fails if I write ncol(as.data.table(eval(tt[[i]], parent.frame()))) <= 1, no idea why... (keep this for later even though all tests pass with ncol(.).. because base uses as.data.frame(.))
        if (is.null(ncol(x[[i]]))) { 
            if ((tmp <- deparse(tt[[i]])[1]) == make.names(tmp))
                vnames[i] <- tmp
        }
    }
    tt = vnames==""
    if (any(tt)) vnames[tt] = paste("V", which(tt), sep = "")
    # so now finally we have good column names. We also will use novname later to know which were explicitly supplied in the call.
    n <- length(x)
    if (n < 1L)
        return( null.data.table() )
    if (length(vnames) != n) stop("logical error in vnames")
    vnames <- as.list.default(vnames)
    nrows = integer(n)          # vector of lengths of each column. may not be equal if silent repetition is required.
    numcols = integer(n)         # the ncols of each of the inputs (e.g. if inputs contain matrix or data.table)
    for (i in seq_len(n)) {
        xi = x[[i]]
        if (is.null(xi)) stop("column or argument ",i," is NULL")
        if ("POSIXlt" %chin% class(xi)) {
            warning("POSIXlt column type detected and converted to POSIXct. We do not recommend use of POSIXlt at all because it uses 40 bytes to store one date.")
            x[[i]] = as.POSIXct(xi)
        }
        if (is.matrix(xi) || is.data.frame(xi)) {  # including data.table (a data.frame, too)
            xi = as.data.table(xi, keep.rownames=keep.rownames)       # TO DO: allow a matrix to be a column of a data.table. This could allow a key'd lookup to a matrix, not just by a single rowname vector, but by a combination of several columns. A matrix column could be stored either by row or by column contiguous in memory.
            x[[i]] = xi
            numcols[i] = length(xi)
        }
        nrows[i] <- NROW(xi)    # for a vector (including list() columns) returns the length
        if (numcols[i]>0L) {
            namesi <- names(xi)  # works for both data.frame's, matrices and data.tables's
            if (length(namesi)==0L) namesi = rep.int("",ncol(xi))
            namesi[is.na(namesi)] = ""
            tt = namesi==""
            if (any(tt)) namesi[tt] = paste("V", which(tt), sep = "")
            if (novname[i]) vnames[[i]] = namesi
            else vnames[[i]] = paste(vnames[[i]], namesi, sep=".")
        }
    }
    nr <- max(nrows)
    ckey = NULL
    recycledkey = FALSE
    for (i in seq_len(n)) {
        xi = x[[i]]
        if (is.data.table(xi) && haskey(xi)) {
            if (nrows[i]<nr) recycledkey = TRUE
            else ckey = c(ckey, key(xi))
        }
    }
    for (i in which(nrows < nr)) {
        # TO DO ... recycle in C, but not high priority as large data already regular from database or file
        xi <- x[[i]]
        if (identical(xi,list())) {
            x[[i]] = vector("list", nr)
            next
        }
        if (nrows[i]==0L) stop("Item ",i," has no length. Provide at least one item (such as NA, NA_integer_ etc) to be repeated to match the ",nr," rows in the longest column. Or, all columns can be 0 length, for insert()ing rows into.")
        # Implementing FR #4813 - recycle with warning when nr %% nrows[i] != 0L
        if (nr%%nrows[i] != 0L) warning("Item ", i, " is of size ", nrows[i], " but maximum size is ", nr, " (recycled leaving remainder of ", nr%%nrows[i], " items)")
        # if (nr%%nrows[i] == 0L) {
            if (is.data.frame(xi)) {   # including data.table
                ..i = rep(seq_len(nrow(xi)), length.out = nr)
                x[[i]] = xi[..i,,drop=FALSE]
                next
            }
            if (is.atomic(xi) || is.list(xi)) {
                # TO DO: surely use set() here, or avoid the coercion
                x[[i]] = rep(xi, length.out = nr)
                next
            }
            stop("problem recycling column ",i,", try a simpler type")
        # }
        stop("argument ",i," (nrow ",nrows[i],") cannot be recycled without remainder to match longest nrow (",nr,")")
    }
    if (any(numcols>0L)) {
        value = vector("list",sum(pmax(numcols,1L)))
        k = 1L
        for(i in seq_len(n)) {
            if (is.list(x[[i]]) && !is.ff(x[[i]])) {
                for(j in seq_len(length(x[[i]]))) {
                    value[[k]] = x[[i]][[j]]
                    k=k+1L
                }
            } else {
                value[[k]] = x[[i]]
                k=k+1L
            }
        }
    } else {
        value = x
    }
    vnames <- unlist(vnames)
    if (check.names)   # default FALSE
        vnames <- make.names(vnames, unique = TRUE)
    setattr(value,"names",vnames)
    setattr(value,"row.names",.set_row_names(nr))
    setattr(value,"class",c("data.table","data.frame"))
    if (!is.null(key)) {
      if (!is.character(key)) stop("key argument of data.table() must be character")
      if (length(key)==1L) {
          key = strsplit(key,split=",")[[1L]]
          # eg key="A,B"; a syntax only useful in key argument to data.table(), really.
      }
      setkeyv(value,key)
    } else {
       # retain key of cbind(DT1, DT2, DT3) where DT2 is keyed but not DT1. cbind calls data.table().
       # If DT inputs with keys have been recycled then can't retain key
       if (length(ckey)
           && !recycledkey
           && !any(duplicated(ckey))
           && all(ckey %in% names(value))
           && !any(duplicated(names(value)[names(value) %in% ckey])))
           setattr(value, "sorted", ckey)
    }
    alloc.col(value)  # returns a NAMED==0 object, unlike data.frame()
}

.massagei = function(x) {
    if (is.call(x) && as.character(x[[1L]]) %chin% c("J","."))
        x[[1L]] = quote(list)
    x
}

"[.data.table" = function (x, i, j, by, keyby, with=TRUE, nomatch=getOption("datatable.nomatch"), mult="all", roll=FALSE, rollends=if (roll=="nearest") c(TRUE,TRUE) else if (roll>=0) c(FALSE,TRUE) else c(TRUE,FALSE), which=FALSE, .SDcols, verbose=getOption("datatable.verbose"), allow.cartesian=getOption("datatable.allow.cartesian"), drop=NULL, rolltolast=FALSE)
{
    # ..selfcount <<- ..selfcount+1  # in dev, we check no self calls, each of which doubles overhead, or could
    # test explicitly if the caller is [.data.table (even stronger test. TO DO.)
    # the drop=NULL is to sink drop argument when dispatching to [.data.frame; using '...' stops test 147
    if (!cedta()) {
        # Fix for #5070 (to do)
        Nargs = nargs() - (!missing(drop))
        ans = if (Nargs<3L) `[.data.frame`(x,i)  # drop ignored anyway by DF[i]
              else if (missing(drop)) `[.data.frame`(x,i,j)
              else `[.data.frame`(x,i,j,drop)
        # added is.data.table(ans) check to fix bug #5069
        if (!missing(i) & is.data.table(ans)) setkey(ans,NULL)  # See test 304
        return(ans)
    }
    if (!mult %chin% c("first","last","all")) stop("mult argument can only be 'first','last' or 'all'")
    if (length(roll)!=1L || is.na(roll)) stop("roll must be a single TRUE, FALSE, positive/negative integer/double including +Inf and -Inf or 'nearest'")
    if (is.character(roll)) {
        if (roll!="nearest") stop("roll is '",roll,"' (type character). Only valid character value is 'nearest'.")
    } else {
        roll = if (isTRUE(roll)) +Inf else as.double(roll)
    }
    force(rollends)
    if (!missing(rolltolast)) {
        warning("'rolltolast' has been marked 'deprecated' in ?data.table since v1.8.8 on CRAN 3 Mar 2013, see NEWS. Please change to the more flexible 'rollends' instead. 'rolltolast' will be removed in the next version.")
        if (isTRUE(rolltolast)) { roll=+Inf; rollends=c(FALSE,FALSE) }
    }
    if (!is.logical(rollends)) stop("rollends must be a logical vector")
    if (length(rollends)>2) stop("rollends must be length 1 or 2")
    if (length(rollends)==1) rollends=rep.int(rollends,2L)
    # TO DO (document/faq/example). Removed for now ... if ((roll || rolltolast) && missing(mult)) mult="last" # for when there is exact match to mult. This does not control cases where the roll is mult, that is always the last one.
    missingnomatch = missing(nomatch)
    if (!is.na(nomatch) && nomatch!=0L) stop("nomatch must either be NA or 0, or (ideally) NA_integer_ or 0L")
    nomatch = as.integer(nomatch)
    if (!is.logical(which) || length(which)>1) stop("'which' must be a logical vector length 1. Either FALSE, TRUE or NA.")
    if ((isTRUE(which)||is.na(which)) && !missing(j)) stop("'which' is ",which," (meaning return row numbers) but 'j' is also supplied. Either you need row numbers or the result of j, but only one type of result can be returned.")
    if (!is.na(nomatch) && is.na(which)) stop("which=NA with nomatch=0 would always return an empty vector. Please change or remove either which or nomatch.")
    if (missing(i) && missing(j)) {
        # ...[] == oops at console, forgot print(...)
        # or some kind of dynamic construction that has edge case of no contents inside [...]
        .global$print=TRUE
        return(x)
    }
    if (!with && missing(j)) stop("j must be provided when with=FALSE")
    bysub=NULL
    if (!missing(by)) bysub=substitute(by)
    if (!missing(keyby)) {
        if (!missing(by)) stop("Provide either 'by' or 'keyby' but not both")
        by=bysub=substitute(keyby)
        # Assign to 'by' so that by is no longer missing and we can proceed as if there were one by
    }
    byjoin = FALSE
    if (!missing(by)) {
        if (missing(j)) stop("'by' or 'keyby' is supplied but not j")
        byjoin = is.symbol(bysub) && bysub==".EACHI"
    }
    irows = NULL  # Meaning all rows. We avoid creating 1:nrow(x) for efficiency.
    notjoin = FALSE
    rightcols = leftcols = integer(0)

    if (!missing(i)) {
        isub = substitute(i)
        # Fixes 4994: a case where quoted expression with a "!", ex: expr = quote(!dt1); dt[eval(expr)] requires 
        # the "eval" to be checked before `as.name("!")`. Therefore interchanged.
        if (is.call(isub) && isub[[1L]]=="eval") {  # TO DO: or ..()
            isub = eval(.massagei(isub[[2L]]), parent.frame(), parent.frame())
            if (is.expression(isub)) isub=isub[[1L]]
        }
        if (is.call(isub) && isub[[1L]] == as.name("!")) {
            notjoin = TRUE
            if (!missingnomatch) stop("not-join '!' prefix is present on i but nomatch is provided. Please remove nomatch.");
            nomatch = 0L
            isub = isub[[2L]]
        }
        if (is.call(isub) && isub[[1L]] == as.name("order") && getOption("datatable.optimize") >= 1) { # optimize here so that we can switch it off if needed
            if (verbose) cat("order optimisation is on, i changed from 'order(...)' to 'forder(DT, ...)'.\n")
            isub = as.list(isub)
            isub = as.call(c(list(as.name("forder"), substitute(x)), isub[-1L]))
        }
        if (is.null(isub)) return( null.data.table() )
        if (is.call(isub) && isub[[1L]] == as.name("forder")) {
            order_env = new.env(parent=parent.frame())            # until 'forder' is exported
            assign("forder", forder, order_env)
            i = eval(isub, order_env, parent.frame())             # for optimisation of 'order' to 'forder'
            # that forder returns integer(0) is taken care of internally within forder
        } else if (!is.name(isub)) i = eval(.massagei(isub), x, parent.frame())
        else i = eval(isub, parent.frame(), parent.frame())
        if (is.matrix(i)) stop("i is invalid type (matrix). Perhaps in future a 2 column matrix could return a list of elements of DT (in the spirit of A[B] in FAQ 2.14). Please let datatable-help know if you'd like this, or add your comments to FR #1611.")
        if (is.logical(i)) {
            if (notjoin) {
                notjoin = FALSE
                i = !i
            }
            if (identical(i,NA)) i = NA_integer_  # see DT[NA] thread re recycling of NA logical
        }
        if (is.null(i)) return( null.data.table() )
        if (is.character(i)) i = data.table(V1=i)   # for user convenience; e.g. DT["foo"] without needing DT[.("foo")]
        else if (identical(class(i),"list") && length(i)==1L && is.data.frame(i[[1L]])) i = as.data.table(i[[1L]])
        else if (identical(class(i),"data.frame")) i = as.data.table(i)
        else if (identical(class(i),"list")) i = as.data.table(i)
        if (is.data.table(i)) {
            if (!haskey(x)) stop("When i is a data.table (or character vector), x must be keyed (i.e. sorted, and, marked as sorted) so data.table knows which columns to join to and take advantage of x being sorted. Call setkey(x,...) first, see ?setkey.")
            rightcols = chmatch(key(x),names(x))   # NAs here (i.e. invalid data.table) checked in bmerge()
            leftcols = if (haskey(i))
                chmatch(head(key(i),length(rightcols)),names(i))
            else
                seq_len(min(length(i),length(rightcols)))
            rightcols = head(rightcols,length(leftcols))
            origi = i       # Only needed for factor to factor joins, to recover the original levels
                            # Otherwise, types of i join columns are alyways promoted to match x's
                            # types (with warning or verbose)
            i = shallow(i)  # careful to only plonk syntax on i from now on (otherwise i would change)
                            # TO DO: enforce via .internal.shallow attribute and expose shallow() to users
                            # This is why shallow() is very importantly internal only, currently.
            resetifactor = NULL  # Keep track of any factor to factor join cols (only time we keep orig)
            for (a in seq_along(leftcols)) {
                # This loop is simply to support joining factor columns
                # Note that if i is keyed, if this coerces, i's key gets dropped and the key may not be retained
                lc = leftcols[a]   # i   # TO DO: rename left and right to i and x
                rc = rightcols[a]  # x
                icnam = names(i)[lc]
                xcnam = names(x)[rc]
                if (is.character(x[[rc]])) {
                    if (is.character(i[[lc]])) next
                    if (!is.factor(i[[lc]]))
                        stop("x.'",xcnam,"' is a character column being joined to i.'",icnam,"' which is type '",typeof(i[[lc]]),"'. Character columns must join to factor or character columns.")
                    if (verbose) cat("Coercing factor column i.'",icnam,"' to character to match type of x.'",xcnam,"'.\n",sep="")
                    set(i,j=lc,value=as.character(i[[lc]]))
                    # no longer copies all of i, thanks to shallow() and :=/set
                    next
                }
                if (is.factor(x[[rc]])) {
                    if (is.character(i[[lc]])) {
                        if (verbose) cat("Coercing character column i.'",icnam,"' to factor to match type of x.'",xcnam,"'. If possible please change x.'",xcnam,"' to character. Character columns are now preferred in joins.\n",sep="")
                        set(i,j=lc,value=factor(i[[lc]]))
                    } else {
                        if (!is.factor(i[[lc]]))
                            stop("x.'",xcnam,"' is a factor column being joined to i.'",icnam,"' which is type '",typeof(i[[lc]]),"'. Factor columns must join to factor or character columns.")
                        resetifactor = c(resetifactor,lc)
                        # Retain original levels of i's factor columns in factor to factor joins (important when NAs,
                        # see tests 687 and 688).
                    }
                    if (roll!=0.0 && a==length(leftcols)) stop("Attempting roll join on factor column x.",names(x)[rc],". Only integer, double or character colums may be roll joined.")   # because the chmatch on next line returns NA for missing chars in x (rather than some integer greater than existing). Note roll!=0.0 is ok in this 0 special floating point case e.g. as.double(FALSE)==0.0 is ok, and "nearest"!=0.0 is also true.
                    newfactor = chmatch(levels(i[[lc]]), levels(x[[rc]]), nomatch=NA_integer_)[i[[lc]]]
                    levels(newfactor) = levels(x[[rc]])
                    class(newfactor) = "factor"
                    set(i,j=lc,value=newfactor)
                    # NAs can be produced by this level match, in which case the C code (it knows integer value NA)
                    # can skip over the lookup. It's therefore important we pass NA rather than 0 to the C code.
                }
                if (is.integer(x[[rc]]) && is.double(i[[lc]])) {
                    # TO DO: add warning if reallyreal about loss of precision
                    # or could coerce in binary search on the fly, at cost
                    if (verbose) cat("Coercing 'double' column i.'",icnam,"' to 'integer' to match type of x.'",xcnam,"'. Please avoid coercion for efficiency.\n",sep="")
                    newval = i[[lc]]
                    mode(newval) = "integer"  # retains column attributes (such as IDateTime class)
                    set(i,j=lc,value=newval)
                }
                if (is.double(x[[rc]]) && is.integer(i[[lc]])) {
                    if (verbose) cat("Coercing 'integer' column i.'",icnam,"' to 'double' to match type of x.'",xcnam,"'. Please avoid coercion for efficiency.\n",sep="")
                    newval = i[[lc]]
                    mode(newval) = "double"
                    set(i,j=lc,value=newval)
                }
            }
            f__ = integer(nrow(i))   # these could be returned as a list from bmerge?
            len__ = integer(nrow(i))
            allLen1 = logical(1)
            if (verbose) {last.started.at=proc.time()[3];cat("Starting bmerge ...");flush.console()}
            .Call(Cbmerge, i, x, as.integer(leftcols), as.integer(rightcols), haskey(i), roll, rollends, nomatch, f__, len__, allLen1)
            if (verbose) {cat("done in",round(proc.time()[3]-last.started.at,3),"secs\n");flush.console}
            # length of input nomatch (single 0 or NA) is 1 in both cases.
            # When no match, len__ is 0 for nomatch=0 and 1 for nomatch=NA, so len__ isn't .N
            if (is.na(which)) return(which(if (notjoin) f__!=0L else is.na(f__)))
            for (ii in resetifactor) set(i,j=ii,value=origi[[ii]])
            if (mult=="all") {
                if (!byjoin) {
                    irows = if (allLen1) f__ else vecseq(f__,len__,if(allow.cartesian)NULL else as.integer(max(nrow(x),nrow(i))))
                }
            } else {
                irows = if (mult=="first") f__ else f__+len__-1L
                if (identical(nomatch,0L)) irows = irows[len__>0L]  # 0s are len 0, so this removes -1 irows
                if (length(len__)) len__ = pmin(len__,1L)  # for test 456, and consistency generally
                                                           # the if() is for R < 2.15.1 when pmin was enhanced, see v1.8.6.
            }
        } else {
            # i is not a data.table
            if (!is.logical(i) && !is.numeric(i)) stop("i has not evaluated to logical, integer or double")
            if (is.logical(i)) {
                if (length(i)==nrow(x)) irows=which(i)   # e.g. DT[colA>3,which=TRUE]
                else irows=seq_len(nrow(x))[i]  # e.g. recycling DT[c(TRUE,FALSE),which=TRUE], for completeness 
                # it could also be DT[!TRUE, which=TRUE] (silly cases, yes). 
                # replaced the "else if (!isTRUE(i))" to just "else". Fixes bug report #4930 
            } else {
                irows = as.integer(i)  # e.g. DT[c(1,3)]
                # fixes #2697. If irows is -ve, o__ is. When "by", "Cdogroups" uses -ve indexing. Line 154 (I think) doesn't compute correct "rownum" in Cdogroups.
                if (all(is.finite(irows)) && all(irows < 0) && length(irows) > 0) {
                    irows = abs(irows)
                    irowsgt = which(irows > nrow(x))
                    if (length(irowsgt) > 0) warning("row(s) ", paste(irows[irowsgt], collapse=","), " do not exist to be removed from subsetting.");
                    irows = setdiff(seq_len(nrow(x)), irows)
                }
            }
        }
        if (notjoin) {
            if (byjoin || !is.integer(irows) || is.na(nomatch)) stop("Internal error: notjoin but byjoin or !integer or nomatch==NA")
            irows = irows[irows!=0L]
            i = irows = if (length(irows)) seq_len(nrow(x))[-irows] else NULL  # NULL meaning all rows i.e. seq_len(nrow(x))
            leftcols = integer()  # proceed as if row subset from now on, length(leftcols) is switched on later
            rightcols = integer()
            # Doing this once here, helps speed later when repeatedly subsetting each column. R's [irows] would do this for each
            # column when irows contains negatives.
        }
        if (which) return( if (is.null(irows)) seq_len(nrow(x)) else irows )
    } else {  # missing(i)
        i = NULL
        leftcols = integer()
        rightcols = integer()
    }

    byval = NULL
    xnrow = nrow(x)
    xcols = xcolsAns = icols = icolsAns = integer()
    
    if (missing(j)) {
        # missing(by)==TRUE was already checked above before dealing with i
        if (!length(x)) return(null.data.table())
        if (!length(leftcols)) {               
            ansvars = names(x)
            jisvars = character()
            xcols = xcolsAns = seq_along(x)
        } else {
            jisvars = names(i)[-leftcols]
            tt = jisvars %chin% names(x)
            if (length(tt)) jisvars[tt] = paste("i.",jisvars[tt],sep="")
            ansvars = c(names(x), jisvars)
            icols = c(leftcols, seq_along(i)[-leftcols])
            icolsAns = c(rightcols, seq.int(ncol(x)+1L, length.out=ncol(i)-length(leftcols)))
            xcols = xcolsAns = seq_along(x)[-rightcols]
        }
    } else {
        jsub = substitute(j)
        # deconstruct and eval everything with just one argument, then reconstruct back to a call
        if (is.call(jsub))
            jsub = construct(deconstruct_and_eval(jsub, parent.frame(), parent.frame()))
        if (is.null(jsub)) return(NULL)

        if (!with && is.call(jsub) && jsub[[1L]]==as.name(":=")) {
            if (is.null(names(jsub)) && is.name(jsub[[2L]])) {
                # TO DO: uncomment these warnings in next release. Later, make both errors.
                ## warning("with=FALSE is deprecated when using :=. Please wrap the LHS of := with parentheses; e.g., DT[,(myVar):=sum(b),by=a] to assign to column name(s) held in variable myVar. See ?':=' for other examples.")
                jsub[[2L]] = eval(jsub[[2L]], parent.frame(), parent.frame()) 
            } else {
                ## warning("with=FALSE ignored, it isn't needed when using :=. See ?':=' for examples.")
            }
            with = TRUE
        }
        if (!with) {
            # missing(by)==TRUE was already checked above before dealing with i
            if (is.call(jsub) && jsub[[1]]==as.name("!")) {
                notj = TRUE
                jsub = jsub[[2]]
            } else notj = FALSE
            if (notj) j = eval(jsub, parent.frame(), parent.frame()) # else j will be evaluated for the first time on next line
            if (is.logical(j)) j <- which(j)
            if (!length(j)) return( null.data.table() )
            if (is.factor(j)) j = as.character(j)  # fix for FR: #4867
            if (is.character(j)) {
                if (notj) {
                    w = chmatch(j, names(x))
                    if (any(is.na(w))) {
                        warning("column(s) not removed because not found: ",paste(j[is.na(w)],collapse=","))
                        w = w[!is.na(w)]
                    } 
                    ansvars = if (length(w)) names(x)[-w] else names(x)
                } else {
                    ansvars = j   # x. and i. prefixes may be in here, and they'll be dealt with below 
                }
            } else if (is.numeric(j)) {
                if (any(abs(j) > ncol(x) | j==0L)) stop("j out of bounds")
                if (any(j<0L) && any(j>0L)) stop("j mixes positive and negative")
                if (any(j<0L)) j = seq_len(ncol(x))[j]
                ansvars = names(x)[ if (notj) -j else j ]  # DT[,!"columntoexclude",with=FALSE], if a copy is needed, rather than :=NULL
            }
            
        } else {   # with=TRUE and byjoin could be TRUE
            bynames = NULL
            allbyvars = NULL
            if (byjoin) {
                bynames = names(x)[rightcols]
            } else if (!missing(by)) {
                # deal with by before j because we need byvars when j contains .SD 
                # may evaluate to NULL | character() | "" | list(), likely a result of a user expression where no-grouping is one case being loop'd through 
                bysubl = as.list.default(bysub)
                bysuborig = bysub
                if (is.name(bysub) && !(as.character(bysub) %chin% names(x))) {  # TO DO: names(x),names(i),and i. and x. prefixes
                    bysub = eval(bysub, parent.frame(), parent.frame())
                    # fix for # 5106 - http://stackoverflow.com/questions/19983423/why-by-on-a-vector-not-from-a-data-table-column-is-very-slow
                    # case where by=y where y is not a column name, and not a call/symbol/expression, but an atomic vector outside of DT.
                    # note that if y is a list, this'll return an error (not sure if it should).
                    if (is.atomic(bysub)) bysubl = list(bysuborig) else bysubl = as.list.default(bysub)
                }
                if (length(bysubl) && identical(bysubl[[1L]],quote(eval))) {    # TO DO: or by=..()
                    bysub = eval(bysubl[[2]], parent.frame(), parent.frame())
                    if (is.expression(bysub)) bysub=bysub[[1L]]
                    bysubl = as.list.default(bysub)
                } else if (is.call(bysub) && as.character(bysub[[1L]]) %chin% c("c","key","names")) {
                    # catch common cases, so we don't have to copy x[irows] for all columns
                    # *** TO DO ***: try() this eval first (as long as not list() or .()) and see if it evaluates to column names
                    # to avoid the explicit c,key,names which already misses paste("V",1:10) for example
                    #        tried before but since not wrapped in try() it failed on some tests
                    # or look for column names used in this by (since if none it wouldn't find column names anyway
                    # when evaled within full x[irows]).  Trouble is that colA%%2L is a call and should be within frame.
                    tt = eval(bysub, parent.frame(), parent.frame())
                    if (!is.character(tt)) stop("by=c(...), key(...) or names(...) must evaluate to 'character'")
                    bysub=tt
                } else if (is.call(bysub) && !as.character(bysub[[1L]]) %chin% c("list", "as.list", "{")) {
                    # potential use of function, ex: by=month(date). catch it and wrap with "(", because we need to set "bysameorder" to FALSE as we don't know if the function will return ordered results just because "date" is ordered. Fixes #2670.
                    bysub = as.call(c(as.name('('), list(bysub)))
                    bysubl = as.list.default(bysub)
                }
                if (mode(bysub) == "character") {
                    if (length(grep(",",bysub))) {
                        if (length(bysub)>1L) stop("'by' is a character vector length ",length(bysub)," but one or more items include a comma. Either pass a vector of column names (which can contain spaces, but no commas), or pass a vector length 1 containing comma separated column names. See ?data.table for other possibilities.")
                        bysub = strsplit(bysub,split=",")[[1L]]
                    }
                    tt = grep("^[^`]+$",bysub)
                    if (length(tt)) bysub[tt] = paste("`",bysub[tt],"`",sep="")
                    bysub = parse(text=paste("list(",paste(bysub,collapse=","),")",sep=""))[[1L]]
                    bysubl = as.list.default(bysub)
                }
                allbyvars = intersect(all.vars(construct(bysubl), FALSE),names(x))
                
                orderedirows = .Call(CisOrderedSubset, irows, nrow(x))  # TRUE when irows is NULL (i.e. no i clause)
                # orderedirows = is.sorted(f__)
                bysameorder = orderedirows && haskey(x) && all(sapply(bysubl,is.name)) && identical(allbyvars,head(key(x),length(allbyvars)))
                if (is.null(irows))
                    byval = eval(bysub, x, parent.frame())
                else {
                    if (!is.integer(irows)) stop("Internal error: irows isn't integer")  # length 0 when i returns no rows
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
                    byval = eval(bysub, xss, parent.frame())
                    xnrow = nrow(xss)
                    # TO DO: pass xss (x subset) through into dogroups. Still need irows there (for :=), but more condense
                    # and contiguous to use xss to form .SD in dogroups than going via irows
                }
                if (!length(byval) && xnrow>0L) {
                    # see missing(by) up above for comments
                    # by could be NULL or character(0) for example (e.g. passed in as argument in a loop of different bys)
                    bysameorder = FALSE  # 1st and only group is the entire table, so could be TRUE, but FALSE to avoid
                                         # a key of empty character()
                    byval = list()
                    bynames = allbyvars = NULL
                    # the rest now fall through
                } else bynames = names(byval)
                if (is.atomic(byval)) {
                    if (is.character(byval) && length(byval)<=ncol(x) && !(is.name(bysub) && as.character(bysub)%chin%names(x)) ) {
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
                if (!is.list(byval)) stop("'by' or 'keyby' must evaluate to vector or list of vectors (where 'list' includes data.table and data.frame which are lists, too)")
                for (jj in seq_len(length(byval))) {
                    if (!typeof(byval[[jj]]) %chin% c("integer","logical","character","double")) stop("column or expression ",jj," of 'by' or 'keyby' is type ",typeof(byval[[jj]]),". Do not quote column names. Usage: DT[,sum(colC),by=list(colA,month(colB))]")
                }
                tt = sapply(byval,length)
                if (any(tt!=xnrow)) stop("The items in the 'by' or 'keyby' list are length (",paste(tt,collapse=","),"). Each must be same length as rows in x or number of rows returned by i (",xnrow,").")
                if (is.null(bynames)) bynames = rep.int("",length(byval))
                if (any(bynames=="")) {
                    if (length(bysubl)<2) stop("When 'by' or 'keyby' is list() we expect something inside the brackets")
                    for (jj in seq_along(bynames)) {
                        if (bynames[jj]=="") {
                            # Best guess. Use "month" in the case of by=month(date), use "a" in the case of by=a%%2
                            byvars = all.vars(bysubl[[jj+1L]], functions = TRUE)
                            if (length(byvars) == 1) tt = byvars
                            else tt = grep("^eval|^[^[:alpha:]. ]",byvars,invert=TRUE,value=TRUE)[1L]

                            if (!length(tt)) tt = all.vars(bysubl[[jj+1L]])[1L]
                            bynames[jj] = tt
                            # if user doesn't like this inferred name, user has to use by=list() to name the column
                        }
                    }
                }
                setattr(byval, "names", bynames)  # byval is just a list not a data.table hence setattr not setnames
            }
            
            jvnames = NULL
            if (is.name(jsub)) {
                # j is a single unquoted column name
                if (jsub!=".SD") {
                    jvnames = gsub("^[.]N$","N",as.character(jsub))
                    # jsub is list()ed after it's eval'd inside dogroups.
                }
            } else if (is.call(jsub) && jsub[[1L]] == "list") {
                jsubl = as.list.default(jsub)  # TO DO: names(jsub) and names(jsub)="" seem to work so make use of that
                if (length(jsubl)>1) {
                    jvnames = names(jsubl)[-1L]   # check list(a=sum(v),v)
                    if (is.null(jvnames)) jvnames = rep.int("", length(jsubl)-1L)
                    for (jj in seq.int(2L,length(jsubl))) {
                        if (jvnames[jj-1L] == "" && mode(jsubl[[jj]])=="name")
                            jvnames[jj-1L] = gsub("^[.]N$","N",deparse(jsubl[[jj]]))
                        # TO DO: if call to a[1] for example, then call it 'a' too
                    }
                    setattr(jsubl, "names", NULL)  # drops the names from the list so it's faster to eval the j for each group. We'll put them back aftwards on the result.
                    jsub = as.call(jsubl)
                } # else empty list is needed for test 468: adding an empty list column
            } # else maybe a call to transform or something which returns a list.
            av = all.vars(jsub,TRUE)  # TRUE fixes bug #1294 which didn't see b in j=fns[[b]](c)
            if (".SD" %chin% av) {
                if (missing(.SDcols)) {
                    ansvars = setdiff(names(x),union(bynames,allbyvars))   # TO DO: allbyvars here for vars used by 'by'. Document.
                    # just using .SD in j triggers all non-by columns in the subset even if some of
                    # those columns are not used. It would be tricky to detect whether the j expression
                    # really does use all of the .SD columns or not, hence .SDcols for grouping
                    # over a subset of columns
                } else {
                    # FR #4979 - negative numeric and character indices for SDcols
                    colsub = substitute(.SDcols)
                    # fix for #5190. colsub[[1L]] gave error when it's a symbol.
                    if (is.call(colsub) && colsub[[1L]] == "-") {
                        colm = TRUE
                        .SDcols = eval(colsub[[2L]], parent.frame(), parent.frame())
                    } else colm = FALSE
                    if (is.numeric(.SDcols)) {
                        if (length(unique(sign(.SDcols))) != 1L) stop(".SDcols is numeric but has both +ve and -ve indices")
                        if (any(is.na(.SDcols)) || any(abs(.SDcols)>ncol(x)) || any(abs(.SDcols)<1L)) stop(".SDcols is numeric but out of bounds (or NA)")
                        if (colm) ansvars = setdiff(names(x)[-.SDcols], bynames) else ansvars = names(x)[.SDcols]
                    } else {
                        if (!is.character(.SDcols)) stop(".SDcols should be column numbers or names")
                        if (any(is.na(.SDcols)) || any(!.SDcols %chin% names(x))) stop("Some items of .SDcols are not column names (or are NA)")
                        if (colm) ansvars = setdiff(setdiff(names(x), .SDcols), bynames) else ansvars = .SDcols
                    }
                    # .SDcols might include grouping columns if users wants that, but normally we expect user not to include them in .SDcols
                }
            } else {
                if (!missing(.SDcols)) warning("This j doesn't use .SD but .SDcols has been supplied. Ignoring .SDcols. See ?data.table.")
                ansvars = setdiff(intersect(av,c(names(x),names(i),paste("i.",names(i),sep=""))), bynames)
                if (verbose) cat("Detected that j uses these columns:",if (!length(ansvars)) "<none>" else paste(ansvars,collapse=","),"\n")
                # using a few named columns will be faster
                # Consider:   DT[,max(diff(date)),by=list(month=month(date))]
                # and:        DT[,lapply(.SD,sum),by=month(date)]
                # We don't want date in .SD in the latter, but we do in the former; hence the union() above.
            }
            # if (!length(ansvars)) Leave ansvars empty. Important for test 607.
            if ("get" %chin% av) {
                if (verbose) {
                    cat("'get' found in j. ansvars being set to all columns. Use .SDcols or eval(macro) instead. Both will detect the columns used which is important for efficiency.\nOld:", paste(ansvars,collapse=","),"\n")
                    # get('varname') is too difficult to detect which columns are used in general
                    # eval(macro) column names are detected via the  if jsub[[1]]==eval switch earlier above.
                }
                ansvars = setdiff(names(x),bynames)
                if (verbose) cat("New:",paste(ansvars,collapse=","),"\n")
            }

            lhs = NULL
            newnames = NULL
            suppPrint = identity
            if (length(av) && av[1L] == ":=") {
                if (identical(attr(x,".data.table.locked"),TRUE)) stop(".SD is locked. Using := in .SD's j is reserved for possible future use; a tortuously flexible way to modify by group. Use := in j directly to modify by group by reference.")
                if (Cstack_info()[["eval_depth"]] <= .global$depthtrigger) {
                    suppPrint = function(x) { .global$print=FALSE; x }
                    # Suppress print when returns ok not on error, bug #2376. Thanks to: http://stackoverflow.com/a/13606880/403310
                    # All appropriate returns following this point are wrapped i.e. return(suppPrint(x)).
                }
                # FR #4996 - verbose message and return when a join matches nothing with `:=` in j
                if (byjoin & !notjoin) {
                    # Note: !notjoin is here only until the notjoin is implemented as a "proper" byjoin
                    if ((all(is.na(f__)) | (all(f__ == 0L) & nomatch == 0L))) {
                        if (verbose) cat("No rows pass i clause so quitting := early with no changes made.\n")
                        return(suppPrint(x))
                    }
                }
                if (!is.null(irows)) {
                    if (!length(irows)) {
                        if (verbose) cat("No rows pass i clause so quitting := early with no changes made.\n")
                        return(suppPrint(x))
                    } else
                        if (!with) irows <- irows[!is.na(irows)] # fixes 2445. TO DO: added a message if(verbose) or warning?
                        if (!missing(keyby)) stop("When i is present, keyby := on a subset of rows doesn't make sense. Either change keyby to by, or remove i")
                }
                if (is.null(names(jsub))) {
                    # regular LHS:=RHS usage, or `:=`(...) with no named arguments (an error)
                    # `:=`(LHS,RHS) is valid though, but more because can't see how to detect that, than desire
                    if (length(jsub)!=3L) stop("In `:=`(col1=val1, col2=val2, ...) form, all arguments must be named.")
                    lhs = jsub[[2]]
                    jsub = jsub[[3]]
                    if (is.name(lhs)) {
                        lhs = as.character(lhs)
                    } else {
                        # e.g. (MyVar):= or get("MyVar"):=
                        lhs = eval(lhs, parent.frame(), parent.frame())
                    }
                } else {
                    # `:=`(c2=1L,c3=2L,...)
                    lhs = names(jsub)[-1]
                    if (any(lhs=="")) stop("In `:=`(col1=val1, col2=val2, ...) form, all arguments must be named.")
                    names(jsub)=""
                    jsub[[1]]=as.name("list")
                }
                av = all.vars(jsub,TRUE)
                if (!is.atomic(lhs)) stop("LHS of := must be a symbol, or an atomic vector (column names or positions).")
                if (is.character(lhs))
                    m = chmatch(lhs,names(x))
                else if (is.numeric(lhs)) {
                    m = as.integer(lhs)
                    if (any(m<1L | ncol(x)<m)) stop("LHS of := appears to be column positions but are outside [1,ncol] range. New columns can only be added by name.")
                    lhs = names(x)[m]
                } else
                    stop("LHS of := isn't column names ('character') or positions ('integer' or 'numeric')")
                if (all(!is.na(m))) {
                    # updates by reference to existing columns
                    cols = as.integer(m)
                    newnames=NULL
                } else {
                    # Adding new column(s). TO DO: move after the first eval in case the jsub has an error.
                    newnames=setdiff(lhs,names(x))
                    m[is.na(m)] = ncol(x)+seq_len(length(newnames))
                    cols = as.integer(m)
                    if ((ok<-selfrefok(x,verbose))==0L)   # ok==0 so no warning when loaded from disk (-1) [-1 considered TRUE by R]
                        warning("Invalid .internal.selfref detected and fixed by taking a copy of the whole table so that := can add this new column by reference. At an earlier point, this data.table has been copied by R (or been created manually using structure() or similar). Avoid key<-, names<- and attr<- which in R currently (and oddly) may copy the whole data.table. Use set* syntax instead to avoid copying: ?set, ?setnames and ?setattr. Also, in R<=v3.0.2, list(DT1,DT2) copied the entire DT1 and DT2 (R's list() used to copy named objects); please upgrade to R>v3.0.2 if that is biting. If this message doesn't help, please report to datatable-help so the root cause can be fixed.")
                    if ((ok<1L) || (truelength(x) < ncol(x)+length(newnames))) {
                        n = max(ncol(x)+100, ncol(x)+2*length(newnames))
                        name = substitute(x)
                        if (is.name(name) && ok && verbose) { # && NAMED(x)>0 (TO DO)    # ok here includes -1 (loaded from disk)
                            cat("Growing vector of column pointers from truelength ",truelength(x)," to ",n,". A shallow copy has been taken, see ?alloc.col. Only a potential issue if two variables point to the same data (we can't yet detect that well) and if not you can safely ignore this. To avoid this message you could alloc.col() first, deep copy first using copy(), wrap with suppressWarnings() or increase the 'datatable.alloccol' option.\n")
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
                        alloc.col(x, n, verbose=verbose)   # always assigns to calling scope; i.e. this scope
                        if (is.name(name)) {
                            assign(as.character(name),x,parent.frame(),inherits=TRUE)
                        } else if (is.call(name) && name[[1L]]=="[[" && is.name(name[[2L]])) {
                            k = eval(name[[2L]], parent.frame(), parent.frame())
                            origj = j = eval(name[[3L]], parent.frame(), parent.frame())
                            if (is.character(j)) {
                                if (length(j)!=1L) stop("L[[i]][,:=] syntax only valid when i is length 1, but it's length %d",length(j))
                                j = match(j, names(k))
                                if (is.na(j)) stop("Item '",origj,"' not found in names of list")
                            }
                            .Call(Csetlistelt,k,as.integer(j), x)
                        } # TO DO: else if env$<- or list$<-
                    }
                }
            }
        }
        
        if (length(ansvars)) {
            w = chmatch(ansvars, names(x))
            if (length(rightcols) && missing(by)) w[ w %in% rightcols ] = NA
            if (!any(wna <- is.na(w))) {
                xcols = w
                xcolsAns = seq_along(ansvars)
                icols = icolsAns = integer()
            } else {
                if (!length(leftcols)) stop("column(s) not found: ", paste(ansvars[wna],sep=", "))
                xcols = w[!wna]
                xcolsAns = which(!wna)
                ivars = names(i)
                ivars[leftcols] = names(x)[rightcols]
                w2 = chmatch(ansvars[wna], ivars)
                if (any(w2na <- is.na(w2))) {
                    ivars = paste("i.",ivars,sep="")
                    ivars[leftcols] = names(i)[leftcols]
                    w2[w2na] = chmatch(ansvars[wna][w2na], ivars)
                    if (any(w2na <- is.na(w2))) {
                        ivars[leftcols] = paste("i.",ivars[leftcols],sep="")
                        w2[w2na] = chmatch(ansvars[wna][w2na], ivars)
                        if (any(w2na <- is.na(w2))) stop("column(s) not found: ", paste(ansvars[wna][w2na],sep=", "))
                    }
                }
                icols = w2
                icolsAns = which(wna)
            }
        }
    }  # end of  if !missing(j)
    
    SDenv = new.env(parent=parent.frame())
    # hash=TRUE (the default) does seem better as expected using e.g. test 645.  TO DO experiment with 'size' argument

    if (missing(by) || (!byjoin && !length(byval))) {
        # No grouping: 'by' = missing | NULL | character() | "" | list()
        # Considered passing a one-group to dogroups but it doesn't do the recycling of i within group, that's done here
        if (length(ansvars)) {
            ans = vector("list", length(ansvars))
            if (length(i) && length(icols)) {
                if (allLen1 && (is.na(nomatch) || !any(f__==0L))) {   # nomatch=0 should drop rows in i that have no match
                    for (s in seq_along(icols)) {
                        target = icolsAns[s]
                        source = icols[s]
                        ans[[target]] = i[[source]]
                        if (address(ans[[target]]) == address(i[[source]])) ans[[target]] = copy(ans[[target]])
                    }
                } else {
                    ii = rep.int(seq_len(nrow(i)),len__)
                    for (s in seq_along(icols)) {
                        target = icolsAns[s]
                        source = icols[s]
                        ans[[target]] = i[[source]][ii]
                        copyattr(i[[source]], ans[[target]])
                    }
                }
            }
            if (is.null(irows)) {
                for (s in seq_along(xcols)) {  # xcols means non-join x columns, since join columns come from i
                    target = xcolsAns[s]
                    source = xcols[s]
                    ans[[target]] = x[[source]]
                    if (address(ans[[target]]) == address(x[[source]])) ans[[target]] = copy(ans[[target]])
                }
            } else {
                for (s in seq_along(xcols)) {
                    target = xcolsAns[s]
                    source = xcols[s]
                    ans[[target]] = x[[source]][irows]
                    copyattr(x[[source]], ans[[target]])
                }
            }
            # the address==address is a temp fix for R >= 3.1. TO DO: allow shallow copy here, then copy only when user uses :=
            # or set* on the result by using NAMED/REFCNT on columns, with warning if they copy. Since then, even foo = DT$b
            # would cause the next set or := to copy that column (so the warning is needed). To tackle that, we could have our
            # own DT.NAMED attribute, perhaps.
            # Or keep the rule that [.data.table always returns new memory, and create view() or view= as well, maybe cleaner.
            
            setattr(ans, "names", ansvars)
            if (haskey(x)) {
                keylen = which.first(!key(x) %chin% ansvars)-1L
                if (is.na(keylen)) keylen = length(key(x))
                if (keylen > length(rightcols) && !.Call(CisOrderedSubset, irows, nrow(x))) keylen = length(rightcols)
                if (keylen && ((is.data.table(i) && haskey(i)) || is.logical(i) || .Call(CisOrderedSubset, irows, nrow(x))))
                    setattr(ans,"sorted",head(key(x),keylen))
            }
            setattr(ans, "class", class(x)) # fix for #5296
            setattr(ans, "row.names", .set_row_names(nrow(ans)))
            
            if (!with || missing(j)) return(alloc.col(ans))

            SDenv$.SD = ans
            SDenv$.N = nrow(SDenv$.SD)

        } else {
            SDenv$.SD = null.data.table()   # no columns used by j so .SD can be empty. Only needs to exist so that we can rely on it being there when locking it below for example. If .SD were used by j, of course then xvars would be the columns and we wouldn't be in this leaf.
            SDenv$.N = if (is.null(irows)) nrow(x) else sum(!is.na(irows) & irows>0L)
        }
        SDenv$.I = seq_len(SDenv$.N)
        SDenv$.GRP = 1L
        setattr(SDenv$.SD,".data.table.locked",TRUE)   # used to stop := modifying .SD via j=f(.SD), bug#1727. The more common case of j=.SD[,subcol:=1] was already caught when jsub is inspected for :=.
        lockBinding(".SD",SDenv)
        lockBinding(".N",SDenv)
        lockBinding(".I",SDenv)
        lockBinding(".GRP",SDenv)
        for (ii in ansvars) assign(ii, SDenv$.SD[[ii]], SDenv)
        # Since .SD is inside SDenv, alongside its columns as variables, R finds .SD symbol more quickly, if used.
        # There isn't a copy of the columns here, the xvar symbols point to the SD columns (copy-on-write).

        jval = eval(jsub, SDenv, parent.frame())

        if (!is.null(lhs)) {   # *** TO DO ***: use set() here now that it can add new column(s) and remove newnames and alloc logic above
            if (verbose) cat("Assigning to ",if (is.null(irows)) "all " else paste(length(irows),"row subset of "), nrow(x)," rows\n",sep="")
            .Call(Cassign,x,irows,cols,newnames,jval,verbose)
            return(suppPrint(x))
        }
        if ((is.call(jsub) && is.list(jval) && !is.object(jval)) || !missing(by)) {
            # is.call: selecting from a list column should return list
            # is.object: for test 168 and 168.1 (S4 object result from ggplot2::qplot). Just plain list results should result in data.table
            if (is.atomic(jval)) {
                setattr(jval,"names",NULL)
                jval = data.table(jval)
            } else {
                if (is.null(jvnames)) jvnames=names(jval)
                jval = as.data.table.list(jval)   # does the vector expansion to create equal length vectors
            }
            if (is.null(jvnames)) jvnames = character(length(jval)-length(bynames))
            ww = which(jvnames=="")
            if (any(ww)) jvnames[ww] = paste("V",ww,sep="")
            setnames(jval, jvnames)
        }

        # fix for bug #5114 from GSee's - .data.table.locked=TRUE.   # TO DO: more efficient way e.g. address==address (identical will do that but then proceed to deep compare if !=, wheras we want just to stop?)
        if (identical(jval, SDenv$.SD)) return(copy(jval))
        
        if (is.data.table(jval)) {
            setattr(jval, 'class', class(x)) # fix for #5296
            if (haskey(x) && all(key(x) %chin% names(jval)) && suppressWarnings(is.sorted(jval, by=key(x))))  # TO DO: perhaps this usage of is.sorted should be allowed internally then (tidy up and make efficient)
                setattr(jval, 'sorted', key(x))
        }
        return(jval)
    }

    ###########################################################################
    # Grouping ...
    ###########################################################################
    
    o__ = integer()
    if (".N" %chin% ansvars) stop("The column '.N' can't be grouped because it conflicts with the special .N variable. Try setnames(DT,'.N','N') first.")
    SDenv$.iSD = NULL  # null.data.table()
    SDenv$.xSD = NULL  # null.data.table() - introducing for FR #2693 and Gabor's post on fixing for FAQ 2.8

    assign("print", function(x,...){base::print(x,...);NULL}, SDenv)
    # Now ggplot2 returns data from print, we need a way to throw it away otherwise j accumulates the result

    SDenv$.SD = null.data.table()  # e.g. test 607. Grouping still proceeds even though no .SD e.g. grouping key only tables, or where j consists of .N only
    SDenv$.N = as.integer(0)     # not 0L for the reson on next line :
    SDenv$.GRP = as.integer(1)   # oddly using 1L doesn't work reliably here! Possible R bug? TO DO: create reproducible example and report. To reproduce change to 1L and run test.data.table, test 780 fails. The assign seems ineffective and a previous value for .GRP from a previous test is retained, despite just creating a new SDenv.

    if (byjoin) {
        # The groupings come instead from each row of the i data.table.
        # Much faster for a few known groups vs a 'by' for all followed by a subset
        if (!is.data.table(i)) stop("logicial error. i is not data.table, but mult='all' and 'by'=.EACHI")
        byval = i
        bynames = head(key(x),length(leftcols))
        allbyvars = NULL
        bysameorder = haskey(i) || is.sorted(f__)
        ##  'av' correct here ??  *** TO DO ***
        xjisvars = intersect(av, names(x)[rightcols])  # no "x." for xvars.
        jisvars = intersect(gsub("^i[.]","", setdiff(av, xjisvars)),names(i))
        # JIS (non join cols) but includes join columns too (as there are named in i)
        if (length(jisvars)) {
            tt = min(nrow(i),1L)
            SDenv$.iSD = i[tt,jisvars,with=FALSE]
            for (ii in jisvars) {
                assign(ii, SDenv$.iSD[[ii]], SDenv)
                assign(paste("i.",ii,sep=""), SDenv$.iSD[[ii]], SDenv)
            }
        }

    } else {
        # Find the groups, using 'byval' ...
        if (missing(by)) stop("Internal error, by is missing")
        if (verbose) {last.started.at=proc.time()[3];cat("Finding groups (bysameorder=",bysameorder,") ... ",sep="");flush.console()}
        if (length(byval) && length(byval[[1]])) {
            if (!bysameorder) {
                o__ = forderv(byval, sort=FALSE, retGrp=TRUE)   # returns integer() (not NULL) if already ordered, to save 1:xnrow for efficiency
                bysameorder = orderedirows && !length(o__)
                f__ = attr(o__, "starts")
                len__ = uniqlengths(f__, xnrow)
                if (!bysameorder) {    # TO DO: lower this into forder.c
                    firstofeachgroup = o__[f__]    
                    if (length(origorder <- forderv(firstofeachgroup))) {
                        f__ = f__[origorder]
                        len__ = len__[origorder]
                    }
                }
                if (!orderedirows && !length(o__)) o__ = 1:xnrow  # temp fix.  TO DO: revist orderedirows
            } else {
                f__ = uniqlist(byval)
                len__ = uniqlengths(f__, xnrow)
                # TO DO: combine uniqlist and uniquelengths into one call.  Or, just set len__ to NULL when dogroups infers that.
            }
        } else {
            f__=NULL
            len__=0L
            bysameorder=TRUE   # for test 724
        }
        if (verbose) {cat("done in ",round(proc.time()[3]-last.started.at,3),"secs. bysameorder=",bysameorder," and o__ is length ",length(o__),"\n",sep="");flush.console}
        # TO DO: allow secondary keys to be stored, then we see if our by matches one, if so use it, and no need to sort again. TO DO: document multiple keys.
    }
    alloc = if (length(len__)) seq_len(max(len__)) else 0L
    SDenv$.I = alloc
    if (length(xcols)) {
        SDenv$.SD = x[alloc, xcols, with=FALSE]
        # TO DO: **** Instead just for loop assigning vector(typeof(...)). Saves recursion overhead.
        # Must not shallow copy xvars here. This is the allocation for the largest group. Since i is passed in here, it won't shallow copy, even in future. Only DT[,xvars,with=FALSE] might ever shallow copy automatically.
    }
    if (nrow(SDenv$.SD)==0L) setattr(SDenv$.SD,"row.names",c(NA_integer_,0L))
    # .set_row_names() basically other than not integer() for 0 length, otherwise dogroups has no [1] to modify to -.N
    setattr(SDenv$.SD,".data.table.locked",TRUE)   # used to stop := modifying .SD via j=f(.SD), bug#1727. The more common case of j=.SD[,subcol:=1] was already caught when jsub is inspected for :=.
    lockBinding(".SD",SDenv)
    lockBinding(".N",SDenv)
    lockBinding(".GRP",SDenv)
    lockBinding(".I",SDenv)
    lockBinding(".iSD",SDenv)
    
    GForce = FALSE
    if (getOption("datatable.optimize")>=1 && is.call(jsub)) {  # Ability to turn off if problems or to benchmark the benefit
        # Optimization to reduce overhead of calling lapply over and over for each group
        oldjsub = jsub
        # convereted the lapply(.SD, ...) to a function and used below, easier to implement FR #2722 then.
        .massageSD <- function(jsub) {
            txt = as.list(jsub)[-1L]
            if (length(names(txt))>1L) .Call(Csetcharvec, names(txt), 2L, "")  # fixes bug #4839
            fun = txt[[2L]]
            if (is.call(fun) && fun[[1L]]=="function") {
                # Fix for #2381: added SDenv$.SD to 'eval' to take care of cases like: lapply(.SD, function(x) weighted.mean(x, bla)) where "bla" is a column in DT
                # http://stackoverflow.com/questions/13441868/data-table-and-stratified-means
                # adding this does not compromise in speed (that is, not any lesser than without SDenv$.SD)
                # replaced SDenv$.SD to SDenv to deal with Bug #5007 reported by Ricardo (Nice catch!)
                assign("..FUN",eval(fun, SDenv, SDenv), SDenv)  # to avoid creating function() for each column of .SD
                lockBinding("..FUN",SDenv)
                txt[[1L]] = as.name("..FUN")
            } else {
                if (is.character(fun)) fun = as.name(fun)
                txt[[1L]] = fun
            }
            ans = vector("list",length(ansvars)+1L)
            ans[[1L]] = as.name("list")
            for (ii in seq_along(ansvars)) {
                txt[[2L]] = as.name(ansvars[ii])
                ans[[ii+1L]] = as.call(txt)
            }
            jsub = as.call(ans)  # important no names here
            jvnames = ansvars      # but here instead
            list(jsub, jvnames)
            # It may seem inefficient to constuct a potentially long expression. But, consider calling
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
        if (jsub[[1L]]=="lapply" && jsub[[2L]]==".SD" && length(xcols)) {
            deparse_ans = .massageSD(jsub)
            jsub = deparse_ans[[1L]]
            jvnames = deparse_ans[[2L]]
        } else if (jsub[[1L]] == "c" && length(jsub) > 1L) {
            # TODO, TO DO: raise the checks for 'jvnames' earlier (where jvnames is set by checking 'jsub') and set 'jvnames' already.
            # FR #2722 - optimisation of j=c(..., lapply(.SD, .), ...)
            is_valid = TRUE
            any_SD = FALSE
            jsubl = as.list.default(jsub)
            oldjvnames = jvnames
            jvnames = NULL           # TODO: not let jvnames grow, maybe use (number of lapply(.SD, .))*lenght(ansvars) + other jvars ?? not straightforward.
            for (i in 2:length(jsubl)) {
                this = jsub[[i]]
                if (is.call(this) && this[[1L]]=="lapply" && this[[2L]]==".SD" && length(xcols)) {
                    any_SD = TRUE
                    deparse_ans = .massageSD(this)
                    jsubl[[i]] = as.list(deparse_ans[[1L]][-1L]) # just keep the '.' from list(.)
                    jvnames = c(jvnames, deparse_ans[[2L]])
                } else {
                    if (any(all.vars(this) == ".SD")) {
                        # TODO, TO DO: revisit complex cases (as illustrated below)
                        # complex cases like DT[, c(.SD, .SD[x>1], .SD[J(.)], c(.SD), a + .SD, lapply(.SD, sum)), by=grp]
                        # hard to optimise such cases (+ difficulty in counting exact columns and therefore names). revert back to no optimisation.
                        is_valid=FALSE
                        break
                    } else if (is.name(this)) {
                        if ((is.null(names(jsubl)) || names(jsubl)[i] == "") && this == ".N") jvnames = c(jvnames, "N")
                        else jvnames = c(jvnames, if (is.null(names(jsubl))) "" else names(jsubl)[i])
                    } else if (is.call(this)) {
                        jvnames = c(jvnames, if (is.null(names(jsubl))) "" else names(jsubl)[i])
                    } else { # just to be sure that any other case (I've overlooked) runs smoothly, without optimisation
                        # TO DO, TODO: maybe a message/warning here so that we can catch the overlooked cases, if any?
                        is_valid=FALSE
                        break
                    }
                }
            }
            if (!is_valid || !any_SD) { # restore if c(...) doesn't contain lapply(.SD, ..) or if it's just invalid
                jvnames=oldjvnames             # reset jvnames
                jsub = oldjsub                 # reset jsub
                jsubl = as.list.default(jsubl) # reset jsubl
            } else {
                setattr(jsubl, 'names', NULL)
                jsub = as.call(unlist(jsubl))
                jsub[[1L]] = quote(list)
            }
        }
        if (verbose) {
            if (!identical(oldjsub, jsub))
                cat("lapply optimization changed j from '",deparse(oldjsub),"' to '",deparse(jsub,width.cutoff=200),"'\n",sep="")
            else
                cat("lapply optimization is on, j unchanged as '",deparse(jsub,width.cutoff=200),"'\n",sep="")
        }
        if (getOption("datatable.optimize")>=2 && !byjoin && !length(irows) && length(f__) && length(ansvars) && !length(lhs)) {
            # Apply GForce
            gfuns = c("sum","mean")
            .ok = function(q) {
                ans = is.call(q) && as.character(q[[1L]]) %chin% gfuns && !is.call(q[[2L]]) && (length(q)==2 || identical("na",substring(names(q)[3L],1,2)))
                if (is.na(ans)) ans=FALSE
                ans
            }
            if (jsub[[1L]]=="list") {
                GForce = TRUE
                for (ii in seq_along(jsub)[-1L]) if (!.ok(jsub[[ii]])) GForce = FALSE
            } else GForce = .ok(jsub)
            if (GForce) {
                if (jsub[[1L]]=="list")
                    for (ii in seq_along(jsub)[-1L]) { 
                        jsub[[ii]][[1L]] = as.name(paste("g", jsub[[ii]][[1L]], sep=""))
                        if (length(jsub[[ii]])==3) jsub[[ii]][[3]] = eval(jsub[[ii]][[3]], parent.frame())  # tests 1187.2 & 1187.4
                    }
                else {
                    jsub[[1L]] = as.name(paste("g", jsub[[1L]], sep=""))
                    if (length(jsub)==3) jsub[[3]] = eval(jsub[[3]], parent.frame())   # tests 1187.3 & 1187.5
                }
                if (verbose) cat("GForce optimized j to '",deparse(jsub,width.cutoff=200),"'\n",sep="")
            } else if (verbose) cat("GForce is on, left j unchanged\n");
        }
        if (!GForce) {
            # Still do the old speedup for mean, for now
            nomeanopt=FALSE  # to be set by .optmean() using <<- inside it
            oldjsub = jsub
            if (jsub[[1L]]=="list") {
                for (ii in seq_along(jsub)[-1L])
                    if (is.call(jsub[[ii]]) && jsub[[ii]][[1L]]=="mean")
                        jsub[[ii]] = .optmean(jsub[[ii]])
            } else if (jsub[[1L]]=="mean") {
                jsub = .optmean(jsub)
            }
            if (nomeanopt) {
                warning("Unable to optimize call to mean() and could be very slow. You must name 'na.rm' like that otherwise if you do mean(x,TRUE) the TRUE is taken to mean 'trim' which is the 2nd argument of mean. 'trim' is not yet optimized.",immediate.=TRUE)
            }
            if (verbose) {
                if (!identical(oldjsub, jsub))
                    cat("Old mean optimization changed j from '",deparse(oldjsub),"' to '",deparse(jsub,width.cutoff=200),"'\n",sep="")
                else
                    cat("Old mean optimization is on, left j unchanged.\n")
            }
            assign("Cfastmean", Cfastmean, SDenv)
            assign("mean", base::mean.default, SDenv)
            # Old comments still here for now ...
            # Here in case nomeanopt=TRUE or some calls to mean weren't detected somehow. Better but still slow.
            # Maybe change to :
            #     assign("mean", fastmean, SDenv)  # neater than the hard work above, but slower
            # when fastmean can do trim.
        }
    } else if (verbose) {
        if (getOption("datatable.optimize")<1) cat("All optimizations are turned off\n")
        else cat("Optimization is on but left j unchanged (single plain symbol): '",deparse(jsub,width.cutoff=200),"'\n",sep="")
    }
    if (byjoin) {
        groups = i
        grpcols = leftcols # 'leftcols' are the columns in i involved in the join (either head of key(i) or head along i)
        jiscols = chmatch(jisvars,names(i))  # integer() if there are no jisvars (usually there aren't, advanced feature)
        xjiscols = chmatch(xjisvars, names(x))
        SDenv$.xSD = x[min(nrow(i), 1L), xjisvars, with=FALSE]
    } else {
        groups = byval
        grpcols = seq_along(byval)
        jiscols = NULL   # NULL rather than integer() is used in C to know when using by
        xjiscols = NULL
    }
    lockBinding(".xSD", SDenv)
    grporder = o__
    if (length(irows) && !isTRUE(irows)) {
        # fix for bug #2758. TO DO: provide a better error message
        if (length(irows) > 1 && length(zo__ <- which(irows == 0)) > 0) stop("i[", zo__[1], "] is 0. While grouping, i=0 is allowed when it's the only value. When length(i) > 1, all i should be > 0.")
        if (length(o__) && length(irows)!=length(o__)) stop("Internal error: length(irows)!=length(o__)")
        o__ = if (length(o__)) irows[o__]  # better do this once up front (even though another alloc) than deep repeated branch in dogroups.c
              else irows
    } # else grporder is left bound to same o__ memory (no cost of copy)
    if (is.null(lhs)) cols=NULL
    if (!length(f__)) {
        # for consistency of empty case in test 184
        f__=len__=0L
    }
    if (GForce) {
        thisEnv = new.env()  # not parent=parent.frame() so that gsum is found
        for (ii in ansvars) assign(ii, x[[ii]], thisEnv)
        gstart(o__, f__, len__)
        ans = eval(jsub, thisEnv)
        if (is.atomic(ans)) ans=list(ans)  # won't copy named argument in new version of R, good
        gend()
        gi = if (length(o__)) o__[f__] else f__
        g = lapply(grpcols, function(i) groups[[i]][gi])
        ans = c(g, ans)
    } else {
        if (verbose) {last.started.at=proc.time()[3];cat("Starting dogroups ... ");flush.console()}
        ans = .Call(Cdogroups, x, xcols, groups, grpcols, jiscols, xjiscols, grporder, o__, f__, len__, jsub, SDenv, cols, newnames, verbose)
        if (verbose) {cat("done dogroups in",round(proc.time()[3]-last.started.at,3),"secs\n");flush.console()}
    }
    # TO DO: xrows would be a better name for irows: irows means the rows of x that i joins to
    # Grouping by i: icols the joins columns (might not need), isdcols (the non join i and used by j), all __ are length x
    # Grouping by by: i is by val, icols NULL, o__ may be subset of x, f__ points to o__ (or x if !length o__)
    # TO DO: setkey could mark the key whether it is unique or not.

    if (!is.null(lhs)) {
        if (any(names(x)[cols] %chin% key(x)))
            setkey(x,NULL)
        if (!missing(keyby)) {
            cnames = as.character(bysubl)[-1]
            if (all(cnames %chin% names(x)))
                setkeyv(x,cnames)  # TO DO: setkey before grouping to get memcpy benefit.
            else warning(":= keyby not straightforward character column names or list() of column names, treating as a by:",paste(cnames,collapse=","),"\n")
        }
        return(suppPrint(x))
    }
    if (is.null(ans)) {
        ans = as.data.table.list(lapply(groups,"[",0L))  # side-effects only such as test 168
        setnames(ans,seq_along(bynames),bynames)   # TO DO: why doesn't groups have bynames in the first place?
        return(ans)
    }
    setattr(ans,"row.names",.set_row_names(length(ans[[1L]])))
    setattr(ans,"class",class(x)) # fix for #5296
    if (is.null(names(ans))) {
        # Efficiency gain of dropping names has been successful. Ordinarily this will run.
        if (is.null(jvnames)) jvnames = character(length(ans)-length(bynames))
        if (length(bynames)+length(jvnames)!=length(ans))
            stop("Internal error: jvnames is length ",length(jvnames), " but ans is ",length(ans)," and bynames is ", length(bynames))
        ww = which(jvnames=="")
        if (any(ww)) jvnames[ww] = paste("V",ww,sep="")
        setattr(ans, "names", c(bynames, jvnames))
    } else {
        setnames(ans,seq_along(bynames),bynames)   # TO DO: reinvestigate bynames flowing from dogroups here and simplify
    }
    if (!missing(keyby)) {
        setkeyv(ans,names(ans)[seq_along(byval)])
        # but if 'bykey' and 'bysameorder' then the setattr in branch above will run instead for
        # speed (because !missing(by) when bykey, too)
    } else if (haskey(x) && bysameorder) {
        setattr(ans,"sorted",names(ans)[seq_along(grpcols)])
    }
    alloc.col(ans)   # TO DO: overallocate in dogroups in the first place and remove this line
}

.optmean = function(expr) {   # called by optimization of j inside [.data.table only. Outside for a small speed advantage.
    if (length(expr)==2L)  # no parameters passed to mean, so defaults of trim=0 and na.rm=FALSE
        return(call(".External",quote(Cfastmean),expr[[2L]], FALSE))
        # return(call(".Internal",expr))  # slightly faster than .External, but R now blocks .Internal in coerce.c from apx Sep 2012
    if (length(expr)==3L && identical("na",substring(names(expr)[3L],1,2)))   # one parameter passed to mean()
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


as.matrix.data.table = function(x,...)
{
    dm <- dim(x)
    cn <- names(x)
    if (any(dm == 0L))
        return(array(NA, dim = dm, dimnames = list(NULL, cn)))
    p <- dm[2L]
    n <- dm[1L]
    collabs <- as.list(cn)
    X <- x
    class(X) <- NULL
    non.numeric <- non.atomic <- FALSE
    all.logical <- TRUE
    for (j in seq_len(p)) {
        if (is.ff(X[[j]])) X[[j]] <- X[[j]][]   # to bring the ff into memory, since we need to create a matrix in memory
        xj <- X[[j]]
        if (length(dj <- dim(xj)) == 2L && dj[2L] > 1L) {
            if (inherits(xj, "data.table"))
                xj <- X[[j]] <- as.matrix(X[[j]])
            dnj <- dimnames(xj)[[2]]
            collabs[[j]] <- paste(collabs[[j]], if (length(dnj) >
                0L)
                dnj
            else seq_len(dj[2L]), sep = ".")
        }
        if (!is.logical(xj))
            all.logical <- FALSE
        if (length(levels(xj)) > 0 || !(is.numeric(xj) || is.complex(xj)) ||
            (!is.null(cl <- attr(xj, "class")) && any(cl %chin%
                c("Date", "POSIXct", "POSIXlt"))))
            non.numeric <- TRUE
        if (!is.atomic(xj))
            non.atomic <- TRUE
    }
    if (non.atomic) {
        for (j in seq_len(p)) {
            xj <- X[[j]]
            if (is.recursive(xj)) {
            }
            else X[[j]] <- as.list(as.vector(xj))
        }
    }
    else if (all.logical) {
    }
    else if (non.numeric) {
        for (j in seq_len(p)) {
            if (is.character(X[[j]]))
                next
            xj <- X[[j]]
            miss <- is.na(xj)
            xj <- if (length(levels(xj)))
                as.vector(xj)
            else format(xj)
            is.na(xj) <- miss
            X[[j]] <- xj
        }
    }
    X <- unlist(X, recursive = FALSE, use.names = FALSE)
    dim(X) <- c(n, length(X)/n)
    dimnames(X) <- list(NULL, unlist(collabs, use.names = FALSE))
    X
}

as.data.table.matrix = function(x, keep.rownames=FALSE)
{
    if (keep.rownames) return(data.table(rn=rownames(x), x, keep.rownames=FALSE))
    d <- dim(x)
    nrows <- d[1L]
    ir <- seq_len(nrows)
    ncols <- d[2L]
    ic <- seq_len(ncols)
    dn <- dimnames(x)
    collabs <- dn[[2L]]
    if (any(empty <- nchar(collabs) == 0L))
        collabs[empty] <- paste("V", ic, sep = "")[empty]
    value <- vector("list", ncols)
    if (mode(x) == "character") {
        for (i in ic) value[[i]] <- as.factor(x[, i])       # for efficiency.
    }
    else {
        for (i in ic) value[[i]] <- as.vector(x[, i])       # to drop any row.names that would otherwise be retained inside every column of the data.table
    }
    if (length(collabs) == ncols)
        setattr(value, "names", collabs)
    else
        setattr(value, "names", paste("V", ic, sep = ""))
    setattr(value,"row.names",.set_row_names(nrows))
    setattr(value,"class",c("data.table","data.frame"))
    alloc.col(value)
}

as.data.table.data.frame = function(x, keep.rownames=FALSE)
{
    if (keep.rownames) return(data.table(rn=rownames(x), x, keep.rownames=FALSE))
    ans = copy(x)  # TO DO: change this deep copy to be shallow.
    setattr(ans,"row.names",.set_row_names(nrow(x)))
    tt = class(x)
    n=chmatch("data.frame",tt)
    tt = c( head(tt,n-1L), "data.table","data.frame", tail(tt, length(tt)-n) )
    # for nlme::groupedData which has class c("nfnGroupedData","nfGroupedData","groupedData","data.frame")
    # See test 527.
    setattr(ans,"class",tt)
    alloc.col(ans)
}

as.data.table.list = function(x, keep.rownames=FALSE) {
    if (!length(x)) return( null.data.table() )
    n = vapply(x, length, 0L)
    mn = max(n)
    x = copy(x)
    if (any(n<mn)) 
    for (i in which(n<mn)) {
        if (!is.null(x[[i]])) {# avoids warning when a list element is NULL
            # Implementing FR #4813 - recycle with warning when nr %% nrows[i] != 0L
            if (mn %% n[i] != 0) 
                warning("Item ", i, " is of size ", n[i], " but maximum size is ", mn, " (recycled leaving a remainder of ", mn%%n[i], " items)")
            x[[i]] = rep(x[[i]], length.out=mn)
        }
    }
    if (is.null(names(x))) setattr(x,"names",paste("V",seq_len(length(x)),sep=""))
    setattr(x,"row.names",.set_row_names(max(n)))
    setattr(x,"class",c("data.table","data.frame"))
    alloc.col(x)
}

as.data.table.data.table = function(x, keep.rownames=FALSE) return(x)

# takes care of logical, character, numeric, integer
as.data.table.factor <- as.data.table.ordered <- 
as.data.table.integer <- as.data.table.numeric <- 
as.data.table.logical <- as.data.table.character <- 
as.data.table.Date <- function(x, keep.rownames=FALSE) {
    tt = deparse(substitute(x))[1]
    nm = names(x)
    # FR #2356 - transfer names of named vector as "rn" column if required
    if (keep.rownames & !is.null(nm)) 
        x <- list(nm, unname(x))
    else x <- list(x)
    if (tt == make.names(tt))
        setattr(x, 'names', c(if (length(x) == 2) "rn", tt))
    as.data.table.list(x, keep.rownames)
}

R300_provideDimnames = function (x, sep = "", base = list(LETTERS))   # backported from R3.0.0 so data.table can depend on R 2.14.0 
{
    dx <- dim(x)
    dnx <- dimnames(x)
    if (new <- is.null(dnx)) 
        dnx <- vector("list", length(dx))
    k <- length(M <- vapply(base, length, 1L))
    for (i in which(vapply(dnx, is.null, NA))) {
        ii <- 1L + (i - 1L)%%k
        dnx[[i]] <- make.unique(base[[ii]][1L + 0:(dx[i] - 1L)%%M[ii]], 
            sep = sep)
        new <- TRUE
    }
    if (new) 
        dimnames(x) <- dnx
    x
}

# as.data.table.table - FR #4848
as.data.table.table <- function(x, keep.rownames=FALSE) {
    # Fix for bug #5408 - order of columns are different when doing as.data.table(with(DT, table(x, y)))
    val = rev(dimnames(R300_provideDimnames(x)))
    if (is.null(names(val)) || all(nchar(names(val)) == 0L)) 
        setattr(val, 'names', paste("V", rev(seq_along(val)), sep=""))
    ans <- data.table(do.call(CJ, c(val, sorted=FALSE)), N = as.vector(x))
    setcolorder(ans, c(rev(head(names(ans), -1)), "N"))
    ans
}

# bug #2375. fixed. same as head.data.frame and tail.data.frame to deal with negative indices
head.data.table = function(x, n=6, ...) {
    if (!cedta()) return(NextMethod())
    stopifnot(length(n) == 1L)  
    i = seq_len(if (n<0L) max(nrow(x)+n, 0L) else min(n,nrow(x)))
    x[i]
}
tail.data.table = function(x, n=6, ...) {
    if (!cedta()) return(NextMethod())
    stopifnot(length(n) == 1L)  
    n <- if (n<0L) max(nrow(x) + n, 0L) else min(n, nrow(x))
    i = seq.int(to=nrow(x), length.out=n)
    x[i]
}

"[<-.data.table" = function (x, i, j, value) {
    # [<- is provided for consistency, but := is preferred as it allows by group and by reference to subsets of columns
    # with no copy of the (very large, say 10GB) columns at all. := is like an UPDATE in SQL and we like and want two symbols to change.
    if (!cedta()) {
        x = if (nargs()<4) `[<-.data.frame`(x, i, value=value)
            else `[<-.data.frame`(x, i, j, value)
        return(alloc.col(x))    # over-allocate (again).   Avoid all this by using :=.
    }
    # TO DO: warning("Please use DT[i,j:=value] syntax instead of DT[i,j]<-value, for efficiency. See ?':='")
    if (!missing(i)) {
        isub=substitute(i)
        i = eval(.massagei(isub), x, parent.frame())
        if (is.matrix(i)) {
            if (!missing(j)) stop("When i is matrix in DT[i]<-value syntax, it doesn't make sense to provide j")
            x = `[<-.data.frame`(x, i, value=value)
            return(alloc.col(x))
        }
        i = x[i, which=TRUE]
        # Tried adding ... after value above, and passing ... in here (e.g. for mult="first") but R CMD check
        # then gives "The argument of a replacement function which corresponds to the right hand side must be
        # named 'value'".  So, users have to use := for that.
    } else i = NULL          # meaning (to C code) all rows, without allocating 1L:nrow(x) vector
    if (missing(j)) j=names(x)
    if (!is.atomic(j)) stop("j must be atomic vector, see ?is.atomic")
    if (any(is.na(j))) stop("NA in j")
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
        # DT["a",]$y <- 1.1  winds up creating `*tmp*` subset of rows and assigning _all_ the columns into x and
        # over-writing the key columns with the same value (not just the single 'y' column).
        # That isn't good for speed; it's an R thing. Solution is to use := instead to avoid all this, but user
        # expects key to be retained in this case because _he_ didn't assign to a key column (the internal base R
        # code did).
        reinstatekey=key(x)
    }
    if (!selfrefok(x) || truelength(x) < ncol(x)+length(newnames)) {
        x = alloc.col(x,length(x)+length(newnames)) # because [<- copies via *tmp* and main/duplicate.c copies at length but copies truelength over too
        # search for one other .Call to assign in [.data.table to see how it differs
    }
    verbose=getOption("datatable.verbose")
    if (!.R.subassignCopiesOthers) {   # From 3.1, DF[2,"b"] = 7 no longer copies DF$a, but the VECSXP is copied (i.e. a shallow copy).
        x = .Call(Cassign,copy(x),i,cols,newnames,value,verbose)
    } else {
        .Call(Cassign,x,i,cols,newnames,value,verbose)
    }
    alloc.col(x)  #  can maybe avoid this realloc, but this is (slow) [<- anyway, so just be safe.
    if (length(reinstatekey)) setkeyv(x,reinstatekey)
    invisible(x)
    # no copy at all if user calls directly; i.e. `[<-.data.table`(x,i,j,value)
    # or uses data.table := syntax; i.e. DT[i,j:=value]
    # but, there is one copy by R in [<- dispatch to `*tmp*`; i.e. DT[i,j]<-value. *Update: not from R > 3.0.2, yay*
    # That copy is via main/duplicate.c which preserves truelength but copies length amount. Hence alloc.col(x,length(x)).
    # No warn passed to assign here because we know it'll be copied via *tmp*.
    # := allows subassign to a column with no copy of the column at all,  and by group, etc.
}

"$<-.data.table" = function(x, name, value) {
    if (!cedta()) {
        ans = `$<-.data.frame`(x, name, value)
        return(alloc.col(ans))           # over-allocate (again)
    }
    x = copy(x)
    `[<-.data.table`(x,j=name,value=value)  # important i is missing here
}

.rbind.data.table = function(..., use.names = TRUE, fill = FALSE) {
    # See FAQ 2.23
    # Called from base::rbind.data.frame
    original.names = lapply(list(...), names)
    allargs = lapply(list(...), as.data.table)
    # To fix the issue of rbind(DT, NULL) to work - NULL data.tables should be removed even when fill=FALSE (like base:::rbind does)
    # thanks to Garrett See for reporting - added as a new bug #5300 for future reference
    allargs = allargs[sapply(allargs, length) > 0L]

    n = length(allargs)
    if (n == 0L) return(null.data.table())
    if (n == 1L) return(allargs[[1L]])
    ncols = sapply(allargs, length)
    if (length(unique(ncols)) != 1L && !fill) {
        f = which(ncols != ncols[1L])[1L]
        stop("When fill=FALSE, all arguments to rbind must have the same number of columns. Item 1 has ",ncols[1L]," but item ",f," has ",ncols[f],"column(s).")
    }
    if (fill) {
        if (!use.names)
            warning("use.names=FALSE is ignored when fill is TRUE")

        all.names = lapply(allargs, names)
        unq.names = unique(unlist(all.names))
        return(rbindlist(lapply(seq_along(allargs), function(x) {
            tt = allargs[[x]]
            alloc.col(tt)
            cols = unq.names[!unq.names %chin% all.names[[x]]]
            if (length(cols) != 0L)
                tt[, c(cols) := NA]
            setcolorder(tt, unq.names)
        })))
    } else if (!use.names) {
        return(rbindlist(allargs))
    } else { # use.names == TRUE && fill == FALSE
        final.names = NULL    # find the first list item with names, these will be the final column names
        for (i in seq_along(original.names)) {
            if (!is.null(original.names[[i]])) {
                final.names = original.names[[i]]
                break
            }
        }
        if (is.null(final.names)) stop("use.names is TRUE but no input has any column names")
        match.names = make.names(final.names, unique=TRUE)  # make duplicate names unique, and convert NA colnames to "NA."
        for (i in seq.int(1,n)) if (!is.null(original.names[[i]])) {
            tmp = make.names(original.names[[i]], unique=TRUE)   # make.names for the NA in test #1006
            if (!all(tmp %chin% match.names))
                stop("Some colnames of argument ",i," (",paste(setdiff(names(allargs[[i]]),match.names),collapse=","),") are not present in colnames of item 1. If an argument has colnames they can be in a different order, but they must all be present. Alternatively, you can supply unnamed lists and they will then be joined by position. Or, set use.names=FALSE.")
            if (!all(tmp == match.names))
                if (missing(use.names)) warning("Argument ",i," has names in a different order. Columns will be bound by name for consistency with base. You can supply unnamed lists and the columns will then be joined by position, or set use.names=FALSE. Alternatively, explicitly setting use.names to TRUE will remove this warning.")
        }
        ret = rbindlist(lapply(seq_along(allargs), function(x) {
            tt = allargs[[x]]
            if (!is.null(original.names[[x]]))
                setcolorder(tt, chmatch(match.names, make.names(original.names[[x]], unique=TRUE)))
            tt
        }))
        setnames(ret, final.names)
        return(ret)
    }
}

as.data.table = function(x, keep.rownames=FALSE)
{
    if (is.null(x))
        return(null.data.table())
    UseMethod("as.data.table")
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
        if (!identical(class(x),c("data.table","data.frame"))) stop("data.table inherits from data.frame (from v1.5) but this data.table does not. Has it been created manually (e.g. by using 'structure' rather than 'data.table') or saved to disk using a prior version of data.table? The correct class is c('data.table','data.frame').")
        return(`dimnames.data.frame`(x))
    }
    list(NULL, names(x))
}

"dimnames<-.data.table" = function (x, value)   # so that can do  colnames(dt)=<..>  as well as names(dt)=<..>
{
    if (!cedta()) return(`dimnames<-.data.frame`(x,value))  # won't maintain key column (if any). Revisit if ever causes a compatibility problem but don't think it's likely that packages change column names using dimnames<-. See names<-.data.table below.
    warning("The dimnames(x)<-value syntax copies the whole table. This is due to <- in R itself. Please change to setnames() which doesn't copy and is faster. See help('setnames'). You can safely ignore this warning if it is inconvenient to change right now. Setting options(warn=2) turns this warning into an error, so you can then use traceback() to find and change your dimnames<- calls.")
    if (!is.list(value) || length(value) != 2) stop("attempting to assign invalid object to dimnames of a data.table")
    if (!is.null(value[[1L]])) stop("data.tables do not have rownames")
    if (ncol(x) != length(value[[2]])) stop("can't assign",length(value[[2]]),"colnames to a",ncol(x),"column data.table")
    setnames(x,as.character(value[[2]]))
    x  # it's this returned value that is copied via *tmp* and we cannot avoid that when using <- currently in R
}

"names<-.data.table" = function(x,value)
{
    # When non data.table aware packages change names, we'd like to maintain the key, too.
    # If call is names(DT)[2]="newname", R will call this names<-.data.table function (notice no i) with 'value' already prepared to be same length as ncol
    caller = as.character(sys.call(-2L))[1L]
    if ( ((tt<-identical(caller,"colnames<-")) && cedta(3)) ||
         cedta() ) warning("The ",if(tt)"col","names(x)<-value syntax copies the whole table. This is due to <- in R itself. Please change to setnames(x,old,new) which does not copy and is faster. See help('setnames'). You can safely ignore this warning if it is inconvenient to change right now. Setting options(warn=2) turns this warning into an error, so you can then use traceback() to find and change your ",if(tt)"col","names<- calls.")
    if (is.null(value))
        setattr(x,"names",NULL)   # e.g. plyr::melt() calls base::unname()
    else
        setnames(x,value)
    x   # it's this returned value that is copied via *tmp* and we cannot avoid that when using <- currently in R
}

within.data.table <- function (data, expr, ...)
# basically within.list but retains key (if any)
# will be slower than using := or a regular query (see ?within for further info).
{
    if (!cedta()) return(NextMethod())
    parent <- parent.frame()
    e <- evalq(environment(), data, parent)
    eval(substitute(expr), e)  # might (and it's known that some user code does) contain rm()
    l <- as.list(e)
    l <- l[!sapply(l, is.null)]
    nD <- length(del <- setdiff(names(data), (nl <- names(l))))
    ans = copy(data)
    if (length(nl)) ans[,nl] <- l
    if (nD) ans[,del] <- NULL
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


transform.data.table <- function (`_data`, ...)
# basically transform.data.frame with data.table instead of data.frame, and retains key
{
    if (!cedta()) return(NextMethod())
    e <- eval(substitute(list(...)), `_data`, parent.frame())
    tags <- names(e)
    inx <- chmatch(tags, names(`_data`))
    matched <- !is.na(inx)
    if (any(matched)) {
        `_data`[,inx[matched]] <- e[matched]
        `_data` <- data.table(`_data`)
    }
    if (!all(matched)) {
        ans <- do.call("data.table", c(list(`_data`), e[!matched]))
    } else {
        ans <- `_data`
    }
    key.cols <- key(`_data`)
    if (!any(tags %chin% key.cols)) {
        setattr(ans, "sorted", key.cols)
    }
    ans
}

subset.data.table <- function (x, subset, select, ...)
{
    key.cols <- key(x)

    if (missing(subset)) {
        r <- TRUE
    } else {
        e <- substitute(subset)
        r <- eval(e, x, parent.frame())
        if (!is.logical(r))
            stop("'subset' must evaluate to logical")
        r <- r & !is.na(r)
    }

    if (missing(select)) {
        vars <- seq_len(ncol(x))
    } else {
        nl <- as.list(seq_len(ncol(x)))
        setattr(nl,"names",names(x))
        vars <- eval(substitute(select), nl, parent.frame())  # e.g.  select=colF:colP
        if (is.numeric(vars)) vars=names(x)[vars]
        key.cols <- intersect(key.cols, vars) ## Only keep key.columns found in the select clause
    }

    ans <- x[r, vars, with = FALSE]

    if (nrow(ans) > 0L) {
        if (!missing(select) && length(key.cols)) {
            ## Set the key on the returned data.table as long as the key
            ## columns that "remain" are the same as the original, or a
            ## prefix of it.
            is.prefix <- all(key(x)[seq_len(length(key.cols))] == key.cols)
            if (is.prefix) {
                setattr(ans, "sorted", key.cols)
            }
        }
    } else {
        setkey(ans,NULL)
    }
    ans
}

na.omit.data.table <- function (object, ...)
{
    if (!cedta()) return(NextMethod())
    omit = FALSE
    for (i in seq_len(ncol(object))) omit = omit | is.na(object[[i]])
    object[!omit]
    # compare the above to stats:::na.omit.data.frame
}

is.na.data.table <- function (x) {
    if (!cedta()) return(`is.na.data.frame`(x))
    do.call("cbind", lapply(x, "is.na"))
}

# not longer needed as inherits ...
#    t.data.table <- t.data.frame
#    Math.data.table <- Math.data.frame
#    summary.data.table <- summary.data.frame

Ops.data.table <- function(e1, e2 = NULL)
{
    ans = NextMethod()
    if (cedta() && is.data.frame(ans))
        ans = as.data.table(ans)
    ans
}


split.data.table = function(...) {
    if (cedta() && getOption("datatable.dfdispatchwarn"))  # or user can use suppressWarnings
        warning("split is inefficient. It copies memory. Please use [,j,by=list(...)] syntax. See data.table FAQ.")
    NextMethod()  # allow user to do it though, split object will be data.table's with 'NA' repeated in row.names silently
}

# TO DO, add more warnings e.g. for by.data.table(), telling user what the data.table syntax is but letting them dispatch to data.frame if they want

copy = function(x) {
    newx = .Call(Ccopy,x)  # copies at length but R's duplicate() also copies truelength over.
                           # TO DO: inside Ccopy it could reset tl to 0 or length, but no matter as selfrefok detects it
                           # TO DO: revisit duplicate.c in R 3.0.3 and see where it's at
    if (!is.data.table(x)) return(newx)   # e.g. in as.data.table.list() the list is copied before changing to data.table
    setattr(newx,".data.table.locked",NULL)
    alloc.col(newx)
}

copyattr = function(from, to) {
    .Call(Ccopyattr, from, to)
}

point = function(to, to_idx, from, from_idx) {
    .Call(CpointWrapper, to, to_idx, from, from_idx)
}

shallow = function(x) {
    if (!is.data.table(x)) stop("x is not a data.table. Shallow copy is a copy of the vector of column pointers (only), so is only meaningful for data.table")
    .Call(Cshallowwrapper,x)  # copies VECSXP only
}

alloc.col = function(DT, n=getOption("datatable.alloccol"), verbose=getOption("datatable.verbose"))
{
    name = substitute(DT)
    if (identical(name,quote(`*tmp*`))) stop("alloc.col attempting to modify `*tmp*`")
    ans = .Call(Calloccolwrapper,DT,as.integer(eval(n)),verbose)
    for (i in seq_along(ans)) {
        # clear the same excluded by copyMostAttrib(). Primarily for data.table and as.data.table, but added here centrally (see #4890).
        setattr(ans[[i]],"names",NULL)
        setattr(ans[[i]],"dim",NULL)
        setattr(ans[[i]],"dimnames",NULL)
    }
    if (is.name(name)) {
        name = as.character(name)
        assign(name,ans,parent.frame(),inherits=TRUE)
    }
    .Call(Csetnamed,ans,0L)
}

selfrefok = function(DT,verbose=getOption("datatable.verbose")) {
    .Call(Cselfrefokwrapper,DT,verbose)
}

truelength = function(x) .Call(Ctruelength,x)
# deliberately no "truelength<-" method.  alloc.col is the mechanism for that.
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
    if (name=="names" && is.data.table(x) && length(attr(x,"names")) && !is.null(value))
        setnames(x,value)
        # Using setnames here so that truelength of names can be retained, to carry out integrity checks such as not
        # creating names longer than the number of columns of x, and to change the key, too
        # For convenience so that setattr(DT,"names",allnames) works as expected without requiring a switch to setnames.
    else
        .Call(Csetattrib, x, name, value)
        # If name=="names" and this is the first time names are assigned (e.g. in data.table()), this will be grown
        # by alloc.col very shortly afterwards in the caller.
    invisible(x)
}

setnames = function(x,old,new) {
    # Sets by reference, maintains truelength, no copy of table at all.
    # But also more convenient than names(DT)[i]="newname"  because we can also do setnames(DT,"oldname","newname")
    # without an onerous match() ourselves. old can be positions, too, but we encourage by name for robustness.
    if (!is.data.frame(x)) stop("x is not a data.table or data.frame")
    if (!length(attr(x,"names"))) stop("x has no column names")  # because setnames is for user user. Internally, use setattr(x,"names",...)
    if (missing(new)) {
        # for setnames(DT,new); e.g., setnames(DT,c("A","B")) where ncol(DT)==2
        if (!is.character(old)) stop("Passed a vector of type '",typeof(old),"'. Needs to be type 'character'.")
        if (length(old) != ncol(x)) stop("Can't assign ",length(old)," names to a ",ncol(x)," column data.table")
        m = chmatch(key(x), names(x))
        if (length(m) && any(is.na(m))) stop("Internal error: attr(x,'sorted') not all in names(x)")
        .Call(Csetcharvec, names(x), seq_along(names(x)), old)
        # setcharvec (rather than setattr) so as not to affect selfref.
        if (length(m)) .Call(Csetcharvec, attr(x,"sorted"), seq_along(key(x)), old[m])
        return(invisible(x))
    } else {
        if (missing(old)) stop("When 'new' is provided, 'old' must be provided too")
        if (length(new)!=length(old)) stop("'old' is length ",length(old)," but 'new' is length ",length(new))
    }
    if (is.numeric(old)) {
        tt = old<1L | old>length(x) | is.na(old)
        if (any(tt)) stop("Items of 'old' either NA or outside range [1,",length(x),"]: ",paste(old[tt],collapse=","))
        i = as.integer(old)
        if (any(duplicated(i))) stop("Some duplicates exist in 'old': ",paste(i[duplicated(i)],collapse=","))
    } else {
        if (!is.character(old)) stop("'old' is type ",typeof(old)," but should be integer, double or character")
        if (any(duplicated(old))) stop("Some duplicates exist in 'old': ", paste(old[duplicated(old)],collapse=","))
        i = chmatch(old,names(x))
        if (any(is.na(i))) stop("Items of 'old' not found in column names: ",paste(old[is.na(i)],collapse=","))
        if (any(tt<-!is.na(chmatch(old,names(x)[-i])))) stop("Some items of 'old' are duplicated (ambiguous) in column names: ",paste(old[tt],collapse=","))
    }
    if (!is.character(new)) stop("'new' is not a character vector")
    if (length(names(x)) != length(x)) stop("dt is length ",length(dt)," but its names are length ",length(names(x)))

    # update the key too if the column name being change is in the key
    m = chmatch(names(x)[i], key(x))
    w = which(!is.na(m))
    .Call(Csetcharvec, attr(x,"names"), as.integer(i), new)
    if (length(w))
        .Call(Csetcharvec, attr(x,"sorted"), as.integer(m[w]), new[w])
    invisible(x)
}

setcolorder = function(x,neworder)
{
    if (!is.data.table(x)) stop("x is not a data.table")
    if (length(neworder)!=length(x)) stop("neworder is length ",length(neworder)," but x has ",length(x)," columns.")
    if (is.character(neworder)) {
        if (any(duplicated(neworder))) stop("neworder contains duplicate column names")
        if (any(duplicated(names(x)))) stop("x has some duplicated column name(s): ",paste(names(x)[duplicated(names(x))],collapse=","),". Please remove or rename the duplicate(s) and try again.")
        o = as.integer(chmatch(neworder,names(x)))
        if (any(is.na(o))) stop("Names in neworder not found in x: ",paste(neworder[is.na(o)],collapse=","))
    } else {
        if (!is.numeric(neworder)) stop("neworder is not a character or numeric vector")
        o = as.integer(neworder)
        m = !(o %in% seq_len(length(x)))
        if (any(m)) stop("Column numbers in neworder out of bounds: ",paste(o[m],collapse=","))
    }
    .Call(Csetcolorder,x,o)
    invisible(x)
}

set = function(x,i=NULL,j,value)  # low overhead, loopable
{
    if (is.atomic(value)) {
        # protect NAMED of atomic value from .Call's NAMED=2 by wrapping with list()
        l = vector("list",1)
        .Call(Csetlistelt,l,1L,value)  # to avoid the copy by list() in R < 3.1
        value = l
    }
    .Call(Cassign,x,i,j,NULL,value,FALSE)   #  verbose=FALSE for speed to avoid getOption()  TO DO: somehow read getOption("datatable.verbose") from C level
    invisible(x)
}

chmatch = function(x,table,nomatch=NA_integer_)
    .Call(Cchmatchwrapper,x,table,as.integer(nomatch),FALSE)

"%chin%" = function(x,table) {
    # TO DO  if table has 'ul' then match to that
    .Call(Cchmatchwrapper,x,table,NA_integer_,TRUE)
}

chorder = function(x) {
    o = forderv(x, sort=TRUE, retGrp=FALSE)
    if (length(o)) o else seq_along(x)
}

chgroup = function(x) {
    # TO DO: deprecate and remove this. It's exported but doubt anyone uses it. Think the plan was to use it internally, but forderv superceded.
    o = forderv(x, sort=FALSE, retGrp=TRUE)
    if (length(o)) as.vector(o) else seq_along(x)  # as.vector removes the attributes
}

rbindlist = function(l) {
    ans = .Call(Crbindlist,l)
    if (!length(ans)) return(null.data.table())
    setattr(ans,"row.names",.set_row_names(length(ans[[1L]])))
    setattr(ans,"class",c("data.table","data.frame"))
    alloc.col(ans)
}

vecseq = function(x,y,clamp) .Call(Cvecseq,x,y,clamp)

address = function(x) .Call(Caddress,x)

":=" = function(...) stop('Check that is.data.table(DT) == TRUE. Otherwise, := and `:=`(...) are defined for use in j, once only and in particular ways. See help(":=").')

setDT <- function(x, giveNames=TRUE) {
    giveNames <- as.logical(giveNames[1L])
    name = substitute(x)
    if (is.na(giveNames))
        stop("Argument 'giveNames' to 'setDT' must be logical TRUE/FALSE")
    if (is.data.table(x)) {
        return(invisible(x))
    } else if (is.data.frame(x)) {
        setattr(x, "row.names", .set_row_names(nrow(x)))
        tt = class(x)
        n = chmatch("data.frame", tt)
        tt = c(head(tt, n - 1L), "data.table", "data.frame", tail(tt, 
            length(tt) - n))
        setattr(x, "class", tt)
        alloc.col(x)
    } else if (is.list(x)) {
        # copied from as.data.table.list - except removed the copy
        if (!length(x)) return( null.data.table() )
        n = vapply(x, length, 0L)
        mn = max(n)
        if (any(n<mn))
            stop("All elements in argument 'x' to 'setDT' must be of same length")
        xn = names(x)
        if (is.null(xn)) {
            if (giveNames) setattr(x, "names", paste("V",seq_len(length(x)),sep=""))
            else setattr(x, "names", rep("", length(x)))
        } else {
            idx = xn == ""
            if (any(idx) && giveNames) {
                xn[idx] = paste("V", seq_along(which(idx)), sep="")
                setattr(x, "names", xn)
            }
        }
        setattr(x,"row.names",.set_row_names(max(n)))
        setattr(x,"class",c("data.table","data.frame"))
        alloc.col(x)
    } else {
        stop("Argument 'x' to 'setDT' should be a 'list', 'data.frame' or 'data.table'")
    }
    if (is.name(name)) {
        name = as.character(name)
        assign(name, x, parent.frame(), inherits=TRUE)
    }
    invisible(x)
}

gsum = function(x, na.rm=FALSE) .Call(Cgsum, x, na.rm)
gmean = function(x, na.rm=FALSE) .Call(Cgmean, x, na.rm)
gstart = function(o, f, l) .Call(Cgstart, o, f, l)
gend = function() .Call(Cgend)


