
dim.data.table <- function(x) {
    if (length(x)) c(length(x[[1L]]), length(x))
    else c(0L,0L)
    # TO DO: consider placing "dim" as an attibute updated on inserts. Saves this 'if'.
}

.global = new.env()  # thanks to: http://stackoverflow.com/a/12605694/403310
.global$print = TRUE
.global$depthtrigger =
    if (exists(".global",.GlobalEnv) || getRversion() < "2.14.0") {
        7L # this is either development, or pre 2.14, where package isn't compiled
    } else {
        2L # normal value when package is loaded in 2.14+ (where functions are compiled in namespace)
    }

print.data.table = function(x,
    topn=getOption("datatable.print.topn"),   # (5) print the top topn and bottom topn rows with '---' inbetween
    nrows=getOption("datatable.print.nrows"), # (100) under this the whole (small) table is printed, unless topn is provided
    digits=NULL, ...)
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
           cat("NULL data.table\n")
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
    toprint=format.data.table(toprint, digits=digits, na.encode = FALSE)
    rownames(toprint)=paste(format(rn,right=TRUE),":",sep="")
    if (printdots) {
        toprint = rbind(head(toprint,topn),"---"="",tail(toprint,topn))
        rownames(toprint) = format(rownames(toprint),justify="right")
        print(toprint,right=TRUE,quote=FALSE)
        return(invisible())
    }
    if (nrow(toprint)>20L)
        # repeat colnames at the bottom if over 20 rows so you don't have to scroll up to see them
        toprint=rbind(toprint,matrix(names(x),nrow=1))
    print(toprint,right=TRUE,quote=FALSE)
    invisible()
}

format.data.table <- function (x, ..., justify = "none") {
    format.item = function(x) {
        if (is.atomic(x))
            paste(c(head(x,6),if(length(x)>6)""),collapse=",")
        else
            paste("<",class(x)[1L],">",sep="")
    }
    do.call("cbind",lapply(x,function(col,...){
        if (is.list(col))
            col = sapply(col,format.item)
        format(col,justify=justify,...)
    }))
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
    settruelength(ans,0L)
    alloc.col(ans)
}

data.table = function(..., keep.rownames=FALSE, check.names=FALSE, key=NULL)
{
    # creates a data.table directly, without the overhead of creating a data.frame first (ie the overhead of rownames)
    # you can convert from matrix and data.frame, to data.table, and retain the rownames in the first column - see as.data.table.matrix()
    # multiple matrices, and data.frames can be passed in (each with colnames) and each of the colnames will be retained in the new data.table
    # DONE: allow silent repition of input arguments, like data.frame() does
    # DONE: expression text is checked to be strict name before using as a column name.
    # DONE: if M is a matrix with cols a,b and c,  data.table(A=M,B=M) will create colnames A.a,A.b,A.c,B.a,B,b,B.c.  Also  data.table(M,M) will use make.names to make the columns unique (adds .1,.2,.3)
    # NOTE: It may be faster in some circumstances to create a data.table by creating a list l first, and then class(l)="data.table" at the expense of checking.
    #  TO DO: Could use nargs()-!missing(keep.rownames)-!missing(check.names)-!missing(key) to get length of `...`
    x <- list(...)  # NAMED OBJECTS WILL BE A COPY HERE, TO DO: avoid.
    # TO DO: use ..1 instead, but not clear exactly how in this case.
    # One reason data.table() is needed, different and independent from data.frame(), is to allow list() columns.
    if (identical(x, list(NULL))) return( null.data.table() )
    # the arguments to the data.table() function form the column names,  otherwise the expression itself
    tt <- as.list(substitute(list(...)))[-1L]  # Intention here is that data.table(X,Y) will automatically put X and Y as the column names.  For longer expressions, name the arguments to data.table(). But in a call to [.data.table, wrap in list() e.g. DT[,list(a=mean(v),b=foobarzoo(zang))] will get the col names
    vnames = names(tt)
    exptxt = as.character(tt)
    if (is.null(vnames)) vnames = rep("",length(x))
    vnames[is.na(vnames)] = ""
    novname = vnames==""
    if (any(!novname)) {
        if (any(vnames[!novname] == ".SD")) stop("A column may not be called .SD. That has special meaning.")
    }
    if (any(novname) && length(exptxt)==length(vnames)) {
        okexptxt =  exptxt[novname] == make.names(exptxt[novname])
        vnames[novname][okexptxt] = exptxt[novname][okexptxt]
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
        if (is.matrix(xi) || is.data.frame(xi)) {  # including data.table (a data.frame, too)
            xi = as.data.table(xi, keep.rownames=keep.rownames)       # TO DO: allow a matrix to be a column of a data.table. This could allow a key'd lookup to a matrix, not just by a single rowname vector, but by a combination of several columns. A matrix column could be stored either by row or by column contiguous in memory.
            x[[i]] = xi
            numcols[i] = length(xi)
        }
        nrows[i] <- NROW(xi)    # for a vector (including list() columns) returns the length
        if (numcols[i]>0L) {
            namesi <- names(xi)  # works for both data.frame's, matrices and data.tables's
            if (length(namesi)==0L) namesi = rep("",ncol(xi))
            namesi[is.na(namesi)] = ""
            tt = namesi==""
            if (any(tt)) namesi[tt] = paste("V", which(tt), sep = "")
            if (novname[i]) vnames[[i]] = namesi
            else vnames[[i]] = paste(vnames[[i]], namesi, sep=".")
        }
    }
    nr <- max(nrows)
    for (i in which(nrows < nr)) {
        # TO DO ... recycle in C, but not high priority as large data already regular from database or file
        xi <- x[[i]]
        if (identical(xi,list())) {
            x[[i]] = vector("list", nr)
            next
        }
        if (nrows[i]==0L) stop("Item ",i," has no length. Provide at least one item (such as NA, NA_integer_ etc) to be repeated to match the ",nr," rows in the longest column. Or, all columns can be 0 length, for insert()ing rows into.")
        if (nr%%nrows[i] == 0L) {
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
        }
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
    }
    alloc.col(value)  # returns a NAMED==0 object, unlike data.frame()
}

is.sorted = function(x)identical(FALSE,is.unsorted(x))    # NA's anywhere need to result in 'not sorted' e.g. test 251 where i table is not sorted but f__ without NAs is sorted. Could check if i is sorted too, but that would take time and that's what SJ is for to make the calling code clear to the reader.

.massagei = function(x) {
    if (is.call(x) && as.character(x[[1L]]) %chin% c("J","."))
        x[[1L]] = quote(list)
    x
}

"[.data.table" = function (x, i, j, by, keyby, with=TRUE, nomatch=getOption("datatable.nomatch"), mult="all", roll=FALSE, rolltolast=FALSE, which=FALSE, .SDcols, verbose=getOption("datatable.verbose"), allow.cartesian=getOption("datatable.allow.cartesian"), drop=NULL)
{
    # ..selfcount <<- ..selfcount+1  # in dev, we check no self calls, each of which doubles overhead, or could
    # test explicitly if the caller is [.data.table (even stronger test. TO DO.)
    # the drop=NULL is to sink drop argument when dispatching to [.data.frame; using '...' stops test 147
    if (!cedta()) {
        Nargs = nargs() - (!missing(drop))
        ans = if (Nargs<3L) `[.data.frame`(x,i)  # drop ignored anyway by DF[i]
              else if (missing(drop)) `[.data.frame`(x,i,j)
              else `[.data.frame`(x,i,j,drop)
        if (!missing(i)) setkey(ans,NULL)  # See test 304
        return(ans)
    }
    if (!mult %chin% c("first","last","all")) stop("mult argument can only be 'first','last' or 'all'")
    if (roll && rolltolast) stop("roll and rolltolast cannot both be true")
    # TO DO. Removed for now ... if ((roll || rolltolast) && missing(mult)) mult="last" # for when there is exact match to mult. This does not control cases where the roll is mult, that is always the last one.
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
    if (!missing(by) && missing(j)) stop("'by' or 'keyby' is supplied but not j")
    irows = NULL  # Meaning all rows. We avoid creating 1:nrow(x) for efficiency.
    bywithoutby=FALSE
    potentialredundantby = FALSE
    notjoin = FALSE
    if (!missing(i)) {
        isub = substitute(i)
        if (is.call(isub) && isub[[1L]] == as.name("!")) {
            notjoin = TRUE
            if (!missingnomatch) stop("not-join '!' prefix is present on i but nomatch is provided. Please remove nomatch.");
            nomatch = 0L
            isub = isub[[2L]]
        }
        if (is.null(isub)) return( null.data.table() )
        if (is.call(isub) && isub[[1L]]=="eval") {  # TO DO: or ..()
            isub = eval(.massagei(isub[[2L]]), parent.frame(), parent.frame())
            if (is.expression(isub)) isub=isub[[1L]]
        }
        if (!is.name(isub))
            i = eval(.massagei(isub), x, parent.frame())
        else
            i = eval(isub, parent.frame(), parent.frame())
        if (is.matrix(i)) stop("i is invalid type (matrix). Perhaps in future a 2 column matrix could return a list of elements of DT (in the spirit of A[B] in FAQ 2.14). Please let datatable-help know if you'd like this, or add your comments to FR #1611.")
        if (is.logical(i)) {
            if (identical(i,NA)) i = NA_integer_  # see DT[NA] thread re recycling of NA logical
            else i[is.na(i)] = FALSE              # avoids DT[!is.na(ColA) & !is.na(ColB) & ColA==ColB], just DT[ColA==ColB]
        }
        if (is.null(i)) return( null.data.table() )
        if (is.character(i)) i = data.table(V1=i)   # for user convenience
        else if (identical(class(i),"list") && length(i)==1L && is.data.frame(i[[1L]])) i = as.data.table(i[[1L]])
        else if (identical(class(i),"list") || identical(class(i),"data.frame")) i = as.data.table(i)
        if (is.data.table(i)) {
            if (!haskey(x)) stop("When i is a data.table (or character vector), x must be keyed (i.e. sorted, and, marked as sorted) so data.table knows which columns to join to and take advantage of x being sorted. Call setkey(x,...) first, see ?setkey.")
            rightcols = chmatch(key(x),names(x))   # NAs here (i.e. invalid data.table) checked in binarysearch
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
                    if ((roll || rolltolast) && a==length(leftcols)) stop("Attempting roll join on factor column x.",names(x)[rc],". Only integer, double or character colums may be roll joined.")   # because the chmatch on next line returns NA for missing chars in x (rather than some integer greater than existing).
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
            f__ = integer(nrow(i))
            len__ = integer(nrow(i))
            allLen1 = TRUE
            if (verbose) {last.started.at=proc.time()[3];cat("Starting binary search ...");flush.console()}
            .Call(Cbinarysearch, i, x, as.integer(leftcols-1L), as.integer(rightcols-1L), haskey(i), roll, rolltolast, nomatch, sqrt(.Machine$double.eps), f__, len__, allLen1)
            if (verbose) {cat("done in",round(proc.time()[3]-last.started.at,3),"secs\n");flush.console}
            # length of input nomatch (single 0 or NA) is 1 in both cases.
            # When no match, len__ is 0 for nomatch=0 and 1 for nomatch=NA, so len__ isn't .N
            if (is.na(which)) return(which(if (notjoin) f__!=0L else is.na(f__)))
            for (ii in resetifactor) set(i,j=ii,value=origi[[ii]])
            if (mult=="all") {
                if (!missing(by) || which || missing(j) || !with || notjoin) {
                    irows = if (allLen1) f__ else vecseq(f__,len__,if(allow.cartesian)NULL else as.integer(max(nrow(x),nrow(i))))
                    if (!missing(by) && !which && !missing(j) && with && !notjoin)
                        potentialredundantby = TRUE
                } else {
                    bywithoutby=TRUE  # TO DO: use allLen1 to detect if joining to unique rows?
                    # Important that f__ is as long as nrow(i) (rather than taking away 0s), to know the i groups
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
                else if (!isTRUE(i)) irows=seq_len(nrow(x))[i]  # e.g. recycling DT[c(TRUE,FALSE),which=TRUE], for completeness
            } else {
                irows = as.integer(i)  # e.g. DT[c(1,3)]
                irows[irows>nrow(x)] = NA_integer_  # not needed for vector subsetting, but for is.unsorted to return NA
            }
        }
        if (notjoin) {
            if (bywithoutby || !is.integer(irows) || is.na(nomatch)) stop("Internal error: notjoin but bywithoutby or !integer or nomatch==NA")
            irows = irows[irows!=0L]
            i = irows = if (length(irows)) seq_len(nrow(x))[-irows] else NULL  # NULL meaning all rows i.e. seq_len(nrow(x))
            # Doing this once here, helps speed later when repeatedly subsetting each column. R's [irows] would do this for each
            # column when irows contains negatives.
        }
        if (which) return(if (is.null(irows)) seq_len(nrow(x)) else irows)
        if (missing(j)) {
            if (!is.data.table(i)) {
                if (is.null(irows)) return(copy(x))  # Must return a copy. Shallow copy only when i is missing.
            	ans = vector("list",ncol(x))
                for (s in seq_len(ncol(x))) {
                    ans[[s]] = x[[s]][irows]
                    copyattr(x[[s]],ans[[s]])
                }
                setattr(ans, "names", names(x))
                if (haskey(x) && (is.logical(i) || is.sorted(irows))) {
                    setattr(ans,"sorted",key(x))
                }
            } else {
                ans = vector("list",ncol(i)+ncol(x)-length(leftcols))
                inonjoin = seq_len(ncol(i))[-leftcols]
                if ((allLen1 || mult!="all") && (is.na(nomatch) || !any(f__==0L))) {
                    for (s in seq_along(leftcols)) ans[[s]] = i[[leftcols[s]]]  # won't copy so will retain attribs such as "comments"
                    for (s in seq_along(inonjoin)) ans[[s+ncol(x)]] = i[[inonjoin[s]]]
                } else {
                    ii = rep.int(seq_len(nrow(i)),len__)
                    for (s in seq_along(leftcols)) {ans[[s]] = i[[leftcols[s]]][ii]; copyattr(i[[leftcols[s]]], ans[[s]])}
                    for (s in seq_along(inonjoin)) {ans[[s+ncol(x)]] = i[[inonjoin[s]]][ii]; copyattr(i[[inonjoin[s]]], ans[[s+ncol(x)]])}
                }
                xnonjoin = seq_len(ncol(x))[-rightcols]
                if (is.null(irows))
                    for (s in seq_along(xnonjoin)) ans[[s+length(leftcols)]] = x[[xnonjoin[s]]]
                else
                    for (s in seq_along(xnonjoin)) {ans[[s+length(leftcols)]] = x[[xnonjoin[s]]][irows]; copyattr(x[[xnonjoin[s]]], ans[[s+length(leftcols)]])}
                setattr(ans, "names", make.unique(c(names(x)[rightcols],names(x)[-rightcols],names(i)[-leftcols])))
                if (haskey(i) || is.sorted(f__) || (is.na(nomatch) && any(is.na(f__)) && is.sorted(fastorder(i,leftcols))) ||
                                                   (nomatch==0L && any(f__==0L) && is.sorted(f__[f__!=0L])))
                    # TO DO: any(is.na()) could be anyNA() and any0, and we need an efficient is.unsorted(DT) method.
                    # But we only need is.sorted(i) when there are NA matches, so the above should rarely bite.
                    # Most efficient method would be || (is.na(nomatch) && anyNA(f__) && !is.unsorted(f__,na.rm=TRUE) && **where there are NA's, those rows in i are >= previous row in i**!
                    setattr(ans,"sorted",key(x))
            }
            setattr(ans,"class",c("data.table","data.frame"))
            setattr(ans,"row.names",.set_row_names(nrow(ans)))
            settruelength(ans,0L)  # what 2.14.0+ would be for consistency, not nrow(ans) (could be too, but then we couldn't drop this line later if we ever make a dependency on 2.14.0+)
            return(alloc.col(ans))
        }
    } # end of  if !missing(i)
    if (missing(j)) stop("logical error, j missing")
    jsub = substitute(j)
    if (!with && is.call(jsub) && jsub[[1]]==as.name("!")) {
        notj = TRUE
        jsub = jsub[[2]]
    } else notj = FALSE
    if (is.null(jsub)) return(NULL)
    jsubl = as.list.default(jsub)
    if (identical(jsubl[[1L]],quote(eval))) {
        jsub = eval(jsubl[[2L]], parent.frame(), parent.frame())
        if (is.expression(jsub)) jsub = jsub[[1L]]
    }
    lhs = NULL
    av = all.vars(jsub,TRUE)
    newnames = NULL
    if (length(av) && av[1L] == ":=") {
        if (identical(attr(x,".data.table.locked"),TRUE)) stop(".SD is locked. Using := in .SD's j is reserved for possible future use; a tortuously flexible way to modify by group. Use := in j directly to modify by group by reference.")
        if (notj) stop("doesn't make sense to combine !j with :=")
        if (.Call(CEvalDepth)<=.global$depthtrigger)
            .global$print = FALSE
        if (!is.null(irows)) {
            if (!length(irows)) {
                if (verbose) cat("No rows pass i clause so quitting := early with no changes made.\n")
                return(x)
            }
            if (!missing(keyby)) stop("When i is present, keyby := on a subset of rows doesn't make sense. Either change keyby to by, or remove i")
        }
        if (is.null(names(jsub))) {
            # regular LHS:=RHS usage, or `:=`(...) with no named arguments (an error)
            # `:=`(LHS,RHS) is valid though, but more because can't see how to detect that, than desire
            if (length(jsub)!=3L) stop("In `:=`(col1=val1, col2=val2, ...) form, all arguments must be named.")
            lhs = jsub[[2]]
            jsub = jsub[[3]]
            if (is.name(lhs) && with)
                lhs = as.character(lhs)
            else {
                if (!with && !is.name(lhs) && verbose) cat("with=FALSE ignored (LHS of := isn't a symbol)\n")  # TO DO: upgrade to warning
                lhs = eval(lhs, parent.frame(), parent.frame())
            }
            # to use a variable of column names on LHS, use eval(mycols):=  or get("mycols"):=, or with=FALSE for backwards compatibility
        } else {
            # `:=`(c2=1L,c3=2L,...)
            if (!with) warning("with=FALSE ignored. Not needed in `:=`(...) syntax.")
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
            # Adding new column(s). TO DO: move after the first eval incase the jsub has an error.
            newnames=setdiff(lhs,names(x))
            m[is.na(m)] = ncol(x)+seq_len(length(newnames))
            cols = as.integer(m)
            if ((ok<-selfrefok(x,verbose))==0L)   # ok==0 so no warning when loaded from disk (-1) [-1 considered TRUE by R]
                warning("Invalid .internal.selfref detected and fixed by taking a copy of the whole table, so that := can add this new column by reference. At an earlier point, this data.table has been copied by R. Avoid key<-, names<- and attr<- which in R currently (and oddly) may copy the whole data.table. Use set* syntax instead to avoid copying: setkey(), setnames() and setattr(). Also, list(DT1,DT2) will copy the entire DT1 and DT2 (R's list() copies named objects), use reflist() instead if needed (to be implemented). If this message doesn't help, please report to datatable-help so the root cause can be fixed.")
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
    } else if (!with) {
        if (!missing(by)) warning("'with=FALSE ignored when 'by' or 'keyby' is provided and not using :=")
        if (notj) j = eval(jsub, parent.frame(), parent.frame()) # else j will be evaluated for the first time on next line
        if (is.logical(j)) j <- which(j)
        if (!length(j)) return( null.data.table() )
        if (is.character(j)) {
            origj = j
            j = chmatch(j, names(x))
            if (any(is.na(j))) {
                if (notj) {
                    warning("column(s) not removed because not found: ",paste(origj[is.na(j)],collapse=","))
                    j = j[!is.na(j)]
                } else {
                    stop("column(s) not found: ",paste(origj[is.na(j)],collapse=","))
                }
            }
        }
        if (any(abs(j) > ncol(x) | j==0L)) stop("j out of bounds")
        if (any(j<0L) && any(j>0L)) stop("j mixes positive and negative")
        if (any(j<0L)) j = seq_len(ncol(x))[j]
        if (notj) j = seq_len(ncol(x))[-j]  # DT[,!"columntoexclude",with=FALSE], if a copy is needed, rather than :=NULL
        ans = vector("list",length(j))
        if (is.null(irows))
            for (s in seq_along(j)) ans[[s]] = x[[j[s]]]  # TO DO: return marked/safe shallow copy back to user
        else {
            if (is.data.table(i) && is.na(nomatch) && any(is.na(irows))) {   # TO DO: any(is.na()) => anyNA() and presave it
                if (any(j %in% rightcols)) ii = rep(seq_len(nrow(i)),len__)
                for (s in which(j %in% rightcols))
                    ans[[s]] = i[[leftcols[match(j[s],rightcols)]]][ii]
                    # So that NA matches from i get the i values, as xss expects further below.
                for (s in which(!j %in% rightcols))
                    ans[[s]] = x[[j[s]]][irows]
                # TO DO: allow i's non join columns to be included by name
            } else {
                for (s in seq_along(j)) ans[[s]] = x[[j[s]]][irows]
            }
        }
        setattr(ans, "names", names(x)[j])
        if (haskey(x) && all(key(x) %chin% names(ans)) && (missing(i) || is.logical(i) || (is.data.table(i) && haskey(i)) || is.sorted(irows))) {
            setattr(ans,"sorted",key(x))
            # TO DO: see ordered subset comments above
        }
        setattr(ans,"class",c("data.table","data.frame"))
        setattr(ans,"row.names",.set_row_names(nrow(ans)))
        settruelength(ans,0L)
        return(alloc.col(ans))
    }
    o__ = integer()
    xnrow = nrow(x)
    if (missing(by) && !bywithoutby) {
        if (is.null(irows)) {
            len__ = 0L   # len 0 here so that max(len__) is 0 later to leave NULL irows unchanged.
            bysameorder = TRUE  # 1st and only group is the entire table
        } else {
            len__ = length(irows)
            bysameorder = is.sorted(irows)
        }
        byval = list()
        bynames = allbyvars = NULL
    } else {
        if (bywithoutby) {
            # The groupings come instead from each row of the i data.table.
            # Much faster for a few known groups vs a 'by' for all followed by a subset
            if (!missing(by)) stop("logical error, by not missing in bywithoutby")
            if (!is.data.table(i)) stop("logicial error. i is not data.table, but mult='all' and 'by' is missing")
            byval = i
            bynames = head(key(x),length(leftcols))
            allbyvars = NULL
            bysameorder = haskey(i)
        } else {
            # Find the groups, using 'by' ...
            if (missing(by)) stop("logical error, by is missing")
            bysubl = as.list.default(bysub)
            bysuborig = bysub
            if (is.name(bysub) && !(as.character(bysub) %chin% names(x))) {
                bysub = eval(bysub, parent.frame(), parent.frame())
                bysubl = as.list.default(bysub)
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
            allbyvars = intersect(unlist(sapply(bysubl,all.vars,functions=TRUE)),names(x))
            if (potentialredundantby && all(sapply(bysubl,is.name)) && identical(allbyvars,names(x)[rightcols]) && options("datatable.warnredundantby")[[1L]]) {
                warning("by is not necessary in this query; it equals all the join columns in the same order. j is already evaluated by group of x that each row of i matches to (by-without-by, see ?data.table). Setting by will be slower because a subset of x is taken and then grouped again. Consider removing by, or changing it.")
            }
            orderedirows = is.sorted(irows)  # TRUE when irows is NULL (i.e. no i clause)
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
                if (!is.na(nomatch)) irows = irows[irows!=0L]
                if (length(allbyvars)) {
                    if (verbose) cat("i clause present and columns used in by detected, only these subset:",paste(allbyvars,collapse=","),"\n")
                    xss = x[irows,allbyvars,with=FALSE,nomatch=nomatch,mult=mult,roll=roll,rolltolast=rolltolast]
                } else {
                    if (verbose) cat("i clause present but columns used in by not detected. Having to subset all columns before evaluating 'by': '",deparse(by),"'\n",sep="")
                    xss = x[irows,nomatch=nomatch,mult=mult,roll=roll,rolltolast=rolltolast]
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
                    stop("'by' appears to evaluate to column names but isn't c() or key(). Use by=list(...) if you can. Otherwise, by=eval(",deparse(bysub),") should work. This is for efficiency so data.table can detect which columns are needed.")
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
                if (!typeof(byval[[jj]]) %chin% c("integer","logical","character","double")) stop("column or expression ",jj," of 'by' or 'keyby' is type ",typeof(byval[[jj]]),". Do not quote column names. Useage: DT[,sum(colC),by=list(colA,month(colB))]")
            }
            tt = sapply(byval,length)
            if (any(tt!=xnrow)) stop("The items in the 'by' or 'keyby' list are length (",paste(tt,collapse=","),"). Each must be same length as rows in x or number of rows returned by i (",xnrow,").")
            if (is.null(bynames)) bynames = rep("",length(byval))
            if (any(bynames=="")) {
                if (length(bysubl)<2) stop("When 'by' or 'keyby' is list() we expect something inside the brackets")
                for (jj in seq_along(bynames)) {
                    if (bynames[jj]=="") {
                        # Best guess. Use "month" in the case of by=month(date), use "a" in the case of by=a%%2
                        tt = grep("^eval|^[^[:alpha:] ]",all.vars(bysubl[[jj+1L]],functions=TRUE),invert=TRUE,value=TRUE)[1L]
                        if (!length(tt)) tt = all.vars(bysubl[[jj+1L]])[1L]
                        bynames[jj] = tt
                        # if user doesn't like this inferred name, user has to use by=list() to name the column
                    }
                }
            }
            setattr(byval, "names", bynames)  # byval is just a list not a data.table hence setattr not setnames
            if (verbose) {last.started.at=proc.time()[3];cat("Finding groups (bysameorder=",bysameorder,") ... ",sep="");flush.console()}
            if (length(byval) && length(byval[[1]])) {
                if (!bysameorder) {
                    o__ = fastorder(byval)
                    bysameorder = orderedirows && is.sorted(o__)
                    if (bysameorder) o__ = integer()   # skip the 1:xnrow vector for efficiency
                }
                if (bysameorder) {
                    f__ = duplist(byval)   # find group starts, given we know they are already grouped.
                    len__ = as.integer(c(diff(f__), xnrow-last(f__)+1L))
                    # TO DO: return both f__ and len__ from C level, and, there's a TO DO in duplist.R
                } else {
                    f__ = duplist(byval,order=o__)
                    len__ = as.integer(c(diff(f__), xnrow-last(f__)+1L))
                    firstofeachgroup = o__[f__]
                    origorder = sort.list(firstofeachgroup, na.last=FALSE, decreasing=FALSE)
                    # radixsort isn't appropriate in this case. 'firstofeachgroup' are row numbers from the
                    # (likely) large table so if nrow>100,000, range will be >100,000. Save time by not trying radix.
                    # TO DO: remove sort.list call by changing fastorder to fastgroup in first place.
                    f__ = f__[origorder]
                    len__ = len__[origorder]
                }
            } else {
                f__=NULL
                len__=0L
                bysameorder=TRUE   # for test 724
            }
            if (verbose) {cat("done in ",round(proc.time()[3]-last.started.at,3),"secs. bysameorder=",bysameorder," and o__ is length ",length(o__),"\n",sep="");flush.console}
            # TO DO: allow secondary keys to be stored, then we see if our by matches one, if so use it, and no need to sort again. TO DO: document multiple keys.
        }
    }
    # j to be run for each group (either from by, bywithoutby, or, whole DT when by is missing or length 0)
    jvnames = NULL
    if (mode(jsub)=="name") {
        # j is a single unquoted column name
        if (jsub!=".SD") {
            jvnames = gsub("^[.]N$","N",deparse(jsub))
            # jsub is list()ed after it's eval'd inside dogroups.
        }
    } else if (length(av) && av[1L] == "list") {
        jsubl = as.list.default(jsub)  # TO DO: names(jsub) and names(jsub)="" seem to work so make use of that
        if (length(jsubl)>1) {
            jvnames = names(jsubl)[-1L]   # check list(a=sum(v),v)
            if (is.null(jvnames)) jvnames = rep("", length(jsubl)-1L)
            for (jj in seq.int(2L,length(jsubl))) {
                if (jvnames[jj-1L] == "" && mode(jsubl[[jj]])=="name")
                    jvnames[jj-1L] = gsub("^[.]N$","N",deparse(jsubl[[jj]]))
                # TO DO: if call to a[1] for example, then call it 'a' too
            }
            setattr(jsubl, "names", NULL)  # drops the names from the list so it's faster to eval the j for each group. We'll put them back aftwards on the result.
            jsub = as.call(jsubl)
        } # else empty list is needed for test 468: adding an empty list column
    } # else maybe a call to transform or something which returns a list.
    ws = all.vars(jsub,TRUE)  # TRUE fixes bug #1294 which didn't see b in j=fns[[b]](c)
    if (".SD" %chin% ws) {
        if (missing(.SDcols)) {
            xvars = setdiff(names(x),union(bynames,allbyvars))   # TO DO: why allbyvars here too?
            # just using .SD in j triggers all non-by columns in the subset even if some of
            # those columns are not used. It would be tricky to detect whether the j expression
            # really does use all of the .SD columns or not, hence .SDcols for grouping
            # over a subset of columns
        } else {
            if (is.numeric(.SDcols)) {
                if (any(is.na(.SDcols)) || any(.SDcols>ncol(.SDcols)) || any(.SDcols<1L)) stop(".SDcols is numeric but out of bounds (or NA)")
                xvars = names(x)[.SDcols]
            } else {
                if (!is.character(.SDcols)) stop(".SDcols should be column numbers or names")
                if (any(is.na(.SDcols)) || any(!.SDcols %chin% names(x))) stop("Some items of .SDcols are not column names (or are NA)")
                xvars = .SDcols
            }
            # .SDcols might include grouping columns if users wants that, but normally we expect user not to include them in .SDcols
        }
    } else {
        if (!missing(.SDcols)) warning("This j doesn't use .SD but .SDcols has been passed in. Ignoring .SDcols. data.table inspects j anyway to see which columns are used, so there is only a need to provide .SDcols (for speed) when j uses .SD, and, you know or wish to specify a subset of columns for .SD. See ?data.table.")
        xvars = setdiff(intersect(ws,names(x)),bynames)
        if (verbose) cat("Detected that j uses these columns:",if (!length(xvars)) "<none>" else paste(xvars,collapse=","),"\n")
        # using a few named columns will be faster
        # Consider:   DT[,max(diff(date)),by=list(month=month(date))]
        # and:        DT[,sapply(.SD,sum),by=month(date)]
        # We don't want date in .SD in the latter, but we do in the former; hence the union() above.
    }
    #if (!length(xvars)) {
    #    Leave xvars empty. Important for test 607.
    #}
    if ("get" %chin% ws) {
        if (verbose) {
            cat("'get' found in j. xvars being set to all columns. Use .SDcols or eval(macro) instead. Both will detect the columns used which is important for efficiency.\nOld:", paste(xvars,collapse=","),"\n")
            # get('varname') is too difficult to detect which columns are used in general
            # eval(macro) column names are detected via the  if jsub[[1]]==eval switch earlier above.
        }
        xvars = setdiff(names(x),bynames)
        if (verbose) cat("New:",paste(xvars,collapse=","),"\n")
    }

    SDenv = new.env(parent=parent.frame())
    # hash=TRUE (the default) does seem better as expected using e.g. test 645
    # TO DO: experiment with size argument

    if (".N" %chin% xvars) stop("The column '.N' can't be grouped because it conflicts with the special .N variable. Try setnames(DT,'.N','N') first.")
    SDenv$.iSD = NULL  # null.data.table()
    if (bywithoutby) {
        jisvars = intersect(gsub("^i[.]","",ws),names(i))
        # JIS (non join cols) but includes join columns too (as there are named in i)
        if (length(jisvars)) {
            tt = min(nrow(i),1L)
            SDenv$.iSD = i[tt,jisvars,with=FALSE]
            for (ii in jisvars) {
                assign(ii, SDenv$.iSD[[ii]], SDenv)
                assign(paste("i.",ii,sep=""), SDenv$.iSD[[ii]], SDenv)
            }
        }
    }

    for (ii in names(SDenv$.BY)) assign(ii, SDenv$.BY[[ii]], SDenv)

    assign("print", function(x,...){base::print(x,...);NULL}, SDenv)
    # Now ggplot2 returns data from print, we need a way to throw it away otherwise j accumulates the result

    lockBinding(".iSD",SDenv)
    SDenv$.SD = null.data.table()  # e.g. test 607. Grouping still proceeds even though no .SD e.g. grouping key only tables, or where j consists of .N only
    SDenv$.N = as.integer(0)     # not 0L for the reson on next line :
    SDenv$.GRP = as.integer(1)   # oddly using 1L doesn't work reliably here! Possible R bug? TO DO: create reproducible example and report. To reproduce change to 1L and run test.data.table, test 780 fails. The assign seems ineffective and a previous value for .GRP from a previous test is retained, despite just creating a new SDenv.
    if ((missing(by) || !length(byval)) && !bywithoutby) {
        # No grouping, or mult="first"|"last", or by=NULL|character()|""|list()
        if (length(xvars)) {
            if (is.null(irows)) {
                SDenv$.SD = x[, xvars, with=FALSE]  # TO DO: *** shallow copy here. Could just have x, but then the columns of .SD wouldn't be consistent for lapply()ing etc, so need xvars here. Plus, if just x, then the lock below would lock x and that's untidy. Best is to shallow copy.
            } else {
                SDenv$.SD = x[irows, xvars, with=FALSE]   # Needs to be a copy of the i subset, even when that subset is contiguous, since we can't point to subsets of a vector (due to DATAPTR being an offset)
            }
            SDenv$.N = nrow(SDenv$.SD)
        } else {
            SDenv$.SD = null.data.table()   # no columns used by j so .SD can be empty. Only needs to exist so that we can rely on it being there when locking it below for example. If .SD were used by j, of course then xvars would be the columns and we wouldn't be in this leaf.
            SDenv$.N = if (is.null(irows)) nrow(x) else sum(!is.na(irows) & irows>0L)
        }
        SDenv$.I = seq_len(SDenv$.N)
        setattr(SDenv$.SD,".data.table.locked",TRUE)   # used to stop := modifying .SD via j=f(.SD), bug#1727. The more common case of j=.SD[,subcol:=1] was already caught when jsub is inspected for :=.
        lockBinding(".SD",SDenv)
        lockBinding(".N",SDenv)
        lockBinding(".I",SDenv)
        for (ii in xvars) assign(ii, SDenv$.SD[[ii]], SDenv)
        # Since .SD is inside SDenv, alongside its columns as variables, R finds .SD symbol more quickly, if used.
        # There isn't a copy of the columns here, the xvar symbols point to the SD columns (copy-on-write).

        jval = eval(jsub, SDenv, parent.frame())

        if (!is.null(lhs)) {
            if (verbose) cat("Assigning to ",if (is.null(irows)) "all " else paste(length(irows),"row subset of "), nrow(x)," rows\n",sep="")
            .Call(Cassign,x,irows,cols,newnames,jval,verbose)
            return(x)
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
        return(jval)
    }
    alloc = if (length(len__)) seq_len(max(len__)) else 0L
    SDenv$.I = alloc
    if (length(xvars)) {
        SDenv$.SD = x[alloc, xvars, with=FALSE]
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

    if (options("datatable.optimize")[[1L]]>0L) {  # Abilility to turn off if problems
        # Optimization to reduce overhead of calling mean and lapply over and over for each group
        nomeanopt=FALSE  # to be set by .optmean() using <<- inside it
        oldjsub = jsub
        if (is.call(jsub)) {
            if (jsub[[1L]]=="lapply" && jsub[[2L]]==".SD" && length(xvars)) {
                txt = as.list(jsub)[-1L]
                fun = txt[[2L]]
                if (is.call(fun) && fun[[1L]]=="function") {
                    assign("..FUN",eval(fun),SDenv)  # to avoid creating function() for each column of .SD
                    lockBinding("..FUN",SDenv)
                    txt[[1L]] = as.name("..FUN")
                } else {
                    if (is.character(fun)) fun = as.name(fun)
                    txt[[1L]] = fun
                }
                ans = vector("list",length(xvars)+1L)
                ans[[1L]] = as.name("list")
                for (ii in seq_along(xvars)) {
                    txt[[2L]] = as.name(xvars[ii])
                    ans[[ii+1L]] = as.call(txt)
                }
                jsub = as.call(ans)  # important no names here
                jvnames = xvars      # but here instead
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
            if (jsub[[1L]]=="list") {
                for (ii in seq_along(jsub)[-1L])
                    if (is.call(jsub[[ii]]) && jsub[[ii]][[1L]]=="mean")
                        jsub[[ii]] = .optmean(jsub[[ii]])
            } else if (jsub[[1L]]=="mean") {
                jsub = .optmean(jsub)
            }
        }
        if (nomeanopt) {
            warning("Unable to optimize call to mean() and could be very slow. Only mean(x) and mean(x,na.rm=TRUE) are optimized, and you must name 'na.rm' like that otherwise if you do mean(x,TRUE) the TRUE is taken to mean 'trim' which is the 2nd argument. 'trim' is not yet optimized.",immediate.=TRUE)
        }
        if (verbose) {
            if (!identical(oldjsub, jsub))
                cat("Optimized j from '",deparse(oldjsub),"' to '",deparse(jsub,width.cutoff=200),"'\n",sep="")
            else
                cat("Optimization is on but j left unchanged as '",deparse(jsub,width.cutoff=200),"'\n",sep="")
        }
        assign("Cfastmean", Cfastmean, SDenv)
        assign("mean", base::mean.default, SDenv)
        # Here in case nomeanopt=TRUE or some calls to mean weren't detected somehow. Better but still very slow.
        # Maybe change to :
        #     assign("mean", fastmean, SDenv)  # neater than the hard work above, but slower
        # when fastmean can do trim.
    }

    xcols = chmatch(xvars,names(x))
    if (bywithoutby) {
        groups = i
        grpcols = leftcols # 'leftcols' are the columns in i involved in the join (either head of key(i) or head along i)
        jiscols = chmatch(jisvars,names(i))  # integer() if there are no jisvars (usually there aren't, advanced feature)
        SDenv$.BY = groups[1L,grpcols,with=FALSE]
    } else {
        groups = byval
        grpcols = seq_along(byval)
        jiscols = NULL   # NULL rather than integer() is used in C to know when using by
        SDenv$.BY = lapply(byval,"[",1L)  # byval is list() not data.table
    }
    lockBinding(".BY",SDenv)
    grporder = o__
    if (length(irows) && !isTRUE(irows)) {
        if (length(o__) && length(irows)!=length(o__)) stop("Internal error: length(irows)!=length(o__)")
        o__ = if (length(o__)) irows[o__]  # better do this once up front (even though another alloc) than deep repeated branch in dogroups.c
              else irows
    } # else grporder is left bound to same o__ memory (no cost of copy)
    if (is.null(lhs)) cols=NULL
    if (!length(f__)) {
        # for consistency of empty case in test 184
        f__=len__=0L
    }

    if (verbose) {last.started.at=proc.time()[3];cat("Starting dogroups ... ");flush.console()}
    ans = .Call(Cdogroups, x, xcols, groups, grpcols, jiscols, grporder, o__, f__, len__, jsub, SDenv, cols, newnames, verbose)
    if (verbose) {cat("done dogroups in",round(proc.time()[3]-last.started.at,3),"secs\n");flush.console}

    # TO DO: xrows would be a better name for irows: irows means the rows of x that i joins to
    # Grouping by i: icols the joins columns (might not need), isdcols (the non join i and used by j), all __ are length x
    # Grouping by by: i is by val, icols NULL, o__ may be subset of x, f__ points to o__ (or x if !length o__)

    # TO DO: at the top of dogroups change xcols, grpcols, jiscols, irows, o__ and f__ to be 0-based than 1-based, by
    # reference in C, to save lots of -1 inside C for loops.

    # TO DO: don't want to eval j for every row of i unless it really is mult='all'
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
        return(x)
    }
    if (is.null(ans)) {
        ans = as.data.table.list(lapply(groups,"[",0L))  # side-effects only such as test 168
        setnames(ans,seq_along(bynames),bynames)   # TO DO: why doesn't groups have bynames in the first place?
        return(ans)
    }
    setattr(ans,"row.names",.set_row_names(length(ans[[1L]])))
    setattr(ans,"class",c("data.table","data.frame"))
    if (is.null(names(ans))) {
        # Efficiency gain of dropping names has been successful. Ordinarily this will run.
        if (is.null(jvnames)) jvnames = character(length(ans)-length(bynames))
        if (length(bynames)+length(jvnames)!=length(ans))
            stop("Internal error: jvnames is length ",length(jvnames), " but ans is ",length(ans)," and bynames is ", length(bynames))
        ww = which(jvnames=="")
        if (any(ww)) jvnames[ww] = paste("V",ww,sep="")
        setattr(ans, "names", c(bynames, jvnames))
    } else {
        setnames(ans,seq_along(bynames),bynames)   # TO DO: why doesn't .BY (where dogroups gets names when j is named list) have bynames?
    }
    if (haskey(x) && ((!missing(by) && bysameorder) || (!missing(i) && (haskey(i) || is.sorted(f__))))) {
        setattr(ans,"sorted",names(ans)[seq_along(grpcols)])
    } else if (!missing(keyby)) {
        setkeyv(ans,names(ans)[seq_along(byval)])
        # but if 'bykey' and 'bysameorder' then the setattr in branch above will run instead for
        # speed (because !missing(by) when bykey, too)
    }
    settruelength(ans,0L)  # TO DO: overallocate in dogroups in the first place and remove these lines
    alloc.col(ans)
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
    settruelength(value,0L)
    alloc.col(value)
}

as.data.table.data.frame = function(x, keep.rownames=FALSE)
{
    if (keep.rownames) return(data.table(rn=rownames(x), x, keep.rownames=FALSE))
    ans = copy(x)
    setattr(ans,"row.names",.set_row_names(nrow(x)))
    tt = class(x)
    n=chmatch("data.frame",tt)
    tt = c( head(tt,n-1L), "data.table","data.frame", tail(tt, length(tt)-n) )
    # for nlme::groupedData which has class c("nfnGroupedData","nfGroupedData","groupedData","data.frame")
    # See test 527.
    setattr(ans,"class",tt)
    settruelength(ans,0L)
    alloc.col(ans)
}

as.data.table.list = function(x, keep.rownames=FALSE) {
    if (!length(x)) return( null.data.table() )
    n = sapply(x,length)
    x = copy(x)
    if (any(n<max(n)))
        for (i in which(n<max(n))) x[[i]] = rep(x[[i]],length=max(n))
    if (is.null(names(x))) setattr(x,"names",paste("V",seq_len(length(x)),sep=""))
    setattr(x,"row.names",.set_row_names(max(n)))
    setattr(x,"class",c("data.table","data.frame"))
    settruelength(x,0L)
    alloc.col(x)
}

as.data.table.data.table = function(x, keep.rownames=FALSE) return(x)

head.data.table = function(x, n=6, ...) {
    if (!cedta()) return(NextMethod())
    i = seq_len(min(n,nrow(x)))
    x[i]
}
tail.data.table = function(x, n=6, ...) {
    if (!cedta()) return(NextMethod())
    i = seq.int(max(min(1,nrow(x)),nrow(x)-n+1), nrow(x))  # min for 0-row x
    x[i]
}

"[<-.data.table" = function (x, i, j, value) {
    # It is not recommended to use <-. Instead, use := for efficiency.
    # [<- is still provided for consistency and backwards compatibility, but we hope users don't use it.
    #settruelength(x,0L)  # it was copied to `*tmp*`, so truelength is length, now. But set to 0L to reset it and be safe.
    if (!cedta()) {
        x = if (nargs()<4) `[<-.data.frame`(x, i, value=value)
            else `[<-.data.frame`(x, i, j, value)
        settruelength(x,0L)     # TO DO: remove all settruelength(), no longer needed.
        return(alloc.col(x))    # over-allocate (again).   Avoid all this by using :=.
    }
    # TO DO: warning("Please use DT[i,j:=value] syntax instead of DT[i,j]<-value, for efficiency. See ?':='")
    if (!missing(i)) {
        isub=substitute(i)
        i = eval(.massagei(isub), x, parent.frame())
        if (is.matrix(i)) {
            if (!missing(j)) stop("When i is matrix in DT[i]<-value syntax, it doesn't make sense to provide j")
            x = `[<-.data.frame`(x, i, value=value)
            settruelength(x,0L)
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
    .Call(Cassign,x,i,cols,newnames,value,verbose)
    settruelength(x,0L) #  can maybe avoid this realloc, but this is (slow) [<- anyway, so just be safe.
    alloc.col(x)
    if (length(reinstatekey)) setkeyv(x,reinstatekey)
    invisible(x)
    # no copy at all if user calls directly; i.e. `[<-.data.table`(x,i,j,value)
    # or uses data.table := syntax; i.e. DT[i,j:=value]
    # but, there is one copy by R in [<- dispatch to `*tmp*`; i.e. DT[i,j]<-value
    # (IIUC, and, as of R 2.13.1
    # That copy is via main/duplicate.c which preserves truelength but copies length amount. Hence alloc.col(x,length(x)).
    # No warn passed to assign here because we know it'll be copied via *tmp*.
    # Simplest is ... just use := to avoid all this.
}

"$<-.data.table" = function(x, name, value) {
    # TO DO: remove these 2 lines.  ... settruelength(x,0L)
    # x = alloc.col(x)  #,length(x))
    if (!cedta()) {
        ans = `$<-.data.frame`(x, name, value)
        settruelength(ans,0L)    # alloc.col(ans,0L)  # length(ans))  # set to what is allocated, see above
        return(alloc.col(ans))           # over-allocate (again)
    }
    x = copy(x)
    `[<-.data.table`(x,j=name,value=value)  # important i is missing here
}

.rbind.data.table = function(...,use.names=TRUE) {
    # See FAQ 2.23
    # Called from base::rbind.data.frame
    match.names <- function(clabs, nmi) {
        if (all(clabs == nmi))
            NULL
        else if (all(nii <- chmatch(nmi, clabs, 0L)))
            nii
        else stop("names don't match previous names:\n\t", paste(nmi[nii ==
            0L], collapse = ", "))
    }
    allargs <- list(...)
    allargs <- allargs[sapply(allargs, length) > 0L]
    n <- length(allargs)
    if (n == 0L)
        return( null.data.table() )
    if (!all(sapply(allargs, is.list))) stop("All arguments to rbind must be lists (data.frame and data.table are already lists)")
    ncols = sapply(allargs, length)
    if (length(unique(ncols)) != 1L) {
        f=which(ncols!=ncols[1L])[1L]
        stop("All arguments to rbind must have the same number of columns. Item 1 has ",ncols[1L]," but item ",f," has ",ncols[f],"column(s).")
    }
    nm = names(allargs[[1L]])
    if (use.names && length(nm) && n>1L) {
        for (i in seq.int(2,n)) if (length(names(allargs[[i]]))) {
            if (!all(names(allargs[[i]]) %chin% nm))
                stop("Some colnames of argument ",i," (",paste(setdiff(names(allargs[[i]]),nm),collapse=","),") are not present in colnames of item 1. If an argument has colnames they can be in a different order, but they must all be present. Alternatively, you can drop names (by using an unnamed list) and the columns will then be joined by position. Or, set use.names=FALSE.")
            if (!all(names(allargs[[i]]) == nm))
                if (missing(use.names)) warning("Argument ",i," has names in a different order. Columns will be bound by name for consistency with base. You can drop names (by using an unnamed list) and the columns will then be joined by position, or set use.names=FALSE. Alternatively, explicitly setting use.names to TRUE will remove this warning.")
                allargs[[i]] = as.list(allargs[[i]])[nm]
                # TO DO : could use setcolorder() to speed this line up but i) it only currently works on data.table
                # (not data.frame or list) and ii) it would change the original by reference so would need to be copied
                # anyway. So, take a shallow copy somehow, or leave as-is and do the 'do.call(' below differently.
        }
    }
    allargs = lapply(allargs, as.data.table)  # To recycle items to match length if necessary, bug #2003. rbind will always be slow anyway, so not worrying about efficiency here. Later there'll be fast insert() by reference.

    if (!any(sapply(allargs[[1L]],is.factor)))
        return(rbindlist(allargs))  # do.call("c",...) is now in C
    # TO DO: Move earlier logic above for use.names (binding by name) into rbindlist C, too.
    l = lapply(seq_along(allargs[[1L]]), function(i) do.call("c", lapply(allargs, "[[", i)))
    # This is why we currently still need c.factor.
    # Now that character columns are allowed and recommended, c.factor isn't needed as much.
    # Also, rbind is not called as much as the past since dogroups.c has moved a lot into C.
    # TO DO: Convert factor to character first, so do.call() can be done by rbindlist too. Then
    # call factor() aferwards on those columns. Tidy up c.factor by removing it, depending on what
    # rbind.data.frame does, considering consistency.
    setattr(l,"names",nm)
    setattr(l,"row.names",.set_row_names(length(l[[1L]])))
    setattr(l,"class",c("data.table","data.frame"))
    settruelength(l,0L)
    alloc.col(l)
    # return(data.table(l))
    # much of the code in rbind.data.frame that follows this point is either to do with row.names, or coercing various types (and silent rep) which is already done by data.table. therefore removed.
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
    suppressWarnings(settruelength(ans,0L))
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
    if (!is.data.table(x)) return(newx)   # e.g. in as.data.table.list() the list is copied before changing to data.table
    setattr(newx,".data.table.locked",NULL)
    settruelength(newx,0L)
    alloc.col(newx)
    # TO DO: speedup duplicate.c using memcpy, suggested to r-devel, would benefit copy()
    # Could construct data.table and use memcpy ourselves but deep copies e.g. list() columns
    # may be tricky; more robust to rely on R's duplicate which deep copies.
}

copyattr = function(from, to) {
    .Call(Ccopyattr, from, to)
}

shallow = function(x) {
    if (!is.data.table(x)) stop("x is not a data.table. Shallow copy is a copy of the vector of column pointers (only), so is only meaningful for data.table")
    .Call(Cshallowwrapper,x)  # copies VECSXP only
}

alloc.col = function(DT, n=options("datatable.alloccol")[[1L]], verbose=options("datatable.verbose")[[1L]])
{
    name = substitute(DT)
    if (identical(name,quote(`*tmp*`))) stop("alloc.col attempting to modify `*tmp*`")
    ans = .Call(Calloccolwrapper,DT,as.integer(eval(n)),verbose)
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
# deliberately no "truelength<-" method.  alloc.col is the mechanism for that (maybe alloc.col should be renamed "truelength<-".

settruelength = function(x,n) {
    "Truly an internal function only. Users can call this using :::, but please don't."
    # if (n!=0L) warning("settruelength should only be used to set to 0, and is for 2.13.2-")
    #if (getRversion() >= "2.14.0")
    #    if (truelength(x) != 0L) warning("This is R>=2.14.0 but truelength isn't initialized to 0")
        # grep for suppressWarnings(settruelength) for where this is needed in 2.14.0+ (otherwise an option would be to make data.table depend on 2.14.0 so settruelength could be removed)


    .Call(Csettruelength,x,as.integer(n))



    #if (is.data.table(x))
    #    .Call(Csettruelength,attr(x,"class"),-999L)
    #    # So that (in R 2.13.2-) we can detect tables loaded from disk (tl is not initialized there)
}

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
        if (any(duplicated(names(x)))) stop("'old' is character but there are duplicate column names: ",paste(names(x)[duplicated(names(x))],collapse=","))
        i = chmatch(old,names(x))
        if (any(is.na(i))) stop("Items of 'old' not found in column names: ",paste(old[is.na(i)],collapse=","))
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
    if (any(duplicated(names(x)))) stop("x has some duplicated column name(s): ",paste(names(x)[duplicated(names(x))],collapse=","),". Please remove or rename the duplicate(s) and try again.")
    if (length(neworder)!=length(x)) stop("neworder is length ",length(neworder)," but x has ",length(x)," columns.")
    if (is.character(neworder)) {
        if (any(duplicated(neworder))) stop("neworder contains duplicate column names")
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

set = function(x,i=NULL,j,value)
{
    .Call(Cassign,x,i,j,NULL,value,FALSE)
    # TO DO: When R itself assigns to char vectors, check a copy is made and 'ul' lost, in tests.Rraw.
    # TO DO: When := or set() do it, make them aware of 'ul' and drop it if necessary.
    invisible(x)
}

chmatch = function(x,table,nomatch=NA_integer_)
    .Call(Cchmatchwrapper,x,table,as.integer(nomatch),FALSE)

"%chin%" = function(x,table) {
    # TO DO  if table has 'ul' then match to that
    .Call(Cchmatchwrapper,x,table,NA_integer_,TRUE)
}

chorder = function(x) .Call(Ccountingcharacter,x,TRUE)
chgroup = function(x) .Call(Ccountingcharacter,x,FALSE)

rbindlist = function(l) {
    ans = .Call(Crbindlist,l)
    if (!length(ans)) return(null.data.table())
    setattr(ans,"row.names",.set_row_names(length(ans[[1L]])))
    setattr(ans,"class",c("data.table","data.frame"))
    settruelength(ans,0L)
    alloc.col(ans)
}

vecseq = function(x,y,clamp) .Call(Cvecseq,x,y,clamp)

":=" = function(LHS,RHS) stop(':= is defined for use in j only, and (currently) only once; i.e., DT[i,col:=1L] and DT[,newcol:=sum(colB),by=colA] are ok, but not DT[i,col]:=1L, not DT[i]$col:=1L and not DT[,{newcol1:=1L;newcol2:=2L}]. Please see help(":="). Check is.data.table(DT) is TRUE.')


