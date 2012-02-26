
dim.data.table <- function(x) {
    if (length(x)) c(length(x[[1]]), length(x))
    else c(0,0)
    # TO DO: consider placing "dim" as an attibute updated on inserts. Saves this 'if'.
}

print.data.table = function (x, digits = NULL, quote = FALSE, right = TRUE, nrows = 100, ...)
{
    if (nrow(x) == 0) {
        cat("NULL data table\n")
        return()
    }
    if (nrow(x)>nrows) {
        if (missing(nrows)) nrows=10
        msg=paste("First",nrows,"rows of",nrow(x),"printed.")
        toprint = head(x,nrows)
    } else {
        toprint = x
        msg=""
    }
    cn = if (nrow(toprint)>20) colnames(x) else NULL  # only repeat colnames at the bottom if over 20 rows
    print(rbind(format.data.table(toprint, digits = digits, na.encode = FALSE), cn),
          digits=digits, quote=quote, right=right, ...)
    if (msg!="") cat(msg,"\n")
    invisible()
}

format.data.table <- function (x, ..., justify = "none") {
    do.call("cbind",lapply(x,format,justify=justify,...))
}

is.data.table = function(x) inherits(x, "data.table")
is.ff = function(x) inherits(x, "ff")  # define this in data.table so that we don't have to require(ff), but if user is using ff we'd like it to work

#NCOL = function(x) {
#    # copied from base, but additionally covers data.table via is.list()
#    # because NCOL in base explicity tests using is.data.frame()
#    if (is.list(x) && !is.ff(x)) return(length(x))
#    if (is.array(x) && length(dim(x)) > 1) ncol(x) else as.integer(1)
#}
#NROW = function(x) {
#    if (is.data.frame(x) || is.data.table(x)) return(nrow(x))
#    if (is.list(x) && !is.ff(x)) stop("List is not a data.frame or data.table. Convert first before using NROW")   # list may have different length elements, which data.table and data.frame's resolve.
#    if (is.array(x)) nrow(x) else length(x)
#}

null.data.table = function() {
    ans = list()
    setattr(ans,"class",c("data.table","data.frame"))
    setattr(ans,"row.names",.set_row_names(0))
    settruelength(ans,0L)
    alloc.col(ans)
}

data.table = function(..., keep.rownames=FALSE, check.names = TRUE, key=NULL)
{
    # creates a data.table directly, without the overhead of creating a data.frame first (ie the overhead of rownames)
    # you can convert from matrix and data.frame, to data.table, and retain the rownames in the first column - see as.data.table.matrix()
    # multiple matrices, and data.frames can be passed in (each with colnames) and each of the colnames will be retained in the new data.table
    # DONE: allow silent repition of input arguments, like data.frame() does
    # DONE: expression text is checked to be strict name before using as a column name.
    # DONE: if M is a matrix with cols a,b and c,  data.table(A=M,B=M) will create colnames A.a,A.b,A.c,B.a,B,b,B.c.  Also  data.table(M,M) will use make.names to make the columns unique (adds .1,.2,.3)
    # NOTE: It may be faster in some circumstances to create a data.table by creating a list l first, and then class(l)="data.table" at the expense of checking.
    x <- list(...)
    # TO DO: use ..1 instead, but not clear exactly how in this case.
    # One reason data.table() is needed, different and independent from data.frame(), is to allow list() columns.
    if (identical(x, list(NULL))) return( null.data.table() )
    # the arguments to the data.table() function form the column names,  otherwise the expression itself
    tt <- as.list(substitute(list(...)))[-1]  # Intention here is that data.table(X,Y) will automatically put X and Y as the column names.  For longer expressions, name the arguments to data.table(). But in a call to [.data.table, wrap in list() e.g. DT[,list(a=mean(v),b=foobarzoo(zang))] will get the col names
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
    if (n < 1)
        return( null.data.table() )
    if (length(vnames) != n) stop("logical error in vnames")
    vnames <- as.list.default(vnames)
    # ncols <- integer(n)        # the nrows and ncols of each of the inputs (could be varying lengths)
    nrows = integer(n)          # vector of lengths of each column. may not be equal if silent repetition is required.
    hascols = logical(n)
    for (i in 1:n) {
        xi = x[[i]]
        if (is.null(xi)) stop("column or argument ",i," is NULL")
        hascols[i] = FALSE   # list() columns are considered atomic
        if (is.matrix(xi) || is.data.frame(xi)) {  # including data.table (a data.frame, too)
            xi = as.data.table(xi, keep.rownames=keep.rownames)       # TO DO: allow a matrix to be a column of a data.table. This could allow a key'd lookup to a matrix, not just by a single rowname vector, but by a combination of several columns. A matrix column could be stored either by row or by column contiguous in memory.
            x[[i]] = xi
            hascols[i] = TRUE
        } else {
            if ((is.atomic(xi) || is.factor(xi) || is.array(xi)) && !is.list(xi)) {     # is.atomic is true for vectors with attributed, but is.vector() is FALSE. this is why we use is.atomic rather than is.vector
                # the is.array is required when, for example, you have a tapply inside a j=data.table(...) expression
                # if (is.atomic(xi) || is.array(xi)) xi = as.vector(xi)   # to remove any vector names, or dimnames in an array, or attributes, otherwise they get silently retaining in the data.table list members, thus bloating memory
                # if (is.factor(xi)) xi = as.character(xi)      # to use R_StringHash
                # possibly add && getOption("stringsAsfactors",FALSE)
                if (is.character(xi) && class(xi)!="AsIs") xi = factor(xi)  # coerce characters to factor unless wrapped in I()
                x[[i]] = xi
            }
        }
        # ncols[i] <- NCOL(xi)  # Returns 1 for vectors.
        nrows[i] <- NROW(xi)    # for a vector (including list() columns) returns the length
        if (hascols[i]) {
            namesi <- colnames(xi)  # works for both data.frame's, matrices and data.tables's
            if (length(namesi)==0) namesi = rep("",ncol(xi))
            namesi[is.na(namesi)] = ""
            tt = namesi==""
            if (any(tt)) namesi[tt] = paste("V", which(tt), sep = "")
            if (novname[i]) vnames[[i]] = namesi
            else vnames[[i]] = paste(vnames[[i]], namesi, sep=".")
        }
    }
    nr <- max(nrows)
    for (i in (1:n)[nrows < nr]) {
        xi <- x[[i]]
        if (identical(xi,list())) {
            x[[i]] = vector("list", nr)
            next
        }
        if (nrows[i]==0) stop("Item ",i," has no length. Provide at least one item (such as NA, NA_integer_ etc) to be repeated to match the ",nr," rows in the longest column. Or, all columns can be 0 length, for insert()ing rows into.")
        if (nr%%nrows[i] == 0) {
            if (is.atomic(xi) || is.list(xi)) {
                x[[i]] = rep(xi, length.out = nr)
                next
            }
            # don't know why this is here, take it out and see what bites
            # if (is.character(xi) && class(xi) == "AsIs") {
            #    cl <- class(xi)
            #    x[[i]] <- list(structure(rep(xi, length.out = nr), class = cl))
            #    next
            #}
            stop("problem recycling column ",i,", try a simpler type")
        }
        stop("arguments cannot be silently repeated to match max nr: ", paste(unique(nrows), collapse = ", "))
    }
    if (any(hascols)) {
        value = list()
        k = 1
        for(i in 1:n) {
            if (is.list(x[[i]]) && !is.ff(x[[i]])) {
                for(j in 1:length(x[[i]])) {
                    value[[k]] = x[[i]][[j]]
                    k=k+1
                }
            } else {
                value[[k]] = x[[i]]
                k=k+1
            }
        }
    } else {
        value = x
    }
    vnames <- unlist(vnames)
    if (check.names)
        vnames <- make.names(vnames, unique = TRUE)
    setattr(value,"names",vnames)
    setattr(value,"row.names",.set_row_names(nr))
    setattr(value,"class",c("data.table","data.frame"))
    alloc.col(value)
    if (!is.null(key)) {
      if (!is.character(key)) stop("key argument of data.table() must be character")
      if (length(key)==1) {
          key = strsplit(key,split=",")[[1]]
          # eg key="A,B"; a syntax only useful in key argument to data.table(), really.
      }
      setkeyv(value,key)
    }
    value
    #alloc.col(value)
    # TO DO. Revisit this comment. Applied when 'alloc.col(value)' was the last line....Despite the thread on r-devel "Confused about NAMED" about data.frame(), returning it this way makes a NAM(1) data.table. Returning "value=alloc.col(value);value" is the same but returns a NAM(2) data.table. Unlike data.frame which is always NAMED==2. We ignore NAMED in data.table generally, but it would be useful to avoid the grow warning when NAMED=1. However the print method still bumps NAMED (as it does in data.frame) (TO DO - stop that bump and warn on grow via := will be even less often).
}


"[.data.table" = function (x, i, j, by=NULL, with=TRUE, nomatch=getOption("datatable.nomatch",NA), mult="all", roll=FALSE, rolltolast=FALSE, which=FALSE, .SDcols, verbose=getOption("datatable.verbose",FALSE), drop=NULL)
{
    # the drop=NULL is to sink drop argument when dispatching to [.data.frame; using '...' stops test 147
    if (!cedta()) {
        ans = if (missing(drop)) `[.data.frame`(x,i,j) else `[.data.frame`(x,i,j,drop)
        if (!missing(i)) setkey(ans,NULL)  # See test 304
        return(ans)
    }
    if (!missing(by) && missing(j)) stop("'by' is supplied but not j")
    if (!mult %in% c("first","last","all")) stop("mult argument can only be 'first','last' or 'all'")
    if (roll && rolltolast) stop("roll and rolltolast cannot both be true")
    # TO DO. Removed for now ... if ((roll || rolltolast) && missing(mult)) mult="last" # for when there is exact match to mult. This does not control cases where the roll is mult, that is always the last one.
    if (!(is.na(nomatch) || nomatch==0)) stop("nomatch must either be NA or 0")
    if (which && !missing(j)) stop("'which' is true but 'j' is also supplied")
    if (missing(i) && missing(j)) stop("must provide either i or j or both. Try DT instead of DT[].")
    if (!with && missing(j)) stop("j must be provided when with=FALSE")
    if (!with && !missing(by)) stop("with must be TRUE when by is provided")
    irows = TRUE
    bywithoutby=FALSE
    if (!missing(i)) {
        isub = substitute(i)
        isubl = as.list.default(isub)
        if (identical(isubl[[1]],quote(eval))) {
            isub = eval(isubl[[2]],parent.frame())
            if (is.expression(isub)) isub=isub[[1]]
        }
        if (!is.name(isub))
            i = eval(isub, envir=x, enclos=parent.frame())
        else
            i = eval(isub,parent.frame())
        if (is.matrix(i)) stop("i is invalid type (matrix). Perhaps in future a 2 column matrix could return a list of elements of DT (in the spirit of A[B] in FAQ 2.14). Please let maintainer('data.table') know if you'd like this, or add your comments to FR #1611.")
        if (is.logical(i)) {
            if (identical(i,NA)) i = NA_integer_  # see DT[NA] thread re recycling of NA logical
            else i[is.na(i)] = FALSE              # avoids DT[!is.na(ColA) & !is.na(ColB) & ColA==ColB], just DT[ColA==ColB]
        }
        if (is.null(i)) return( null.data.table() )
        if (is.character(i)) {
            # for user convenience
            if (!haskey(x)) stop("The data.table has no key but i is character. Call setkey first, see ?setkey.")
            if (!is.factor(x[[match(key(x)[1], colnames(x))]])) stop("The data.table has a key, but the first column of that key is not factor. i cannot be character in this case")
            i = J(i)    # TO DO: add to FAQ ... so DT[c("e","f")] returns different order to DT[c("f","e")] as is most natural. Pass in SJ(c("f","e")) to guarantee result of grouping is sorted by the groups and is key'd by group
        }
        if (identical(class(i),"list") || identical(class(i),"data.frame")) i = as.data.table(i)
        if (is.data.table(i)) {
            if (!haskey(x)) stop("When i is a data.table, x must be keyed (i.e. sorted, and, marked as sorted) so data.table knows which columns to join to and so it can take advantage of x being sorted. Call setkey first, see ?setkey.")
            rightcols = match(key(x),colnames(x))
            if (any(is.na(rightcols))) stop("sorted columns of x don't exist in the colnames. data.table is not a valid data.table")
            for (a in rightcols) if (!typeof(x[[a]]) %in% c("integer","logical")) stop("sorted column ",colnames(x)[a], " in x is not internally type integer")
            origi = i  # TO DO. due to match between factor levels below. Time to try character() again?
                       # This origi will copy, not nice.  Trouble with character in the binarysearch
                       # is the if and Rf_Scollate we're trying to avoid.
            if (haskey(i)) {
                leftcols = match(key(i),colnames(i))
                if (any(is.na(leftcols))) stop("sorted columns of i don't exist in the colnames of i. data.table is not a valid data.table")
                if (length(rightcols) < length(leftcols)) {
                    if (verbose) cat("i's key is longer (",length(leftcols),") than x's key (",length(rightcols),"). Using the first ",length(rightcols)," columns of i's key in the join.\n", sep="")
                    leftcols = head(leftcols,length(rightcols))
                }
                byval = i[,leftcols,with=FALSE]  # TO DO: remove this subset; just needs pointer
                for (a in seq(along=leftcols)) {
                    # When/if we move to character, this entire for() will not be required.
                    lc = leftcols[a]
                    rc = rightcols[a]
                    if (!typeof(i[[lc]])%in%c("integer","logical")) stop("sorted column ",colnames(i)[lc], " of i must be internally type integer")
                    if (is.factor(i[[lc]])) {
                        if ((roll || rolltolast) && a==length(leftcols)) stop("Attempting roll join on factor column i.",colnames(i)[lc],". Only integer columns (i.e. not factors) may be roll joined.")   # because the sortedmatch a few lines below returns NA for missing chars in x (rather than some integer greater than existing). Using character rather than factor solves this if we get over the speed issues with character.
                        if (!is.factor(x[[rc]])) stop("i.",colnames(i)[lc]," is a factor joining to x.",colnames(x)[rc]," which is not a factor. Factors must join to factors.")
                        i[[lc]] = sortedmatch(levels(i[[lc]]), levels(x[[rc]]), nomatch=NA)[i[[lc]]]
                        levels(i[[lc]]) = levels(x[[rc]])
                        class(i[[lc]]) = "factor"
                        # factor levels are always sorted in data.table, so this match will be sorted too. There is no need to re-sort.
                        # NAs can be produced by this level match, in which case the C code (it knows integer value NA) can skip over the lookup.
                        # its therefore important we pass NA rather than 0 to the C code.
                        # For factor levels in the order of several thousand, standard match is extremely innefficient, sortedmatch yields a 96% saving.
                    } else {
                        if (is.factor(x[[rc]])) stop("x.",colnames(x)[rc]," is a factor but joining to i.",colnames(i)[lc]," which is not a factor. Factors must join to factors.")
                    }
                }
                iby = key(x)[seq(along=leftcols)]
            } else {
                leftcols = 1:min(ncol(i),length(rightcols))     # The order of the columns in i must match to the order of the sorted columns in x
                byval = i[,leftcols,with=FALSE]  # TO DO: remove this subset; just needs pointer
                for (lc in leftcols) {
                    # When/if we move to character, this entire for() will not be required.
                    rc = rightcols[lc]
                    if (!typeof(i[[lc]])%in%c("integer","logical")) stop("unsorted column ",colnames(i)[lc], " of i is not internally type integer. i doesn't need to be keyed, just convert the (likely) character column to factor")
                    if (is.factor(i[[lc]])) {
                        if ((roll || rolltolast) && lc==last(leftcols)) stop("Attempting roll join on factor column i.",colnames(i)[lc],". Only integer columns (i.e. not factors) may be roll joined.")
                        if (!is.factor(x[[rc]])) stop("i.",colnames(i)[lc]," is a factor but the corresponding x.",colnames(x)[rc]," is not a factor, Factors must join to factors.")
                        i[[lc]] = sortedmatch(levels(i[[lc]]), levels(x[[rc]]), nomatch=NA)[i[[lc]]]
                        levels(i[[lc]]) = levels(x[[rc]])  # We need the x levels later, in the byval, to transfer to result
                        class(i[[lc]]) = "factor"
                    } else {
                        if (is.factor(x[[rc]])) stop("x.",colnames(x)[rc]," is a factor but joining to i.",colnames(i)[lc]," which is not a factor. Factors must join to factors.")
                    }
                }
                iby = key(x)[leftcols]
            }
            idx.start = integer(nrow(i))
            idx.end = integer(nrow(i))
            .Call("binarysearch", i, x, as.integer(leftcols-1), as.integer(rightcols-1), haskey(i), roll, rolltolast, idx.start, idx.end, PACKAGE="data.table")
            if (mult=="all") {
                # TO DO: move this inside binarysearch.c
                lengths = idx.end - idx.start + 1L
                idx.diff = rep(1L, sum(lengths))
                idx.diff[head(cumsum(lengths), -1L) + 1L] = tail(idx.start, -1L) - head(idx.end, -1L)
                idx.diff[1L] = idx.start[1L]
                irows = cumsum(idx.diff)
                if (missing(by)) bywithoutby=TRUE  # TO DO: detect if joining to unique rows
            } else {
                irows = if (mult=="first") idx.start else idx.end
                lengths=rep(1L,length(irows))
            }
            if (is.na(nomatch) || nomatch!=0L) irows[irows==0L] = nomatch
            else lengths[idx.start==0L] = 0L
            if (which) return(irows)
        } else {
            # i is not a data.table
            if (!is.logical(i) && !is.numeric(i)) stop("i has not evaluated to logical, integer or double")
            if (which) {
                if (is.logical(i)) {
                    if (length(i)==nrow(x)) return(which(i))   # e.g. DT[colA>3,which=TRUE]
                    else return((1:nrow(x))[i])   # e.g. recycling DT[c(TRUE,FALSE),which=TRUE], for completeness
                }
                else return(as.integer(i))  # e.g. DT["A",which=TRUE],  or DT[c(1,3)]
            }
            irows = i
        }
        if (missing(j)) {
            if (!is.data.table(i)) {
            	ans = vector("list",ncol(x))
                for (s in seq_len(ncol(x))) ans[[s]] = x[[s]][irows]
                setattr(ans, "names", names(x))
                if (haskey(x) && (is.logical(irows) || length(irows)==1)) {
                    setattr(ans,"sorted",key(x))
                    # TO DO: detect more ordered subset cases, e.g. if irows is monotonic
                }
            } else {
                ans = vector("list",ncol(i)+ncol(x)-length(leftcols))
                inonjoin = seq_len(ncol(i))[-leftcols]
                if (!all(lengths==1L)) {
                    ii = rep(1:nrow(i),lengths)
                    for (s in seq_along(leftcols)) ans[[s]] = origi[[leftcols[s]]][ii]
                    for (s in seq_along(inonjoin)) ans[[s+ncol(x)]] = origi[[inonjoin[s]]][ii]
                } else {
                    for (s in seq_along(leftcols)) ans[[s]] = origi[[leftcols[s]]]
                    for (s in seq_along(inonjoin)) ans[[s+ncol(x)]] = origi[[inonjoin[s]]]
                }
                rightcols = head(rightcols,length(leftcols))
                xnonjoin = seq_len(ncol(x))[-rightcols]
                for (s in seq_along(xnonjoin)) ans[[s+length(leftcols)]] = x[[xnonjoin[s]]][irows]
                setattr(ans, "names", make.names(c(colnames(x)[rightcols],colnames(x)[-rightcols],colnames(i)[-leftcols]),unique=TRUE))
                if (haskey(i) || nrow(i)==1)
                    setattr(ans,"sorted",key(x))
                    # TO DO: detect more ordered subset cases e.g. DT["A"] or DT[c("A","B")] where the
                    # irows are an ordered subset. Or just .C("isordered",irows) or inside sortedmatch
                    # The nrow(i)==1 is the simplest case and been there for a while (and tested)
            }
            setattr(ans,"class",c("data.table","data.frame"))
            setattr(ans,"row.names",.set_row_names(nrow(ans)))
            settruelength(ans,0L)  # what 2.14.0+ would be for consistency, not nrow(ans) (could be too, but then we couldn't drop this line later if we ever make a dependency on 2.14.0+)
            return(alloc.col(ans))
        }
    } # end of  if !missing(i)
    if (missing(j)) stop("logical error, j missing")
    jsub = substitute(j)
    if (is.null(jsub)) return(NULL)
    jsubl = as.list.default(jsub)
    if (identical(jsubl[[1]],quote(eval))) {
        jsub = eval(jsubl[[2]],parent.frame())
        if (is.expression(jsub)) jsub = jsub[[1]]
    }
    av = all.vars(jsub,TRUE)
    if (":=" %in% sapply(sapply(jsub,all.vars,TRUE),"[",1)) {   # TO DO: Quite slow, move the sapply(sapply into C.
        # The double sapply is for FAQ 4.5
        if (!missing(by)) stop("Combining := in j with by is not yet implemented. Please let maintainer('data.table') know if you are interested in this.")
        if (bywithoutby && nrow(i)>1) stop("combining bywithoutby with := in j is not yet implemented.")
        if (as.character(jsub[[1]]) != ":=") stop("Currently only one `:=` may be present in j. This may be expanded in future.")
        rhsav = all.vars(jsub[[3]],TRUE)
        if (length(rhsav) && length(rhsvars <- intersect(rhsav,colnames(x)))) {
            tmpx = if (identical(irows,TRUE)) x[,rhsvars,with=FALSE] else x[irows,rhsvars,with=FALSE]
            # The 'if' isn't necessary but we do that for a bit of speed.
            rhs = eval(jsub[[3]], envir=tmpx, enclos=parent.frame())
        } else {
            rhs = eval(jsub[[3]], envir=parent.frame(), enclos=parent.frame())
        }
        if (with) {
            if (!is.name(jsub[[2]])) stop("LHS of := must be a single column name, when with=TRUE. When with=FALSE the LHS may be a vector of column names or positions.")
            lhs = as.character(jsub[[2]])
        } else {
            lhs = eval(jsub[[2]], envir=parent.frame(), enclos=parent.frame())
            if (!is.atomic(lhs)) stop("LHS of := must evaluate to an atomic vector (column names or positions) when with=FALSE")
            if (is.numeric(lhs)) {
                if (!all(1<=lhs | lhs<=ncol(x))) stop("LHS of := out of bounds")
                lhs = colnames(x)[lhs]
            }
            if (!is.character(lhs)) stop("Logical error. LHS of := wasn't atomic column names or positions")
        }
        m = match(lhs,names(x))
        if (all(!is.na(m))) {
            # updates by reference to existing columns
            cols = as.integer(m)
            newcolnames=NULL
        } else {
            # Adding new column(s)
            newcolnames=setdiff(lhs,names(x))
            cols = as.integer(c(m[!is.na(m)],ncol(x)+1:length(newcolnames)))
            if (notok<-!selfrefok(x,verbose))
                warning("Invalid .internal.selfref detected and fixed by taking a copy of the whole table, so that := can add this new column by reference. At an earlier point, this data.table has been copied by R. Avoid key<-, names<- and attr<- which in R currently (and oddly) all copy the whole data.table. Use set* syntax instead to avoid copying: setkey(), setnames() and setattr(). If this message doesn't help, please report to datatable-help so the root cause can be fixed.")
            if (notok || (truelength(x) < ncol(x)+length(newcolnames))) {
                n = max(ncol(x)+100, ncol(x)+2*length(newcolnames))
                name = substitute(x)
                if (is.name(name) && !notok && verbose) { # && NAMED(x)>0 (TO DO)
                    # Do the warning before allocating and assigning, so we can track where we are when verbose=TRUE
                    # in case an error occurs in alloc.col or assign
                    warning("growing vector of column pointers from truelength ",truelength(x)," to ",n,". A shallow copy has been taken, see ?alloc.col. Only a potential issue if two variables point to the same data (we can't yet detect that well) and if not you can safely ignore this warning. To avoid this warning you could alloc.col() first, deep copy first using copy(), wrap with suppressWarnings() or increase the 'datatable.alloccol' option.")
                # TO DO test   DT[....][,foo:=42L],  i.e. where the foo does the realloc. Would it see DT name or the call.
                # inherits=TRUE is correct and mimicks what the setVar in C was doing before being moved up to R level.
                # Commnent moved up from C ... to revisit ... Note that the NAMED(dt)>1 doesn't work because .Call
                # always sets to 2 (see R-ints), it seems. Work around
                # may be possible but not yet working. When the NAMED test works, we can drop allocwarn argument too
                # because that's just passed in as FALSE from [<- where we know `*tmp*` isn't really NAMED=2.
                # Note also that this growing will happen for missing columns assigned NULL, too. But so rare, we
                # don't mind.
                }
                alloc.col(x, n, verbose=verbose)   # always assigns to calling scope; i.e. this scope
                if (is.name(name))
                    assign(as.character(name),x,pos=parent.frame(),inherits=TRUE)
            }
        }
        ssrows =
            if (!is.logical(irows)) as.integer(irows)          # DT[J("a"),z:=42L]
            else if (identical(irows,TRUE)) as.integer(NULL)   # DT[,z:=42L]
            else {
                tt = if (length(irows)==nrow(x)) which(irows)  # DT[colA>3,z:=42L]
                     else (1:nrow(x))[irows]                   # DT[c(TRUE,FALSE),z:=42L] (recycling)
                if (!length(tt)) return(x)                     # DT[FALSE,z:=42L]
                tt
            }
        if (!missing(verbose) && verbose) cat("Assigning",if (!length(ssrows)) paste("all",nrow(x)) else length(ssrows),"row(s)\n")
        # the !missing is for speed to avoid calling getOption() which then calls options().
        # better to do verbosity before calling C, to make tracing easier if there's a problem in assign.c
        return(.Call("assign",x,ssrows,cols,newcolnames,rhs,verbose,PACKAGE="data.table"))
        # Allows 'update and then' queries such as DT[J(thisitem),done:=TRUE][,sum(done)]
        # Could return number of rows updated but even when wrapped in invisible() it seems
        # the [.class method doesn't respect invisible, which may be confusing to user.
        # NB: Tried assign(":=",function(lhs,rhs) {...},envir=parent.frame()) which worked for whole columns
        # but how to pass a subset of rows that way, or more crucially an assignment within groups (TO DO)
    }
    if (!with) {
        if (!length(j)) return( null.data.table() )
        if (is.character(j)) j = match(j, names(x))
        # TO DO:  DT[,"-columntoremove",with=FALSE] might be nice
        if (any(is.na(j))) stop("undefined columns selected")  # TO DO: include which ones in msg
        if (any(abs(j) > ncol(x) | j==0)) stop("j out of bounds")
        if (any(j<0) && any(j>0)) stop("j mixes positive and negative")
        if (any(j<0)) j = seq_len(ncol(x))[j]
        ans = vector("list",length(j))
        if (identical(irows,TRUE))
            for (s in seq(along=j)) ans[[s]] = x[[j[s]]]
        else {
            for (s in seq(along=j)) ans[[s]] = x[[j[s]]][irows]
            # TO DO: allow i's non join columns to be included by name
        }
        setattr(ans, "names", names(x)[j])
        if (haskey(x) && all(key(x) %in% names(ans)) && (is.logical(irows) || length(irows)==1 || haskey(i) || (is.data.table(i) && nrow(i)==1))) {
            setattr(ans,"sorted",key(x))
            # TO DO: see ordered subset comments above
        }
        setattr(ans,"class",c("data.table","data.frame"))
        setattr(ans,"row.names",.set_row_names(nrow(ans)))
        settruelength(ans,0L)
        return(alloc.col(ans))
    }
    if (!missing(by) && !missing(i)) {
        x = x[irows]
        # TO DO: efficiency gain by taking only the columns of x that j and by need.
        bywithoutby=FALSE
    }
    if ((missing(by) && !bywithoutby) || nrow(x)<1) {
        jvars = intersect(av,colnames(x))
        if (!identical(irows,TRUE)) {
            x = x[irows,jvars,with=FALSE]
        }
        if (mode(jsub)!="name" && as.character(jsub[[1]]) == "list") {
            jsub[[1]]=as.name("data.table")
            # we need data.table here because i) it grabs the column names from objects and ii) it does the vector expansion
            # data.table() will also call alloc.col() here
        }
        return(eval(jsub, envir=x, enclos=parent.frame()))
    }
    o__ = as.integer(NULL)

    if (bywithoutby) {
        # The groupings come instead from each row of the i data.table.
        # Much faster for a few known groups vs a 'by' for all followed by a subset
        if (!missing(by)) stop("logical error, by not missing in bywithoutby")
        f__ = idx.start
        len__ = as.integer(idx.end-idx.start+1)
        setattr(byval, "names", iby)
        bysameorder = haskey(i)
        if (!is.data.table(i)) stop("logicial error. i is not data.table, but mult='all' and 'by' is missing")
        bynames = allbyvars = NULL
    } else {
        # Find the groups, using 'by' ...
        if (missing(by)) stop("logical error, by is missing")

        bysub = substitute(by)
        bysubl = as.list.default(bysub)
        bysuborig = bysub
        if (is.name(bysub) && !(as.character(bysub) %in% colnames(x))) {
            bysub = eval(bysub,parent.frame())
            bysubl = as.list.default(bysub)
        }
        if (length(bysubl) && identical(bysubl[[1]],quote(eval))) {
            bysub = eval(bysubl[[2]],parent.frame())
            if (is.expression(bysub)) bysub=bysub[[1]]
            bysubl = as.list.default(bysub)
        }
        if (mode(bysub) == "character") {
            bysub = parse(text=paste("list(",paste(bysub,collapse=","),")",sep=""))[[1]]
            bysubl = as.list.default(bysub)
        }
        allbyvars = intersect(unlist(sapply(bysubl,all.vars,functions=TRUE)),colnames(x))
        bysameorder = haskey(x) && all(sapply(bysubl,is.name)) && identical(allbyvars,head(key(x),length(allbyvars)))
        byval = eval(bysub, x, parent.frame())
        if (!length(byval)) {
            # see missing(by) up above for comments
            # by could be NULL or character(0) for example
            if (mode(jsub)!="name" && as.character(jsub[[1]]) == "list")
                jsub[[1]]=as.name("data.table")
            return(eval(jsub, envir=x, enclos=parent.frame()))
        }
        if (is.atomic(byval)) {
            if (is.character(byval) && length(byval)<=ncol(x) && !(is.name(bysub) && as.character(bysub)%in%colnames(x)) ) {
                # e.g. by is key(DT) or c("colA","colB"), but not the value of a character column
                if (!all(byval %in% colnames(x)))
                    stop("'by' seems like a column name vector but these elements are not column names (first 5):",paste(head(byval[!byval%in%colnames(x)],5),collapse=""))
                # byvars = byval
                byval=as.list(x[,byval,with=FALSE])
            } else {
                byval = list(byval) # name : by may be a single unquoted column name but it must evaluate to list so this is a convenience to users
                setattr(byval, "names", as.character(bysuborig))
            }
        }
        if (!is.list(byval)) stop("by must evaluate to vector or list of vectors (where 'list' includes data.table and data.frame which are lists, too)")
        for (jj in seq_len(length(byval))) {
            if (is.character(byval[[jj]])) {
                byval[[jj]] = factor(byval[[jj]])
                next
            }
            if (typeof(byval[[jj]]) == "double") {
                toint = as.integer(byval[[jj]])  # drops attributes (such as class) so as.vector() needed on next line
                if (isTRUE(all.equal(as.vector(byval[[jj]]),toint))) {
                    mode(byval[[jj]]) = "integer"  # retains column attributes (such as IDateTime class)
                    next
                }
                else stop("Column ",jj," of 'by' is type 'double' and contains fractional data so cannot be coerced to integer in this particular case without losing information.")
            }
            if (!typeof(byval[[jj]]) %in% c("integer","logical")) stop("column or expression ",jj," of 'by' is type ",typeof(byval[[jj]]),". Do not quote column names. Useage: DT[,sum(colC),by=list(colA,month(colB))]")
        }
        tt = sapply(byval,length)
        if (any(tt!=nrow(x))) stop("Each item in the 'by' list must be same length as rows in x (",nrow(x),"): ",paste(tt,collapse=","))
        bynames = names(byval)
        if (is.null(bynames)) bynames = rep("",length(byval))
        if (any(bynames=="")) {
            if (length(bysubl)<2) stop("When by is list() we expect something inside the brackets")
            for (jj in seq_along(bynames)) {
                if (bynames[jj]=="") bynames[jj] = all.vars(bysubl[[jj+1]])[1]
                # if you don't want the byvar named as the column (to use it in j as vector) then
                # rename the byvar using list() notation.
            }
            setattr(byval, "names", bynames)
        }
        if (verbose) {last.started.at=proc.time()[3];cat("Finding groups (bysameorder=",bysameorder,") ... ",sep="");flush.console()}
        if (bysameorder) {
            f__ = duplist(byval)   # find group starts, given we know they are already grouped.
            for (jj in seq_along(byval)) byval[[jj]] = byval[[jj]][f__]
            len__ = as.integer(c(diff(f__), nrow(x)-last(f__)+1))
        } else {
            o__ = fastorder(byval)
            f__ = duplist(byval,order=o__)
            len__ = as.integer(c(diff(f__), nrow(x)-last(f__)+1))
            firstofeachgroup = o__[f__]
            origorder = .Internal(order(na.last=FALSE, decreasing=FALSE, firstofeachgroup))
            # radixsort isn't appropriate in this case. 'firstofeachgroup' are row numbers from the
            # (likely) large table so if nrow>100,000, range will be >100,000. Save time by not trying radix.
            f__ = f__[origorder]
            len__ = len__[origorder]
            firstofeachgroup = firstofeachgroup[origorder]
            for (jj in seq_along(byval)) byval[[jj]] = byval[[jj]][firstofeachgroup]
        }
        if (verbose) {cat("done in",round(proc.time()[3]-last.started.at,3),"secs\n");flush.console}
        # TO DO: allow secondary keys to be stored, then we see if our by matches one, if so use it, and no need to sort again. TO DO: document multiple keys.
    }

    # j to be run for each group (either from by, or bywithoutby)
    jvnames = NULL
    if (mode(jsub)=="name") {
        # j is a single unquoted column name, convenience to use to auto wrap it with list()
        if (jsub!=".SD") {
            jvnames = deparse(jsub)
            jsub = call("list",jsub)  # this should handle backticked names ok too
        }
    } else if (as.character(jsub[[1]])[1] == "list") {
        jsubl = as.list.default(jsub)
        if (length(jsubl)<2) stop("When j is list() or DT() we expect something inside the brackets")
        jvnames = names(jsubl)[-1]   # check list(a=sum(v),v)
        if (is.null(jvnames)) jvnames = rep("", length(jsubl)-1)
        for (jj in 2:length(jsubl)) {
            if (jvnames[jj-1] == "" && mode(jsubl[[jj]])=="name") jvnames[jj-1] = deparse(jsubl[[jj]])
            # TO DO: if call to a[1] for example, then call it 'a' too
        }
        setattr(jsub, "names", NULL)  # drops the names from the list so it's faster to eval the j for each group. We'll put them back aftwards on the result.
    } # else maybe a call to transform or something which returns a list.
    ws = all.vars(jsub,TRUE)  # TRUE fixes bug #1294 which didn't see b in j=fns[[b]](c)
    if (".SD" %in% ws) {
        if (missing(.SDcols)) {
            xvars = setdiff(colnames(x),union(bynames,allbyvars))
            # just using .SD in j triggers all non-by columns in the subset even if some of
            # those columns are not used. It would be tricky to detect whether the j expression
            # really does use all of the .SD columns or not, hence .SDcols for grouping
            # over a subset of columns
        } else {
            if (is.numeric(.SDcols)) {
                if (any(is.na(.SDcols)) || any(.SDcols>ncol(.SDcols)) || any(.SDcols<1)) stop(".SDcols is numeric but out of bounds (or NA)")
                xvars = colnames(x)[.SDcols]
            } else {
                if (!is.character(.SDcols)) stop(".SDcols should be column numbers or names")
                if (any(is.na(.SDcols)) || any(!.SDcols %in% colnames(x))) stop("Some items of .SDcols are not column names (or are NA)")
                xvars = .SDcols
            }
            # .SDcols might include grouping columns if users wants that, but normally we expect user not to include them in .SDcols
        }
    } else {
        if (!missing(.SDcols)) stop("j doesn't use .SD but .SDcols has been passed in")
        xvars = setdiff(intersect(ws,colnames(x)),bynames)
        # using a few named columns will be faster
        # Consider:   DT[,max(diff(date)),by=list(month=month(date))]
        # and:        DT[,sapply(.SD,sum),by=month(date)]
        # We don't want date in .SD in the latter, but we do in the former; hence the union() above.
    }
    if (!length(xvars)) {
        # For macros such as DT[,macro(),by=...]. A macro is a function with no arguments
        # evaluated within the scope of x.
        # TO DO: inspect macro definition to discover columns it uses.
        xvars = setdiff(colnames(x),union(bynames,allbyvars))
    }
    ivars = if (bywithoutby) intersect(gsub("^i[.]","",ws),colnames(i)) else NULL   # JIS

    if (verbose) {last.started.at=proc.time()[3];cat("Starting dogroups ...\n");flush.console()}
    if (f__[1]==0 && is.na(nomatch)) {
        itestj = NA_integer_
    } else {
        itestj = as.integer(seq.int(f__[1],length.out=len__[1]))
        if (length(o__)) itestj = o__[itestj]
    }
    
    SDenv = new.env(parent=parent.frame()) # use an environment to get the variable scoping right
    oldlen = length(itestj)
    length(itestj) = max(len__)  # pad with NA to allocate enough for largest group, will re-use it in dogroups.c
    if (itestj[1]==0 && !is.na(nomatch)) {
        itestj[1] = NA_integer_
        oldlen = 0L
    }
    if (!is.integer(itestj)) stop("Logical error: itestj is not type integer")
    SDenv$.SD = x[itestj, xvars, with=FALSE]
    # the subset above keeps factor levels in full
    # TO DO: drop factor levels altogether (as option later) ... for (col in 1:ncol(.SD)) if(is.factor(.SD[[col]])) .SD[[col]] = as.integer(.SD[[col]])
    # TO DO: change all 1 to 1L internally.
    for (ii in seq(along=xvars)) setlength(SDenv$.SD[[ii]], oldlen)
    SDenv$.BY = lapply(byval,"[",1)  # byval is list() not data.table
    if (!is.na(nomatch) && is.na(itestj[1])) {
        # This is a very ugly switch. It gets tests 499 and 500 to work for now. TO DO: revisit and rationalise.
        SDenv$.N = 0L
        for (ii in ivars) {
            assign(ii, i[[ii]][0L], envir=SDenv)
            assign(paste("i.",ii,sep=""), i[[ii]][0L], envir=SDenv)
        }
    } else {
        SDenv$.N = as.integer(len__[1L])
        for (ii in ivars) {
            assign(ii, i[[ii]][1L], envir=SDenv)  # for JIS
            assign(paste("i.",ii,sep=""), i[[ii]][1L], envir=SDenv)
        }
    }
    for (ii in names(SDenv$.BY)) assign(ii, SDenv$.BY[[ii]], envir=SDenv)
    for (ii in xvars) assign(ii, SDenv$.SD[[ii]], envir=SDenv)
    lockBinding(".SD",SDenv)
    lockBinding(".BY",SDenv)
    lockBinding(".N",SDenv)
    testj = eval(jsub, SDenv)
    maxn=0
    if (!is.null(testj)) {
        if (is.atomic(testj)) {
            jvnames = ""
            jsub = call("list",jsub)
            testj = list(testj)
        } else if (is.list(testj)) {
            if (is.null(jvnames))   # if we didn't deliberately drop the names and store them earlier
                jvnames = if (is.null(names(testj))) rep("",length(testj)) else names(testj)
        } else {
            stop("j must evaluate to an atomic vector (inc factor, Date etc), list of atomic vectors, or NULL")
        }
        if (verbose && !is.null(names(testj))) cat("testj evaluates to a list with names, this may slow down grouping")
        if (is.list(testj) && any(sapply(testj,is.data.frame))) stop("All items in j=list(...) should be atomic vectors or lists, currently. Consider cbind or merge afterwards until := by group is implemented.")
        maxn = max(sapply(testj,length))   # this could be 0 here too
    } else {
        maxn = 0
        # e.g. if j is for side effects only: printing or plotting
    }
    byretn = if (maxn == 0) 0
    else if (maxn==1 && len__[1]>1) length(f__)   # Most common case 1 : j is a list of simple aggregates i.e. list of atoms only
    else if (maxn==len__[1]) sum(len__)  # Most common case 2 : j returns as many rows as there are in the group (maybe a join)
    else sum(len__)
    # TO DO: we might over allocate above e.g. if first group has 1 row and j is actually a single row aggregate
    # TO DO: user warning when it detects over-allocation is currently off in dogroups.c
    byretn = max(byretn,maxn) # if the result for the first group is larger than the table itself(!) Unusual case where the optimisations for common query patterns. Probably a join is being done in the j via .SD and the 1-row table is an edge condition of bigger picture.
    byretn = as.integer(byretn)
    
    xcols = as.integer(match(xvars,colnames(x)))
    icols = NULL
    if (!missing(i) && is.data.table(i)) icols = as.integer(match(ivars,colnames(i)))
    else i=NULL
    ans = .Call("dogroups",x,xcols,o__,f__,len__,jsub,SDenv,testj,byretn,byval,i,as.integer(icols),i[1,ivars,with=FALSE],if(length(ivars))paste("i.",ivars,sep=""),is.na(nomatch),verbose,PACKAGE="data.table")
    #print(ans)
    #stop("stopping early")
    # why is byval copying data out of i? If there aren't any
    # expressions of columns then it could just be i directly.
    # This will reduce the 'many groups' timing vs sqldf even more.

    # don't want to eval j for every row of i unless it really is mult='all'

    # setkey could mark the key whether it is unique or not.

    # TO DO : play with hash and size arguments of the new.env().
    if (verbose) {cat("... done dogroups in",round(proc.time()[3]-last.started.at,3),"secs\n");flush.console}
    # if (byretn==0) return(NULL)  # user wanted side effects only (e.g. plotting).
    ww = which(jvnames=="")
    if (any(ww)) jvnames[ww] = paste("V",ww,sep="")
    setattr(ans, "names", c(names(byval), jvnames))
    if (length(ans[[1]])) {
        ww = which(sapply(byval,is.factor))
        for (jj in ww) {levels(ans[[jj]]) = levels(byval[[jj]]); class(ans[[jj]])="factor"}
        ww = which(sapply(testj,is.factor))
        for (jj in ww) {levels(ans[[length(byval)+jj]]) = levels(testj[[jj]]); class(ans[[length(byval)+jj]])="factor"}

        # bit of a klude to retain the classes. Rather than allocating in C, we could allocate ans in R before dogroups, and create the right class at that stage
        for (jj in seq_along(byval)) class(ans[[jj]]) = class(byval[[jj]])
        for (jj in seq_along(testj)) class(ans[[length(byval)+jj]]) = class(testj[[jj]])
    }
    setattr(ans,"row.names",.set_row_names(length(ans[[1]])))
    setattr(ans,"class",c("data.table","data.frame"))
    if ((!missing(by) && bysameorder) || (!missing(i) && haskey(i))) {
        setattr(ans,"sorted",colnames(ans)[seq_along(byval)])
    }
    settruelength(ans,0L)
    alloc.col(ans)
}


#  [[.data.frame is now dispatched due to inheritance.
#  The code below tried to avoid that but made things
#  very slow (462 times faster down to 1 in the timings test).
#  TO DO. Reintroduce the bvelow but dispatch straight to
#  .C("do_subset2") or better.

#"[[.data.table" = function(x,...) {
#    if (!cedta()) return(`[[.data.frame`(x,...))
#    class(x)=NULL
#    x[[...]]
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
    cn <- colnames(x)
    if (any(dm == 0))
        return(array(NA, dim = dm, dimnames = list(NULL, cn)))
    p <- dm[2]
    n <- dm[1]
    collabs <- as.list(cn)
    X <- x
    class(X) <- NULL
    non.numeric <- non.atomic <- FALSE
    all.logical <- TRUE
    for (j in 1:p) {
        if (is.ff(X[[j]])) X[[j]] <- X[[j]][]   # to bring the ff into memory, since we need to create a matrix in memory
        xj <- X[[j]]
        if (length(dj <- dim(xj)) == 2 && dj[2] > 1) {
            if (inherits(xj, "data.table"))
                xj <- X[[j]] <- as.matrix(X[[j]])
            dnj <- dimnames(xj)[[2]]
            collabs[[j]] <- paste(collabs[[j]], if (length(dnj) >
                0)
                dnj
            else 1:dj[2], sep = ".")
        }
        if (!is.logical(xj))
            all.logical <- FALSE
        if (length(levels(xj)) > 0 || !(is.numeric(xj) || is.complex(xj)) ||
            (!is.null(cl <- attr(xj, "class")) && any(cl %in%
                c("Date", "POSIXct", "POSIXlt"))))
            non.numeric <- TRUE
        if (!is.atomic(xj))
            non.atomic <- TRUE
    }
    if (non.atomic) {
        for (j in 1:p) {
            xj <- X[[j]]
            if (is.recursive(xj)) {
            }
            else X[[j]] <- as.list(as.vector(xj))
        }
    }
    else if (all.logical) {
    }
    else if (non.numeric) {
        for (j in 1:p) {
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
    nrows <- d[1]
    ir <- seq(length = nrows)
    ncols <- d[2]
    ic <- seq(length = ncols)
    dn <- dimnames(x)
    collabs <- dn[[2]]
    if (any(empty <- nchar(collabs) == 0))
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
    setattr(ans,"class",c("data.table","data.frame"))
    settruelength(ans,0L)
    alloc.col(ans)
}

as.data.table.list = function(x, keep.rownames=FALSE) {
    if (!length(x)) return( null.data.table() )
    n = sapply(x,length)
    x = copy(x)
    if (any(n<max(n)))
        for (i in which(n<max(n))) x[[i]] = rep(x[[i]],length=max(n))
    if (is.null(names(x))) setattr(x,"names",paste("V",1:length(x),sep=""))
    setattr(x,"row.names",.set_row_names(max(n)))
    setattr(x,"class",c("data.table","data.frame"))
    settruelength(x,0L)
    alloc.col(x)
}

as.data.table.data.table = function(x, keep.rownames=FALSE) return(x)

head.data.table = function(x, n=6, ...) {
    if (!cedta()) return(NextMethod())
    i = seq(len=min(n,nrow(x)))
    x[i]
}
tail.data.table = function(x, n=6, ...) {
    if (!cedta()) return(NextMethod())
    i = seq(to=nrow(x), length=min(n, nrow(x)))
    x[i]
}

"[<-.data.table" = function (x, i, j, value) {
    # It is not recommended to use <-. Instead, use := for efficiency.
    # [<- is still provided for consistency and backwards compatibility, but we hope users don't use it.
    #settruelength(x,0L)  # it was copied to `*tmp*`, so truelength is length, now. But set to 0L to reset it and be safe.
    if (!cedta()) {
        x = `[<-.data.frame`(x, i, j, value)
        settruelength(x,0L)     # TO DO: remove comment : make it what it is
        return(alloc.col(x))            # over-allocate (again).   Avoid all this by using :=.
    }
    if (!missing(i)) {
        isub=substitute(i)
        i = eval(isub, x, parent.frame())
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
    } else i = as.integer(NULL)   # meaning (to C code) all rows, without allocating 1:nrow(x) vector
    if (missing(j)) j=colnames(x)
    if (!is.atomic(j)) stop("j must be atomic vector, see ?is.atomic")
    if (any(is.na(j))) stop("NA in j")
    if (is.character(j)) {
        newcolnames = setdiff(j,names(x))
        cols = as.integer(match(j, c(names(x),newcolnames)))
        # We can now mix existing columns and new columns
    } else {
        if (!is.numeric(j)) stop("j must be vector of column name or positions")
        if (any(j>ncol(x))) stop("Attempt to assign to column position greater than ncol(x). Create the column by name, instead. This logic intends to catch (most likely) user errors.")
        cols = as.integer(j)  # for convenience e.g. to convert 1 to 1L
        newcolnames = NULL
    }
    reinstatekey=NULL
    if (haskey(x) && identical(key(x),key(value)) &&
        identical(colnames(x),colnames(value)) &&
        !is.unsorted(i) &&
        identical(substitute(x),quote(`*tmp*`))) {
        # DT["a",]$y <- 1.1  winds up creating `*tmp*` subset of rows and assigning _all_ the columns into x and
        # over-writing the key columns with the same value (not just the single 'y' column).
        # That isn't good for speed; it's an R thing. Solution is to use := instead to avoid all this, but user
        # expects key to be retained in this case because _he_ didn't assign to a key column (the internal base R 
        # code did).
        reinstatekey=key(x)
    }
    if (!selfrefok(x) || truelength(x) < ncol(x)+length(newcolnames)) {
        x = alloc.col(x,length(x)+length(newcolnames)) # because [<- copies via *tmp* and main/duplicate.c copies at length but copies truelength over too
        # search for one other .Call to assign in [.data.table to see how it differs
    }
    verbose=getOption("datatable.verbose",FALSE)
    .Call("assign",x,i,cols,newcolnames,value,verbose,PACKAGE="data.table")
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
        else if (all(nii <- match(nmi, clabs, 0)))
            nii
        else stop("names don't match previous names:\n\t", paste(nmi[nii ==
            0], collapse = ", "))
    }
    allargs <- list(...)
    allargs <- allargs[sapply(allargs, length) > 0]
    n <- length(allargs)
    if (n == 0)
        return( null.data.table() )
    if (!all(sapply(allargs, is.list))) stop("All arguments to rbind must be lists (data.frame and data.table are already lists)")
    ncols = sapply(allargs, length)
    if (length(unique(ncols)) != 1) {
        f=which(ncols!=ncols[1])[1]
        stop("All arguments to rbind must have the same number of columns. Item 1 has ",ncols[1]," but item ",f," has ",ncols[f],"column(s).")
    }
    l = list()
    nm = names(allargs[[1]])
    if (use.names && length(nm) && n>1) {
        for (i in 2:n) if (length(names(allargs[[i]]))) {
            if (!all(names(allargs[[i]]) %in% nm))
                stop("Some colnames of argument ",i," (",paste(setdiff(names(allargs[[i]]),nm),collapse=","),") are not present in colnames of item 1. If an argument has colnames they can be in a different order, but they must all be present. Alternatively, you can drop names (by using an unnamed list) and the columns will then be joined by position. Or, set use.names=FALSE.")
            if (!all(names(allargs[[i]]) == nm))
                warning("Argument ",i," has names in a different order. Columns will be bound by name for consistency with base. Alternatively, you can drop names (by using an unnamed list) and the columns will then be joined by position. Or, set use.names=FALSE.")
                allargs[[i]] = as.list(allargs[[i]])[nm]
                # TO DO : could use setcolorder() to speed this line up but i) it only currently works on data.table
                # (not data.frame or list) and ii) it would change the original by reference so would need to be copied
                # anyway. So, take a shallow copy somehow, or leave as-is and do the 'do.call(' below differently.
        }
    }
    for (i in 1:length(allargs[[1]])) l[[i]] = do.call("c", lapply(allargs, "[[", i))
    # This is why we currently still need c.factor.
    # TO DO: much easier with character columns, when they are allowed.
    setattr(l,"names",nm)
    setattr(l,"row.names",.set_row_names(length(l[[1]])))
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
    # Similar to as.list.data.frame in base.
    ans <- unclass(x)
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
    if (!is.null(value[[1]])) stop("data.tables do not have rownames")
    if (ncol(x) != length(value[[2]])) stop("can't assign",length(value[[2]]),"colnames to a",ncol(x),"column data.table")
    setnames(x,as.character(value[[2]]))
    x  # it's this returned value that is copied via *tmp* and we cannot avoid that when using <- currently in R
}

"names<-.data.table" = function(x,value)
{
    # When non data.table aware packages change names, we'd like to maintain the key, too.
    # If call is names(DT)[2]="newname", R will call this names<-.data.table function (notice no i) with 'value' already prepared to be same length as ncol
    caller = as.character(sys.call(-2))[1]
    if ( ((tt<-identical(caller,"colnames<-")) && cedta(3)) ||
         cedta() ) warning("The ",if(tt)"col","names(x)<-value syntax copies the whole table. This is due to <- in R itself. Please change to setnames(x,old,new) which does not copy and is faster. See help('setnames'). You can safely ignore this warning if it is inconvenient to change right now. Setting options(warn=2) turns this warning into an error, so you can then use traceback() to find and change your ",if(tt)"col","names<- calls.")
    setnames(x,value)
    x   # it's this returned value that is copied via *tmp* and we cannot avoid that when using <- currently in R
}


last = function(x) x[NROW(x)]     # last row for a data.table, last element for a vector.

within.data.table <- function (data, expr, ...)
# basically within.list but retains key (if any)
# will be slower than using := or a regular query
# `within` and other similar functions in data.table are
# not just provided for users who expect them to work, or
# prefer syntax they're used to, but for non-data.table-aware
# packages to retain keys (for example). Hopefully users and
# code will use the faster data.table syntax in time.
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
    if (haskey(data) && all(key(data) %in% names(ans))) {
        x = TRUE
        for (i in key(data)) {
            x = identical(data[[i]],ans[[i]])
            if (!x) break
        }
        if (x) setattr(ans,"sorted",key(data))
    }
    ans
}


# basically transform.data.frame with data.table instead of data.frame
transform.data.table <- function (`_data`, ...)
{
    if (!cedta()) return(NextMethod())
    e <- eval(substitute(list(...)), `_data`, parent.frame())
    tags <- names(e)
    inx <- match(tags, names(`_data`))
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
    if (!any(tags %in% key.cols)) {
        setattr(ans, "sorted", key.cols)
    }
    ans
}

# not exported or documented, yet
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
        vars <- 1:ncol(x)
    } else {
        nl <- as.list(1L:ncol(x))
        setattr(nl,"names",names(x))
        vars <- eval(substitute(select), nl, parent.frame())
        if (!is.null(key.cols)) {
            ## Only keep key.columns found in the select clause
            key.cols <- key.cols[key.cols %in% vars]
            if (length(key.cols) == 0L) {
                key.cols <- NULL
            }
        }
    }

    ans <- x[r, vars, with = FALSE]
    
    if (nrow(ans) > 0L) {
        if (!missing(select) && !is.null(key.cols)) {
            ## Set the key on the returned data.table as long as the key
            ## columns that "remain" are the same as the original, or a
            ## prefix of it.
            is.prefix <- all(key(x)[1:length(key.cols)] == key.cols)
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
    if (cedta() && getOption("datatable.dfdispatchwarn",TRUE))  # or user can use suppressWarnings
        warning("split is inefficient. It copies memory. Please use [,j,by=list(...)] syntax. See data.table FAQ.")
    NextMethod()  # allow user to do it though, split object will be data.table's with 'NA' repeated in row.names silently
}

# TO DO, add more warnings e.g. for by.data.table(), telling user what the data.table syntax is but letting them dispatch to data.frame if they want


copy = function(x) {
    newx = .Call("copy",x,PACKAGE="data.table")  # copies at length but R's duplicate() also copies truelength over.
    if (!is.data.table(x)) return(newx)   # e.g. in as.data.table.list() the list is copied before changing to data.table
    settruelength(newx,0L)
    alloc.col(newx)
    # TO DO: speedup duplicate.c using memcpy, suggested to r-devel, would benefit copy()
    # Could construct data.table and use memcpy ourselves but deep copies e.g. list() columns
    # may be tricky; more robust to rely on R's duplicate which deep copies.
}

alloc.col = function(DT, n=getOption("datatable.alloccol",quote(max(100,2*ncol(DT)))), verbose=getOption("datatable.verbose",FALSE)) 
{
    name = substitute(DT)
    if (identical(name,quote(`*tmp*`))) stop("alloc.col attempting to modify `*tmp*`")
    ans = .Call("alloccolwrapper",DT,as.integer(eval(n)),verbose,PACKAGE="data.table")
    if (is.name(name)) {
        name = as.character(name)
        assign(name,ans,parent.frame(),inherits=TRUE)
    }
    ans
}

selfrefok = function(DT,verbose=getOption("datatable.verbose",FALSE)) {
    .Call("selfrefokwrapper",DT,verbose,PACKAGE="data.table")
}

truelength = function(x) .Call("truelength",x,PACKAGE="data.table")
# deliberately no "truelength<-" method.  alloc.col is the mechanism for that (maybe alloc.col should be renamed "truelength<-".

settruelength = function(x,n) {
    "Truly an internal function only. Users can call this using :::, but please don't."
    # if (n!=0) warning("settruelength should only be used to set to 0, and is for 2.13.2-")
    #if (getRversion() >= "2.14.0")
    #    if (truelength(x) != 0) warning("This is R>=2.14.0 but truelength isn't initialized to 0")
        # grep for suppressWarnings(settruelength) for where this is needed in 2.14.0+ (otherwise an option would be to make data.table depend on 2.14.0 so settruelength could be removed)
    
    
    .Call("settruelength",x,as.integer(n),PACKAGE="data.table")
    
    
    
    #if (is.data.table(x))
    #    .Call("settruelength",attr(x,"class"),-999L,PACKAGE="data.table")
    #    # So that (in R 2.13.2-) we can detect tables loaded from disk (tl is not initialized there)
}

setlength = function(x,n) {
    .Call("setlength",x,as.integer(n),PACKAGE="data.table")
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
        .Call("setattrib", x, name, value, PACKAGE="data.table")
        # If name=="names" and this is the first time names are assigned (e.g. in data.table()), this will be grown
        # by alloc.col very shortly afterwards in the caller.
    invisible(x)
}

setnames = function(x,old,new) {
    # Sets by reference, maintains truelength, no copy of table at all.
    # But also more convenient than names(DT)[i]="newname"  because we can also do setnames(DT,"oldname","newname")
    # without an onerous match() ourselves. old can be positions, too, but we encourage by name for robustness.
    if (!is.data.table(x)) stop("x is not a data.table")
    if (!length(attr(x,"names"))) stop("x has no column names")  # because setnames is for user user. Internally, use setattr(x,"names",...)
    if (missing(new)) {
        # so that setnames(DT,new) works too, e.g., setnames(DT,c("A","B")) where ncol(DT)==2
        if (length(old) != ncol(x)) stop("Can't assign ",length(old)," names to a ",ncol(x)," column data.table")
        new=old
        old=names(x)
    } else {
        if (missing(old)) stop("When 'new' is provided, 'old' must be provided too")
        if (length(new)!=length(old)) stop("'old' is length ",length(old)," but 'new' is length ",length(new))
    }
    if (is.numeric(old)) {
        tt = old<1L | old>length(x)
        if (any(tt)) stop("Items of 'old' outside range [1,",length(x),"]: ",paste(old[tt],collapse=","))
        old = names(x)[old]
    }
    if (!is.character(old)) stop("'old' is type ",typeof(old)," but should be integer, double or character")
    i = match(old,names(x))
    if (any(is.na(i))) stop("Items of 'old' not found in column names: ",paste(old[is.na(i)],collapse=","))
    if (!is.character(new)) stop("'new' is not a character vector")
    
    if (length(names(x)) != length(x)) stop("dt is length ",length(dt)," but its names are length ",length(names(x)))

    # update the key too if the column name being change is in the key
    m = match(old, key(x))  # use old here before .Call that (when old=names(DT)) changes old by reference, too 
    w = which(!is.na(m))
    .Call("setcharvec", attr(x,"names"), as.integer(i), new, PACKAGE="data.table")
    if (length(w))
        .Call("setcharvec", attr(x,"sorted"), as.integer(m[w]), new[w], PACKAGE="data.table")
        
    invisible(x)
}

setcolorder = function(x,neworder)
{
    if (!is.data.table(x)) stop("x is not a data.table")
    if (length(neworder)!=length(x)) stop("neworder is length ",length(neworder)," but x has ",length(x)," columns.")
    if (is.character(neworder)) {
        if (any(duplicated(neworder))) stop("neworder contains duplicate column names")
        o = as.integer(match(neworder,names(x)))
        if (any(is.na(o))) stop("Names in neworder not found in x: ",paste(neworder[is.na(m)],collapse=","))
    } else {
        if (!is.numeric(neworder)) stop("neworder is not a character or numeric vector")
        o = as.integer(neworder)
        m = !(o %in% 1:length(x))
        if (any(m)) stop("Column numbers in neworder out of bounds: ",paste(o[m],collapse=","))
    }
    .Call("setcolorder",x,o,PACKAGE="data.table")
    invisible(x)   
}

set = function(x,i,j,value)
{
    .Call("assign",x,i,j,NULL,value,FALSE,PACKAGE="data.table")
    # TO DO: When R itself assigns to char vectors, check a copy is made and 'ul' lost, in tests.Rraw.
    # TO DO: When := or set() do it, make them aware of 'ul' and drop it if necessary.
    invisible(x)
}

chmatch = function(x,table,nomatch=NA_integer_)
    .Call("chmatchwrapper",x,table,as.integer(nomatch),FALSE,PACKAGE="data.table")

"%chin%" = function(x,table) {
    # TO DO  if table has 'ul' then match to that
    .Call("chmatchwrapper",x,table,NA_integer_,TRUE,PACKAGE="data.table")
}

":=" = function(LHS,RHS) stop(':= is defined for use in j only; i.e., DT[i,col:=1L] not DT[i,col]:=1L or DT[i]$col:=1L. Please see help(":=").')



