
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
    print(rbind(as.matrix(format.data.table(toprint, digits = digits, na.encode = FALSE)), cn),
          digits=digits, quote=quote, right=right, ...)
    if (msg!="") cat(msg,"\n")
    invisible()
}

format.data.table <- function (x, ..., justify = "none") {
    data.table(lapply(x,
                      function(x) {
                          res <- format(x, ..., justify = justify)
                          class(res) <- "AsIs"
                          res
                      }))
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

# TO DO: now data.table inherits from data.frame, do we need data.table creation at all?

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
    if (identical(x, list(NULL))) return( structure(NULL,class=c("data.table","data.frame"),row.names=.set_row_names(0)) )
    if (length(x) == 1 && is.list(x[[1]]) && !is.data.frame(x[[1]]) && !is.data.table(x[[1]]) && !is.ff(x[[1]])) {
        # a list was passed in. Constructing a list and passing it in, should be the same result as passing in each column as arguments to the function.
        x = x[[1]]
        vnames = names(x)   # the names given to the explicity named arguments inside the list,  or the names if the list was a variable with names already
        exptxt = as.character(as.list(substitute(...))[-1])  # the argument expressions as text. if an unnamed list variable is passed in, this will return an empty vector, thats fine.
    } else {
        # the arguments to the data.table() function form the column names,  otherwise the expression itself
        tt <- as.list(substitute(list(...)))[-1]  # used to get the expressions as text, for unnamed inputs.  So data.table(X,Y) will automatically put X and Y as the column names.  For longer expressions, name the arguments to data.table(). But in a call to [.data.table, wrap in list() e.g. DT[,list(a=mean(v),b=foobarzoo(zang))] will get the col names
        vnames = names(tt)
        exptxt = as.character(tt)
    }
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
        return(structure(list(), class = c("data.table","data.frame"), row.names=.set_row_names(0)))
    if (length(vnames) != n) stop("logical error in vnames")
    vnames <- as.list(vnames)
    # ncols <- integer(n)        # the nrows and ncols of each of the inputs (could be varying lengths)
    nrows = integer(n)          # vector of lengths of each column. may not be equal if silent repetition is required.
    hascols = logical(n)
    for (i in 1:n) {
        xi = x[[i]]
        if (is.null(xi)) stop("column or argument ",i," is NULL")
        hascols[i] = TRUE   # so lists and data.tables would have 'hascols'=TRUE
        if (is.matrix(xi) || is.data.frame(xi)) {
            xi = as.data.table(xi, keep.rownames=keep.rownames)       # TO DO: allow a matrix to be a column of a data.table. This could allow a key'd lookup to a matrix, not just by a single rowname vector, but by a combination of several columns. A matrix column could be stored either by row or by column contiguous in memory.
            x[[i]] = xi
        } else {
            if (is.ff(xi))
               hascols[i] = FALSE
            else if ((is.atomic(xi) || is.factor(xi) || is.array(xi)) && !is.list(xi)) {     # is.atomic is true for vectors with attributed, but is.vector() is FALSE. this is why we use is.atomic rather than is.vector
                # the is.array is required when, for example, you have a tapply inside a j=data.table(...) expression
                # if (is.atomic(xi) || is.array(xi)) xi = as.vector(xi)   # to remove any vector names, or dimnames in an array, or attributes, otherwise they get silently retaining in the data.table list members, thus bloating memory
                hascols[i] = FALSE
                # if (is.factor(xi)) xi = as.character(xi)      # to use R_StringHash
                # possibly add && getOption("stringsAsfactors",FALSE)
                if (is.character(xi) && class(xi)!="AsIs") xi = factor(xi)  # coerce characters to factor unless wrapped in I()
                x[[i]] = xi
            }
        }
        # ncols[i] <- NCOL(xi)  # Returns 1 for vectors.
        nrows[i] <- NROW(xi)    # for a vector returns the length
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
    if (nr>0 && any(nrows==0)) stop("every input must have at least one value, unless all columns are empty")
    # So we can create an empty data.table, for inserting data later.  When some but not all columns are empty, you should put NA in the empty ones, to be silently filled to a column of NAs.
    # TO DO - see if silent vector expansion is already done in C somewhere.
    for (i in (1:n)[nrows < nr]) {
        xi <- x[[i]]
        if (nr%%nrows[i] == 0) {
            if (is.atomic(xi) || is.factor(xi)) {   # is.atomic catches as.hexmode(1:100)
                x[[i]] <- rep(xi, length.out = nr)
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
    names(value) <- vnames
    attr(value,"row.names") = .set_row_names(nr)
    attr(value, "class") <- c("data.table","data.frame")
    if (!is.null(key)) {
      if (!is.character(key)) stop("key must be character") 
      if (length(key)==1)
          eval(parse(text=paste("setkey(value,",key,")",sep="")))    # e.g. key = "x,y"
      else
          key(value) = key     # e.g. key=c("col1","col2")
    }
    value
}


"[.data.table" = function (x, i, j, by=NULL, with=TRUE, nomatch=NA, mult="all", roll=FALSE, rolltolast=FALSE, which=FALSE, bysameorder=FALSE, .SDcols, verbose=getOption("datatable.verbose",FALSE), drop=NULL)
{
    # the drop=NULL is to sink drop argument when dispatching to [.data.frame; using '...' stops test 147
    if (!cedta()) {
        ans = if (missing(drop)) `[.data.frame`(x,i,j) else `[.data.frame`(x,i,j,drop)
        if (!missing(i)) key(ans)=NULL  # See test 304
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
        isubl = as.list(isub)
        if (identical(isubl[[1]],quote(eval))) {
            isub = eval(isubl[[2]],parent.frame())
            if (is.expression(isub)) isub=isub[[1]]
        } 
        if (!is.name(isub))
            i = eval(isub, envir=x, enclos=parent.frame())
        else 
            i =eval(isub,parent.frame())
        if (is.logical(i)) {
            if (identical(i,NA)) i = NA_integer_  # see DT[NA] thread re recycling of NA logical
            else i[is.na(i)] = FALSE              # avoids DT[!is.na(ColA) & !is.na(ColB) & ColA==ColB], just DT[ColA==ColB]
        }
        if (is.null(i)) return(structure(NULL,class=c("data.table","data.frame"),row.names=.set_row_names(0)))
        if (is.character(i)) {
            # for user convenience
            if (!haskey(x)) stop("The data.table has no key but i is character. Call setkey first, see ?setkey.")
            if (!is.factor(x[[match(key(x)[1], colnames(x))]])) stop("The data.table has a key, but the first column of that key is not factor. i cannot be character in this case")
            i = J(i)    # TO DO: add to FAQ ... so DT[c("e","f")] returns different order to DT[c("f","e")] as is most natural. Pass in SJ(c("f","e")) to guarantee result of grouping is sorted by the groups and is key'd by group
        }
        if (identical(class(i),"list") || identical(class(i),"data.frame")) i = data.table(i)
        if (is.data.table(i)) {
            if (!haskey(x)) stop("When i is a data.table, x must be sorted to avoid a vector scan of x per row of i")
            rightcols = match(key(x),colnames(x))
            if (any(is.na(rightcols))) stop("sorted columns of x don't exist in the colnames. data.table is not a valid data.table")
            for (a in rightcols) if (!typeof(x[[a]]) %in% c("integer","logical")) stop("sorted column ",colnames(x)[a], " in x is not internally type integer")
            origi = i  # TO DO. due to match between factor levels below. Time to try character() again?
                       # This origi will copy, not nice.  Trouble with character in the binarysearch
                       # is the if and Rf_Scollate we're trying to avoid.
            if (haskey(i)) {
                leftcols = match(key(i),colnames(i))
                if (any(is.na(leftcols))) stop("sorted columns of i don't exist in the colnames of i. data.table is not a valid data.table")
                if (length(rightcols) < length(leftcols)) stop("i has more sorted columns than x has sorted columns, invalid sorted match")
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
                iby = key(x)[seq(1,length(key(i)))]
            } else {
                leftcols = 1:min(ncol(i),length(rightcols))     # The order of the columns in i must match to the order of the sorted columns in x
                byval = i[,leftcols,with=FALSE]  # TO DO: remove this subset; just needs pointer
                for (lc in leftcols) {
                    # When/if we move to character, this entire for() will not be required.
                    rc = rightcols[lc]
                    if (!typeof(i[[lc]])%in%c("integer","logical")) stop("unsorted column ",colnames(i)[lc], " of i is not internally type integer.")
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
                lengths = idx.end - idx.start + 1
                idx.diff = rep(1L, sum(lengths))
                idx.diff[head(cumsum(lengths), -1) + 1] = tail(idx.start, -1) - head(idx.end, -1)
                idx.diff[1] = idx.start[1]
                irows = cumsum(idx.diff)
                if (missing(by)) bywithoutby=TRUE  # TO DO: detect if joining to unique rows
            } else {
                irows = if (mult=="first") idx.start else idx.end
                lengths=1L
            }
            if (is.na(nomatch) || nomatch!=0) irows[irows==0] = nomatch
            else lengths[idx.start==0] = 0
            if (which) return(irows)
        } else {
            # i is not a data.table
            if (!is.logical(i) && !is.numeric(i)) stop("i has not evaluated to logical, integer or double")
            if (is.logical(i)) i = which(i)
            if (which) return(i)  # e.g. DT["A",which=TRUE]
            irows = i
        }
        if (missing(j)) {
            if (!is.data.table(i)) {
            	ans = vector("list",ncol(x))
                for (s in seq_len(ncol(x))) ans[[s]] = x[[s]][irows]
                names(ans) = names(x)
                if (haskey(x) && (is.logical(irows) || length(irows)==1)) {
                    attr(ans,"sorted") = key(x)
                    # TO DO: detect more ordered subset cases, e.g. if irows is monotonic
                }
            } else {
                ans = vector("list",ncol(i)+ncol(x)-length(leftcols))
                inonjoin = seq_len(ncol(i))[-leftcols]
                if (!all(lengths==1)) {
                    ii = rep(1:nrow(i),lengths)
                    for (s in seq_along(leftcols)) ans[[s]] = origi[[leftcols[s]]][ii]
                    for (s in seq_along(inonjoin)) ans[[s+ncol(x)]] = origi[[inonjoin[s]]][ii]
                }
                else {
                    for (s in seq_along(leftcols)) ans[[s]] = origi[[leftcols[s]]]
                    for (s in seq_along(inonjoin)) ans[[s+ncol(x)]] = origi[[inonjoin[s]]]
                }
                rightcols = head(rightcols,length(leftcols))
                xnonjoin = seq_len(ncol(x))[-rightcols]
                for (s in seq_along(xnonjoin)) ans[[s+length(leftcols)]] = x[[xnonjoin[s]]][irows]
                names(ans) = make.names(c(colnames(x)[rightcols],colnames(x)[-rightcols],colnames(i)[-leftcols]),unique=TRUE)
                if (haskey(i) || nrow(i)==1)
                    attr(ans,"sorted") = key(x)
                    # TO DO: detect more ordered subset cases e.g. DT["A"] or DT[c("A","B")] where the
                    # irows are an ordered subset. Or just .C("isordered",irows) or inside sortedmatch
                    # The nrow(i)==1 is the simplest case and been there for a while (and tested)
            }
            class(ans) = c("data.table","data.frame")
            attr(ans,"row.names") = .set_row_names(nrow(ans))
            return(ans)
        }
    } # end of  if !missing(i)
    if (missing(j)) stop("logical error, j missing")
    if (!with) {
        if (!length(j)) return(data.table(NULL))
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
        names(ans) = names(x)[j]
        if (haskey(x) && all(key(x) %in% names(ans)) && (is.logical(irows) || length(irows)==1 || haskey(i) || (is.data.table(i) && nrow(i)==1))) {
            attr(ans,"sorted") = key(x)
            # TO DO: see ordered subset comments above
        }
        class(ans) = c("data.table","data.frame")
        attr(ans,"row.names") = .set_row_names(nrow(ans))
        return(ans)
    }
    jsub = substitute(j)
    if (is.null(jsub)) return(NULL)
    jsubl = as.list(jsub)    
    
    if (!missing(by) && !missing(i)) {
        x = x[irows]
        # TO DO: efficiency gain by taking only the columns of x that j and by need.
        bywithoutby=FALSE
    }
    if (identical(jsubl[[1]],quote(eval))) {
        jsub = eval(jsubl[[2]],parent.frame())
        if (is.expression(jsub)) jsub = jsub[[1]]
    }
    av = all.vars(jsub,TRUE)
    if (":=" %in% av) {
        if (!missing(by)) stop("Combining := in j with by is not yet implemented. Please let maintainer('data.table') know if you are interested in this.")
        if (bywithoutby && nrow(i)>1) stop("combining bywithoutby with := in j is not yet implemented.")
        if (as.character(jsub[[1]]) != ":=") stop("Currently only one `:=` may be present in j. This may be expanded in future.")
        rhsav = all.vars(jsub[[3]],TRUE)
        if (length(rhsav) && length(rhsvars <- intersect(rhsav,colnames(x)))) {
            #rhsvars = intersect(rhsav,colnames(x))
            #if (length(rhsvars)) {
            tmpx = x[irows,rhsvars,with=FALSE]
            rhs = eval(jsub[[3]], envir=tmpx, enclos=parent.frame())
        } else {
            rhs = eval(jsub[[3]], envir=parent.frame(), enclos=parent.frame())
        }
        lhs = as.character(jsub[[2]])
        clearkey = any(!is.na(match(lhs,key(x))))
        m = match(lhs,names(x))
        if (!is.na(m)) {
            cols = as.integer(m)
            newcolnames=NULL
            symbol = rho = NULL
        } else {
            symbol = as.name(substitute(x))
            rho = parent.frame()
            cols = as.integer(ncol(x)+1L)
            newcolnames=lhs
        }
        ssrows = if (identical(irows,TRUE)) as.integer(NULL) else as.integer(irows)
        if (!missing(verbose) && verbose) cat("Assigning",if (!length(ssrows)) paste("all",nrow(x)) else length(ssrows),"row(s)\n")
        # the !missing is for speed to avoid calling getOption() which then calls options().
        # better to do verbosity before calling C, to make tracing easier if there's a problem in assign.c
        return(.Call("assign",x,ssrows,cols,newcolnames,rhs,clearkey,symbol,rho,PACKAGE="data.table"))
        # Allows 'update and then' queries such as DT[J(thisitem),done:=TRUE][,sum(done)]
        # Could return number of rows updated but even when wrapped in invisible() it seems
        # the [.class method doesn't respect invisible, which may be confusing to user. 
        # NB: Tried assign(":=",function(lhs,rhs) {...},envir=parent.frame()) which worked for whole columns
        # but how to pass a subset of rows that way, or more crucially an assignment within groups (TO DO)
    }
    if ((missing(by) && !bywithoutby) || nrow(x)<1) {
        jvars = intersect(av,colnames(x))
        if (!identical(irows,TRUE)) {
            x = x[irows,jvars,with=FALSE]
        }
        if (mode(jsub)!="name" && as.character(jsub[[1]]) == "list") {
            jsub[[1]]=as.name("data.table")
            # we need data.table here because i) it grabs the column names from objects and ii) it does the vector expansion
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
        names(byval) = iby
        bysameorder = haskey(i) #TRUE  # setting 'bysameorder' now is more of a fudge to avoid the o__[f__] later.
        if (!is.data.table(i)) stop("logicial error. i is not data.table, but mult='all' and 'by' is missing")
        bynames = allbyvars = NULL
    } else {
        # Find the groups, using 'by' ...
        if (missing(by)) stop("logical error, by is missing")
        
        bysub = substitute(by)
        bysubl = as.list(bysub)
        bysuborig = bysub
        if (is.name(bysub) && !(as.character(bysub) %in% colnames(x))) {
            bysub = eval(bysub,parent.frame())
            bysubl = as.list(bysub)
        }
        if (identical(bysubl[[1]],quote(eval))) {
            bysub = eval(bysubl[[2]],parent.frame())
            if (is.expression(bysub)) bysub=bysub[[1]]
            bysubl = as.list(bysub)
        }
        if (mode(bysub) == "character") {
            bysub = parse(text=paste("list(",paste(bysub,collapse=","),")",sep=""))[[1]]
            bysubl = as.list(bysub)
        }
        allbyvars = intersect(unlist(sapply(bysubl,all.vars,functions=TRUE)),colnames(x))
        byval = eval(bysub, x, parent.frame())
        if (is.null(byval)) stop("'by' has evaluated to NULL")
        if (is.atomic(byval)) {
            if (is.character(byval) && length(byval)<=ncol(x) && !(is.name(bysub) && as.character(bysub)%in%colnames(x)) ) {
                # e.g. by is key(DT) or c("colA","colB"), but not the value of a character column
                if (!all(byval %in% colnames(x)))
                    stop("'by' seems like a column name vector but these elements are not column names (first 5):",paste(head(byval[!byval%in%colnames(x)],5),collapse=""))
                # byvars = byval
                byval=as.list(x[,byval,with=FALSE])
            } else {
                byval = list(byval) # name : by may be a single unquoted column name but it must evaluate to list so this is a convenience to users
                names(byval) = as.character(bysuborig)
            }
        }
        if (!is.list(byval)) stop("by must evaluate to vector or list of vectors (where 'list' includes data.table and data.frame which are lists, too)")
        for (jj in seq_len(length(byval))) {
            if (is.character(byval[[jj]])) {
                byval[[jj]] = factor(byval[[jj]])
                next
            }
            if (typeof(byval[[jj]]) == "double") {
                toint = as.integer(byval[[jj]])
                if (isTRUE(all.equal(byval[[jj]],toint))) {
                    byval[[jj]] = toint
                    next
                }
                else stop("Column ",jj," of 'by' is float and cannot be auto converted to integer without losing information.")
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
            names(byval) = bynames
        }
        if (missing(bysameorder) && length(bynames) <= length(key(x)) && identical(bynames,head(key(x),length(bynames)))) {
            bysameorder=TRUE
            # table is already sorted by the group criteria, no need to sort
            # fastorder is so fast though that maybe this is not worth worrying about, especially if fastorder is even faster if its already sorted.
            # TO DO: turn off bysameorder, and always call fastorder ?
            # TO DO++: hash the key so sorting never required (hence shash query to r-devel)
        }
        if (verbose) {last.started.at=proc.time()[3];cat("Finding groups (bysameorder=",bysameorder,") ... ",sep="");flush.console()}
        if (bysameorder) {
            f__ = duplist(byval)   # find group starts, assumes they are grouped. 
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
        jsubl = as.list(jsub)
        if (length(jsubl)<2) stop("When j is list() or DT() we expect something inside the brackets")
        jvnames = names(jsubl)[-1]   # check list(a=sum(v),v)
        if (is.null(jvnames)) jvnames = rep("", length(jsubl)-1)
        for (jj in 2:length(jsubl)) {
            if (jvnames[jj-1] == "" && mode(jsubl[[jj]])=="name") jvnames[jj-1] = deparse(jsubl[[jj]])
            # TO DO: if call to a[1] for example, then call it 'a' too
        }
        names(jsub)=""  # drops the names from the list so it's faster to eval the j for each group. We'll put them back aftwards on the result.
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
    ivars = if (bywithoutby) intersect(setdiff(ws,colnames(x)),colnames(i)) else NULL   # JIS

    if (verbose) {last.started.at=proc.time()[3];cat("Starting dogroups ...\n");flush.console()}
    if (f__[1]==0 && is.na(nomatch)) {
        itestj = NA
    } else {
        itestj = seq(f__[1],length=len__[1])
        if (length(o__)) itestj = o__[itestj]
    }
    SDenv = new.env(parent=parent.frame()) # use an environment to get the variable scoping right
    SDenv$.SD = x[itestj, xvars, with=FALSE]
    SDenv$.BY = lapply(byval,"[",1)  # byval is list() not data.table
    SDenv$.N = as.integer(len__[1])
    for (ii in ivars) assign(ii, i[[ii]][1], envir=SDenv)
    for (ii in names(SDenv$.BY)) assign(ii, SDenv$.BY[[ii]], envir=SDenv)
    for (ii in xvars) assign(ii, SDenv$.SD[[ii]], envir=SDenv)
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
    # if (!is.null(names(testj))) warning("j evaluates to a list with names. this is wasteful and likely avoidable")
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
    SDenv$.SD = x[seq(length=max(len__)), xvars, with=FALSE]  # allocate enough for largest group, will re-use it, the data contents doesn't matter at this point
    # the subset above keeps factor levels in full
    # TO DO: drop factor levels altogether (as option later) ... for (col in 1:ncol(.SD)) if(is.factor(.SD[[col]])) .SD[[col]] = as.integer(.SD[[col]])
    xcols = as.integer(match(xvars,colnames(x)))
    icols = NULL
    if (!missing(i) && is.data.table(i)) icols = as.integer(match(ivars,colnames(i)))
    else i=NULL
    ans = .Call("dogroups",x,xcols,o__,f__,len__,jsub,SDenv,testj,byretn,byval,i,as.integer(icols),i[1,ivars,with=FALSE],is.na(nomatch),verbose,PACKAGE="data.table")

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
    names(ans) = c(names(byval), jvnames)
    if (length(ans[[1]])) {
        ww = which(sapply(byval,is.factor))
        for (jj in ww) {levels(ans[[jj]]) = levels(byval[[jj]]); class(ans[[jj]])="factor"}
        ww = which(sapply(testj,is.factor))
        for (jj in ww) {levels(ans[[length(byval)+jj]]) = levels(testj[[jj]]); class(ans[[length(byval)+jj]])="factor"}
            
        # bit of a klude to retain the classes. Rather than allocating in C, we could allocate ans in R before dogroups, and create the right class at that stage
        for (jj in seq_along(byval)) class(ans[[jj]]) = class(byval[[jj]])
        for (jj in seq_along(testj)) class(ans[[length(byval)+jj]]) = class(testj[[jj]])            
    }
    attr(ans,"row.names") = .set_row_names(length(ans[[1]]))
    class(ans) = c("data.table","data.frame")
    if ((!missing(by) && bysameorder) || (!missing(i) && haskey(i))) {
        attr(ans,"sorted") = colnames(ans)[seq_along(byval)]
    }
    ans
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
        names(value) <- collabs
    else
        names(value) <- paste("V", ic, sep = "")
    attr(value,"row.names") = .set_row_names(nrows)
    class(value) <- c("data.table","data.frame")
    value
}

as.data.table.data.frame = function(x, keep.rownames=FALSE)
{
    if (keep.rownames) return(data.table(rn=rownames(x), x, keep.rownames=FALSE))
    attr(x,"row.names") = .set_row_names(nrow(x))
    class(x) = c("data.table","data.frame")
    x
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
    if (!cedta() || missing(j)) return(`[<-.data.frame`(x, i, j, value))
    if (!missing(i)) {
        #if (is.atomic(i) && is.numeric(i)) {
        #    i=as.integer(i)
        #} else {
            # get i based on data.table-style indexing
            isub=substitute(i)
            i = x[eval(isub), which=TRUE, mult="all"]
        #}
    } else i = as.integer(NULL)   # meaning (to C code) all rows, without allocating 1:nrow(x) vector
    if (!is.atomic(j)) stop("j must be atomic vector, see ?is.atomic")
    if (any(is.na(j))) stop("NA in j")
    if (is.character(j)) {
        keycol = any(j %in% key(x))
        newcolnames = setdiff(j,names(x))
        cols = as.integer(match(j, c(names(x),newcolnames)))
        # We can now mix existing columns and new columns
    } else {
        if (!is.numeric(j)) stop("j must be vector of column name or positions")
        if (any(j>ncol(x))) stop("Attempt to assign to column position greater than ncol(x). Create the column by name, instead. This logic intends to catch most likely user errors.")
        keycol = j %in% match(key(x),names(x))
        cols = as.integer(j)  # for convenience e.g. to convert 1 to 1L
        newcolnames = NULL
    }
    if (cols[1]<=ncol(x) && is.character(value) && length(value)<nrow(x) && is.factor(x[[cols[1]]])) {
        # kludge. Doesn't cope with single character on RHS, and multiple columns of varying types on LHS.
        value = match(value,levels(x[[cols[1]]]))
        if (any(is.na(value))) stop("Some or all RHS not present in factor column levels")
        # The length(value)<nrow(x) is to allow coercion of factor columns to character, but we'll
        # do away with factor columns (by default) soon anyway
    }
    .Call("assign",x,i,cols,newcolnames,value,keycol,NULL,NULL,PACKAGE="data.table")
    # no copy at all if user calls directly; i.e. `[<-.data.table`(x,i,j,value)
    # or uses data.table := syntax; i.e. DT[i,j:=value]
    # but, there is one copy by R in [<- dispatch to `*tmp*`; i.e. DT[i,j]<-value
    # (IIUC, and, as of R 2.13.1)
}

"$<-.data.table" = function (x, name, value) {
    if (!cedta()) return(`$<-.data.frame`(x, name, value))
    `[<-.data.table`(x,j=name,value=value)  # important i is missing here
}

cbind = function(...) {
    # according to src/main/bind.c all the items passed to cbind must dispatch to the same
    # cbind method otherwise it falls through to it's default internal cbind.
    # base::cbind calls .Internal; it isn't generic. We tried creating cbind.data.table
    # and cbind.data.frame, and setting both to the same closure but something or other
    # wouldn't work that way. Hence masking cbind itself.
    # All so that cbind(DT,data.frame(...)) works as you would expect (test 324)
    # and also test 230.
    if (is.data.table(list(...)[[1]]))
        data.table(...)
    else
        base::cbind(...)
}

rbind = function (...) {
    # see long comments in cbind, same reason here
    if (!is.data.table(list(...)[[1]])) return(base::rbind(...))
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
        return(structure(list(), class=c("data.table","data.frame"), row.names=.set_row_names(0)))

    # if (!all(sapply(allargs, is.data.table))) stop("All arguments must be data.tables")
    # as of 1.6.4, can rbind a data.frame to a data.table ok
    
    if (length(unique(sapply(allargs, ncol))) != 1) stop("All data.tables must have the same number of columns")

    l = list()
    nm = names(allargs[[1]])
    if (length(nm) && n>1) {
        for (i in 2:n) if (length(names(allargs[[i]])) && !all(names(allargs[[i]]) == nm)) warning("colnames of argument ",i," don't match colnames of argument 1")
    }
    # for (i in 1:length(allargs[[1]])) l[[i]] = unlist(lapply(allargs, "[[", i))
    for (i in 1:length(allargs[[1]])) l[[i]] = do.call("c", lapply(allargs, "[[", i))
    # This is why we currently still need c.factor.
    names(l) = nm
    attr(l,"row.names")=.set_row_names(length(l[[1]]))
    class(l) = c("data.table","data.frame")
    return(l)
    # return(data.table(l))
    # much of the code in rbind.data.frame that follows this point is either to do with row.names, or coercing various types (and silent rep) which is already done by data.table. therefore removed.
}

as.data.table = function(x, keep.rownames=FALSE)
{
    if (is.null(x))
        return(as.data.table(list()))
    UseMethod("as.data.table")
}

as.data.frame.data.table = function(x, ...)
{
    attr(x,"row.names") = .set_row_names(nrow(x))   # since R 2.4.0, data.frames can have non-character row names
    class(x) = "data.frame"
    attr(x,"sorted") = NULL  # remove so if you convert to df, do something, and convert back, it is not sorted
    x
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
    if (!cedta()) return(`dimnames<-.data.frame`(x,value))
    if (!is.list(value) || length(value) != 2) stop("attempting to assign invalid object to dimnames of a data.table")
    if (!is.null(value[[1]])) stop("data.tables do not have rownames")
    if (ncol(x) != length(value[[2]])) stop("can't assign",length(value[[2]]),"colnames to a",ncol(x),"column data.table")
    names(x) <- as.character(value[[2]])
    x
}

"names<-.data.table" = function(x,value)
{
    # if (!cedta())... not this time. When non data.table aware packages change names, we'd like to maintain the key, too.
    if (!is.character(value)) stop("names must be type character")
    if (length(value) != ncol(x)) stop("Can't assign ",length(value)," names to a ",ncol(x)," column data.table")
    # If user calls names(DT)[2]="newname", R will (conveniently, for us) call this names<-.data.table function (notice no i) with 'value' same length as ncol 
    m = match(key(x), attr(x,"names"))
    attr(x,"names") = value  # TO DO: use setAttrib
    if (haskey(x) && any(!is.na(m))) {
        w = which(!is.na(m))
        k = key(x)
        k[w] = value[m[w]]
        attr(x,"sorted") = k   # TO DO: use setAttrib
    }
    x
}


last = function(x) x[NROW(x)]     # last row for a data.table, last element for a vector.

within.data.table <- function (data, expr, keep.key = FALSE, ...) # basically within.list but with a check to avoid messing up the key
{
    if (!cedta()) return(NextMethod())
    parent <- parent.frame()
    e <- evalq(environment(), data, parent)
    eval(substitute(expr), e)
    l <- as.list(e)
    l <- l[!sapply(l, is.null)]
    nD <- length(del <- setdiff(names(data), (nl <- names(l))))
    data[,nl] <- l
    if (nD)
        data[,del] <- if (nD == 1)
            NULL
        else vector("list", nD)
    if (!keep.key) attr(data,"sorted") <- NULL
    data
}


transform.data.table <- function (`_data`, ...) # basically transform.data.frame with data.table instead of data.frame
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
    if (!all(matched))
        do.call("data.table", c(list(`_data`), e[!matched]))
    else `_data`
}

#subset.data.table <- function (x, subset, select, ...) # not exported or documented, yet
#{
#    if (missing(subset))
#        r <- TRUE
#    else {
#        e <- substitute(subset)
#        r <- eval(e, x, parent.frame())
#        if (!is.logical(r))
#            stop("'subset' must evaluate to logical")
#        r <- r & !is.na(r)
#    }
#    if (missing(select))
#        vars <- 1:ncol(x)
#    else {
#        nl <- as.list(1L:ncol(x))
#        names(nl) <- names(x)
#        vars <- eval(substitute(select), nl, parent.frame())
#    }
#    x[r, vars, with = FALSE]
#}

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

Ops.data.table <- function (e1, e2 = NULL)
{
    if (!cedta()) return(`Ops.data.frame`(e1,e2))
    # TO DO, revisit below,  if the same as Ops.data.frame can we leave it to inheritance?
    isList <- function(x) !is.null(x) && is.list(x)
    unary <- nargs() == 1L
    lclass <- nzchar(.Method[1L])
    rclass <- !unary && (nzchar(.Method[2L]))
    value <- list()
    rn <- NULL
    FUN <- get(.Generic, envir = parent.frame(), mode = "function")
    f <- if (unary)
        quote(FUN(left))
    else quote(FUN(left, right))
    lscalar <- rscalar <- FALSE
    if (lclass && rclass) {
        nr <- nrow(e1)
        cn <- names(e1)
        if (any(dim(e2) != dim(e1)))
            stop(.Generic, " only defined for equally-sized data frames")
    }
    else if (lclass) {
        nr <- nrow(e1)
        cn <- names(e1)
        rscalar <- length(e2) <= 1L
        if (isList(e2)) {
            if (rscalar)
                e2 <- e2[[1L]]
            else if (length(e2) != ncol(e1))
                stop(gettextf("list of length %d not meaningful",
                  length(e2)), domain = NA)
        }
        else {
            if (!rscalar)
                e2 <- split(rep(as.vector(e2), length.out = prod(dim(e1))),
                  rep.int(seq_len(ncol(e1)), rep.int(nrow(e1),
                    ncol(e1))))
        }
    }
    else {
        nr <- nrow(e2)
        cn <- names(e2)
        lscalar <- length(e1) <= 1L
        if (isList(e1)) {
            if (lscalar)
                e1 <- e1[[1L]]
            else if (length(e1) != ncol(e2))
                stop(gettextf("list of length %d not meaningful",
                  length(e1)), domain = NA)
        }
        else {
            if (!lscalar)
                e1 <- split(rep(as.vector(e1), length.out = prod(dim(e2))),
                  rep.int(seq_len(ncol(e2)), rep.int(nrow(e2),
                    ncol(e2))))
        }
    }
    for (j in seq_along(cn)) {
        left <- if (!lscalar)
            e1[[j]]
        else e1
        right <- if (!rscalar)
            e2[[j]]
        else e2
        value[[j]] <- eval(f)
    }
    if (.Generic %in% c("+", "-", "*", "/", "%%", "%/%")) {
        names(value) <- cn
        data.table(value, check.names = FALSE)
    }
    else matrix(unlist(value, recursive = FALSE, use.names = FALSE),
        nrow = nr, dimnames = list(NULL, cn))
}
 

split.data.table = function(...) {
    if (cedta() && getOption("datatable.dfdispatchwarn",TRUE))  # or user can use suppressWarnings
        warning("split is inefficient. It copies memory. Please use [,j,by=list(...)] syntax. See data.table FAQ.")
    NextMethod()  # allow user to do it though, split object will be data.table's with 'NA' repeated in row.names silently
}

# TO DO, add more warnings e.g. for by(), telling user what the data.table syntax is but letting them dispatch to data.frame if they want




