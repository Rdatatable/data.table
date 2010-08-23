
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

NCOL = function(x) {
    # copied from base, but additionally covers data.table via is.list()
    # because NCOL in base explicity tests using is.data.frame()
    if (is.list(x) && !is.ff(x)) return(length(x))
    if (is.array(x) && length(dim(x)) > 1) ncol(x) else as.integer(1)
}
NROW = function(x) {
    if (is.data.frame(x) || is.data.table(x)) return(nrow(x))
    if (is.list(x) && !is.ff(x)) stop("List is not a data.frame or data.table. Convert first before using NROW")   # list may have different length elements, which data.table and data.frame's resolve.
    if (is.array(x)) nrow(x) else length(x)
}

# removed DT alias as j=list() is much faster with new dogroups ... DT = function(...) data.table(...)  # DT is alias for data.table intended for use in j expressions.

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
        if (any(vnames[!novname] %in% c(".SD",".SDF"))) stop("A column may not be called .SD or .SDF, those are reserved")
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
            if (is.vector(xi) || is.factor(xi)) {
                x[[i]] <- rep(xi, length.out = nr)
                next
            }
            if (is.character(xi) && class(xi) == "AsIs") {
                cl <- class(xi)
                x[[i]] <- list(structure(rep(xi, length.out = nr), class = cl))
                next
            }
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
      if (!is.character(key) || !length(key)==1) stop("key must be character vector length 1 containing comma seperated column names")
      eval(parse(text=paste("setkey(value,",paste(key,collapse=","),")",sep="")))
    }
    value
}


"[.data.table" = function (x, i, j, by=NULL, with=TRUE, nomatch=NA, mult="first", roll=FALSE, rolltolast=FALSE, which=FALSE, bysameorder=FALSE, verbose=getOption("datatable.verbose",FALSE), drop=NULL)  # the drop is to sink drop argument when dispatch to [.data.frame, but we don't use ... as that stops test 147
{
    if (cendta()) {
        if (missing(drop)) return(`[.data.frame`(x,i,j))
        else return(`[.data.frame`(x,i,j,drop))
    }
    # To add to documentation: DT[i,colA] will return a one-column data.table. To get a vector do DT[i,colA][[1]].  Neater than DT[i,colA,drop=TRUE]. Instead of DT[,colA][[1]], just do DT$colA, if there is no subset i required.
    if (!missing(by) && missing(j)) stop("'by' is supplied but not j")
    if (!mult %in% c("first","last","all")) stop("mult argument can only be 'first','last' or 'all'")   # Since SQL inherently is not ordered it can't do first or last without another sub-query and compute time.
    if (roll && rolltolast) stop("roll and rolltolast cannot both be true")
    # Removed for now ... if ((roll || rolltolast) && missing(mult)) mult="last" # for when there is exact match to mult. This does not control cases where the roll is mult, that is always the last one.
    if (!(is.na(nomatch) || nomatch==0)) stop("nomatch must either be NA or 0")
    if (which && !missing(j)) stop("'which' is true but 'j' is also supplied")
    cols <- names(x)
    if (!missing(i)) {
        isub = substitute(i)
        isubl = as.list(isub)
        if (identical(isubl[[1]],quote(eval))) {
            i = try(eval(isubl[[2]],parent.frame()), silent=TRUE)  # same reason doing it this way as comment further down for bysub
        } else {
            i = try(eval(isub, envir=x, enclos=parent.frame()), silent=TRUE)
        }
        if (inherits(i,"try-error")) {
            cat("Error hint: the i expression sees the column variables. Column names (variables) will mask variables in the calling frame. Check for any conflicts.\n")
            stop(i)
        }
        if (is.logical(i)) {
            if (identical(i,NA)) i = NA_integer_  # see DT[NA] thread re recycling of NA logical
            else i[is.na(i)] = FALSE              # To simplify statement so don't have to do TABLE[!is.na(ColA) & ColA==ColB]
        }    
        if (is.null(i)) return(structure(NULL,class=c("data.table","data.frame"),row.names=.set_row_names(0)))
        if (is.character(i)) {
            # user can feel like they are using rownames if they like
            if (!haskey(x)) stop("The data.table has no key but i is character. Call setkey first, see ?setkey.")
            if (!is.factor(x[[match(key(x)[1], colnames(x))]])) stop("The data.table has a key, but the first column of that key is not factor. i cannot be character in this case")
            i = J(i)    # so DT[c("e","f")] returns different order to DT[c("f","e")] as is most natural. Pass in SJ(c("f","e")) to guarantee result of grouping is sorted by the groups and is key'd by group
        }
        if (is.data.table(i)) {
            if (!haskey(x)) stop("When i is a data.table, x must be sorted to avoid a vector scan of x per row of i")
            rightcols = match(key(x),colnames(x))
            if (any(is.na(rightcols))) stop("sorted columns of x don't exist in the colnames. data.table is not a valid data.table")
            for (a in rightcols) if (!typeof(x[[a]]) %in% c("integer","logical")) stop("sorted column ",colnames(x)[a], " in x is not internally type integer")
            byval = i
            if (haskey(i)) {
                leftcols = match(key(i),colnames(i))
                if (any(is.na(leftcols))) stop("sorted columns of i don't exist in the colnames of i. data.table is not a valid data.table")
                if (length(rightcols) < length(leftcols)) stop("i has more sorted columns than x has sorted columns, invalid sorted match")
                if (missing(mult) && length(rightcols) > length(leftcols)) mult="all"  # See .Rd documentation
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
                iby = key(x)[seq(1,length(attr(i,"sorted")))]
            } else {
                leftcols = 1:min(ncol(i),length(rightcols))     # The order of the columns in i must match to the order of the sorted columns in x
                if (missing(mult) && length(rightcols) > length(leftcols)) mult="all"  # See .Rd documentation
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
                iby = key(x)[seq(1,ncol(i))]
            }
            idx.start = integer(nrow(i))
            idx.end = integer(nrow(i))
            .Call("binarysearch", i, x, as.integer(leftcols-1), as.integer(rightcols-1), haskey(i), roll, rolltolast, idx.start, idx.end, PACKAGE="data.table")            
            if (mult=="all") {
                lengths = idx.end - idx.start + 1
                idx.diff = rep(1L, sum(lengths))
                idx.diff[head(cumsum(lengths), -1) + 1] = tail(idx.start, -1) - head(idx.end, -1)
                idx.diff[1] = idx.start[1]
                irows = cumsum(idx.diff)
            } else {
                irows = if (mult=="first") idx.start else idx.end
            }
            if (is.na(nomatch) || nomatch!=0) irows[irows==0] = nomatch   # typically you would say nomatch=NA to obtain an outer join, and nomatch=0 for an inner join (i.e. to remove rows with no match)
        } else {
            # i is not a data.table
            if (!is.logical(i) && !is.numeric(i)) stop("i has not evaluated to logical, integer or double")
            # i was passed in as integer row numbers, or the i expression evaluation to integer rows
            # i out of bounds is now allowed and just returns NA as a data.frame does. i==0 returns an empty template (see FAQ 2.03)
            irows = i
        }
        if (which) return(irows)
    } # end of  if !missing(i)
    if (!missing(j)) {
        jsub = substitute(j)
        jsubl = as.list(jsub)
        if (identical(jsubl[[1]],quote(eval))) {
            jsub = eval(jsubl[[2]],parent.frame())  # same reason doing it this way as comment further down for bysub
        }
        o__ = as.integer(NULL)
        if (with) {
            # j will be evaluated within the frame of data.table, even if it is a single column number or character name, see FAQ 1.1 and 1.2
            if (mult=="all" && missing(by)) {
                # Like a 'by' but without using 'by'.  The groupings come instead from each row of the i data.table.
                # Useful for a few known groups rather than a 'by' on the whole table followed by a subset afterwards.
                f__ = idx.start
                len__ = as.integer(idx.end-idx.start+1)
                #len__[idx.start==0] = 0L
                # byval = i
                names(byval) = iby
                #if (!is.na(nomatch) && nomatch==0) {
                #    len__ = len__[f__>0]
                #    byval = i[f__>0]  # the levels on x trickle through to byval here, via the trickle above where i factors are mapped to x factors.
                #    #names(byval) = iby  # the names of the group columns come from x since those are the cols we're joining to.  inci argument might later include i columns as they are.
                #    f__ = f__[f__>0]
                #}
                bysameorder = haskey(i) #TRUE  # think of a mult='all' as by'ing by the join. Setting 'bysameorder' now is more of a fudge to avoid the o__[f__] later.
                if (!is.data.table(i)) stop("logicial error. i is not data.table, but mult='all' and 'by' is missing")
            } else {
                if (!missing(i)) {
                    x = x[irows, nomatch=nomatch, roll=roll, rolltolast=rolltolast, mult=mult]   # note this is a once only recursive call to [.data.table since j is not passed into this call
                    # TO DO: can speed up when i is present by not taking a subset of the whole table first, just the grouping columns.
                }
                if (missing(by) || nrow(x)<1) {
                    if (mode(jsub)!="name" && as.character(jsub[[1]]) %in% c("list","DT")) {
                        jdep = deparse(jsub)
                        jdep = gsub("^list","data.table",jdep)   # we need data.table here because i) it grabs the column names from objects and ii) it does the vector expansion 
                        jdep = gsub("^DT","data.table",jdep)   # for backwards compatibility
                        jsub = parse(text=jdep)[[1]]
                    }
                    return(eval(jsub, envir=x, enclos=parent.frame()))
                }
                    
                # mult must be either 'first' or 'last' at this point only. If it were 'all', the grouping clause above would have been followed.
                # For expression DT[,hist(colA+colB/colC,nclass=1000)] you want the histogram side-effect but not the class returned
                # by the hist() call, In this case the breakpoints print to the console, which you don't want. Inside a function it
                # doesn't matter, but at the console just do invisible(DT[,hist(colA+colB/colC,nclass=1000)]) to avoid the printing.
                
                # by is not missing at this point, and mult may be any value ("all", "first" or "last")
                # Find the groups, using by ...
                bysub = substitute(by)
                if (mode(bysub) %in% c("name","character")) {
                    # name : j may be a single unquoted column name but it must evaluate to list so this is a convenience to users
                    # character: for backwards compatibility with v1.2 syntax passing single character to 'by' rather than list()
                    bysub = parse(text=paste("list(",bysub,")",sep=""))[[1]]
                }
                # The by expression also see variables in the calling frame, just like j.
                # Note that 'by' may be a variable in the calling frame if for example several groupings are
                # required with the same long and complicated 'by' but different j.  This used to be a character
                # vector length one,  but from v1.3 is e.g. bycriteria = quote(list(colA,colB%%100)); DT[...,by=bycriteria]
                
                bysubl = as.list(bysub)
                if (identical(bysubl[[1]],quote(eval))) {
                    bysub = eval(bysubl[[2]],parent.frame())  # [[2]] might be say 'grp' holding an expression. Its done this way so it still works if there happens to be a column called grp.                  
                    bysubl = as.list(bysub)
                }
                byval = eval(bysub, x, parent.frame())
                if (!is.list(byval)) stop("by must evaluate to list")
                for (jj in seq_len(length(byval))) if (!typeof(byval[[jj]]) %in% c("integer","logical")) stop("column or expression ",jj," of 'by' list is not internally type integer. Do not quote column names. Example of correct use: by=list(colA,month(colB),...).")
                tt = sapply(byval,length)
                if (any(tt!=nrow(x))) stop("Each item in the 'by' list must be same length as rows in x (",nrow(x),"): ",paste(tt,collapse=","))
                bynames = names(byval)
                if (is.null(bynames)) bynames = rep("",length(byval))
                if (any(bynames=="")) {
                    if (length(bysubl)<2) stop("When by is list() we expect something inside the brackets")
                    for (jj in seq_along(bynames)) {
                        if (bynames[jj]=="") bynames[jj] = all.vars(bysubl[[jj+1]])[1]
                    }
                    names(byval) = bynames
                }
                byvars = all.vars(bysub)   # not a perfect test but works most of the time. When it doesn't work, it just misses opportunity not to re-sort.
                if (missing(bysameorder) && length(byvars) <= length(key(x)) && identical(byvars,head(key(x),length(byvars)))) {
                    bysameorder=TRUE
                    # table is already sorted by the group criteria, no need to sort
                    # fastorder is so fast though that maybe this is not worth worrying about, especially if fastorder is even faster if its already sorted.
                    # TO DO: turn off bysameorder, and always call fastorder ?
                    # TO DO++: hash the key so sorting never required (hence shash query to r-devel)
                }
                if (verbose) {last.started.at=proc.time()[3];cat("Finding groups (bysameorder=",bysameorder,") ... ",sep="");flush.console()}

                if (bysameorder) {
                    f__ = duplist(byval)
                    for (jj in seq_along(byval)) byval[[jj]] = byval[[jj]][f__]
                } else {
                    o__ = fastorder(byval)
                    f__ = duplist(byval,order=o__)
                    for (jj in seq_along(byval)) byval[[jj]] = byval[[jj]][o__[f__]]
                }
                if (f__[1] != 1) stop("Logical error in grouping. First item of the first group should always be 1.")
                len__ = as.integer(c(diff(f__), nrow(x)-last(f__)+1))
                if (verbose) {cat("done in",round(proc.time()[3]-last.started.at,3),"secs\n");flush.console}
                # TO DO: allow secondary keys to be stored, then we see if our by matches one, if so use it, and no need to sort again. TO DO: document multiple keys.
            }
            ##
            ## At this point we want to be grouping (either by 'by', or by groups in i) and we know the groups and j will be run for each group
            ##

            ## isfun <- try(is.function(j), silent = TRUE)
            ## if (!inherits(isfun, "try-error") && isfun) stop("j may not be a function. Use the column names directly, or write an anonymous body wrapped in {}, or use subtable or subframe objects.")
            # TO DO: if j was a function name and a column name was this name too,  check that above stop doesn't happen.
            # TO DO: check that is.function(j) on j directly rather than jsub is ok.
            jvnames = NULL
            if (mode(jsub)=="name") {
                # j is a single unquoted column name, convenience to use to auto wrap it with list()
                jvnames = deparse(jsub)
                jsub = call("list",jsub)
                #jsub = parse(text=paste("list(`",jsub,"`)",sep=""))[[1]]  # the backtick is for when backtick'd names are passed in          
            } else if (as.character(jsub[[1]]) %in% c("list","DT")) {
                jsubl = as.list(jsub)
                if (length(jsubl)<2) stop("When j is list() or DT() we expect something inside the brackets")
                jvnames = names(jsubl)[-1]   # check list(a=sum(v),v)
                if (is.null(jvnames)) jvnames = rep("", length(jsubl)-1)
                for (jj in 2:length(jsubl)) {
                    if (jvnames[jj-1] == "" && mode(jsubl[[jj]])=="name") jvnames[jj-1] = deparse(jsubl[[jj]])
                    # TO DO: if call to a[1] for example, then call it 'a' too
                }
                if(class(jsubl[[2]])!="{") jsub = parse(text=paste("list(",paste(as.character(jsub)[-1],collapse=","),")",sep=""))[[1]]  # this does two things : i) changes 'DT' to 'list' for backwards compatibility and ii) drops the names from the list so its faster to eval the j for each group
            } # else maybe a call to transform or something which returns a list.
            ws = all.vars(jsub)
            if (any(c(".SD",".SDF") %in% ws))
                vars = colnames(x)        # just using .SD or .SDF triggers using all columns in the subset. We don't try and detect which columns of .SD are being used.
            else 
                vars = ws[ws %in% c(colnames(x))]  # using a few named columns will be faster
            if (!length(vars)) {
                # stop("No column names appear in j expression. No use of .SD or .SDF either.")
                # For macros :
                vars=colnames(x)
            }
            if (mult=="all") {
                ####
                #### TO DO -  revisit JIS w.r.t. dogroups logic
                #### just install the symbols from the i environment into the new.env first, easy
                ####
                # Each row in the i table is one per group. This branch allows you to use variables which are in i but which are not in the join columns, inside the j expression. Really very useful. This is called "join inherited scoping".
                # stop("JIS turned off for the moment")
                ivars = ws[(!ws %in% vars) & ws %in% colnames(i)]
                if (length(ivars)) {
                    if (!missing(by)) stop("Cannot use join inherited scope when by is supplied. Remove the by argument.")
                    e__ = paste(e__,paste(ivars,"=i$",ivars,"[g__]",sep="", collapse=";"), sep=";")    # so its important that i is still the data.table at this point
                }
            }

            
            if (verbose) {last.started.at=proc.time()[3];cat("Starting dogroups ...\n");flush.console()}
            # byretn is all about trying to allocate the right amount of memory for the result, first time. Then there
            # will be no need to grow it as the 'by' proceeds, or use memory for a temporary list of the results.
            if (f__[1]==0 && is.na(nomatch)) {
                itestj = NA
            } else {
                itestj = seq(f__[1],length=len__[1])
                if (length(o__)) itestj = o__[itestj]
            }
            SDenv = new.env(parent=parent.frame()) # use an environment to get the variable scoping right
            SDenv$.SD = x[itestj, vars, with=FALSE]
            testj = eval(jsub, SDenv$.SD, enclos = SDenv)    # our test is now the first group. problems before with the largest being repeated.
            maxn=0
            if (!is.null(testj)) {
                if (is.atomic(testj)) {
                    jvnames = ""
                    jsub = call("list",jsub)
                    # jsub = parse(text=paste("list(",deparse(jsub),")",sep=""))[[1]]
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
                
            byretn = as.integer(byretn) 
            .SD = x[seq(length=max(len__)), vars, with=FALSE]  # allocate enough for largest group, will re-use it, the data contents doesn't matter at this point
            # the subset above keeps factor levels in full
            # TO DO: drop factor levels altogether (as option later) ... for (col in 1:ncol(.SD)) if(is.factor(.SD[[col]])) .SD[[col]] = as.integer(.SD[[col]])
            xcols = as.integer(match(vars,colnames(x)))
            # browser()
            
            ans = .Call("dogroups",x,.SD,xcols,o__,f__,len__,jsub,new.env(parent=parent.frame()),testj,byretn,byval,is.na(nomatch),verbose,PACKAGE="data.table")
            
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
                # either a spliced join, likely i was SJ() or a table already with a key,  or a by to the key columns in order
                setkey("ans",colnames(ans)[seq_along(byval)])
                # TO DO - just mark as sorted, does it really need to be re-sorted ?
            }
            # To delete .. for (s in seq_len(ncol(ans))) if (is.factor(ans[[s]])) ans[[s]] = factor(ans[[s]])  # drop unused levels
            return(ans)
            ##
            ## End of grouping
            ##
        }
        # so j was not mode "call" or "name". It was either character or integer, in both cases meaning column lookup, and with=FALSE.
        if (is.character(j)) j = match(j, names(x))
        if (any(is.na(j))) stop("undefined columns selected")
        if (any(abs(j) > ncol(x) | j==0)) stop("j out of bounds") # j may be passed in as integer. '-' means take away that column
    } else {
        j = seq(along=x)    # so the default is 'select *'
    }
    if (!missing(i)) {
        ans = list()
        #  TO DO: its the i join cols that should be in the result, not the x cols (which could be NA)
        for (s in seq(along=j)) {
            # NA in the i are allowed, returning NA in those postions. 0 is allowed, returning no row for that position.
            ans[[s]] = x[[j[s]]][irows]
            #To delete ... if (is.factor(ans[[s]])) ans[[s]] = factor(ans[[s]])  # drop unused levels
            
            # ans[[s]] = if (length(dim(x[[j[s]]])) != 2) x[[j[s]]][irows] else x[[j[s]]][irows, , drop = FALSE]
            # TO DO : reinstate in future to allow data.table's to have a matrix as a column. Would be nice to have binary search on a key'd matrix.
        }
        names(ans) = names(x)[j]
        if (is.logical(irows) || length(irows)==1 || !is.null(attr(i,"sorted")) || (is.data.table(i) && nrow(i)==1)) {
            # The subset of x was an ordered subset so the result can retain its key
            # the nrow(i)==1 is for example DT["FD"] but where key on DT has 2 columns, so this is a mult="all" join from one row in i and can retain x's key. In this case i=J(i) a long way up this function.
            attr(ans,"sorted") = attr(x,"sorted")[attr(x,"sorted") %in% names(ans)]
        }
    } else {
        if (!missing(j)) {
            class(x) = NULL  # otherwise this function gets called again when we try to subset it further on the next line
            ans <- x[j]  # this doesn't copy the data at this stage.  this is subset of the list, returning a list
        } else {
            # both i and j are missing.  This is like a select * which is just typing the data.table only.
            stop("i and j cannot both be missing in a data.table subset. If you want 'select *', just use the data.table name alone i.e. no []")
        }
    }
    class(ans) = c("data.table","data.frame")
    attr(ans,"row.names") = .set_row_names(nrow(ans))
    ans
}

#  [[.data.frame is now dispatched due to inheritance.
#  The code below tried to avoid that but made things
#  very slow (462 times faster down to 1 in the timings test).
#  TO DO. Reintroduce the bvelow but dispatch straight to
#  .C("do_subset2") or better.

#"[[.data.table" = function(x,...) {
#    if (cendta()) return(`[[.data.frame`(x,...))
#    class(x)=NULL
#    x[[...]]
#}

#"[[<-.data.table" = function(x,i,j,value) {
#    if (cendta()) return(`[[<-.data.frame`(x,i,j,value))
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
    if (cendta()) return(NextMethod())
    i = seq(len=min(n,nrow(x)))
    x[i]
}
tail.data.table = function(x, n=6, ...) {
    if (cendta()) return(NextMethod())
    i = seq(to=nrow(x), length=min(n, nrow(x)))
    x[i]
}

"[<-.data.table" = function (x, i, j, value) {
    if (!missing(i) && !cendta()) { # get i based on data.table-style indexing
        i <- x[i, which=TRUE, mult="all"]
    }
    res <- `[<-.data.frame`(x, i, j, value)
    keycol <- match(key(x), names(x))
    if (missing(j)) j <- seq_along(x)
    if (is.character(j)) {
        jseq <- match(j, names(x))
    } else if (is.logical(j) || min(j) < 0L) 
        jseq <- seq_along(x)[j]
    else {
        jseq <- j
    }
    if (any( jseq %in% keycol ))
        key(res) <- NULL
    res
}

"$<-.data.table" = function (x, name, value) {
    res <- `$<-.data.frame`(x, name, value)
    if (any(name %in% key(x)))
        key(res) <- NULL
    res
}

cbind.data.table = function(...) {
    data.table(...)    # for ease of use basically since people often think its like a matrix.
}

rbind.data.table = function (..., deparse.level=1) {
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

    if (!all(sapply(allargs, is.data.table))) stop("All arguments must be data.tables")
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
    if (cendta()) return(`dimnames.data.frame`(x))
    list(NULL, names(x))
}

"dimnames<-.data.table" = function (x, value)   # so that can do  colnames(dt)=<..>  as well as names(dt)=<..>
{
    if (cendta()) return(`dimnames<-.data.frame`(x,value))
    if (!is.list(value) || length(value) != 2) stop("attempting to assign invalid object to dimnames of a data.table")
    if (!is.null(value[[1]])) stop("data.tables do not have rownames")
    if (ncol(x) != length(value[[2]])) stop("can't assign",length(value[[2]]),"colnames to a",ncol(x),"column data.table")
    names(x) <- as.character(value[[2]])
    x
}

last = function(x) x[NROW(x)]     # last row for a data.table, last element for a vector.

within.data.table <- function (data, expr, keep.key = FALSE, ...) # basically within.list but with a check to avoid messing up the key
{
    if (cendta()) return(NextMethod())
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
    if (cendta()) return(NextMethod())
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
    if (cendta()) return(NextMethod())
    omit = FALSE
    for (i in seq_len(ncol(object))) omit = omit | is.na(object[[i]])
    object[!omit]
    # compare the above to stats:::na.omit.data.frame
}

is.na.data.table <- function (x) {
    if (cendta()) return(`is.na.data.frame`(x))
    do.call("cbind", lapply(x, "is.na"))
}

# not longer needed as inherits ...
#    t.data.table <- t.data.frame
#    Math.data.table <- Math.data.frame
#    summary.data.table <- summary.data.frame

Ops.data.table <- function (e1, e2 = NULL)
{
    if (cendta()) return(`Ops.data.frame`(e1,e2))
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
    if (!cendta() && getOption("datatable.dfdispatchwarn",TRUE))  # or user can use suppressWarnings
        warning("split is inefficient. It copies memory. Please use [,j,by=list(...)] syntax. See data.table FAQ.")
    NextMethod()  # allow user to do it though, split object will be data.table's with 'NA' repeated in row.names silently
}

# TO DO, add more warnings e.g. for by(), telling user what the data.table syntax is but letting them dispatch to data.frame if they want




