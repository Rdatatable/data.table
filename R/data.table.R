
dim.data.table <- function(x) {
    if (length(x)) c(length(x[[1]]), length(x))
    else c(0,0)
    # TO DO: consider placing "dim" as an attibute updated on inserts. Saves this 'if'.
}

print.data.table = function (x, digits = NULL, quote = FALSE, right = TRUE, nrows = 100, ...)
{
    if (nrow(x) == 0) {
        cat("NULL data table\n")
    } else {
        if (is.null(names(x))) stop("Invalid data table, no column names")
        if (nrow(x) > nrows) {
            x = head(x,nrows)
            warning("print.data.table is restricted to printing ",nrows," rows. Call print(...,nrows=) to force print more")
            # TO DO: this warning seems to slow things down somewhere,  or could look at x by ref.
        }
        cn = if (nrow(x)>20) colnames(x) else NULL  # only print colnames at the bottom if over 20 rows
        print(rbind(as.matrix(x),cn), digits=digits, quote=quote, right=right, ...)
        #mm = do.call("cbind", lapply(x, format, digits=digits))
        #print(mm, ..., quote = quote, right = right)
    }
    invisible(x)
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

DT = function(...) data.table(...)  # DT is alias for data.table intended for use in j expressions.

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
    if (identical(x, list(NULL))) return( structure(NULL,class="data.table") )
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
    re = "^[a-zA-Z]+[a-zA-Z0-9_\\.]*$"  # valid column names. expressions can be built on column names so need to be strict here.
    novname = vnames==""
    if (any(!novname)) {
        tt = regexpr(re, vnames[!novname]) < 1
        if (any(tt)) stop("invalid explicit column name supplied (",paste('"',vnames[!novname][tt],'"',sep="",collapse=","),"). Must match regular expression ",re)
    }
    if (any(novname) && length(exptxt)==length(vnames)) {
        okexptxt = regexpr(re, exptxt[novname])==1
        vnames[novname][okexptxt] = exptxt[novname][okexptxt]
    }
    tt = vnames==""
    if (any(tt)) vnames[tt] = paste("V", which(tt), sep = "")
    # so now finally we have good column names. We also will use novname later to know which were explicitly supplied in the call.
    n <- length(x)
    if (n < 1)
        return(structure(list(), class = "data.table"))
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
            else if ((is.atomic(xi) || is.factor(xi) || is.array(xi)) && !is.list(xi)) {     # is.atomic is true for vectors with attributed (such as that returned from idtocode()), but is.vector() is FALSE. this is why we use is.atomic rather than is.vector
                # the is.array is required when, for example, you have a tapply inside a j=data.table(...) expression
##                 if (is.atomic(xi) || is.array(xi)) xi = as.vector(xi)   # to remove any vector names, or dimnames in an array, or attributes (such as that returned from idtocode()), otherwise they get silently retaining in the data.table list members, thus bloating memory
                hascols[i] = FALSE
                if (is.character(xi) && class(xi)!="AsIs") xi = factor(xi)      # coerce characters to factor unless wrapped in I()
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
    attr(value, "class") <- "data.table"
    if (!is.null(key)) {
      if (!is.character(key) || !length(key)==1) stop("key must be character vector length 1 containing comma seperated column names")
      eval(parse(text=paste("setkey(value,",paste(key,collapse=","),")",sep="")))
    }
    value
}

"[.data.table" = function (x, i, j, by, ..., with=TRUE, nomatch=NA, mult="first", roll=FALSE, rolltolast=FALSE, simplify=TRUE, which=FALSE, incbycols=!bysameorder, bysameorder=FALSE, verbose=FALSE)
{
    # TODO: DT[i,"colA"] will return a one-column data.table. To get a vector do DT[i,"colA"][[1]].  Neater than DT[i,"colA",drop=TRUE]. Instead of DT[,"colA"][[1]], just do DT$colA, if there is no subset i required.
    # TODO: So drop=FALSE is always implicitly the case.  Its nice for joins to simply say RT[LT[,"val"]] for example.
    # TODO: allow distinction between primary key and non-unique key. Auto checks on duplicates.
    force(incbycols)  # we will change bysameorder later
    if (!missing(by) && missing(j)) stop("'by' is supplied but not j")
    if (!mult %in% c("first","last","all")) stop("mult argument can only be 'first','last' or 'all'")   # Since SQL inherently is not ordered it can't do first or last without another sub-query and compute time.
    if (roll && rolltolast) stop("roll and rolltolast cannot both be true")
    # Confusing this line to change default of arguments, removed for now ... if ((roll || rolltolast) && missing(mult)) mult="last" # for when there is exact match to mult. This does not control cases where the roll is mult, that is always the last one.
    if (!(is.na(nomatch) || nomatch==0)) stop("nomatch must either be NA or 0")
    if (which && !missing(j)) stop("'which' is true but 'j' is also supplied")
    # if (mult=="all" && missing(by) && missing(incbycols)) incbycols = FALSE  # incbycols is set to default FALSE in this case, as you already have the groups being passed in.
    cols <- names(x)
    if (!missing(i)) {
        if (mode(substitute(i))=="call") {
            # so 'i' can be any expression of column names, like the 'where' clause of SQL.
            # i can also include objects from the calling frame e.g. TABLE[ColA %in% Lkp] where Lkp is a vector defined in the calling frame.
            if (substring(deparse(substitute(i))[1],1,2) %in% c("J(","SJ","CJ")) {
                # We want the join table to be constructed in the frame of the caller, not inside the data.table.
                # If "ids" exists as a column name of x, we don't want those to be used by the join function.
                # It doesn't matter if there are no column names the same as variables in the caller.
                i = try(eval(substitute(i), envir=parent.frame(), enclos=parent.frame()),silent=TRUE)
            } else {
                # Usual where clause via vector scan, no join. For example  DT[id=="A"]
                i = try(eval(substitute(i), envir=x, enclos=parent.frame()),silent=TRUE)
            }
            if (inherits(i,"try-error")) {
                cat("Error hint: the i expression sees the column variables. Column names (variables) will mask variables in the calling frame. Check for any conflicts.\n")
                stop(i)
            }
            if (is.logical(i)) i[is.na(i)] = FALSE  # To simplify statement so don't have to do TABLE[!is.na(ColA) & ColA==ColB]
        }
        if (is.null(i)) return(structure(NULL,class="data.table"))
        if (is.character(i)) {
           # user can feel like they are using rownames if they like
           if (is.null(attr(x,"sorted"))) stop("data.table's do not have rownames, call setkey() first on the data.table")
           if (!is.factor(x[[match(attr(x,"sorted")[1], colnames(x))]])) stop("the data.table has a key, but the first column of that key is not factor. i cannot be character in this case")
           i = J(i)    # so DT[c("e","f")] returns different order to DT[c("f","e")] as is most natural. Pass in SJ(c("f","e")) to guarantee result of grouping is sorted by the groups and is key'd by group
        }
        if (is.data.table(i)) {
            # join
            if (is.null(attr(x,"sorted"))) stop("When i is a data.table, x must be sorted to avoid a vector scan of x per row of i")
            rightcols = match(attr(x,"sorted"),colnames(x))
            if (any(is.na(rightcols))) stop("sorted columns of x don't exist in the colnames. data.table is not a valid data.table")
            for (a in rightcols) if (storage.mode(x[[a]])!="integer") stop("sorted column ",colnames(x)[a], " in x is not storage.mode integer")
            if (!is.null(attr(i,"sorted"))) {
                leftcols = match(attr(i,"sorted"),colnames(i))
                if (any(is.na(leftcols))) stop("sorted columns of i don't exist in the colnames of i. data.table is not a valid data.table")
                if (length(rightcols) < length(leftcols)) stop("i has more sorted columns than x has sorted columns, invalid sorted match")
                if (missing(mult) && length(rightcols) > length(leftcols)) mult="all"  # See .Rd documentation
                for (a in seq(along=leftcols)) { #attr(i,"sorted")) {
                    lc = leftcols[a]
                    rc = rightcols[a]
                    if (storage.mode(i[[lc]])!="integer") stop("sorted column ",colnames(i)[lc], " of i must be storage.mode integer")
                    if (is.factor(i[[lc]])) {
                        if ((roll || rolltolast) && a==length(leftcols)) stop("Attempting roll join on factor column i.",colnames(i)[lc],". Only integer columns (i.e. not factors) may be roll joined")
                        if (!is.factor(x[[rc]])) stop("i.",colnames(i)[lc]," is a factor joining to x.",colnames(x)[rc]," which is not a factor. Factors must join to factors.")
                        i[[lc]] = sortedmatch(levels(i[[lc]]), levels(x[[rc]]), nomatch=NA)[i[[lc]]]
                        # since factor levels are always sorted,  this match will be sorted too. There is no need to re-sort.
                        # NAs can be produced by this level match, in which case the C code (it knows integer value NA) can skip over the lookup.
                        # its therefore important we pass NA rather than 0 to the C code.
                        # For factor levels in the order of several thousand, standard match is extremely innefficient, sortedmatch yields a 96% saving.
                    } else {
                        if (is.factor(x[[rc]])) stop("x.",colnames(x)[rc]," is a factor but joining to i.",colnames(i)[lc]," which is not a factor. Factors must join to factors.")
                    }
                }
                iby = attr(x,"sorted")[seq(1,length(attr(i,"sorted")))]
                cfunct = "sortedmatch"
            } else {
                leftcols = 1:length(i)     # The order of the columns in i must match to the order of the sorted columns in x
                if (length(rightcols) < length(leftcols)) stop("i has more columns than x has sorted columns, invalid sorted match")
                if (missing(mult) && length(rightcols) > length(leftcols)) mult="all"  # See .Rd documentation
                for (lc in leftcols) {
                    rc = rightcols[lc]
                    if (storage.mode(i[[lc]])!="integer") stop("unsorted column ",colnames(i)[lc], " of i is not storage.mode integer.")   # i is not sorted so all columns of i form the join criteria to the sorted columns in x
                    if (is.factor(i[[lc]])) {
                        if ((roll || rolltolast) && lc==last(leftcols)) stop("Attempting roll join on factor column i.",colnames(i)[lc],". Only integer columns may be roll joined")
                        if (!is.factor(x[[rc]])) stop("i.",colnames(i)[lc]," is a factor but the corresponding x.",colnames(x)[rc]," is not a factor")
                        i[[lc]] = sortedmatch(levels(i[[lc]]), levels(x[[rc]]), nomatch=NA)[i[[lc]]]
                    } else {
                        if (is.factor(x[[rc]])) stop("x.",colnames(x)[rc]," is a factor but joining to i.",colnames(i)[lc]," which is not a factor. Factors must join to factors.")
                    }
                }
                iby = attr(x,"sorted")[seq(1,ncol(i))]
                cfunct = "unsortedmatch"
            }
            if (mult=="all") {
                idx.start = integer(nrow(i))
                .Call(paste(cfunct,"first",sep=""), i, x, as.integer(leftcols-1), as.integer(rightcols-1), idx.start, as.integer(roll), as.integer(rolltolast),PACKAGE="data.table")
                idx.end = integer(nrow(i))
                .Call(paste(cfunct,"last",sep=""), i, x, as.integer(leftcols-1), as.integer(rightcols-1), idx.end, as.integer(roll), as.integer(rolltolast),PACKAGE="data.table")
##                 irows = as.vector(unlist(mapply(seq, idx.start, idx.end,SIMPLIFY=FALSE))) # replaced by the cumsum'ing below
                lengths = idx.end - idx.start + 1
                idx.diff = rep(1L, sum(lengths))
                idx.diff[head(cumsum(lengths), -1) + 1] = tail(idx.start, -1) - head(idx.end, -1)
                idx.diff[1] = idx.start[1]
                irows = cumsum(idx.diff)
            } else {
                cfunct = paste(cfunct,mult,sep="")
                idx = integer(nrow(i))
                # the work above is easier in R, only do the core search in C, to keep C code small => easy to maintain and debug via gdb
                .Call(cfunct, i, x, as.integer(leftcols-1), as.integer(rightcols-1), idx, as.integer(roll), as.integer(rolltolast),PACKAGE="data.table")
                irows = idx
            }
            if (is.na(nomatch) || nomatch!=0) irows[irows==0] = nomatch   # typically you would say nomatch=NA to obtain an outer join, and nomatch=0 for an inner join (i.e. to remove rows with no match)
        } else {
            if (!is.logical(i)) {
               if (!is.numeric(i)) stop("i has not evaluated to integer or double")
               # i was passed in as integer row numbers, or the i expression evaluation to integer rows, which we should therefore check for our of bounds.
               if (any(abs(i) > nrow(x), na.rm=TRUE)) stop("i out of bounds")  # 0 is allowed to select an empty table e.g. copy a table structure
            }
            irows = i
        }
        if (which) return(irows)    # just return the joined row numbers
    }
    if (!missing(j)) {
        if (with && mode(substitute(j)) %in% c("call","name")) {
            # so 'j' is an expression of column names, like the 'select' clause of SQL
            # This j can also include objects from the calling frame e.g. TABLE[,sum(ColA*K)] where K is a constant defined in the calling frame.
            # If you want columns 4:5, or c("colC","colE"), as you would in a data.frame, then these are expression of mode "call" and
            # if passed in raw would eval literally.  To avoid this set with=FALSE. Alternatives e.g. wrapping j argument with I() was investigated
            # but with=FALSE seems more R-like and more reliable.
            # Similarly do not write DT[,c(colA,colB)] as this will return a single column, instead write DT[,list(colA,colB)].
            # The "__" postfix in the following code is to avoid scoping conflicts with column names.
            if (mult=="all" && missing(by)) {
                f__ = idx.start
                len__ = as.integer(idx.end-idx.start+1)
                len__ = len__[f__>0]
                f__ = f__[f__>0]
                bysameorder = TRUE  # think of a mult='all' as by'ing by the join. Setting 'bysameorder' now is more of a fudge to avoid the o__[f__] later.
                # groups = mapply(seq, idx.start, idx.end, SIMPLIFY=FALSE)
                # we will 'by' by the join to multiple.  In other words, 'by' the columns in the i table
                # This is efficient and helps code brevity.  We don't need to join and then group - both steps are done in one operation. If only a few known groups are required, it saves calculating for all groups then subsetting afterwards.
                # If you don't want to group by the same column in the i table, just provide an explicit 'by'
            } else {
                if (!missing(i)) x = x[irows, nomatch=nomatch, roll=roll, rolltolast=rolltolast, mult=mult]   # note this is a once only recursive call to [.data.table since j is missed here
                # TO DO: can speed up, by not taking a subset of the whole table first, just the grouping columns.
                if (missing(by)) {
                    return(eval(substitute(j), envir=x, enclos=parent.frame()))
                    # mult must not be 'all'
                    # For expression DT[,hist(colA+colB/colC,nclass=1000)] you want the histogram side-effect but not the class returned
                    # by the hist() call, In this case the breakpoints print to the console, which you don't want. Inside a function it
                    # doesn't matter, but at the console just do invisible(DT[,hist(colA+colB/colC,nclass=1000)]) to avoid the printing.
                }
                # by is not missing at this point, but mult may be any value ("all", "first" or "last")
                # Find the groups, using by ...
                if (!is.character(by) || length(by)!=1) stop("by must be character vector length 1 containing comma seperated columns or expressions")
                # Note that 'by' may be a variable in the calling frame which evaluates to the character string to group by. Therefore, do not test for mode(substitute(by)=="character").
                # You can pass functions and factors into j for 'by'ing. Anything that can be evaluated inside the subset frame. Those also see parameters in the calling frame.

                # bysplit = strsplit(by,split=",")[[1]]
                # The parse line below is similar in spirit to the strsplit above. However it copes with 'by' expressions such as  by="grp=rep('ALL',length(o)),d" which have named arguments, and comma's inside each column, so the text has to be evaluated and then split
                bysplit = as.list(as.list(parse(text=paste("c(",by,")",sep="")))[[1]])[-1]
                for (jj in bysplit) if (!with(x, storage.mode(eval(jj)))%in%c("integer","character","logical")) stop("by expression '",deparse(jj),"' is not integer") # numeric, even when appearing to be integer, cause uniqueness problems due to numerical precision.
                by = paste(bysplit,collapse=",")        # in simple cases of comma seperated column names, by ends up the same.  With named arguments in the 'by', the names are discarded by this.

                if (length(bysplit)<=length(attr(x,"sorted")) && identical(as.character(bysplit),head(attr(x,"sorted"),length(bysplit)))) {
                    # table is already sorted by the group criteria
                    # if an expression is passed in the by string, this clause will never happen, and we will have to sort by 'by' in the alternative
                    f__ = with(x, eval(parse(text=paste("duplist(",by,")",sep=""))))     # f will be the first item of each group
                    if (f__[1] != 1) stop("Logical error in grouping by primary key")
                    bysameorder = TRUE   # whatever was passed in to this function is ignored.
                } else {
                    if (bysameorder) stop("bysameorder=TRUE but table is not in same order as the 'by'") # this is when appending a column to a table using by. Specify 'bysameorder' as TRUE to make the assumption about table order explicitly checked.
                    if (verbose) cat("Creating secondary key sort order ...")
                    o__ = with(x, eval(parse(text=paste("fastorder(",by,")",sep=""))))
                    f__ = with(x, eval(parse(text=paste("duplist(",by,",order=o__)",sep=""))))
                    if (f__[1] != 1) stop("Logical error in grouping by secondary key")
                    # TO DO: allows secondary keys to be stored, then we see if our by matches one, if so use it, and no need to sort again. TO DO: document multiple keys.
                    bysameorder = FALSE
                }
                len__ = as.integer(c(diff(f__), nrow(x)-last(f__)+1))
            }
            # Now we have the rows for each group, we look to the j expressions, and evaluate it WITHIN the list frame WITHOUT copying *all* the columns.
            # By replacing the column names with the [subset] afterwards,  you also only reference the columns required, ignoring columns not used in the expression, for efficiency.
            # TODO: This code for grouping has efficiency problems and likely would be better in C.
            #
            isfun <- try(is.function(j), silent = TRUE)
            if (!inherits(isfun, "try-error") && isfun) {
                # Allow j as a function with ... passed as parameters to that function
                # Right now, the ... are not evaluated within the data.table environment
                if (bysameorder) # no resorting needed
                    ans = lapply(1:length(f__), function(g__) j(x[f__[g__] + 1:len__[g__] - 1], ...))
                else
                    ans = lapply(1:length(f__), function(g__) j(x[o__[f__[g__] + 1:len__[g__] - 1]], ...))
            } else {
                ws = unique(words(deparse(substitute(j))))
                vars = ws[ws %in% colnames(x)]
                if (!length(vars)) stop("No column names used in j expression. If j is a function, columns must be passed explicity.")
                cp__ = parse(text=paste(vars,"COL__=",vars,ifelse(bysameorder,"","[o__]"),sep="",collapse=";"))    # make a reference to the original entire vector cp=copy but doesn't really copy. If secondary key, then we'll reorder the columns the expression needs only. Then the rest of the code is the same.
                e__ = paste(vars,"=vecref(",vars,"COL__,f__[g__],len__[g__])",sep="",collapse=";")
                if (mult=="all") {
                    # Each row in the i table is one per group. This branch allows you to use variables which are in i but which are not in the join columns, inside the j expression. Really very useful. Call this "join inherited scoping" and document it (TO DO).
                    ivars = ws[(!ws %in% vars) & ws %in% colnames(i)]
                    if (length(ivars)) {
                        if (!missing(by)) stop("Cannot use join inherited scope when by is supplied. Remove the by argument.")  # TO DO: document "join inherited scoping"
                        e__ = paste(e__,paste(ivars,"=i$",ivars,"[g__]",sep="", collapse=";"), sep=";")    # so its important that i is still the data.table at this point
                    }
                }
                jtxt = as.character(substitute(j))
                if (jtxt[1]=="list") {
                    # take out the names of the named arguments (and store them for later), so each ans element doesn't duplicate the names many times
                    jvnames = names(as.list(substitute(j)))[-1]
                    e__ = parse(text=paste(e__,";list(",paste(jtxt[-1],collapse=","),")",sep=""))
                } else {
                    jvnames = NULL
                    e__ = parse(text=paste(e__,";",paste(trim(deparse(substitute(j))),collapse=""),sep=""))
                    # e__ = parse(text=paste(e__,";",paste(trim(deparse(substitute(j))),collapse=";"),sep=""))
                    # The collapse=";" is for when {} is passed in as the j expression e.g. "{browser();sum(a)}", or "{param=2;sum(a+param)}"
                }
                # new method above allows a subset once, for only the columns we need, as before, but solves the named data.table column problem
                # old method ...e = parse(text=gsubwords(colnames(x), paste(colnames(x),"[groups[[g]]]",sep=""), paste(trim(deparse(substitute(j))),collapse="")))
                # we used to allow functions such as head, or last, but other easier ways to do that, so no longer available here to keep things simple.
                ans = with(x, {eval(cp__); lapply(1:length(f__),function(g__)eval(e__))})
            }
            # TO DO: port this into C, ignoring names etc
            # TO DO: add the following lines into the C (too slow up here in R) :
            #  grouping.started.at = proc.time()
            #  last.printed = grouping.started.at[3]
            #  if (proc.time()[3] > last.printed + 10) {cat("  ",percent(g,length(f))," groups done in ",time.taken(grouping.started.at),"    \r",sep="");last.printed=proc.time()[3]}
            #  if (1 || verbose || last.printed > grouping.started.at[3]) cat(" ",percent(length(f),length(f))," groups done in ",time.taken(grouping.started.at),"    \n",sep="")
            if (simplify) {
                tt = sapply(ans,is.null)        # TO DO : add test case for first null 'by'
                if (all(tt)) return(data.table(NULL))
                first.non.null = fnn = which.first(!tt)
                if (is.null(dim(ans[[fnn]]))) {
                  if (!is.list(ans[[fnn]])) {
                      r = sapply(ans,NROW)
                      ans = data.table(unlist(ans,use.names=FALSE))     # most usually the vector result of sum(<colA>) or similar. We always return a data.table from a data.table subset, so we always know where we are, and don't need to test the result for single case. Unless each j returns a matrix in which case we rbind the matrices together below.
                  } else {
                      r = sapply(ans,function(l)length(l[[1]]))     # we assume each list item is the same length
                      l = lapply(1:length(ans[[1]]), function(l) unlist(lapply(ans,"[[",l)))
                      # the above is not too bad speed wise.  This is why j=list(...) is quicker than j=data.table(...)
                      names(l) = jvnames
                      ans = data.table(l)
                  }
                } else {
                    r = sapply(ans,NROW)
                    ans = do.call("rbind",ans)        # TO DO: use a list in each group rather than the full data.table. All the c.factor will slow down.
                }
                if (!incbycols) {
                    if (NROW(ans) < nrow(x)) warning("incbycols=FALSE but result is not the same length as the table")
                    else if (!bysameorder) warning("incbycols=FALSE but by is not by the table order")
                    # These warning should not be ignored lightly.
                }
                if (incbycols) {
                    if (any(r>1)) f__ = rep(f__,times=r)
                    if (missing(by)) bysplit = iby  # only occurs when mult="all" and by is missing.
                    if (!bysameorder) f__=o__[f__]
                    tt = paste("(",as.character(bysplit),")[f__]",sep="")
                    re = "^[a-zA-Z]+[a-zA-Z0-9_]*$"  # copied from data.table(). Should have global variable in one place ideally.
                    bynames = as.character(bysplit)
                    if (!is.null(names(bysplit))) {
                        xx = names(bysplit)!=""
                        bynames[xx] = names(bysplit)[xx]
                    }
                    bynames[regexpr(re, bynames) < 1] = ""  # when unnamed expressions are passed into by, this line blanks off the line since / * etc are not allowed in column names.
                    havenames = bynames != ""
                    if (any(havenames)) tt[havenames] = paste( bynames[havenames],"=",tt[havenames],sep="")
                    # because we appended the "[f__]" above, data.table() will no longer be able to deduce the column name so we need to append the "<colname>="
                    tt = paste(tt,collapse=",")
                    e__ = parse(text=paste("data.table(",tt,")",sep=""))
                    grps = with(x, eval(e__))
                    key = if ((!missing(by) && bysameorder) || (!missing(i) && !is.null(attr(i,"sorted")))) paste(names(grps),collapse=",") else NULL
                    # either a spliced join, likely i was SJ() or a table already with a key,  or a by to the key columns in order
                    ans = data.table(grps, ans, key=key)
                }
            } else {
                # Set simplify=FALSE for example if you apply lchg per id, where all ids have the same number of dates. Then do unlist afterwards.
                # Could leave it in this format by default to save 'by'ing all the time.  So a table could look like the unique keys then just the first part of the vector printed out.
                # TO DO: Return a key'd list.
            }
            return(ans)
            # Note that aggregate() looks good on first glance, but it doesn't automatically with() the arguments. The above is a lot more powerful.
            #     aggregate also calls tapply for each column and the same grouping,  thats really inefficient.
            #     aggregate also restricts FUN to return a scalar.  so again the above is more powerful.
            # DONE: Detect the columns of x that expression j needs, and only get the subset on those columns, to save copying unnecessary data
            # TO DO: Move the grouping operation in C when simple column names are supplied.  When functions are supplied, evaluate them, add a column with the result, then call the C function on the column names.
        }
        # so j was passed in intending a direct column lookup by name or by position, and with=FALSE.
        if (is.character(j)) j = match(j, names(x))
        if (any(is.na(j))) stop("undefined columns selected")
        if (any(abs(j) > ncol(x) | j==0)) stop("j out of bounds") # j may be passed in as integer. '-' means take away that column
    } else {
        j = seq(along=x)    # so the default is 'select *'
    }
    if (!missing(i)) {
        ans = list()
        for (s in seq(along=j)) {
            # if (!is.null(dim(x[[j[s]]]))) stop("data.tables should only have vectors as columns")
            # ans[[s]] = x[[c(j[s],i)]]  ideal but this fails because [[ can only return 1 result.
            # NA in the i are allowed, returning NA in those postions. 0 is allowed, returning no row for that position.
            if (getRversion() >= "2.4.0") {
                ans[[s]] = x[[j[s]]][irows]
                # Prof Ripley changed [[ as from 2.4.0 (as a result of MD's postings) so that a copy no longer is taken unless necessary.
            } else {
                warning("This R session is < 2.4.0. Please upgrade to 2.4.0+.")
                txt = paste("x$'",names(x)[j[s]],"'[irows]",sep="")
                ans[[s]] = eval(parse(text=txt))    # $colA syntax doesn't copy in versions prior to 2.4, but "[[" does.
            }
            #x[[j]] <- if (length(dim(xj)) != 2)
            #    xj[i]
            #else xj[i, , drop = FALSE]  TO DO: reinstate in future to allow data.table's to have a matrix as a column. Would be nice to have binary search on a key'd matrix.
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
    class(ans) = "data.table"
    ans
}

"[.factor" = function(x, ...)
{
    # change default action of factors to drop unused levels. This saves memory space and copying. It also makes tapply() work as you expect since the levels contain the unique values only, otherwise you get many NAs for the unused factor levels.
    # The base::"[.factor" first creates the integer subset, but with the full levels attached (i.e. copied),  then *if* drop calls factor() on that to then remove the unused levels.
    # Here we force drop=TRUE for efficiency always (not changeable), and do the operation more efficiently.
    # If you really want R's default [.factor,  then call the base version directly using  base::"[.factor"()
    # This [.factor is within the data.table NAMESPACE so users should not see it.
    # R's default [.factor assings constrasts also. Not considered here.
    y <- NextMethod("[")
    u = unique(y)
    su = sort(u)
    attr(y, "levels") = attr(x, "levels")[su]  # relying on the original factor levels being sorted
    y[] = sortedmatch(y, su)
    class(y) = oldClass(x)
    y
}


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
    class(value) <- "data.table"
    value
}

as.data.table.data.frame = function(x, keep.rownames=FALSE)
{
    if (keep.rownames) return(data.table(rn=rownames(x), x, keep.rownames=FALSE))
    attr(x,"row.names") = NULL
    class(x) = "data.table"
    x
}

as.data.table.data.table = function(x, keep.rownames=FALSE) return(x)

head.data.table = function(x, n=6, ...) x[seq(len=min(n,nrow(x)))]
tail.data.table = function(x, n=6, ...) x[seq(to=nrow(x), length=min(n, nrow(x)))]

# TO DO: [[ assignment

"[<-.data.table" <-
function (x, i, j, value)
{
    # TO DO: copied from [<-.data.frame,  remove out all uses of row.names and data.frame
    # TO DO: test this method of assignment as I've tended to use $ on the left hand side.
    nA <- nargs()
    if (nA == 4) {
        has.i <- !missing(i)
        has.j <- !missing(j)
    }
    else if (nA == 3) {
        if (is.atomic(value))
            names(value) <- NULL
        if (missing(i) && missing(j)) {
            i <- j <- NULL
            has.i <- has.j <- FALSE
            if (is.null(value))
                return(x[logical(0)])
        }
        else {
            if (is.logical(i) && is.matrix(i) && all(dim(i) == dim(x))) {
                nreplace <- sum(i, na.rm = TRUE)
                if (!nreplace)
                  return(x)
                N <- length(value)
                if (N > 0 && N < nreplace && (nreplace %% N) == 0)
                  value <- rep(value, length.out = nreplace)
                if (length(value) != nreplace)
                  stop("rhs is the wrong length for indexing by a logical matrix")
                n <- 0
                nv <- nrow(x)
                for (v in seq(len = dim(i)[2])) {
                  thisvar <- i[, v, drop = TRUE]
                  nv <- sum(thisvar, na.rm = TRUE)
                  if (nv) {
                    if (is.matrix(x[[v]]))
                      x[[v]][thisvar, ] <- value[n + (1:nv)]
                    else x[[v]][thisvar] <- value[n + (1:nv)]
                  }
                  n <- n + nv
                }
                return(x)
            }
            if (is.matrix(i))
                stop("only logical matrix subscripts are allowed in replacement")
            j <- i
            i <- NULL
            has.i <- FALSE
            has.j <- TRUE
        }
    }
    else {
        stop("need 0, 1, or 2 subscripts")
    }
    if (has.j && length(j) == 0)
        return(x)
    n <- 0
    nrows <- nrow(x)
    cl <- oldClass(x)
    class(x) <- NULL
    new.cols <- NULL
    nvars <- length(x)
    if (has.i) {
        stop("i not supported, yet")
    }
    else iseq <- NULL
    if (n == 0L)
        n <- nrows
    if (has.j) {
        if (any(is.na(j)))
            stop("missing values are not allowed in subscripted assignments of data frames")
        if (is.character(j)) {
            jj <- match(j, names(x))
            nnew <- sum(is.na(jj))
            if (nnew > 0) {
                n <- is.na(jj)
                jj[n] <- nvars + 1:nnew
                new.cols <- j[n]
            }
            jseq <- jj
        }
        else if (is.logical(j) || min(j) < 0)
            jseq <- seq(along = x)[j]
        else {
            jseq <- j
            if (max(jseq) > nvars) {
                new.cols <- paste("V", seq(from = nvars + 1,
                  to = max(jseq)), sep = "")
                if (length(new.cols) != sum(jseq > nvars))
                  stop("new columns would leave holes after existing columns")
                if (is.list(value) && !is.null(vnm <- names(value))) {
                  p <- length(jseq)
                  if (length(vnm) < p)
                    vnm <- rep(vnm, length.out = p)
                  new.cols <- vnm[jseq > nvars]
                }
            }
        }
    }
    else jseq <- seq(along = x)
    if (any(duplicated(jseq)))
        stop("duplicate subscripts for columns")
    n <- length(iseq)
    if (n == 0)
        n <- nrows
    p <- length(jseq)
    m <- length(value)
    if (!is.list(value)) {
        if (p == 1) {
            N <- NROW(value)
            if (N > n)
                stop(gettextf("replacement has %d rows, data has %d",
                  N, n), domain = NA)
            if (N < n && N > 0)
                if (n%%N == 0 && length(dim(value)) <= 1)
                  value <- rep(value, length.out = n)
                else stop(gettextf("replacement has %d rows, data has %d",
                  N, n), domain = NA)
            names(value) <- NULL
            value <- list(value)
        }
        else {
            if (m < n * p && (n * p)%%m)
                stop(gettextf("replacement has %d items, need %d",
                  m, n * p), domain = NA)
            value <- matrix(value, n, p)
            value <- split(value, col(value))
        }
        dimv <- c(n, p)
    }
    else {
        value <- unclass(value)
        lens <- sapply(value, NROW)
        for (k in seq(along = lens)) {
            N <- lens[k]
            if (n != N && length(dim(value[[k]])) == 2)
                stop(gettextf("replacement element %d is a matrix/data frame of %d rows, need %d",
                  k, N, n), domain = NA)
            if (N > 0 && N < n && n%%N)
                stop(gettextf("replacement element %d has %d rows, need %d",
                  k, N, n), domain = NA)
            if (N > 0 && N < n)
                value[[k]] <- rep(value[[k]], length.out = n)
            if (N > n) {
                warning(gettextf("replacement element %d has %d rows to replace %d rows",
                  k, N, n), domain = NA)
                value[[k]] <- value[[k]][1:n]
            }
        }
        dimv <- c(n, length(value))
    }
    nrowv <- dimv[1]
    if (nrowv < n && nrowv > 0) {
        if (n%%nrowv == 0)
            value <- value[rep(1:nrowv, length.out = n), , drop = FALSE]
        else stop(gettextf("%d rows in value to replace %d rows",
            nrowv, n), domain = NA)
    }
    else if (nrowv > n)
        warning(gettextf("replacement data has %d rows to replace %d rows",
            nrowv, n), domain = NA)
    ncolv <- dimv[2]
    jvseq <- seq(len = p)
    if (ncolv < p)
        jvseq <- rep(1:ncolv, length.out = p)
    else if (ncolv > p)
        warning(gettextf("provided %d variables to replace %d variables",
            ncolv, p), domain = NA)
    if (length(new.cols)) {
        nm <- names(x)
        rows <- attr(x, "row.names")
        x <- c(x, vector("list", length(new.cols)))
        names(x) <- c(nm, new.cols)
        attr(x, "row.names") <- rows
    }
    if (has.i)
        for (jjj in seq(len = p)) {
            jj <- jseq[jjj]
            vjj <- value[[jvseq[[jjj]]]]
            if (jj <= nvars) {
                if (length(dim(x[jj])) != 2)
                  x[[jj]][iseq] <- vjj
                else x[[jj]][iseq, ] <- vjj
            }
            else {
                length(vjj) <- nrows
                x[[jj]] <- vjj
            }
        }
    else if (p > 0)
        for (jjj in p:1) {
            jj <- jseq[jjj]
            x[[jj]] <- value[[jvseq[[jjj]]]]
            if (is.atomic(x[[jj]]))
                names(x[[jj]]) <- NULL
        }
    if (length(new.cols) > 0) {
        new.cols <- names(x)
        if (any(duplicated(new.cols)))
            names(x) <- make.unique(new.cols)
    }
    class(x) <- cl
    x
}

"$<-.data.table" = function (x, i, value)
{
    cl <- oldClass(x)
    nrows <- nrow(x)
    class(x) <- NULL
    if (!is.null(value)) {
        N <- NROW(value)
        if (N > nrows)
            stop(gettextf("replacement has %d rows, data has %d",
                N, nrows), domain = NA)
        if (N < nrows && N > 0)
            if (nrows%%N == 0 && length(dim(value)) <= 1)
                value <- rep(value, length.out = nrows)
            else stop(gettextf("replacement has %d rows, data has %d",
                N, nrows), domain = NA)
        if (is.atomic(value))
            names(value) <- NULL
    }
    x[[i]] <- value
    class(x) <- cl
    return(x)
}


cbind.data.table = function(...) data.table(...)    # for ease of use basically since people often think its like a matrix.

rbind.data.table = function (...) {
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
        return(structure(list(), class = "data.table"))

    if (!all(sapply(allargs, is.data.table))) stop("All arguments must be data.tables")
    if (length(unique(sapply(allargs, ncol))) != 1) stop("All data.tables must have the same number of columns")

    l = list()
    nm = names(allargs[[1]])
    if (length(nm) && n>1) {
        for (i in 2:n) if (length(names(allargs[[i]])) && !all(names(allargs[[i]]) == nm)) warning("colnames of argument ",i," don't match colnames of argument 1")
    }
    #for (i in 1:length(allargs[[1]])) l[[i]] = unlist(lapply(allargs, "[[", i))
    for (i in 1:length(allargs[[1]])) l[[i]] = do.call("c", lapply(allargs, "[[", i))
    # Changed from unlist to do.call("c",...) so that c.factor is called on factor columns
    names(l) = nm
    class(l) = "data.table"
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
    attr(x,"row.names") = 1:nrow(x) # since R 2.4.0, data.frames can have non-character row names
    class(x) = "data.frame"
    x
}


dimnames.data.table = function(x) list(NULL, names(x))

"dimnames<-.data.table" = function (x, value)   # so that can do  colnames(dt)=<..>  as well as names(dt)=<..>
{
    if (!is.list(value) || length(value) != 2) stop("attempting to assign invalid object to dimnames of a data.table")
    if (!is.null(value[[1]])) stop("data.tables do not have rownames")
    if (ncol(x) != length(value[[2]])) stop("can't assign",length(value[[2]]),"colnames to a",ncol(x),"column data.table")
    names(x) <- as.character(value[[2]])
    x
}

last = function(x) x[NROW(x)]     # last row for a data.table, last element for a vector.

within.data.table <- function (data, expr, keep.key = FALSE, ...) # basically within.list but with a check to avoid messing up the key
{
    parent <- parent.frame()
    e <- evalq(environment(), data, parent)
    eval(substitute(expr), e)
    l <- as.list(e)
    l <- l[!sapply(l, is.null)]
    nD <- length(del <- setdiff(names(data), (nl <- names(l))))
    data[nl] <- l
    if (nD)
        data[del] <- if (nD == 1)
            NULL
        else vector("list", nD)
    if (!keep.key) attr(data,"sorted") <- NULL
    data
}


transform.data.table <- function (`_data`, ...) # basically transform.data.frame with data.table instead of data.frame
{
    e <- eval(substitute(list(...)), `_data`, parent.frame())
    tags <- names(e)
    inx <- match(tags, names(`_data`))
    matched <- !is.na(inx)
    if (any(matched)) {
        `_data`[inx[matched]] <- e[matched]
        `_data` <- data.table(`_data`)
    }
    if (!all(matched))
        do.call("data.table", c(list(`_data`), e[!matched]))
    else `_data`
}

subset.data.table <- function (x, subset, select, ...) # not exported or documented, yet
{
    if (missing(subset))
        r <- TRUE
    else {
        e <- substitute(subset)
        r <- eval(e, x, parent.frame())
        if (!is.logical(r))
            stop("'subset' must evaluate to logical")
        r <- r & !is.na(r)
    }
    if (missing(select))
        vars <- 1:ncol(x)
    else {
        nl <- as.list(1L:ncol(x))
        names(nl) <- names(x)
        vars <- eval(substitute(select), nl, parent.frame())
    }
    x[r, vars, with = FALSE]
}
