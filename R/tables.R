# globals to pass NOTE from R CMD check, see http://stackoverflow.com/questions/9439256
MB = NCOL = NROW = NULL

tables <- function(mb=TRUE, order.col="NAME", width=80,
                   env=parent.frame(), silent=FALSE, index=FALSE)
{
    # Prints name, size and colnames of all data.tables in the calling environment by default
    all_obj = objects(envir=env, all.names=TRUE)
    is_DT = which(vapply_1b(all_obj, function(x) is.data.table(get(x, envir=env))))
    if (!length(is_DT)) {
        if (!silent) cat("No objects of class data.table exist in .GlobalEnv\n")
        return(invisible(data.table(NULL)))
    }
    DT_names = all_obj[is_DT]
    info = rbindlist(lapply(DT_names, function(dt_n){
        DT = get(dt_n, envir=env)   # doesn't copy
        info_i =
            data.table(NAME = dt_n,
                       NROW = nrow(DT),
                       NCOL = ncol(DT))
        if (mb)
          # mb is an option because object.size() appears to be slow.
          # **TO DO: revisit**
          set(info_i, , "MB",
                #1048576 = 1024^2
                round(as.numeric(object.size(DT))/1048576))
        set(info_i, , "COLS", list(list(names(DT))))
        set(info_i, , "KEY", list(list(key(DT))))
        if (index) set(info_i, , "INDICES", list(list(indices(DT))))
        info_i
    }))
    info[ , NROW := format(sprintf("%4s", prettyNum(NROW, big.mark=",")), justify="right")]   # %4s is for minimum width
    info[ , NCOL := format(sprintf("%4s", prettyNum(NCOL, big.mark=",")), justify="right")]
    if (mb) {
        total = sum(info$MB)
        info[ , MB := format(sprintf("%2s", prettyNum(MB, big.mark=",")), justify="right")]
    }
    if (!order.col %in% names(info)) stop("order.col='",order.col,"' not a column name of info")
    info = info[base::order(info[[order.col]])]  # base::order to maintain locale ordering of table names
    m = as.matrix(info)
    colnames(m)[2] = sprintf(paste("%",nchar(m[1,"NROW"]), "s", sep=""), "NROW")
    colnames(m)[3] = sprintf(paste("%",nchar(m[1,"NCOL"]), "s", sep=""), "NCOL")
    if (mb) colnames(m)[4] = sprintf(paste("%", nchar(m[1,"MB"]), "s", sep=""), "MB")
    m[ , "COLS"] = substring(m[,"COLS"], 1L, width)
    m[ , "KEY"] = substring(m[,"KEY"], 1L, width)
    if (!silent) {
        print(m, quote=FALSE, right=FALSE)
        if (mb) cat("Total: ", prettyNum(as.character(total), big.mark=","), "MB\n", sep="")
    }
    invisible(info)
}


