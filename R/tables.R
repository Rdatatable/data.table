
MB = NCOL = NROW = NULL   # globals to pass NOTE from R CMD check

tables = function(mb=TRUE,order.col="NAME",width=80,env=parent.frame(),silent=FALSE)
{
    # Prints name, size and colnames of all data.tables in the calling environment by default
    tt = objects(envir=env, all.names=TRUE)
    ss = which(as.logical(sapply(tt, function(x) is.data.table(get(x,envir=env)))))
    if (!length(ss)) {
        if (!silent) cat("No objects of class data.table exist in .GlobalEnv\n")
        return(invisible(data.table(NULL)))
    }
    tab = tt[ss]
    info = data.table(NAME=tab)
    for (i in seq_along(tab)) {
        DT = get(tab[i],envir=env)   # doesn't copy
        set(info,i,"NROW",nrow(DT))
        set(info,i,"NCOL",ncol(DT))
        if (mb) set(info,i,"MB",ceiling(as.numeric(object.size(DT))/1024^2))  # mb is an option because object.size() appears to be slow. TO DO: revisit
        set(info,i,"COLS",paste(colnames(DT),collapse=","))
        set(info,i,"KEY",paste(key(DT),collapse=","))
    }
    info[,NROW:=format(sprintf("%4s",prettyNum(NROW,big.mark=",")),justify="right")]   # %4s is for minimum width
    info[,NCOL:=format(sprintf("%4s",prettyNum(NCOL,big.mark=",")),justify="right")]
    if (mb) {
        total = sum(info$MB)
        info[, MB:=format(sprintf("%2s",prettyNum(MB,big.mark=",")),justify="right")]
    }
    if (!order.col %in% names(info)) stop("order.col='",order.col,"' not a column name of info") 
    info = info[base::order(info[[order.col]])]  # base::order to maintain locale ordering of table names
    m = as.matrix(info)
    colnames(m)[2] = sprintf(paste("%",nchar(m[1,"NROW"]),"s",sep=""), "NROW")
    colnames(m)[3] = sprintf(paste("%",nchar(m[1,"NCOL"]),"s",sep=""), "NCOL")
    if (mb) colnames(m)[4] = sprintf(paste("%",nchar(m[1,"MB"]),"s",sep=""), "MB")
    m[,"COLS"] = substring(m[,"COLS"],1,width)
    m[,"KEY"] = substring(m[,"KEY"],1,width)
    if (!silent) {
        print(m, quote=FALSE, right=FALSE)
        if (mb) cat("Total: ",prettyNum(as.character(total),big.mark=","),"MB\n",sep="")
    }
    invisible(info)
}


