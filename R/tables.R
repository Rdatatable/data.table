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
    info = data.table(
        NAME=tab,
        NROW=format(sprintf("%4s",prettyNum(as.character(sapply(tab, function(x) nrow(get(x,envir=env)))),big.mark=",")),justify="right")
    )
    if (mb) {
        # mb is an option because object.size appears slow
        s = sapply(tab, function(x) ceiling(object.size(get(x,envir=env))/1024^2))
        info$MB = format(s,justify="right")
        total = sum(s)
    }
    info$COLS = as.vector(sapply(tab, function(x) paste(colnames(get(x,envir=env)),collapse=",")))
    info$KEY = as.vector(sapply(tab, function(x) paste(attr(get(x,envir=env),"sorted"),collapse=",")))
    if (!order.col %in% names(info)) stop("order.col='",order.col,"' not a column name of info") 
    info = info[base::order(info[[order.col]])]  # base::order to maintain locale ordering of table names
    m = as.matrix(info)
    colnames(m)[2] = sprintf(paste("%",nchar(m[1,"NROW"]),"s",sep=""), "NROW")
    if (mb) colnames(m)[3] = sprintf(paste("%",nchar(m[1,"MB"]),"s",sep=""), "MB")
    m[,"COLS"] = substring(m[,"COLS"],1,width)
    m[,"KEY"] = substring(m[,"KEY"],1,width)
    if (!silent) {
        print(m, quote=FALSE, right=FALSE)
        if (mb) cat("Total: ",prettyNum(as.character(total),big.mark=","),"MB\n",sep="")
    }
    invisible(info)
}
