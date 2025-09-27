"tables" <-
function(mb=FALSE,order.col="NAME",width=80)
{
    # Prints name, size and colnames of all data.tables in .GlobalEnv
    tt = objects(env=.GlobalEnv)
    ss = which(sapply(tt, function(x) class(get(x,envir=.GlobalEnv))) == "data.table")
    if (!length(ss)) {
        cat("No objects of class data.table exist in .GlobalEnv\n")
        return(invisible())
    }
    tab = tt[ss]
    info = data.table(
        NAME=tab, 
        NROW=format(sprintf("%4s",prettyNum(as.character(sapply(tab, function(x) nrow(get(x,envir=.GlobalEnv)))),big.mark=",")),justify="right")
    )
    if (mb) {
        s = sapply(tab, function(x) ceiling(object.size(get(x,envir=.GlobalEnv))/1024^2))
        info$MB = format(s,justify="right")
        total = sum(s)
    }
    info$COLS = as.vector(sapply(tab, function(x) paste(colnames(get(x,envir=.GlobalEnv)),collapse=",")))
    info = info[order(get(order.col))]
    m = as.matrix(info)
    colnames(m)[2] = sprintf(paste("%",nchar(m[1,"NROW"]),"s",sep=""), "NROW")
    if (mb) colnames(m)[3] = sprintf(paste("%",nchar(m[1,"MB"]),"s",sep=""), "MB")
    m[,"COLS"] = substring(m[,"COLS"],1,width)
    print(m, quote=FALSE, right=FALSE)
    if (mb) cat("Total: ",prettyNum(as.character(total),big.mark=","),"MB\n",sep="")
    invisible(info)
}

