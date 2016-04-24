MB = NCOL = NROW = UNQ_NROW = 
  KEY_UNQ_NROW = TABLE_NAME = COLUMN_NAME = 
  ORDER = IS_KEY = IS_INDEX = COUNT = COUNT_NA =
  RATIO_NA = NULL   # globals to pass NOTE from R CMD check

tables <- function(mb=TRUE, order.col="NAME", width=80L,
                   env=parent.frame(), silent=FALSE, pretty=TRUE, 
                   stats=FALSE, index=FALSE, address=FALSE)
{
    #to eliminate overlap with address function
    where = address; rm(address)
    # Prints name, size and colnames of all data.tables in the 
    #   calling environment by default
    all_obj = objects(envir=env, all.names=TRUE)
    DT_ind = which(as.logical(vapply(all_obj, function(x) 
      is.data.table(get(x, envir=env)), logical(1L))))
    if (!length(DT_ind)) {
        if (!silent) cat("No objects of class data.table exist in",
                         "environment", environmentName(env), "\n")
        return(invisible(data.table(NULL)))
    }
    DT_n = all_obj[DT_ind]
    info = rbindlist(lapply(DT_n, function(dt_n){
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
      if (pretty) {
        set(info_i, , "COLS", paste(names(DT), collapse=","))
        set(info_i, , "KEY", paste(key(DT), collapse=","))
        if (index)
          set(info_i, , "INDICES", paste(indices(DT), collapse=","))
      } else {
        set(info_i, , "COLS",list(list(names(DT))))
        set(info_i, , "KEY", list(list(key(DT))))
        if (index)
          set(info_i, , "INDICES", list(list(indices(DT))))
      }
      if (stats) {
        set(info_i, , "UNQ_NROW", 
            if (all_atomic <- all(vapply(DT, is.atomic, logical(1L)))) 
              uniqueN(DT, by=NULL)
            else NA_integer_)
        set(info_i, , "KEY_UNQ_NROW", 
            if (is.null(ky <- key(DT)) && !all_atomic)
              NA_integer_
            else uniqueN(DT, by=ky))
      }
      if (where)
        set(info_i, , "ADDRESS", paste0("<", address(DT), ">"))
      info_i
    }))
    if (pretty) {
        # %4s is for minimum width
        prettify <- function(x) 
          format(sprintf("%4s", prettyNum(x, big.mark=",")), justify = "right")
        pretty_cols <- c("NROW", "NCOL", 
                         if (stats) c("UNQ_NROW", "KEY_UNQ_NROW"))
        info[ , (pretty_cols) := lapply(.SD, prettify), .SDcols=pretty_cols]
    }
    if (mb) {
        total = sum(info$MB)
        if (pretty) 
          set(info, , "MB", 
              with(info, format(sprintf("%2s", prettyNum(MB, big.mark=",")), 
                                justify="right")))
    }
    if (!order.col %in% names(info)) 
      stop("order.col='", order.col, "' is not a column name of info table")
    # base::order to maintain local ordering of table names
    info = info[base::order(info[[order.col]])]  
    if (silent){
        return(invisible(info))
    } else if(!pretty) {
        return(info)
    } else {
      m = as.matrix(info)
      rownames(m) <- paste0(seq_len(nrow(m)), ":")
      colnames(m)[2L] = 
        sprintf(paste("%", nchar(m[1L, "NROW"]), "s", sep=""), "NROW")
      colnames(m)[3L] = 
        sprintf(paste("%", nchar(m[1L, "NCOL"]), "s", sep=""), "NCOL")
      if (mb) 
        colnames(m)[4L] = 
        sprintf(paste("%", nchar(m[1L, "MB"]), "s", sep=""), "MB")
      m[ , "COLS"] = substring(m[ , "COLS"], 1L, width)
      m[ , "KEY"] = substring(m[ , "KEY"], 1L, width)
      if (index)
        m[ , "INDICES"] = substring(m[ , "INDICES"], 1L, width)
      print(m, quote=FALSE, right=FALSE)
      if (mb) 
        cat("Total: ", 
            prettyNum(as.character(total), big.mark=","),
            "MB\n", sep="")
      return(invisible(info))
    }
}

columns <- function(env=parent.frame(), silent=FALSE,
                    pretty=TRUE, stats=FALSE, address=FALSE)
{
    #to eliminate overlap with address function
    where = address; rm(address)
    all_obj = objects(envir=env, all.names=TRUE)
    DT_ind = which(as.logical(vapply(all_obj, function(x) 
      is.data.table(get(x, envir=env)), logical(1L))))
    if (!length(DT_ind)) {
        if (!silent) cat("No objects of class data.table exist in",
                         "environment", environmentName(env), "\n")
        return(invisible(data.table(NULL)))
    }
    DT_n = all_obj[DT_ind]
    info = rbindlist(lapply(DT_n, function(dt_n){
      cols = names(DT <- get(dt_n, envir=env))
      if (!length(cols)) return(data.table(TABLE_NAME=dt_n))
      info_i <- 
        data.table(TABLE_NAME=dt_n, COLUMN_NAME=cols,
                   ORDER=seq_len(length(cols)),
                   TYPE=vapply(DT, typeof, character(1L)))
      set(info_i, , "IS_KEY", 
          with(info_i, COLUMN_NAME %chin% key(DT)))
      set(info_i, , "IS_INDEX",
          with(info_i, if (is.null(idx <- indices(DT))) FALSE
               else COLUMN_NAME %chin%
                 unlist(strsplit(idx, split = "__", fixed = TRUE))))
      if (pretty){
        set(info_i, , "CLASS", vapply(DT, function(col)
          paste(class(col), collapse=","), character(1L)))
      } else {
        set(info_i, , "CLASS", list(lapply(DT, class)))
      }
      if (stats){
        set(info_i, , "COUNT", nrow(DT))
        set(info_i, , "COUNT_UNQ", vapply(names(DT), function(jj) 
          DT[ , if (is.atomic(x <- eval(as.name(jj))))
            uniqueN(x) else length(unique(x))], integer(1L)))
        set(info_i, , "COUNT_NA", vapply(names(DT), function(jj) 
          # based on http://stackoverflow.com/questions/29683752/
          DT[ , sum(is.na(eval(as.name(jj))))], integer(1L)))
        set(info_i, , "RATIO_NA",
            with(info_i, {x <- COUNT_NA / COUNT
            x[COUNT == 0] <- NA_real_; x}))
      }
      if (where)
        set(info_i, , "ADDRESS", vapply(names(DT), function(jj)
          paste0("<", address(DT[[jj]]), ">"), character(1L)))
      info_i
    }), fill=TRUE)
    if (silent) invisible(info) else info[]
}
