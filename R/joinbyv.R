joinbyv <- function(master, join, 
                    by = lapply(join, key), 
                    col.subset = lapply(join, names), 
                    row.subset = as.list(rep(TRUE,length(join))),
                    nomatch = lapply(row.subset, function(x) if(is.expression(x)) 0 else getOption("datatable.nomatch")),
                    allow.cartesian = as.list(rep(getOption("datatable.allow.cartesian"),length(join)))){
  # input check
  stopifnot(all(!missing(master),!missing(join))) # non missing mandatory args
  if(!is.data.table(master)) master <- master[[1]] # conver master list(DT) to DT
  stopifnot(all(
    is.data.table(master), !is.data.table(join), is.list(join), # master is DT, join is list
    is.list(by), is.list(col.subset), is.list(row.subset), is.list(nomatch), is.list(allow.cartesian) # all other args are lists
  ))
  # at least key(join) or by provided - raise error on missing
  valid_key <- mapply(join = join, by = by, 
                      FUN = function(join, by) if(is.null(by) & is.null(key(join))) FALSE else TRUE,
                      SIMPLIFY = FALSE)
  if(any(!(unlist(valid_key)))) stop(paste0("Missing key on 'join' data.table AND corresponding setkeyv vector in 'by' argument for join DT#: ",paste(which(!(unlist(valid_key))),collapse=", "),". Read: ?joinbyv."),call.=FALSE) # raise error cause it is mandatory
  # col.subset names match
  valid_col <- mapply(join = join, col.subset = col.subset, 
                      FUN = function(join, col.subset) if(!all(col.subset %in% names(join))) FALSE else TRUE,
                      SIMPLIFY = FALSE)
  if(any(!(unlist(valid_col)))) stop(paste0("Column names provided in 'col.subset' does not exists in corresponding 'join' object. Read: ?joinbyv"),call.=FALSE)
  # equal length for args
  stopifnot(length(unique(c(length(join),length(by),length(col.subset),length(row.subset),length(nomatch),length(allow.cartesian))))==1)
  
  # fill provided NULL with default value for each arg
  by <- mapply(join = join, by = by, FUN = function(join, by) if(is.null(by)) key(join) else by, SIMPLIFY=FALSE)
  col.subset <- mapply(join = join, col.subset = col.subset, FUN = function(join, col.subset) if(is.null(col.subset)) names(join) else col.subset, SIMPLIFY=FALSE)
  row.subset <- lapply(row.subset, FUN = function(row.subset) if(is.null(row.subset)) TRUE else row.subset)
  nomatch <- mapply(row.subset = row.subset, nomatch = nomatch, 
                    FUN = function(row.subset, nomatch){
                      return(if(is.null(nomatch)){
                        if(is.expression(row.subset)) 0 else getOption("datatable.nomatch")
                      } else nomatch)
                    }, SIMPLIFY = FALSE)
  allow.cartesian <- lapply(allow.cartesian,
                            FUN = function(allow.cartesian){
                              if(is.null(allow.cartesian)) getOption("datatable.allow.cartesian") else allow.cartesian
                            })
  
  # setkey on join tables
  invisible(mapply(join = join, by = by, i = seq_along(join),
                   FUN = function(join, by, i){
                     if(is.null(key(join))) setkeyv(join,by) else{
                       if(!identical(key(join),by)){
                         setkeyv(join,by)
                         warning(paste("Origin key of join element overwritten by key defined in 'by' argument for DT#:",i), call.=FALSE)
                       } else NULL
                     }
                   }, SIMPLIFY = FALSE))
  
  # check col.subset column names overlap handling
  lookup.cols <- unlist(col.subset)[!(unlist(col.subset) %in% unlist(lapply(join, key)))]
  if(length(unique(lookup.cols))!=length(lookup.cols)){
    non_unq <- unique(lookup.cols[lookup.cols %in% names(which(table(lookup.cols)>1))])
    warning(paste0("Column names overlaps in 'col.subset': ",paste(non_unq,collapse=", "),". Columns will be removed from the result. Check the uniqueness of 'col.subset' elements, specify unique column names to be kept, alternatively rename columns on input. Read: ?joinbyv"), call.=FALSE)
    col.subset <- lapply(col.subset, function(col.subset) col.subset[!(col.subset %in% non_unq)])
  }
  
  # joinby fun
  joinby <- function(master, join, by, col.subset, row.subset, nomatch, allow.cartesian){
    # loop level check: key(join) %in% names(master) - check case placed here to handle also dimension hierarchy in Snowflake schema where columns used for next joins are taken from previous joins, not from the initial master table.
    if(!all(key(join) %in% names(master))) stop(paste0("Missing columns ",paste(key(join)[!(key(join) %in% names(master))],collapse=", ")," in master table, cannot perform join."),call.=FALSE)
    # setkey on master
    if(!identical(key(master),key(join))) setkeyv(master,key(join)) # resorting issue to each join, possible improvement after FR: #691, #692.
    # join
    join[tryCatch(expr = eval(row.subset), # row.subset eval expression error handling
                  error = function(e) stop(paste0("Provided 'row.subset' expression results error: ",paste(as.character(c(e$call,e$message)),collapse=" : ")),call.=FALSE)),
         .SD, # col.subset
         .SDcols = unique(c(key(join),col.subset))
         ][master, # join master 
           nomatch = nomatch, # outer / inner join
           allow.cartesian = allow.cartesian # cartesian product allowed?
           ][, 
             .SD, # col.subset post-join, keep only columns asked in joinbyv function call
             .SDcols = unique(c(col.subset,names(master)))
             ]
  }
  # exec main loop
  for(i in 1:length(join)){
    master <- joinby(master = master, join[[i]], by[[i]], col.subset[[i]], row.subset[[i]], nomatch[[i]], allow.cartesian[[i]])
  }
  # clean last used key
  setkeyv(master,NULL)
}
