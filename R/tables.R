# globals to pass NOTE from R CMD check, see http://stackoverflow.com/questions/9439256
MB = NCOL = NROW = INDICES = NULL

type_size = function(DT) {
  # for speed and ram efficiency, a lower bound by not descending into character string lengths or list items
  # if a more accurate and higher estimate is needed then user can pass object.size or alternative to mb=
  # in case number of columns is very large (e.g. 1e6 columns) then we use a for() to avoid allocation of sapply()
  ans = 0.0
  lookup = c("raw"=1L, "integer"=4L, "double"=8L, "complex"=16L)
  for (i in seq_along(DT)) {
    col = DT[[i]]
    tt = lookup[storage.mode(col)]
    if (is.na(tt)) tt = .Machine$sizeof.pointer
    tt = tt*nrow(DT)
    if (is.factor(col)) tt = tt + nlevels(col)*.Machine$sizeof.pointer
    ans = ans + tt
  }
  ans + ncol(DT)*.Machine$sizeof.pointer  # column name pointers
}

tables = function(mb=type_size, order.col="NAME", width=80L,
                  env=parent.frame(), silent=FALSE, index=FALSE,
                  depth=0L)
{
  # Prints name, size and colnames of all data.tables in the calling environment by default
  mb_name = as.character(substitute(mb))
  if (isTRUE(mb)) { mb=type_size; mb_name="type_size" }
  names = ls(envir=env, all.names=TRUE)  # include "hidden" objects (starting with .)
  obj = mget(names, envir=env)  # doesn't copy; mget is ok with ... unlike get, #5197
  w = which(vapply_1b(obj, is.data.table))

  info = NULL
  # we check if depth=1L is requested and add found tables to w
  if (depth == 1L) {
    is_list = vapply_1b(obj, is.list)
    is_df = vapply_1b(obj, is.data.frame)
    is_dt = vapply_1b(obj, is.data.table)
    # list_index is a index of list which is not data.frame or data.table
    list_index = which(is_list & !is_dt & !is_df)
    # obj_list is a list of lists of data.tables found inside lists
    obj_list = vector("list", length(list_index))
    #make a listof size list_index and add wl in it
    total_dt = 0L
    # filling obj_list and counting total_dt
    for (i in seq_along(list_index)) {
      L = obj[[list_index[i]]]
      wl = which(vapply_1b(L, is.data.table))
      total_dt = total_dt + length(wl)
      obj_list[[i]] = L[wl]
    }
    name_count = length(w) + total_dt
    # initialize info data.table with total number of data.tables found
    if (name_count == 0L) {
      # nocov start. Requires long-running test case
      if (!silent) catf("No objects of class data.table exist in %s\n", if (identical(env, .GlobalEnv)) ".GlobalEnv" else format(env))
      # nocov end
      return(invisible(data.table(NULL)))
    }
    # create info data.table with total rows equal to number of data.tables found
    info = data.table(NAME=character(name_count), NROW=0L, NCOL=0L, MB=0.0, COLS=list(), KEY=list(), INDICES=list())
    # fill in the names of data.tables found in w
    for (i in seq_along(w)) {  # names of w items
      set(info, i, "NAME", names[w[i]])
    }
    # now fill in the data.tables found inside lists
    cnt = 1L
    if (total_dt > 0L) {
      for (i in seq_along(list_index)) {
        if (length(obj_list[[i]]) == 0L) next
        # get the parent list name
        parent_name = names[list_index[i]]
        for (j in seq_along(obj_list[[i]])) {
          elem_names = names(obj[[list_index[i]]])
          if (!is.null(elem_names) && nzchar(elem_names[j])) {
            new_name = paste0(parent_name, "$", elem_names[j])
          } else {
            new_name = paste0(parent_name, "[[", j, "]]")
          }
          DT = obj_list[[i]][[j]]
          k = cnt + length(w) # row number in info data.table
          cnt = cnt + 1L
          set(info, k, "NAME", new_name)
          set(info, k, "NROW", nrow(DT))
          set(info, k, "NCOL", ncol(DT))
          if (is.function(mb)) set(info, k, "MB", as.integer(mb(DT)/1048576L)) # i.e. 1024**2
          if (!is.null(tt<-names(DT))) set(info, k, "COLS", tt)  # TODO: don't need these if()s when #5526 is done
          if (!is.null(tt<-key(DT))) set(info, k, "KEY", tt)
          if (index && !is.null(tt<-indices(DT))) set(info, k, "INDICES", tt)
        }
      }
    }
  } else if (depth == 0L) {
    # if depth is 0
    if (!length(w)) {
      if (!silent) catf("No objects of class data.table exist in %s\n", if (identical(env, .GlobalEnv)) ".GlobalEnv" else format(env))
      return(invisible(data.table(NULL)))
    }
    info = data.table(NAME=names[w], NROW=0L, NCOL=0L, MB=0.0, COLS=list(), KEY=list(), INDICES=list())
  } else {
    # for depth greater than 1L,recursion is not implemented yet
    stop("depth > 1L is not implemented yet", call. = FALSE)
  }
  for (i in seq_along(w)) {  # avoid rbindlist(lapply(DT_names)) in case of a large number of tables
    DT = obj[[w[i]]]
    set(info, i, "NROW", nrow(DT))
    set(info, i, "NCOL", ncol(DT))
    if (is.function(mb)) set(info, i, "MB", as.integer(mb(DT)/1048576L)) # i.e. 1024**2
    if (!is.null(tt<-names(DT))) set(info, i, "COLS", tt)  # TODO: don't need these if()s when #5526 is done
    if (!is.null(tt<-key(DT))) set(info, i, "KEY", tt)
    if (index && !is.null(tt<-indices(DT))) set(info, i, "INDICES", tt)
  }
  if (!is.function(mb)) info[,MB:=NULL]
  if (!index)           info[,INDICES:=NULL]
  if (!order.col %chin% names(info)) stopf("order.col='%s' not a column name of info", order.col)
  info = info[base::order(info[[order.col]])]  # base::order to maintain locale ordering of table names
  if (!silent) {
    # add commas into NROW, NCOL and MB when displayed on console
    # but this added all these numbers as strings to the character cache which causes the character cache to
    # grow especially with a lot of tables, or changing tables over time. Stopped for now to avoid a tipping
    # point in RSS in #5520
    # pretty_format = function(x, width) format(prettyNum(x, big.mark=","), width=width, justify="right")
    # tt = shallow(info)
    # tt[ , NROW := pretty_format(NROW, width=4L)]
    # tt[ , NCOL := pretty_format(NCOL, width=4L)]
    # if (is.function(mb)) tt[ , MB := pretty_format(MB, width=2L)]
    print(info, class=FALSE, nrows=Inf)
    if (is.function(mb)) catf("Total: %sMB using %s\n", prettyNum(sum(info$MB), big.mark=","), mb_name)
  }
  invisible(info)
}
