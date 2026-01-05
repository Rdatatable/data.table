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
                  list_search=FALSE, list_len_threshold=100000L)
{
  # Prints name, size and colnames of all data.tables in the calling environment by default
  mb_name = as.character(substitute(mb))
  if (isTRUE(mb)) { mb=type_size; mb_name="type_size" }
  names = ls(envir=env, all.names=TRUE)  # include "hidden" objects (starting with .)
  obj = mget(names, envir=env)  # doesn't copy; mget is ok with ... unlike get, #5197
  w = which(vapply_1b(obj, is.data.table))

  list_info = NULL
  if (list_search) {
    is_list = vapply_1b(obj, is.list)
    dt_mask = logical(length(obj))
    if (length(w)) dt_mask[w] = TRUE
    is_df = vapply_1b(obj, is.data.frame)
    list_candidates = which(is_list & !dt_mask & !is_df)

    if (length(list_candidates)) {
      n_extra = 0L
      list_locations = vector("list", length(list_candidates))
      for (j in seq_along(list_candidates)) {
        obj_idx = list_candidates[j]
        L = obj[[obj_idx]]
        if (!is.list(L)) next
        lenL = length(L)
        if (is.finite(list_len_threshold) && lenL > list_len_threshold) next
        elem_is_dt = vapply_1b(L, is.data.table)
        idx = which(elem_is_dt)
        if (length(idx)) {
          list_locations[[j]] = idx
          n_extra = n_extra + length(idx)
        }
      }

      if (n_extra) {
        list_info = data.table(NAME=character(n_extra), NROW=0L, NCOL=0L, MB=0.0, COLS=list(), KEY=list(), INDICES=list())
        extra_row = 0L
        for (j in seq_along(list_candidates)) {
          obj_idx = list_candidates[j]
          idx = list_locations[[j]]
          if (is.null(idx) || !length(idx)) next
          L = obj[[obj_idx]]
          obj_name = names[obj_idx]
          elem_names = names(L)
          for (k in idx) {
            extra_row = extra_row + 1L
            DT = L[[k]]
            if (!is.null(elem_names) && length(elem_names) >= k && nzchar(elem_names[k])) {
              nm = paste0(obj_name, "$", elem_names[k])
            } else {
              nm = paste0(obj_name, "[[", k, "]]")
            }
            set(list_info, extra_row, "NAME", nm)
            set(list_info, extra_row, "NROW", nrow(DT))
            set(list_info, extra_row, "NCOL", ncol(DT))
            if (is.function(mb)) set(list_info, extra_row, "MB", as.integer(mb(DT)/1048576L))
            if (!is.null(tt<-names(DT))) set(list_info, extra_row, "COLS", tt)
            if (!is.null(tt<-key(DT))) set(list_info, extra_row, "KEY", tt)
            if (index && !is.null(tt<-indices(DT))) set(list_info, extra_row, "INDICES", tt)
          }
        }
      }
    }
  }

  info = NULL
  if (length(w)) {
    info = data.table(NAME=names[w], NROW=0L, NCOL=0L, MB=0.0, COLS=list(), KEY=list(), INDICES=list())
    for (i in seq_along(w)) {  # avoid rbindlist(lapply(DT_names)) in case of a large number of tables
      DT = obj[[w[i]]]
      set(info, i, "NROW", nrow(DT))
      set(info, i, "NCOL", ncol(DT))
      if (is.function(mb)) set(info, i, "MB", as.integer(mb(DT)/1048576L)) # i.e. 1024**2
      if (!is.null(tt<-names(DT))) set(info, i, "COLS", tt)  # TODO: don't need these if()s when #5526 is done
      if (!is.null(tt<-key(DT))) set(info, i, "KEY", tt)
      if (index && !is.null(tt<-indices(DT))) set(info, i, "INDICES", tt)
    }
  }

  if (!is.null(list_info) && nrow(list_info)) {
    if (is.null(info)) {
      info = list_info
    } else {
      info = rbind(info, list_info)
    }
  }

  if (is.null(info) || !nrow(info)) {
    if (!silent) catf("No objects of class data.table exist in %s\n", if (identical(env, .GlobalEnv)) ".GlobalEnv" else format(env))
    return(invisible(data.table(NULL)))
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
