# mergelist join tester vs SQLite, based on v1.9.8 non-equi join tester

if (!all(
  packageVersion("RSQLite") > "1.0.0", ## we use window function which is not available in RSQLite v1.0.0 (sqlite 3.8.6)
  packageVersion("RSQLite") < "1.1.0"  ## we don't want upstream RSQLite because it is not lite anymore
))
  stop('Use RSQLite which is up-to-date and lightweight:\ninstall.packages(c("DBI","RSQLite"), repos="https://jangorecki.gitlab.io/rsqlite")')

# as of now run by: Rscript inst/tests/sqlite.R
#library(data.table); shallow = data.table:::shallow
cc(quiet=TRUE)

# funs ----

# produce SQL statement
# ln, rn: lhs names, rhs names
sql = function(how, on, mult, ln, rn, notjoin=FALSE) {
  stopifnot(length(on)==1L)
  # building sql query
  if (how=="full") {
    return(sprintf(
      "%s\nUNION ALL\n%s",
      sql("left", on, mult, ln, rn),
      sql("right", on, mult, ln, rn, notjoin=TRUE)
    ))
  }
  nm = list()
  if (how=="right") {l = "i"; r = "x"; jn="LEFT"}
  else {l = "x"; r = "i"; jn=toupper(how)}
  nm[["x"]] = ln; nm[["i"]] = rn
  using = sprintf("USING (%s)", paste(on, collapse=", "))
  if (mult=="all") {
    lhs = sprintf("(\n  SELECT %s FROM %s\n) %s", paste(setdiff(nm[["x"]],"row_id"), collapse=", "), "x", "x")
    rhs = sprintf("(\n  SELECT %s FROM %s\n) %s", paste(setdiff(nm[["i"]],"row_id"), collapse=", "), "i", "i")
  } else {
    lhs = sprintf("(\n  SELECT %s FROM %s\n) %s", paste(setdiff(nm[["x"]],"row_id"), collapse=", "), "x", "x")
    rhs = sprintf("(SELECT %s FROM (\n  SELECT *, ROW_NUMBER() OVER (PARTITION BY %s ORDER BY row_id %s) AS rownum FROM %s\n) %s WHERE rownum=1) %s",
                  paste(setdiff(nm[["i"]],c("row_id","rownum")), collapse=", "),
                  paste(on, collapse=", "),
                  if (mult=="first") "ASC" else "DESC",
                  "i", "i", "i")
  }
  if (how=="right") {
    join = sprintf("%s\nLEFT JOIN\n%s\nUSING (%s)",
                   rhs, lhs, paste(on, collapse=", "))
  } else {
    join = sprintf("%s\n%s JOIN\n%s\nUSING (%s)",
                   lhs, toupper(how), rhs, paste(on, collapse=", "))
  }
  if (!notjoin) where = ""
  else where = sprintf("\nWHERE %s IS NULL", paste(r, on, sep="."))
  select = sprintf("%s, %s, %s",
                   paste(l, on, sep="."),
                   paste("x",setdiff(nm[["x"]], c("row_id",on)),sep=".",collapse=", "),
                   paste("i",setdiff(nm[["i"]], c("row_id",on)),sep=".",collapse=", "))
  sprintf("SELECT %s FROM\n%s%s", select, join, where)
}

# .conn SQLite connection, if provided it will use it instead of creating temporary one
# .drop logical TRUE (default) will drop db tables before and after and populate new, when FALSE it expects tables to be populated
join.sql.equal = function(l, on, how="inner", mult="all", allow.cartesian=TRUE, .conn, .drop=TRUE, .debug=interactive(), ans) {
  if (!requireNamespace("DBI", quietly=TRUE)) stop("join.sql.equal requires DBI package")
  if (!requireNamespace("RSQLite", quietly=TRUE)) stop("join.sql.equal uses RSQLite package to validate results")
  if (is.null(names(l))) names(l) = c("x","i")
  x = l[["x"]]
  i = l[["i"]]
  stopifnot(is.data.table(x), is.data.table(i),
            is.character(how), is.character(mult), length(mult)==1L,
            is.character(on),
            is.logical(allow.cartesian), is.logical(.drop))
  if (mult=="error") {
    dt = try(silent=TRUE, mergelist(list(x, i), on=on, how=how, mult=mult))
    return(inherits(dt, "try-error"))
  }
  # row_id column required as SQL is not ordered, creating on R side
  if (!"row_id" %in% names(x)) x = shallow(x)[, "row_id" := seq_len(.N)]
  if (!"row_id" %in% names(i)) i = shallow(i)[, "row_id" := seq_len(.N)]
  # preparing sql environment
  conn = if (new.conn <- missing(.conn)) DBI::dbConnect(RSQLite::SQLite()) else .conn
  if (.drop) {
    try(DBI::dbSendQuery(conn, "DROP TABLE x;"), silent = TRUE)
    try(DBI::dbSendQuery(conn, "DROP TABLE i;"), silent = TRUE)
    DBI::dbWriteTable(conn, name = "x", value = x)
    DBI::dbWriteTable(conn, name = "i", value = i)
  }
  # building sql query
  s = sql(how, on, mult, names(x), names(i))
  s = paste0(s,";\n")
  # run data.table and SQLite
  dt = mergelist(list(x[,!"row_id"], i[,!"row_id"]), on=on, how=how, mult=mult)
  sq = as.data.table(DBI::dbGetQuery(conn, s))
  # compare results
  a = all.equal.data.table(dt, sq, ignore.row.order=TRUE)
  b = all.equal.data.table(dt, sq, ignore.row.order=TRUE, ignore.col.order=TRUE)
  if (!missing(ans)) {
    r = all.equal.data.table(ans, sq, ignore.row.order=TRUE)
    if (!isTRUE(r)) {
      if (.debug) browser()
      stop("sql does not match to reference answer")
    }
  }
  if (.drop) {
    DBI::dbSendQuery(conn, "DROP TABLE x;")
    DBI::dbSendQuery(conn, "DROP TABLE i;")
  }
  if (new.conn) suppressWarnings(DBI::dbDisconnect(conn))
  if (isTRUE(b) && !isTRUE(a)) {
    if (.debug) browser()
    stop("only column order mismatch")
  }
  if (!isTRUE(a)) {
    if (.debug) browser()
    cat(sep="\n",c(
      sprintf("# dtq:\nmergelist(list(x, i), on='%s', how='%s', mult='%s')", paste(on, collapse=", "), how, mult),
      sprintf("# sql:\n%s", s),
      a, "\n"))
  }
  isTRUE(a)
}

batch.join.sql.equal = function(cases, on, hows=c("inner","left","right","full"), mults=c("all","first","last","error")) {
  p = proc.time()[[3L]]
  conn = DBI::dbConnect(RSQLite::SQLite())
  ans = list()
  dup_n = 0L
  for (case in cases) {
    l = data(case)
    case = as.character(case)
    x = l$x; i = l$i
    ans[[case]] = list()
    # reuse tables, to test if affects sqlite efficiency
    try(DBI::dbSendQuery(conn, "DROP TABLE x;"), silent = TRUE)
    try(DBI::dbSendQuery(conn, "DROP TABLE i;"), silent = TRUE)
    # row_id column required as SQL is not ordered, creating on R side
    if (!"row_id" %in% names(x)) x = shallow(x)[, "row_id" := seq_len(.N)]
    if (!"row_id" %in% names(i)) i = shallow(i)[, "row_id" := seq_len(.N)]
    DBI::dbWriteTable(conn, name = "x", value = x)
    DBI::dbWriteTable(conn, name = "i", value = i)
    len = prod(length(cases), length(hows), length(mults))
    if (len > (len.warn <- getOption("tests.length.warning", 1e3)))
      warning(sprintf("You are about to run %s number of tests. To suppress this warning use 'tests.length.warning' option, set to numeric threshold or Inf.", len.warn))
    for (how in hows) {
      ans[[case]][[how]] = list()
      for (mult in mults) {
        if (!is.null(ans[[case]][[how]][[mult]])) {
          dup_n = dup_n+1L
          next #warning("Some tests are duplicated, so far ", dup_n)
        }
        ans[[case]][[how]][[mult]] = join.sql.equal(list(x=x, i=i), on=on, how=how, mult=mult, .conn=conn, .drop=FALSE, .debug=FALSE)
      }
    }
    DBI::dbSendQuery(conn, "DROP TABLE x;")
    DBI::dbSendQuery(conn, "DROP TABLE i;")
  }
  suppressWarnings(DBI::dbDisconnect(conn))
  cat(sprintf("batch.join.sql.equal: %s%s tests completed in %.1fs\n",
              len, if (dup_n) sprintf(" (%s duplicated)", dup_n) else "", proc.time()[[3L]] - p))
  ans
}
data = function(case) {
  if (case == 1L) {         # 2 match
    x = data.table(id = c(1L,5L,3L,7L), v1=1:4)
    i = data.table(id = c(2L,4L,3L,5L), v2=1:4)
  } else if (case == 2L) {  # 4 match
    x = data.table(id = c(1L,5L,3L,7L), v1=1:4)
    i = data.table(id = c(7L,5L,3L,1L), v2=1:4)
  } else if (case == 3L) {  # 1 match
    x = data.table(id = c(1L,5L,3L,7L), v1=1:4)
    i = data.table(id = c(1L,2L,4L,6L), v2=1:4)
  } else if (case == 4L) {  # 0 match
    x = data.table(id = c(1L,5L,3L,7L), v1=1:4)
    i = data.table(id = c(0L,2L,4L,6L), v2=1:4)
  } else if (case == 5L) {  # 0 match dup
    x = data.table(id = c(1L,5L,3L,7L), v1=1:4)
    i = data.table(id = c(0L,2L,2L,6L), v2=1:4)
  } else if (case == 6L) {  # 1 match dup
    x = data.table(id = c(1L,5L,3L,7L), v1=1:4)
    i = data.table(id = c(1L,2L,2L,6L), v2=1:4)
  } else if (case == 7L) {  # 1 match dup match
    x = data.table(id = c(1L,5L,3L,7L), v1=1:4)
    i = data.table(id = c(3L,3L,4L,6L), v2=1:4)
  } else if (case == 8L) {  # 2 match 2 dup match
    x = data.table(id = c(1L,5L,3L,7L), v1=1:4)
    i = data.table(id = c(3L,3L,7L,7L), v2=1:4)
  } else if (case == 9L) {  # 2 dup 2 dup
    x = data.table(id = c(1L,5L,1L,5L), v1=1:4)
    i = data.table(id = c(5L,5L,1L,1L), v2=1:4)
  } else if (case == 10L) { # 4 dup 4 dup match
    x = data.table(id = c(1L,1L,1L,1L), v1=1:4)
    i = data.table(id = c(1L,1L,1L,1L), v2=1:4)
  } else if (case == 11L) { # 4 dup 4 dup nomatch
    x = data.table(id = c(1L,1L,1L,1L), v1=1:4)
    i = data.table(id = c(2L,2L,2L,2L), v2=1:4)
  } else if (case == 12L) { # no match, no overlap
    x = data.table(id = c(1:4), v1=1:4)
    i = data.table(id = c(6:9), v2=1:4)
  } else if (case == 13L) { # all i matches
    x = data.table(id = c(1L,5L,3L,7L,9L), v1=1:5)
    i = data.table(id = c(7L,5L,3L,1L), v2=1:4)
  } else if (case == 14L) { # dup match and 1 non-match
    ## inner join short circuit test
    ## what if some row is excluded but another is duplicated? nrow(i) match
    x = data.table(id = c(1L,5L,3L,7L,3L), v1=1:5)
    i = data.table(id = c(7L,5L,3L,2L), v2=1:4)
  } else stop("case not found")
  list(x=x, i=i)
}

# design ----

## no duplicates
lhs = data.table(id1=1:2, v1=1:2)
rhs = data.table(id1=c(1L,3L), v2=1:2)
stopifnot( ## how
  join.sql.equal(list(lhs, rhs), on="id1", how="inner", ans=data.table(id1=1L, v1=1L, v2=1L)),
  join.sql.equal(list(lhs, rhs), on="id1", how="left",  ans=data.table(id1=1:2, v1=1:2, v2=c(1L,NA))),
  join.sql.equal(list(lhs, rhs), on="id1", how="right", ans=data.table(id1=c(1L,3L), v1=c(1L,NA), v2=1:2)),
  join.sql.equal(list(lhs, rhs), on="id1", how="full",  ans=data.table(id1=1:3, v1=c(1:2,NA), v2=c(1L,NA,2L)))
)

## duplicates in RHS and LHS
lhs = data.table(id1=c(1:3,3L), v1=1:4)
rhs = data.table(id1=c(1L,1L,3:4), v2=1:4)
stopifnot( ## inner + mult
  join.sql.equal(list(lhs, rhs), on="id1", how="inner", mult="all", ans=data.table(id1=c(1L,1L,3L,3L), v1=c(1L,1L,3L,4L), v2=c(1:3,3L))),
  #join.sql.equal(list(lhs, rhs), on="id1", how="inner", mult="first", ans=data.table(id1=1L, v1=1L, v2=1L)),
  #join.sql.equal(list(unique(lhs,by="id1"), unique(rhs,by="id1")), on="id1", how="inner", mult="all", ans=data.table(id1=1L, v1=1L, v2=1L)),
  #join.sql.equal(list(lhs, rhs), on="id1", how="inner", mult="last", ans=data.table(id1=1L, v1=1L, v2=1L)),
  join.sql.equal(list(lhs, rhs), on="id1", how="inner", mult="error")
)
stopifnot( ## left + mult
  join.sql.equal(list(lhs, rhs), on="id1", how="left", mult="all",   ans=data.table(id1=c(1L,1:3,3L), v1=c(1L,1:4), v2=c(1:2,NA,3L,3L))),
  join.sql.equal(list(lhs, rhs), on="id1", how="left", mult="first", ans=data.table(id1=c(1:3,3L), v1=1:4, v2=c(1L,NA,3L,3L))),
  join.sql.equal(list(lhs, rhs), on="id1", how="left", mult="last",  ans=data.table(id1=c(1:3,3L), v1=1:4, v2=c(2L,NA,3L,3L))),
  join.sql.equal(list(lhs, rhs), on="id1", how="left", mult="error")
)
stopifnot( ## right + mult
  join.sql.equal(list(lhs, rhs), on="id1", how="right", mult="all",   ans=data.table(id1=c(1L,1L,3,3:4), v1=c(1L,1L,3:4,NA), v2=c(1:3,3:4))),
  #TODO
  #join.sql.equal(list(lhs, rhs), on="id1", how="right", mult="first", ans=data.table(id1=c(1L,1L,3:4), v1=c(1L,1L,3L,NA), v2=1:4)),
  #join.sql.equal(list(lhs, rhs), on="id1", how="right", mult="last", ans=data.table(id1=c(1L,1L,3:4), v1=c(1L,1L,4L,NA), v2=1:4)),
  join.sql.equal(list(lhs, rhs), on="id1", how="right", mult="error")
)

#lhs = data.table(id1=c(1:3,3L), v1=1:4)
#rhs = data.table(id1=c(1L,1L,3:4), v2=1:4)
#merge.data.table(lhs, rhs, all=TRUE)
#rbindlist(list(lhs[rhs, on="id1"], lhs[!rhs, on="id1"]), use.names=TRUE, fill=TRUE)
stopifnot( ## full + mult
  join.sql.equal(list(lhs, rhs), on="id1", how="full", mult="all",   ans=data.table(id1=c(1L,1L,3L,3L,4L,2L), v1=c(1L,1L,3:4,NA,2L), v2=c(1:3,3:4,NA))),
  #join.sql.equal(list(lhs, rhs), on="id1", how="full", mult="first", ans),
  #join.sql.equal(list(lhs, rhs), on="id1", how="full", mult="last",  ans),
  join.sql.equal(list(lhs, rhs), on="id1", how="full", mult="error")
)
#cat(sql(how="full", on="id1", mult="all", names(lhs), names(rhs)),"\n")

## duplicates in RHS

## duplicates in LHS

cat("design tests passed\n")

# tests ----

if (!interactive()) {
  y = batch.join.sql.equal(cases=c(1:13), on="id", hows=c("left"), mults=c("all","first","last","error"))
  y = rapply(y, isTRUE)
  if (!all(y))
    stop(sprintf("join tests failed for %s cases:\n%s", sum(!y), paste("  ", names(y)[!y], collapse="\n")))
  cat("done\n")
  q("no")
}
