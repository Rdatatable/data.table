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
# ln, rn: lhs names, rhs names, symmult: symmetric mult
mult_all = function(tbl, cols, ...) {sprintf(
  "(\n  SELECT %s FROM %s\n) %s",
  paste(setdiff(cols,"row_id"), collapse=", "), tbl, tbl
)}
mult_one = function(tbl, cols, on, mult) {sprintf(
  "(SELECT %s FROM (\n  SELECT *, ROW_NUMBER() OVER (PARTITION BY %s ORDER BY row_id %s) AS rownum FROM %s\n) %s WHERE rownum=1) %s",
  paste(setdiff(cols,c("row_id","rownum")), collapse=", "),
  paste(on, collapse=", "),
  if (mult=="first") "ASC" else "DESC",
  tbl, tbl, tbl
)}
sql = function(how, on, mult, ln, rn, symmult=FALSE, notjoin=FALSE) {
  stopifnot(length(on)==1L)
  # building sql query
  if (how=="full") {
    return(sprintf(
      "%s\nUNION ALL\n%s",
      sql("left", on, mult, ln, rn, symmult=mult%in%c("first","last")),
      sql("right", on, mult, ln, rn, symmult=mult%in%c("first","last"), notjoin=TRUE)
    ))
  }
  nm = list()
  nm[["lhs"]] = ln; nm[["rhs"]] = rn
  using = sprintf("USING (%s)", paste(on, collapse=", "))
  lhs = "lhs"; rhs = "rhs"
  join = if (how=="inner") {
    if (mult=="all") sprintf("%s\nINNER JOIN\n%s\n%s", mult_all(lhs, nm[[lhs]]), mult_all(rhs, nm[[rhs]]), using)
    else sprintf("%s\nINNER JOIN\n%s\n%s", mult_one(lhs, nm[[lhs]], on, mult), mult_one(rhs, nm[[rhs]], on, mult), using)
  } else if (how=="left") {
    if (mult=="all") sprintf("%s\nLEFT JOIN\n%s\n%s", mult_all(lhs, nm[[lhs]]), mult_all(rhs, nm[[rhs]]), using)
    else sprintf("%s\nLEFT JOIN\n%s\n%s", (if (symmult) mult_one else mult_all)(lhs, nm[[lhs]], on, mult), mult_one(rhs, nm[[rhs]], on, mult), using)
  } else if (how=="right") { ## lhs-rhs swap happens here, mult_one is applied on new rhs
    if (mult=="all") sprintf("%s\nLEFT JOIN\n%s\n%s", mult_all(rhs, nm[[rhs]]), mult_all(lhs, nm[[lhs]]), using)
    else sprintf("%s\nLEFT JOIN\n%s\n%s", (if (symmult) mult_one else mult_all)(rhs, nm[[rhs]], on, mult), mult_one(lhs, nm[[lhs]], on, mult), using)
  }
  if (how=="right") {lhs = "rhs"; rhs = "lhs"} ## this name swap is for notjoin and select below
  where = if (!notjoin) "" else sprintf("\nWHERE %s IS NULL", paste(rhs, on, sep="."))
  select = sprintf("%s, %s, %s", paste(lhs, on, sep="."),
                   paste("lhs", setdiff(nm[["lhs"]], c("row_id",on)),sep=".",collapse=", "),
                   paste("rhs", setdiff(nm[["rhs"]], c("row_id",on)),sep=".",collapse=", "))
  sprintf("SELECT %s FROM\n%s%s", select, join, where)
}

# .conn SQLite connection, if provided it will use it instead of creating temporary one
# .drop logical TRUE (default) will drop db tables before and after and populate new, when FALSE it expects tables to be populated
join.sql.equal = function(l, on, how="inner", mult="all", allow.cartesian=TRUE, .conn, .drop=TRUE, .debug=interactive(), ans, err=FALSE) {
  if (!requireNamespace("DBI", quietly=TRUE)) stop("join.sql.equal requires DBI package")
  if (!requireNamespace("RSQLite", quietly=TRUE)) stop("join.sql.equal uses RSQLite package to validate results")
  nm = names(l)
  stopifnot(is.null(nm) || identical(nm, c("x","i")) || identical(nm, c("lhs","rhs")))
  names(l) = c("lhs","rhs")
  lhs = l[["lhs"]]; rhs = l[["rhs"]]
  stopifnot(is.data.table(lhs), is.data.table(rhs),
            is.character(how), is.character(mult), length(mult)==1L,
            is.character(on),
            is.logical(allow.cartesian), is.logical(.drop))
  if (err && mult=="error") {
    dt = try(silent=TRUE, mergelist(list(lhs, rhs), on=on, how=how, mult=mult))
    if (!inherits(dt, "try-error")) {
      if (.debug) browser()
      stop("no error returned from mergelist(mult='error') but err flag set to TRUE in join.sql.equal")
    }
    err_msg = "mult='error' and multiple matches during merge"
    if (!identical(attr(dt, "condition", TRUE)[["message"]], err_msg)) {
      if (.debug) browser()
      stop("different error returned than expected: ", attr(dt, "condition", TRUE)[["message"]])
    }
    return(TRUE)
  }
  # row_id column required as SQL is not ordered, creating on R side
  if (!"row_id" %in% names(lhs)) lhs = shallow(lhs)[, "row_id" := seq_len(.N)]
  if (!"row_id" %in% names(rhs)) rhs = shallow(rhs)[, "row_id" := seq_len(.N)]
  # preparing sql environment
  conn = if (new.conn <- missing(.conn)) DBI::dbConnect(RSQLite::SQLite()) else .conn
  if (.drop) {
    try(DBI::dbSendQuery(conn, "DROP TABLE lhs;"), silent=TRUE)
    try(DBI::dbSendQuery(conn, "DROP TABLE rhs;"), silent=TRUE)
    DBI::dbWriteTable(conn, name="lhs", value=lhs)
    DBI::dbWriteTable(conn, name="rhs", value=rhs)
  }
  # building sql query
  s = sql(how, on, mult, names(lhs), names(rhs))
  s = paste0(s,";\n")
  # run data.table and SQLite
  dt = mergelist(list(lhs[,!"row_id"], rhs[,!"row_id"]), on=on, how=how, mult=mult)
  sq = try(silent=TRUE, as.data.table(DBI::dbGetQuery(conn, s)))
  if (inherits(sq, "try-error")) {
    if (.debug) {message("error during sql statement"); browser()}
    stop("error during sql statement")
  }
  if (!is.data.table(dt) || !is.data.table(sq)) {
    if (.debug) {message("dt and sq must be data.table already"); browser()}
    stop("dt and sq must be data.table already")
  }
  if (how %in% c("inner","full")) {
    dt2 = mergelist(list(rhs[,!"row_id"], lhs[,!"row_id"]), on=on, how=how, mult=mult)
    r = all.equal(dt, dt2, ignore.row.order=TRUE, ignore.col.order=TRUE)
    ## check it is symetric
    if (!isTRUE(r)) {
      if (.debug) {message("mergelist is not symmetric for ", how); browser()}
      stop("mergelist is not symmetric for ", how)
    }
  }
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
    DBI::dbSendQuery(conn, "DROP TABLE lhs;")
    DBI::dbSendQuery(conn, "DROP TABLE rhs;")
  }
  if (new.conn) suppressWarnings(DBI::dbDisconnect(conn))
  if (isTRUE(b) && !isTRUE(a)) {
    if (.debug) browser()
    stop("only column order mismatch")
  }
  if (!isTRUE(a)) {
    if (.debug) browser()
    cat(sep="\n",c(
      sprintf("# dtq:\nmergelist(l, on='%s', how='%s', mult='%s')", paste(on, collapse=", "), how, mult),
      sprintf("# sql:\n%s", s),
      a, "\n"))
  }
  isTRUE(a)
}

batch.join.sql.equal = function(cases, on, hows=c("inner","left","right","full"), mults=c("all","first","last")) {
  if ("error" %in% mults) stop("mult=error is not supported")
  p = proc.time()[[3L]]
  conn = DBI::dbConnect(RSQLite::SQLite())
  ans = list()
  dup_n = 0L
  for (case in cases) {
    l = data(case)
    stopifnot(c("lhs","rhs") %in% names(l))
    case = as.character(case)
    lhs = l$lhs; rhs = l$rhs
    ans[[case]] = list()
    # reuse tables, to test if affects sqlite efficiency
    try(DBI::dbSendQuery(conn, "DROP TABLE lhs;"), silent = TRUE)
    try(DBI::dbSendQuery(conn, "DROP TABLE rhs;"), silent = TRUE)
    # row_id column required as SQL is not ordered, creating on R side
    if (!"row_id" %in% names(lhs)) lhs = shallow(lhs)[, "row_id" := seq_len(.N)]
    if (!"row_id" %in% names(rhs)) rhs = shallow(rhs)[, "row_id" := seq_len(.N)]
    DBI::dbWriteTable(conn, name="lhs", value=lhs)
    DBI::dbWriteTable(conn, name="rhs", value=rhs)
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
        ans[[case]][[how]][[mult]] = join.sql.equal(list(lhs=lhs, rhs=rhs), on=on, how=how, mult=mult, .conn=conn, .drop=FALSE, .debug=FALSE)
      }
    }
    DBI::dbSendQuery(conn, "DROP TABLE lhs;")
    DBI::dbSendQuery(conn, "DROP TABLE rhs;")
  }
  suppressWarnings(DBI::dbDisconnect(conn))
  cat(sprintf("batch.join.sql.equal: %s%s tests completed in %.1fs\n",
              len, if (dup_n) sprintf(" (%s duplicated)", dup_n) else "", proc.time()[[3L]] - p))
  ans
}
data = function(case) {
  set.seed(108)
  if (case == 1L) {         # 2 match
    lhs = data.table(id = c(1L,5L,3L,7L), v1=1:4)
    rhs = data.table(id = c(2L,4L,3L,5L), v2=1:4)
  } else if (case == 2L) {  # 4 match
    lhs = data.table(id = c(1L,5L,3L,7L), v1=1:4)
    rhs = data.table(id = c(7L,5L,3L,1L), v2=1:4)
  } else if (case == 3L) {  # 1 match
    lhs = data.table(id = c(1L,5L,3L,7L), v1=1:4)
    rhs = data.table(id = c(1L,2L,4L,6L), v2=1:4)
  } else if (case == 4L) {  # 0 match
    lhs = data.table(id = c(1L,5L,3L,7L), v1=1:4)
    rhs = data.table(id = c(0L,2L,4L,6L), v2=1:4)
  } else if (case == 5L) {  # 0 match dup
    lhs = data.table(id = c(1L,5L,3L,7L), v1=1:4)
    rhs = data.table(id = c(0L,2L,2L,6L), v2=1:4)
  } else if (case == 6L) {  # 1 match dup
    lhs = data.table(id = c(1L,5L,3L,7L), v1=1:4)
    rhs = data.table(id = c(1L,2L,2L,6L), v2=1:4)
  } else if (case == 7L) {  # 1 match dup match
    lhs = data.table(id = c(1L,5L,3L,7L), v1=1:4)
    rhs = data.table(id = c(3L,3L,4L,6L), v2=1:4)
  } else if (case == 8L) {  # 2 match 2 dup match
    lhs = data.table(id = c(1L,5L,3L,7L), v1=1:4)
    rhs = data.table(id = c(3L,3L,7L,7L), v2=1:4)
  } else if (case == 9L) {  # 2 dup 2 dup
    lhs = data.table(id = c(1L,5L,1L,5L), v1=1:4)
    rhs = data.table(id = c(5L,5L,1L,1L), v2=1:4)
  } else if (case == 10L) { # 4 dup 4 dup match
    lhs = data.table(id = c(1L,1L,1L,1L), v1=1:4)
    rhs = data.table(id = c(1L,1L,1L,1L), v2=1:4)
  } else if (case == 11L) { # 4 dup 4 dup nomatch
    lhs = data.table(id = c(1L,1L,1L,1L), v1=1:4)
    rhs = data.table(id = c(2L,2L,2L,2L), v2=1:4)
  } else if (case == 12L) { # no match, no overlap
    lhs = data.table(id = c(1:4), v1=1:4)
    rhs = data.table(id = c(6:9), v2=1:4)
  } else if (case == 13L) { # all i matches
    lhs = data.table(id = c(1L,5L,3L,7L,9L), v1=1:5)
    rhs = data.table(id = c(7L,5L,3L,1L), v2=1:4)
  } else if (case == 14L) { # dup match and 1 non-match
    ## inner join short circuit test
    ## what if some row is excluded but another is duplicated? nrow(i) match
    lhs = data.table(id = c(1L,5L,3L,7L,3L), v1=1:5)
    rhs = data.table(id = c(7L,5L,3L,2L), v2=1:4)
  } else if (case == 15L) {
    # does not raise error on mult="error" because dups '13' does not have matching rows!
    lhs = data.table(id = as.integer(c(17,14,11,10,5,1,19,7,16,15)), v1=1:10)
    rhs = data.table(id = as.integer(c(6,20,13,1,8,13,3,10,17,9)), v2=1:10)
  }  else if (case == 16L) {
    lhs = data.table(id = sample(10L, 10L, TRUE), v1=1:10)
    rhs = data.table(id = sample(10L, 10L, TRUE), v2=1:10)
  } else if (case == 17L) {
    lhs = data.table(id = sample(1e2L, 1e2L, TRUE), v1=1:1e2)
    rhs = data.table(id = sample(1e2L, 1e2L, TRUE), v2=1:1e2)
  } else if (case == 18L) {
    lhs = data.table(id = sample(1e2L, 1e2L, TRUE), v1=1:1e2)
    rhs = data.table(id = sample(10L, 20L, TRUE), v2=1:1e2)
  } else if (case==19L) {
    lhs = as.data.table(list(id=sample(1e3), v1=1:1e3))
    rhs = as.data.table(list(id=sample(1e3), v2=1:1e3))
  } else if (case==20L) {
    lhs = as.data.table(list(id=sample(1e3*2L, 1e3), v1=1:1e3))
    rhs = as.data.table(list(id=sample(1e3*2L, 1e3), v2=1:1e3))
  } else if (case==21L) {
    lhs = as.data.table(list(id=sample(1e3, 1e3*2L, TRUE), v1=1:1e3))
    rhs = as.data.table(list(id=sample(1e3, 1e3*2L, TRUE), v2=1:1e3))
  } else stop("case not found")
  list(lhs=lhs, rhs=rhs)
}

# design ----

## no duplicates
l = list(lhs = data.table(id1=1:2, v1=1:2), rhs = data.table(id1=c(1L,3L), v2=1:2))
stopifnot( ## how
  join.sql.equal(l, on="id1", how="inner", ans=data.table(id1=1L, v1=1L, v2=1L)),
  join.sql.equal(l, on="id1", how="left",  ans=data.table(id1=1:2, v1=1:2, v2=c(1L,NA))),
  join.sql.equal(l, on="id1", how="right", ans=data.table(id1=c(1L,3L), v1=c(1L,NA), v2=1:2)),
  join.sql.equal(l, on="id1", how="full",  ans=data.table(id1=1:3, v1=c(1:2,NA), v2=c(1L,NA,2L)))
)

## duplicates in RHS and LHS
l = list(lhs = data.table(id1=c(1:3,3L), v1=1:4), rhs = data.table(id1=c(1L,1L,3:4), v2=1:4))
stopifnot( ## inner + mult
  join.sql.equal(l, on="id1", how="inner", mult="all",   ans=data.table(id1=c(1L,1L,3L,3L), v1=c(1L,1L,3L,4L), v2=c(1:3,3L))),
  join.sql.equal(l, on="id1", how="inner", mult="first", ans=data.table(id1=c(1L,3L), v1=c(1L,3L), v2=c(1L,3L))),
  join.sql.equal(l, on="id1", how="inner", mult="last",  ans=data.table(id1=c(1L,3L), v1=c(1L,4L), v2=2:3)),
  join.sql.equal(l, on="id1", how="inner", mult="error", err=TRUE)
)
stopifnot( ## left + mult
  join.sql.equal(l, on="id1", how="left",  mult="all",   ans=data.table(id1=c(1L,1:3,3L), v1=c(1L,1:4), v2=c(1:2,NA,3L,3L))),
  join.sql.equal(l, on="id1", how="left",  mult="first", ans=data.table(id1=c(1:3,3L), v1=1:4, v2=c(1L,NA,3L,3L))),
  join.sql.equal(l, on="id1", how="left",  mult="last",  ans=data.table(id1=c(1:3,3L), v1=1:4, v2=c(2L,NA,3L,3L))),
  join.sql.equal(l, on="id1", how="left",  mult="error", err=TRUE)
)
stopifnot( ## right + mult
  join.sql.equal(l, on="id1", how="right", mult="all",   ans=data.table(id1=c(1L,1L,3,3:4), v1=c(1L,1L,3:4,NA), v2=c(1:3,3:4))),
  join.sql.equal(l, on="id1", how="right", mult="first", ans=data.table(id1=c(1L,1L,3:4), v1=c(1L,1L,3L,NA), v2=1:4)),
  join.sql.equal(l, on="id1", how="right", mult="last",  ans=data.table(id1=c(1L,1L,3:4), v1=c(1L,1L,4L,NA), v2=1:4)),
  join.sql.equal(l, on="id1", how="right", mult="error", err=TRUE)
)
stopifnot( ## full + mult
  join.sql.equal(l, on="id1", how="full",  mult="all",   ans=data.table(id1=c(1L,1L,3L,3L,4L,2L), v1=c(1L,1L,3:4,NA,2L), v2=c(1:3,3:4,NA))),
  join.sql.equal(l, on="id1", how="full",  mult="first", ans=data.table(id1=1:4, v1=c(1:3,NA), v2=c(1L,NA,3:4))),
  join.sql.equal(l, on="id1", how="full",  mult="last",  ans=data.table(id1=1:4, v1=c(1:2,4L,NA), v2=c(2L,NA,3:4))),
  join.sql.equal(l, on="id1", how="full",  mult="error", err=TRUE)
)

## duplicates in RHS and LHS, but RHS dups does not have matches in LHS (merge.data.table+mult fails)
l = list(lhs = data.table(id1=c(1:3,3L), v1=1:4), rhs = data.table(id1=c(1L,1L,3:4,4L), v2=1:5))
stopifnot( ## inner + mult
  join.sql.equal(l, on="id1", how="inner", mult="all",   ans=data.table(id1=c(1L,1L,3L,3L), v1=c(1L,1L,3L,4L), v2=c(1:3,3L))),
  join.sql.equal(l, on="id1", how="inner", mult="first", ans=data.table(id1=c(1L,3L), v1=c(1L,3L), v2=c(1L,3L))),
  join.sql.equal(l, on="id1", how="inner", mult="last",  ans=data.table(id1=c(1L,3L), v1=c(1L,4L), v2=2:3)),
  join.sql.equal(l, on="id1", how="inner", mult="error", err=TRUE)
)
stopifnot( ## left + mult
  join.sql.equal(l, on="id1", how="left",  mult="all",   ans=data.table(id1=c(1L,1:3,3L), v1=c(1L,1:4), v2=c(1:2,NA,3L,3L))),
  join.sql.equal(l, on="id1", how="left",  mult="first", ans=data.table(id1=c(1:3,3L), v1=1:4, v2=c(1L,NA,3L,3L))),
  join.sql.equal(l, on="id1", how="left",  mult="last",  ans=data.table(id1=c(1:3,3L), v1=1:4, v2=c(2L,NA,3L,3L))),
  join.sql.equal(l, on="id1", how="left",  mult="error", err=TRUE)
)
stopifnot( ## right + mult
  join.sql.equal(l, on="id1", how="right", mult="all",   ans=data.table(id1=c(1L,1L,3L,3L,4L,4L), v1=c(1L,1L,3L,4L,NA,NA), v2=c(1:3,3:5))),
  join.sql.equal(l, on="id1", how="right", mult="first", ans=data.table(id1=c(1L,1L,3L,4L,4L), v1=c(1L,1L,3L,NA,NA), v2=1:5)),
  join.sql.equal(l, on="id1", how="right", mult="last",  ans=data.table(id1=c(1L,1L,3L,4L,4L), v1=c(1L,1L,4L,NA,NA), v2=1:5)),
  join.sql.equal(l, on="id1", how="right", mult="error", err=TRUE)
)
stopifnot( ## full + mult
  join.sql.equal(l, on="id1", how="full",  mult="all",   ans=data.table(id1=c(1L,1:3,3:4,4L), v1=c(1L,1:4,NA,NA), v2=c(1:2,NA,3L,3:5))),
  join.sql.equal(l, on="id1", how="full",  mult="first", ans=data.table(id1=1:4, v1=c(1:3,NA), v2=c(1L,NA,3:4))),
  join.sql.equal(l, on="id1", how="full",  mult="last",  ans=data.table(id1=1:4, v1=c(1:2,4L,NA), v2=c(2L,NA,3L,5L))),
  join.sql.equal(l, on="id1", how="full",  mult="error", err=TRUE)
)

## cartesian match, dups on both sides of match
l = list(lhs = data.table(id1=c(1L,1:2), v1=1:3), rhs = data.table(id1=c(1L,1L,3L), v2=1:3))
stopifnot( ## inner + mult
  join.sql.equal(l, on="id1", how="inner", mult="all",   ans=data.table(id1=c(1L,1L,1L,1L), v1=c(1L,1:2,2L), v2=c(1:2,1:2))),
  join.sql.equal(l, on="id1", how="inner", mult="first", ans=data.table(id1=1L, v1=1L, v2=1L)),
  join.sql.equal(l, on="id1", how="inner", mult="last",  ans=data.table(id1=1L, v1=2L, v2=2L)),
  join.sql.equal(l, on="id1", how="inner", mult="error", err=TRUE)
)
stopifnot( ## left + mult
  join.sql.equal(l, on="id1", how="left",  mult="all",   ans=data.table(id1=c(1L,1L,1L,1L,2L), v1=c(1L,1L,2L,2L,3L), v2=c(1:2,1:2,NA))),
  join.sql.equal(l, on="id1", how="left",  mult="first", ans=data.table(id1=c(1L,1:2), v1=1:3, v2=c(1L,1L,NA))),
  join.sql.equal(l, on="id1", how="left",  mult="last",  ans=data.table(id1=c(1L,1:2), v1=1:3, v2=c(2L,2L,NA))),
  join.sql.equal(l, on="id1", how="left",  mult="error", err=TRUE)
)
stopifnot( ## right + mult
  join.sql.equal(l, on="id1", how="right", mult="all",   ans=data.table(id1=c(1L,1L,1L,1L,3L), v1=c(1L,1L,2L,2L,NA), v2=c(1:2,1:2,NA))),
  join.sql.equal(l, on="id1", how="right", mult="first", ans=data.table(id1=c(1L,1L,3L), v1=c(1L,1L,NA), v2=1:3)),
  join.sql.equal(l, on="id1", how="right", mult="last",  ans=data.table(id1=c(1L,1L,3L), v1=c(2L,2L,NA), v2=1:3)),
  join.sql.equal(l, on="id1", how="right", mult="error", err=TRUE)
)
stopifnot( ## full + mult
  join.sql.equal(l, on="id1", how="full",  mult="all",   ans=data.table(id1=c(1L,1L,1L,1:3), v1=c(1L,1:2,2:3,NA), v2=c(1:2,1:2,NA,3L))),
  join.sql.equal(l, on="id1", how="full",  mult="first", ans=data.table(id1=1:3, v1=c(1L,3L,NA), v2=c(1L,NA,3L))),
  join.sql.equal(l, on="id1", how="full",  mult="last",  ans=data.table(id1=1:3, v1=c(2L,3L,NA), v2=c(2L,NA,3L))),
  join.sql.equal(l, on="id1", how="full",  mult="error", err=TRUE)
)

## duplicates in RHS
l = list(lhs = data.table(id1=1:2, v1=1:2), rhs = data.table(id1=c(2L,2:3), v2=1:3))
stopifnot( ## inner + mult
  join.sql.equal(l, on="id1", how="inner", mult="all",   ans=data.table(id1=c(2L,2L), v1=c(2L,2L), v2=1:2)),
  join.sql.equal(l, on="id1", how="inner", mult="first", ans=data.table(id1=2L, v1=2L, v2=1L)),
  join.sql.equal(l, on="id1", how="inner", mult="last",  ans=data.table(id1=2L, v1=2L, v2=2L)),
  join.sql.equal(l, on="id1", how="inner", mult="error", err=TRUE)
)
stopifnot( ## left + mult
  join.sql.equal(l, on="id1", how="left",  mult="all",   ans=data.table(id1=c(1:2,2L), v1=c(1:2,2L), v2=c(NA,1:2))),
  join.sql.equal(l, on="id1", how="left",  mult="first", ans=data.table(id1=1:2, v1=1:2, v2=c(NA,1L))),
  join.sql.equal(l, on="id1", how="left",  mult="last",  ans=data.table(id1=1:2, v1=1:2, v2=c(NA,2L))),
  join.sql.equal(l, on="id1", how="left",  mult="error", err=TRUE)
)
stopifnot( ## right + mult
  join.sql.equal(l, on="id1", how="right", mult="all",   ans=data.table(id1=c(2L,2:3), v1=c(2L,2L,NA), v2=1:3)),
  join.sql.equal(l, on="id1", how="right", mult="first", ans=data.table(id1=c(2L,2:3), v1=c(2L,2L,NA), v2=1:3)),
  join.sql.equal(l, on="id1", how="right", mult="last",  ans=data.table(id1=c(2L,2:3), v1=c(2L,2L,NA), v2=1:3)),
  join.sql.equal(l, on="id1", how="right", mult="error", ans=data.table(id1=c(2L,2:3), v1=c(2L,2L,NA), v2=1:3), err=FALSE) ## no dups in LHS
)
stopifnot( ## full + mult
  join.sql.equal(l, on="id1", how="full",  mult="all",   ans=data.table(id1=c(1:2,2:3), v1=c(1:2,2L,NA), v2=c(NA,1:3))),
  join.sql.equal(l, on="id1", how="full",  mult="first", ans=data.table(id1=c(1:2,3L), v1=c(1:2,NA), v2=c(NA,1L,3L))),
  join.sql.equal(l, on="id1", how="full",  mult="last",  ans=data.table(id1=c(1:2,3L), v1=c(1:2,NA), v2=c(NA,2:3))),
  join.sql.equal(l, on="id1", how="full",  mult="error", err=TRUE)
)

## duplicates in LHS
l = list(lhs = data.table(id1=c(1:2,2L), v1=1:3), rhs = data.table(id1=2:3, v2=1:2))
stopifnot( ## inner + mult
  join.sql.equal(l, on="id1", how="inner", mult="all",   ans=data.table(id1=c(2L,2L), v1=2:3, v2=c(1L,1L))),
  join.sql.equal(l, on="id1", how="inner", mult="first", ans=data.table(id1=2L, v1=2L, v2=1L)),
  join.sql.equal(l, on="id1", how="inner", mult="last",  ans=data.table(id1=2L, v1=3L, v2=1L)),
  join.sql.equal(l, on="id1", how="inner", mult="error", err=TRUE)
)
stopifnot( ## left + mult
  join.sql.equal(l, on="id1", how="left",  mult="all",   ans=data.table(id1=c(1:2,2L), v1=1:3, v2=c(NA,1L,1L))),
  join.sql.equal(l, on="id1", how="left",  mult="first", ans=data.table(id1=c(1:2,2L), v1=1:3, v2=c(NA,1L,1L))),
  join.sql.equal(l, on="id1", how="left",  mult="last",  ans=data.table(id1=c(1:2,2L), v1=1:3, v2=c(NA,1L,1L))),
  join.sql.equal(l, on="id1", how="left",  mult="error", ans=data.table(id1=c(1:2,2L), v1=1:3, v2=c(NA,1L,1L)), err=FALSE) ## no dups in RHS
)
stopifnot( ## right + mult
  join.sql.equal(l, on="id1", how="right", mult="all",   ans=data.table(id1=c(2L,2:3), v1=c(2:3,NA), v2=c(1L,1:2))),
  join.sql.equal(l, on="id1", how="right", mult="first", ans=data.table(id1=2:3, v1=c(2L,NA), v2=1:2)),
  join.sql.equal(l, on="id1", how="right", mult="last",  ans=data.table(id1=2:3, v1=c(3L,NA), v2=1:2)),
  join.sql.equal(l, on="id1", how="right", mult="error", err=TRUE)
)
stopifnot( ## full + mult
  join.sql.equal(l, on="id1", how="full",  mult="all",   ans=data.table(id1=c(1:2,2:3), v1=c(1:3,NA), v2=c(NA,1L,1:2))),
  join.sql.equal(l, on="id1", how="full",  mult="first", ans=data.table(id1=1:3, v1=c(1:2,NA), v2=c(NA,1:2))),
  join.sql.equal(l, on="id1", how="full",  mult="last",  ans=data.table(id1=1:3, v1=c(1L,3L,NA), v2=c(NA,1:2))),
  join.sql.equal(l, on="id1", how="full",  mult="error", err=TRUE)
)

## no dups, 1:1 match, all are equals below
l = list(lhs = data.table(id1=1:2, v1=1:2), rhs = data.table(id1=1:2, v2=1:2))
stopifnot( ## inner + mult
  join.sql.equal(l, on="id1", how="inner", mult="all",   ans=data.table(id1=1:2, v1=1:2, v2=1:2)),
  join.sql.equal(l, on="id1", how="inner", mult="first", ans=data.table(id1=1:2, v1=1:2, v2=1:2)),
  join.sql.equal(l, on="id1", how="inner", mult="last",  ans=data.table(id1=1:2, v1=1:2, v2=1:2)),
  join.sql.equal(l, on="id1", how="inner", mult="error", ans=data.table(id1=1:2, v1=1:2, v2=1:2), err=FALSE)
)
stopifnot( ## left + mult
  join.sql.equal(l, on="id1", how="left",  mult="all",   ans=data.table(id1=1:2, v1=1:2, v2=1:2)),
  join.sql.equal(l, on="id1", how="left",  mult="first", ans=data.table(id1=1:2, v1=1:2, v2=1:2)),
  join.sql.equal(l, on="id1", how="left",  mult="last",  ans=data.table(id1=1:2, v1=1:2, v2=1:2)),
  join.sql.equal(l, on="id1", how="left",  mult="error", ans=data.table(id1=1:2, v1=1:2, v2=1:2), err=FALSE)
)
stopifnot( ## right + mult
  join.sql.equal(l, on="id1", how="right", mult="all",   ans=data.table(id1=1:2, v1=1:2, v2=1:2)),
  join.sql.equal(l, on="id1", how="right", mult="first", ans=data.table(id1=1:2, v1=1:2, v2=1:2)),
  join.sql.equal(l, on="id1", how="right", mult="last",  ans=data.table(id1=1:2, v1=1:2, v2=1:2)),
  join.sql.equal(l, on="id1", how="right", mult="error", ans=data.table(id1=1:2, v1=1:2, v2=1:2), err=FALSE)
)
stopifnot( ## full + mult
  join.sql.equal(l, on="id1", how="full",  mult="all",   ans=data.table(id1=1:2, v1=1:2, v2=1:2)),
  join.sql.equal(l, on="id1", how="full",  mult="first", ans=data.table(id1=1:2, v1=1:2, v2=1:2)),
  join.sql.equal(l, on="id1", how="full",  mult="last",  ans=data.table(id1=1:2, v1=1:2, v2=1:2)),
  join.sql.equal(l, on="id1", how="full",  mult="error", ans=data.table(id1=1:2, v1=1:2, v2=1:2), err=FALSE)
)

## cross join duplicates
l = list(lhs = data.table(id1=c(1L,1L), v1=1:2), rhs = data.table(id1=c(1L,1L), v2=1:2))
stopifnot( ## inner + mult
  join.sql.equal(l, on="id1", how="inner", mult="all",   ans=data.table(id1=c(1L,1L,1L,1L), v1=c(1L,1:2,2L), v2=c(1:2,1:2))),
  join.sql.equal(l, on="id1", how="inner", mult="first", ans=data.table(id1=1L, v1=1L, v2=1L)),
  join.sql.equal(l, on="id1", how="inner", mult="last",  ans=data.table(id1=1L, v1=2L, v2=2L)),
  join.sql.equal(l, on="id1", how="inner", mult="error", err=TRUE)
)
stopifnot( ## left + mult
  join.sql.equal(l, on="id1", how="left",  mult="all",   ans=data.table(id1=c(1L,1L,1L,1L), v1=c(1L,1:2,2L), v2=c(1:2,1:2))),
  join.sql.equal(l, on="id1", how="left",  mult="first", ans=data.table(id1=c(1L,1L), v1=1:2, v2=c(1L,1L))),
  join.sql.equal(l, on="id1", how="left",  mult="last",  ans=data.table(id1=c(1L,1L), v1=1:2, v2=c(2L,2L))),
  join.sql.equal(l, on="id1", how="left",  mult="error", err=TRUE)
)
stopifnot( ## right + mult
  join.sql.equal(l, on="id1", how="right", mult="all",   ans=data.table(id1=c(1L,1L,1L,1L), v1=c(1L,1:2,2L), v2=c(1:2,1:2))),
  join.sql.equal(l, on="id1", how="right", mult="first", ans=data.table(id1=c(1L,1L), v1=c(1L,1L), v2=1:2)),
  join.sql.equal(l, on="id1", how="right", mult="last",  ans=data.table(id1=c(1L,1L), v1=c(2L,2L), v2=1:2)),
  join.sql.equal(l, on="id1", how="right", mult="error", err=TRUE)
)
stopifnot( ## full + mult
  join.sql.equal(l, on="id1", how="full",  mult="all",   ans=data.table(id1=c(1L,1L,1L,1L), v1=c(1L,1:2,2L), v2=c(1:2,1:2))),
  join.sql.equal(l, on="id1", how="full",  mult="first", ans=data.table(id1=1L, v1=1L, v2=1L)),
  join.sql.equal(l, on="id1", how="full",  mult="last",  ans=data.table(id1=1L, v1=2L, v2=2L)),
  join.sql.equal(l, on="id1", how="full",  mult="error", err=TRUE)
)

cat("design tests passed\n")

# tests ----

if (!interactive()) {
  y = batch.join.sql.equal(cases=1:21, on="id", hows=c("inner","left","right","full"), mults=c("all","first","last"))
  y = rapply(y, isTRUE)
  if (!all(y))
    stop(sprintf("join tests failed for %s cases:\n%s", sum(!y), paste("  ", names(y)[!y], collapse="\n")))
  cat("done\n")
  q("no")
}
