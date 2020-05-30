benchmark.data.table = function(script="benchmarks.Rraw", rbin="Rscript", desc=character(), libs=NULL) {
  stopifnot(length(script)==1L)
  # make revision && Rscript inst/benchmarks/boot.R ## install to various libs with various compilation flags
  # R -q -e 'data.table:::benchmark.data.table(libs=list.dirs("library/gcc", recursive=FALSE))'
  fn = setNames(file.path("inst","benchmarks", script), script) ## this path only for development, ultimately use system.file(package="data.table", "benchmarks", script)
  desc = if (length(desc)) paste0(" ", desc) else ""
  if (is.null(libs)) libs = .libPaths()[1L]
  mth = max.th()
  ths = unique(c(1L, as.integer(mth * c(0.1,0.25,0.5,0.75,1))))
  ths = ths[ths>0L]
  cat("benchmark.data.table() running: ", names(fn), "\n", sep="")
  init = proc.time()[[3L]]
  for (lib in libs) {
    for (th in ths) {
      cmd = sprintf("R_LIBS_USER=%s R_DATATABLE_NUM_THREADS=%s R_DATATABLE_NUM_PROCS_PERCENT=100 %s %s%s", lib, th, rbin, fn, desc)
      cat(cmd,"\n",sep="")
      system(cmd)
    }
  }
  t = proc.time()[[3L]]
  cat("Benchmarks in ", names(fn), " completed in ", trunc(t-init), "s\n", sep="")
  invisible(TRUE)
}
max.th = function() {
  old = setDTthreads(0L)
  th = getDTthreads()
  setDTthreads(old)
  th
}
omp = function() {
  omp = capture.output(th<-getDTthreads(verbose=TRUE))
  val = function(x) tail(strsplit(x, " ", fixed=TRUE)[[1L]], 1L)
  l = list(
    procs = val(grep("omp_get_num_procs", omp, value=TRUE)),
    threads = val(grep("omp_get_max_threads", omp, value=TRUE)),
    dt_procs_pct = val(grep("R_DATATABLE_NUM_PROCS_PERCENT", omp, value=TRUE)),
    dt_threads = val(grep("R_DATATABLE_NUM_THREADS", omp, value=TRUE)),
    th = th
  )
  suppressWarnings(lapply(l, as.integer))
}
benchmarkEnv = function(args) {
  rbin = args[1L]
  args = args[-1L]
  file.i = which(substr(args, 1L, 6L)=="--file")
  rargs = paste(args[seq_len(file.i-1L)], collapse=",")
  script = substr(args[file.i], 8L, nchar(args[file.i]))
  args.i = which(substr(args, 1L, 6L)=="--args")
  if (length(args.i) && length(args)>args.i) {
    script_desc = paste(args[(args.i+1L):length(args)], collapse=",")
  } else script_desc = NA_character_
  cc = readLines(system.file(package="data.table", "cc", mustWork=TRUE))
  l = list(
    batch = as.integer(Sys.time()),
    nodename = Sys.info()[["nodename"]],
    rbin = rbin,
    rargs = rargs,
    script = script,
    rver = base::getRversion(),
    package = "data.table",
    ver = packageVersion("data.table"),
    git = unname(read.dcf(system.file("DESCRIPTION", package="data.table", mustWork=TRUE), fields="Revision")[, "Revision"]),
    cc = sub("CC=", "", cc[1L], fixed=TRUE),
    cflags = sub("CFLAGS=", "", cc[2L], fixed=TRUE),
    script_desc = script_desc
  )
  c(l, omp())
}
funArgs = function(x) {
  stopifnot(is.list(x))
  nam = names(x)
  val = vapply(x, deparse, width.cutoff=500L, "")
  if (is.null(nam)) return(paste(val, collapse=","))
  nam[nzchar(nam)] = paste0(nam[nzchar(nam)],"=")
  paste(paste0(nam, val), collapse=",")
}
benchmark = function(num, expr, desc=NA_character_) {
  ts = as.numeric(Sys.time())
  sub.expr = substitute(expr)
  stopifnot(is.call(sub.expr))
  t = system.time(expr)
  t = `names<-`(as.list(t), gsub(".", "_", names(t), fixed=TRUE))
  l = list(num=num, timestamp=ts, fun=as.character(sub.expr[[1L]]), args=funArgs(as.list(sub.expr)[-1L]), desc=desc)
  l = lapply(c(l, getOption("datatable.env"), t), format, scientific=FALSE)
  setDF(l)
  setcolorder(l, c(
    "nodename","batch","timestamp",
    "rbin","rargs","rver",
    "script","script_desc",
    "package","ver","git","cc","cflags",
    "procs","threads","dt_procs_pct","dt_threads","th",
    "fun","args","desc",
    "user_self","sys_self","elapsed","user_child","sys_child"
  ))
  if (num>0) fwrite(l, "benchmarks.csv", append=TRUE, row.names=FALSE)
  invisible(TRUE)
}
summary.benchmark = function() {
  d = fread("benchmarks.csv")
  d[,.(cflags, num, fun, args, desc, th, user_self, sys_self, elapsed)]
}

