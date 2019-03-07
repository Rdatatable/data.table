
# For data.table dev
#
# In ~/.Rprofile add 2 lines :
# Sys.setenv(CC_DIR=path.expand("~/git/data.table"))
# source(file.path(Sys.getenv("CC_DIR"), "cc.R"))
#
# Normal usage :
# $ R
# > cc()
# # change some files
# > cc()
# # compile, reload but not test
# > cc(F)
# # clean, compile, reload but not test
# > cc(F, T)
# # clean, compile using specific version, reload but not test
# > cc(F, T, CC="gcc-8")
#
# To debug C level :
# $ R -d gdb
# run
# dd()
# Ctrl-C
# break file.c:line
# c
# test and step between R and C

options(datatable.print.class = TRUE)

sourceDir <- function(path=getwd(), trace = TRUE, ...) {
  # copied verbatim from example(source) in base R
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm," ")
      source(file.path(path, nm), ...)
  }
  if(trace) cat("\n")
}

cc = function(test=TRUE, clean=FALSE, debug=FALSE, omp=!debug, cc_dir=Sys.getenv("CC_DIR"), CC="gcc") {
  stopifnot(is.character(CC), length(CC)==1L, !is.na(CC), nzchar(CC))
  gc()

  xx = try(getDLLRegisteredRoutines("datatable",TRUE), silent=TRUE)
  if (!inherits(xx, "try-error")) {
    remove(list=sapply(xx$.Call,'[[',"name"), pos=.GlobalEnv)
    remove(list=sapply(xx$.External,'[[',"name"), pos=.GlobalEnv)
    # if these objects aren't there to remove it's correctly an error (should always be there)
  }

  # Make sure library .so is not loaded (neither installed package nor from dev)
  dll = unlist(do.call("rbind",getLoadedDLLs())[,"path"])
  dll = grep("datatable.so",dll,value=TRUE)
  sapply(dll, dyn.unload)
  gc()

  old = getwd()
  on.exit(setwd(old))
  setwd(file.path(cc_dir,"src"))
  cat(getwd(),"\n")
  if (clean) system("rm *.o *.so")
  OMP = if (omp) "" else "no-"
  if (debug) {
    ret = system(sprintf("MAKEFLAGS='-j CC=%s PKG_CFLAGS=-f%sopenmp CFLAGS=-std=c99\\ -O0\\ -ggdb\\ -pedantic' R CMD SHLIB -d -o data.table.so *.c", CC, OMP))
  } else {
    ret = system(sprintf("MAKEFLAGS='-j CC=%s CFLAGS=-f%sopenmp\\ -std=c99\\ -O3\\ -pipe\\ -Wall\\ -pedantic' R CMD SHLIB -o data.table.so *.c", CC, OMP))
    # TODO add -Wextra too?
  }
  if (ret) return()
  # clang -Weverything includes -pedantic and issues many more warnings than gcc
  # system("R CMD SHLIB -o data.table.so *.c")
  if (any(sapply(objects(envir=.GlobalEnv),function(x){inherits(get(x,.GlobalEnv),"data.table")}))) {
    cat("ABOUT TO RELOAD .SO BUT THERE ARE DATA.TABLE OBJECTS IN .GLOBALENV SO FINALIZER MIGHT CRASH\n")
  }
  dyn.load("datatable.so")
  setwd(old)
  xx = getDLLRegisteredRoutines("datatable",TRUE)
  for (i in seq_along(xx$.Call))
    assign(xx$.Call[[i]]$name,  xx$.Call[[i]]$address, env=.GlobalEnv)
  for (i in seq_along(xx$.External))
    assign(xx$.External[[i]]$name,  xx$.External[[i]]$address, env=.GlobalEnv)
  sourceDir(paste0(cc_dir,"/R"))
  assign("testDir", function(x)paste0(cc_dir,"/inst/tests/",x), envir=.GlobalEnv)
  .onLoad()
  if (is.logical(test) && isTRUE(test)) test.data.table() else if (is.character(test)) test.data.table(script=test)
  gc()
  invisible()
}

dd = function(omp=FALSE)cc(FALSE,debug=TRUE,omp=omp,clean=TRUE)

