
# For data.table dev
#
# In ~/.Rprofile add 2 lines :
# Sys.setenv(CC_DIR=path.expand("~/GitHub/data.table"))
# source(paste0(Sys.getenv("CC_DIR"),"/cc.R"))
#
# Normal usage :
# $ R
# > cc()
# # change some files
# > cc()
#
# To debug C level :
# $ R -d gdb
# run
# dd()
# Ctrl-C
# break file.c:line
# c
# test and step between R and C

sourceDir <- function(path=getwd(), trace = TRUE, ...) {
  # copied verbatim from example(source) in base R
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm," ")
      source(file.path(path, nm), ...)
  }
  if(trace) cat("\n")
}

cc = function(test=TRUE, clean=FALSE, debug=FALSE, cc_dir=Sys.getenv("CC_DIR")) {
  suppressMessages(library(bit64))
  # old = getwd()
  setwd(paste0(cc_dir,"/src"))
  cat(getwd(),"\n")
  gc()

  try(detach("package:data.table"),silent=TRUE)

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

  if (clean) system("rm *.o *.so")

  if (debug) {
    ret = system("MAKEFLAGS='-j CC=gcc CFLAGS=-std=gnu99\\ -O0\\ -ggdb\\ -Wall\\ -pedantic' R CMD SHLIB -d -o data.table.so *.c")
  } else {
    ret = system("MAKEFLAGS='-j CC=gcc CFLAGS=-fopenmp\\ -std=gnu99\\ -O3\\ -pipe\\ -Wall\\ -pedantic' R CMD SHLIB -o data.table.so *.c")
  }
  if (ret) return()
  # clang -Weverything includes -pedantic and issues many more warnings than gcc
  # system("R CMD SHLIB -o data.table.so *.c")
  if (any(sapply(objects(envir=.GlobalEnv),function(x){inherits(get(x,.GlobalEnv),"data.table")}))) {
    cat("ABOUT TO RELOAD .SO BUT THERE ARE DATA.TABLE OBJECTS IN .GLOBALENV SO FINALIZER MIGHT CRASH\n")
  }
  dyn.load("datatable.so")
  xx = getDLLRegisteredRoutines("datatable",TRUE)
  for (i in seq_along(xx$.Call))
    assign(xx$.Call[[i]]$name,  xx$.Call[[i]]$address, env=.GlobalEnv)
  for (i in seq_along(xx$.External))
    assign(xx$.External[[i]]$name,  xx$.External[[i]]$address, env=.GlobalEnv)
  sourceDir(paste0(cc_dir,"/R"))
  setwd(paste0(cc_dir,"/inst/tests/"))  # so that tests using test files here can be F5'd easily
  .onLoad()
  if(test)test.data.table()
  gc()
  invisible()
}

# dummy globals for test() to work in dev
nfail = ntest = lastnum = 0
whichfail = NULL

dd = function()cc(FALSE,debug=TRUE,clean=TRUE)

