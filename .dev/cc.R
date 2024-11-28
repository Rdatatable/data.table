# data.table dev
#
# Normal usage :
# $ R
# > cc()
# # change some files
# > cc()
# # run your tests
# # to compile, reload and run main test script
# > cc(T)
# # clean, compile, reload
# > cc(F, T)
# # clean, compile using specific version, reload
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

sourceDir = function(path=getwd(), trace = TRUE, ...) {
  r_files = list.files(path, pattern = "\\.[RrSsQq]$")
  if (trace) cat("Loading", length(r_files), "R files:")
  # copied verbatim from example(source) in base R
  for (nm in r_files) {
    if(trace) cat(" ", nm, sep="")
      source(file.path(path, nm), ...)
  }
  if(trace) cat("\n")
}

# NB: since we only import from default packages, this is rarely needed, but useful for truly minimal dev environments (#6056)
sourceImports = function(path=getwd(), quiet=FALSE) {
  nsFile = file.path(path, "NAMESPACE")
  if (!file.exists(nsFile)) {
    if (!quiet) warning("No NAMESPACE file found, required to guarantee imports resolve correctly")
    return(invisible())
  }
  suppressWarnings(rm("getRversion", envir=.GlobalEnv)) # clean up from previous cc() because parseNamespaceFile() run getRversion() in NAMESPACE in .GlobalEnv
  nsParsedImports = parseNamespaceFile(basename(path), "..")$imports # weird signature to this function
  if (!quiet && length(nsParsedImports)) cat(sprintf("Ensuring objects from %d import entries in NAMESPACE resolve correctly\n", length(nsParsedImports)))
  for (ii in seq_along(nsParsedImports)) {
    entry = nsParsedImports[[ii]]
    # getNamespaceExports includes weird objects but that's intentional, consider evalq(.__C__VIRTUAL, asNamespace("Rcpp")) due to import(methods) in that NAMESPACE
    imported = if (length(entry) == 1L) getNamespaceExports(entry) else entry[[2L]]
    # Assign directly to better imitate actual namespace imports. Earlier tried to require(include.only=) these objects, but R doesn't allow multiple such require, meaning we can't add more objects later in tests, see:
    #   https://stat.ethz.ch/pipermail/r-devel/2024-April/083319.html
    for (import in imported) assign(import, getExportedValue(entry[[1L]], import), .GlobalEnv)
  }
  return(invisible())
}

cc = function(test=FALSE, clean=FALSE, debug=FALSE, omp=!debug, path=Sys.getenv("PROJ_PATH", unset=normalizePath(".")), CC="gcc", quiet=FALSE) {
  stopifnot(is.character(CC), length(CC)==1L, !is.na(CC), nzchar(CC))
  gc()

  xx = try(getDLLRegisteredRoutines("data_table",TRUE), silent=TRUE)
  if (!inherits(xx, "try-error")) {
    remove(list=sapply(xx$.Call, `[[`, "name"), pos=.GlobalEnv)
    remove(list=sapply(xx$.External, `[[`, "name"), pos=.GlobalEnv)
    # if these objects aren't there to remove it's correctly an error (should always be there)
  }

  # Make sure library .so is not loaded (neither installed package nor from dev)
  dll = unlist(do.call(rbind,getLoadedDLLs())[,"path"])
  dll = grep("data_table.so", dll, fixed=TRUE, value=TRUE)
  sapply(dll, dyn.unload)
  gc()
  
  # hack for windows under mingw-like environment
  # PROJ_PATH must be windows like i.e.: export PROJ_PATH=$(Rscript -e 'getwd()')
  if (.Platform$OS.type == "windows") {
    system = function(command, ...) shell(shQuote(command, "sh"), shell = "sh",...)
    # when -std=c99, macro WIN32 is not defined (MinGW) so we define it
    # otherwise will fail as sys/mman.h does not exists in windows (fread.c)
    W32 = "-DWIN32"
    dt_object = "data_table"
  } else {
    W32 = ""
    dt_object = "data_table.so"
  }

  old = getwd()
  on.exit(setwd(old))
  setwd(file.path(path,"src"))
  if (!quiet) cat(getwd(),"\n")
  if (clean) system("rm *.o *.so")
  OMP = if (omp) "openmp" else "no-openmp"
  if (debug) {
    cmd = sprintf(R"(MAKEFLAGS='-j CC=%s PKG_CFLAGS=-f% CFLAGS=-std=c11\ -O0\ -ggdb\ %s\ -pedantic' R CMD SHLIB -d -o data_table.so *.c)", CC, OMP, W32)
  } else {
    cmd = sprintf(R"(MAKEFLAGS='-j CC=%s CFLAGS=-f%s\ -std=c11\ -O3\ -pipe\ -Wall\ -pedantic\ -Wstrict-prototypes\ -isystem\ /usr/share/R/include\ %s\ -fno-common' R CMD SHLIB -o data_table.so *.c)", CC, OMP, W32)
    # the -isystem suppresses strict-prototypes warnings from R's headers, #5477. Look at the output to see what -I is and pass the same path to -isystem.
    # TODO add -Wextra too?
  }
  cat(sprintf("Running command:\n%s\n", cmd))
  ret = system(cmd, ignore.stdout=quiet)
  if (ret) return()
  # clang -Weverything includes -pedantic and issues many more warnings than gcc
  # system("R CMD SHLIB -o data_table.so *.c")
  for (obj in ls(.GlobalEnv)) {
    if (inherits(.GlobalEnv[[obj]], "data.table")) {
      cat("ABOUT TO RELOAD .SO BUT THERE ARE DATA.TABLE OBJECTS IN .GLOBALENV SO FINALIZER MIGHT CRASH\n")
      break
    }
  }
  dyn.load(dt_object)
  setwd(old)
  xx = getDLLRegisteredRoutines("data_table",TRUE)
  for (Call in xx$.Call)
    .GlobalEnv[[Call$name]] = Call$address
  for (Extern in xx$.External)
    .GlobalEnv[[Extern$name]] = Extern$address
  sourceImports(path, quiet=quiet)
  sourceDir(file.path(path, "R"), trace=!quiet)
  if (base::getRversion()<"4.0.0") rm(list=c("rbind.data.table", "cbind.data.table"), envir=.GlobalEnv) # 3968 follow up
  .GlobalEnv$testDir = function(x) file.path(path,"inst/tests",x)
  .onLoad()
  if (isTRUE(test)) test.data.table() else if (is.character(test)) test.data.table(script=test)
  gc()
  invisible()
}

dd = function(omp=FALSE)cc(test=FALSE,debug=TRUE,omp=omp,clean=TRUE)

