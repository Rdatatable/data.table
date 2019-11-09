# Run by package maintainer via these entries in ~/.bash_aliases :
#   alias revdepsh='cd ~/build/revdeplib/ && export TZ=UTC && export R_LIBS_SITE=none && export R_LIBS=~/build/revdeplib/ && export _R_CHECK_FORCE_SUGGESTS_=false'
#   alias revdepr='revdepsh; R_PROFILE_USER=~/GitHub/data.table/.dev/revdep.R ~/build/R-devel/bin/R'
# revdep = reverse first-order dependency; i.e. the CRAN and Bioconductor packages which directly use data.table (765 at the time of writing)

# Check that env variables have been set correctly:
#   export R_LIBS_SITE=none
#   export R_LIBS=~/build/revdeplib/
#   export _R_CHECK_FORCE_SUGGESTS_=false
stopifnot(identical(length(.libPaths()), 2L))     # revdeplib (writeable by me) and the pre-installed recommended R library (sudo writeable)
stopifnot(identical(file.info(.libPaths())[,"uname"], rep(as.vector(Sys.info()["user"]), 2)))  # 2nd one is root when using default R rather than Rdevel
stopifnot(identical(.libPaths()[1], getwd()))
stopifnot(identical(Sys.getenv("_R_CHECK_FORCE_SUGGESTS_"),"false"))
options(repos = c("CRAN"=c("http://cloud.r-project.org")))
R = "~/build/R-devel/bin/R"  # alias doesn't work from system()

# The alias sets R_PROFILE_USER so that this script runs on R starting up, leaving prompt running.
# But if we don't unset it now, anything else from now on that does something like system("R CMD INSTALL") (e.g. update.packages()
# and BiocManager::install()) will call this script again recursively.
Sys.unsetenv("R_PROFILE_USER")

system(paste0(R," -e \"utils::update.packages('",.libPaths()[2],"', ask=FALSE, checkBuilt=TRUE)\""))

require(utils)  # only base is loaded when R_PROFILE_USER runs
update.packages(ask=FALSE, checkBuilt=TRUE)
# if package not found on mirror, try manually a different one:
#   install.packages("<pkg>", repos="http://cran.stat.ucla.edu/")
#   update.packages(ask=FALSE)   # a repeat sometimes does more, keep repeating until none

# Follow: https://bioconductor.org/install
# Ensure no library() call in .Rprofile, such as library(bit64)
require(BiocManager)
BiocManager::install(ask=FALSE, version="devel", checkBuilt=TRUE)
BiocManager::valid()

avail = available.packages(repos=BiocManager::repositories())  # includes CRAN at the end from getOption("repos"). And ensure latest Bioc version is in repo path here.
deps = tools::package_dependencies("data.table", db=avail, which="all", reverse=TRUE, recursive=FALSE)[[1]]
exclude = c("TCGAbiolinks")  # too long (>30mins): https://github.com/BioinformaticsFMRP/TCGAbiolinks/issues/240
deps = deps[-match(exclude, deps)]
table(avail[deps,"Repository"])
old = 0
new = 0
if (basename(.libPaths()[1]) != "revdeplib") stop("Must start R with exports as above")
for (p in deps) {
  fn = paste0(p, "_", avail[p,"Version"], ".tar.gz")
  if (!file.exists(fn) ||
      identical(tryCatch(packageVersion(p), error=function(e)FALSE), FALSE) ||
      packageVersion(p) != avail[p,"Version"]) {
    system(paste0("rm -rf ", p, ".Rcheck"))  # Remove last check (of previous version) to move its status() to not yet run

    install.packages(p, repos=BiocManager::repositories(), dependencies=TRUE)    # again, bioc repos includes CRAN here
    # To install its dependencies. The package itsef is installed superfluously here because the tar.gz will be passed to R CMD check.
    # If we did download.packages() first and then passed that tar.gz to install.packages(), repos= is set to NULL when installing from
    # local file, so dependencies=TRUE wouldn't know where to get the dependencies. Hence usig install.packages first with repos= set.

    download.packages(p, destdir="~/build/revdeplib", contriburl=avail[p,"Repository"])   # So R CMD check can run on these
    if (!file.exists(fn)) stop("Still does not exist!:", fn)
    new = new+1
  } else {
    old = old+1
  }
}
cat("New downloaded:",new," Already had latest:", old, " TOTAL:", length(deps), "\n")
update.packages(repos=BiocManager::repositories(), checkBuilt=TRUE)  # double-check all dependencies are latest too
cat("This is R ",R.version$major,".",R.version$minor,"; ",R.version.string,"\n",sep="")
cat("Installed packages built using:\n")
drop(table(installed.packages()[,"Built"]))  # ensure all built with this major release of R

# Remove the tar.gz no longer needed :
for (p in deps) {
  f = paste0(p, "_", avail[p,"Version"], ".tar.gz")  # keep this one
  all = system(paste0("ls ",p,"_*.tar.gz"), intern=TRUE)
  old = all[all!=f]
  for (i in old) {
    cat("Removing",i,"because",f,"is newer\n")
    system(paste0("rm ",i))
  }
  all = system("ls *.tar.gz", intern=TRUE)
  all = sapply(strsplit(all, split="_"),'[',1)
  for (i in all[!all %in% deps]) {
    cat("Removing",i,"because it", if (!i %in% rownames(avail)) "has been removed from CRAN/Bioconductor\n" else "no longer uses data.table\n")
    system(paste0("rm ",i,"_*.tar.gz"))
  }
}
num_tar.gz = as.integer(system("ls *.tar.gz | wc -l", intern=TRUE))
if (length(deps) != num_tar.gz) stop("num_tar.gz==",num_tar.gz," but length(deps)==",length(deps))

status = function(which="both") {
  if (which=="both") {
    cat("Installed data.table to be tested against:",
         as.character(packageVersion("data.table")),
         format(as.POSIXct(packageDescription("data.table")$Packaged, tz="UTC"), tz=""),  # local time
         "\n")
    cat("CRAN:\n"); status("cran")
    cat("BIOC:\n"); status("bioc")
    cat("TOTAL          :", length(deps), "\n\n")
    cat("Oldest 00check.log (to check no old stale ones somehow missed):\n")
    system("find . -name '00check.log' | xargs ls -lt | tail -1")
    cat("\n")
    tt = length(system('ps -aux | grep "parallel.*R.* CMD check"', intern=TRUE))>2L
    cat("parallel R CMD check is ", if(tt)"" else "not ", "running\n",sep="")
    if (file.exists("/tmp/started.flag")) {
      # system("ls -lrt /tmp/*.flag")
      tt = as.POSIXct(file.info(c("/tmp/started.flag","/tmp/finished.flag"))$ctime)
      if (is.na(tt[2])) { tt[2] = Sys.time(); cat("Has been running for "); }
      else cat("Ran for ");
      cat(round(diff(as.numeric(tt))/60, 1), "mins\n")
    }
    return(invisible())
  }
  if (which=="cran") deps = deps[-grep("bioc",avail[deps,"Repository"])]
  if (which=="bioc") deps = deps[grep("bioc",avail[deps,"Repository"])]
  x = unlist(sapply(deps, function(x) {
    fn = paste0("./",x,".Rcheck/00check.log")
    if (file.exists(fn)) {
      v = suppressWarnings(system(paste0("grep 'Status:' ",fn), intern=TRUE))
      if (!length(v)) return("RUNNING")
      return(substring(v,9))
    }
    if (file.exists(paste0("./",x,".Rcheck"))) return("RUNNING")
    return("NOT STARTED")
  }))
  e = grep("ERROR",x)
  w = setdiff( grep("WARNING",x), e)
  n = setdiff( grep("NOTE",x), c(e,w) )
  ok = setdiff( grep("OK",x), c(e,w,n) )
  r = grep("RUNNING",x)
  ns = grep("NOT STARTED", x)
  cat(" ERROR   :",sprintf("%3d",length(e)),":",paste(sort(names(x)[e])),"\n",
      "WARNING :",sprintf("%3d",length(w)),":",paste(sort(names(x)[w])),"\n",
      "NOTE    :",sprintf("%3d",length(n)),"\n",  #":",paste(sort(names(x)[n])),"\n",
      "OK      :",sprintf("%3d",length(ok)),"\n",
      "TOTAL   :",length(e)+length(w)+length(n)+length(ok),"/",length(deps),"\n",
      if (length(r))  paste0("RUNNING       : ",paste(sort(names(x)[r]),collapse=" "),"\n"),
      if (length(ns)) paste0("NOT STARTED   : ",paste(sort(names(x)[head(ns,20)]),collapse=" "), if(length(ns)>20)paste(" +",length(ns)-20,"more"), "\n"),
      "\n"
      )
  assign(paste0(".fail.",which), c(sort(names(x)[e]), sort(names(x)[w])), envir=.GlobalEnv)
  invisible()
}

run = function(pkgs=NULL) {
  cat("Installed data.table to be tested against:",as.character(packageVersion("data.table")),"\n")
  if (length(pkgs)==1) pkgs = strsplit(pkgs, split="[, ]")[[1]]
  if (anyDuplicated(pkgs)) stop("pkgs contains dups")
  if (!length(pkgs)) {
    opts = c("not.started","cran.fail","bioc.fail","both.fail","rerun.all")
    cat(paste0(1:length(opts),": ",opts)  , sep="\n")
    w = suppressWarnings(as.integer(readline("Enter option: ")))
    if (is.na(w) || !w %in% seq_along(opts)) stop(w," is invalid")
    which = opts[w]
    numtgz = as.integer(system("ls -1 *.tar.gz | wc -l", intern=TRUE))
    stopifnot(numtgz==length(deps))
    if (which=="rerun.all") {
      pkgs = "__ALL__"
      cmd = "rm -rf *.Rcheck"
      cat("WIPE ALL CHECK RESULTS:",cmd,"\n")
      cat("Proceed? (ctrl-c or enter)\n")
      scan(quiet=TRUE)
      system(cmd)
    } else {
      pkgs = NULL
      if (which=="not.started") pkgs = deps[!file.exists(paste0("./",deps,".Rcheck"))]  # those that haven't run
      if (which %in% c("cran.fail","both.fail")) pkgs = union(pkgs, .fail.cran)  # .fail.* were written to .GlobalEnv by status()
      if (which %in% c("bioc.fail","both.fail")) pkgs = union(pkgs, .fail.bioc)
    }
  }
  if (length(pkgs)==0) { cat("No packages to run\n"); return(invisible()); }
  if (identical(pkgs,"__ALL__")) {
    cat("Running all",length(deps),"packages\n")
    filter = ""
  } else {
    cat("Running",length(pkgs),"packages:", paste(pkgs), "\n")
    filter = paste0("| grep -E '", paste0(paste0(pkgs,"_"),collapse="|"), "' ")
  }
  cat("Proceed? (ctrl-c or enter)\n")
  scan(quiet=TRUE)
  if (!identical(pkgs,"_ALL_")) for (i in pkgs) system(paste0("rm -rf ./",i,".Rcheck"))
  cmd = paste0("ls -1 *.tar.gz ", filter, "| TZ='UTC' OMP_THREAD_LIMIT=2 parallel --max-procs 50% ",R," CMD check")
  # TZ='UTC' because some packages have failed locally for me but not on CRAN or for their maintainer, due to sensitivity of tests to timezone
  if (as.integer(system("ps -e | grep perfbar | wc -l", intern=TRUE)) < 1) system("perfbar",wait=FALSE)
  system("touch /tmp/started.flag ; rm -f /tmp/finished.flag")
  system(paste("((",cmd,">/dev/null 2>&1); touch /tmp/finished.flag)"), wait=FALSE)
}

inst = function() {
  last = system("ls -t ~/GitHub/data.table/data.table_*.tar.gz", intern=TRUE)[1L]  # latest timestamp should be dev version
  cat("Installing",last,"...\n")
  system(paste0(R," CMD INSTALL ",last))
}

log = function(bioc=FALSE, fnam="~/fail.log") {
  x = c(.fail.cran, if (bioc) .fail.bioc)
  cat("Writing 00check.log for",length(x),"packages to",fnam,":\n")
  cat(paste(x,collapse=" "), "\n")
  require(BiocManager)  # to ensure Bioc version is included in attached packages sessionInfo. It includes the minor version this way; e.g. 1.30.4
  cat(capture.output(sessionInfo()), "\n", file=fnam, sep="\n")
  for (i in x) {
    system(paste0("ls | grep '",i,".*tar.gz' >> ",fnam))
    if (i %in% .fail.bioc) {
      # for Bioconductor only, now include the git commit and date. Although Bioc dev check status online may show OK :
      #   https://bioconductor.org/checkResults/devel/bioc-LATEST/
      # the Bioc package maintainer has to remember to bump the version number otherwise Bioc will not propogate it,
      # and BiocManager::install(version="devel") will not update to the latest version, even though the Bioc dev
      # status online does update. For packages which fail/warn locally but state OK on Bioc check page, compare the
      # output here to the commit hash and date displayed by Bioc-dev check results. If there's a mismatch, contact
      # the maintainer and ask them to bump their version number.
      M = as.matrix(unclass(packageDescription(i)))
      mode(M) = "character"
      out = capture.output(print(M[grep("commit|Date|Packaged",rownames(M),ignore.case=TRUE),,drop=FALSE]))
      cat(out[-1], sep="\n", file=fnam, append=TRUE)
    }
    system(paste0("grep -H . ./",i,".Rcheck/00check.log >> ",fnam))  # the fail messages
    cat("\n\n", file=fnam, append=TRUE)
  }
}

status()

# Now R prompt is ready to fix any problems with CRAN or Bioconductor updates.
# Then run run(), status() and log() as per section in CRAN_Release.cmd

