# Run by package maintainer via aliases revdepsh and revdepr in .dev/.bash_aliases. See
# that file for comments.
# revdep = reverse first-order dependency; i.e. the CRAN and Bioconductor packages which directly use data.table

Sys.unsetenv("R_PROFILE_USER")
# The alias sets R_PROFILE_USER so that this script runs on R starting up, and leaves the R prompt running.
# But if we don't unset it now, anything else from now on that does something like system("R CMD INSTALL"), e.g. update.packages()
# and BiocManager::install(), will call this script again recursively.

# options copied from .dev/.Rprofile that aren't run due to the way this script is started via a profile
options(help_type="html")
options(error=quote(utils::dump.frames()))
options(width=200)      # for cran() output not to wrap

# Check that env variables have been set correctly:
#   export R_LIBS_SITE=NULL   # R 4.2.0 changed to NULL but it doesn't appear to work
#   export R_LIBS=~/build/revdeplib/
#   export _R_CHECK_FORCE_SUGGESTS_=true
if (length(.libPaths())==3L) .libPaths(.libPaths()[-2L], include.site=FALSE)  # workaround as I couldn't get R_LIBS_SITE=NULL to be effective
stopifnot(identical(length(.libPaths()), 2L))  # revdeplib writeable by me, and the pre-installed recommended R library (sudo writeable)
stopifnot(identical(.libPaths()[1L], getwd()))
tt = file.info(.libPaths())[,"uname"]
stopifnot(identical(length(tt), 2L))
stopifnot(tt[1L]==Sys.info()["user"])
if (grepl("devel", .libPaths()[2L])) {
  stopifnot(tt[2L]==Sys.info()["user"])
  R = "~/build/R-devel/bin/R"  # would use Rdevel alias but the bash alias doesn't work from system()
} else {
  stopifnot(tt[2L]=="root")
  R = "R"  # R-release
}
if (length(dup_pkgs <- intersect(dir(.libPaths()[1]), dir(.libPaths()[2])))) {
  stop("Package(s) installed in both paths: ", paste(dup_pkgs, collapse=" "))
  # S4 issues are frustrating as it seems that dependencies need to be reinstalled to reset the nobody-knows-how-it-works S4 cache?
  # For example, to fix prioritizr's error in its examples :
  #  error in evaluating the argument 'x' in selecting a method for function 'print':
  #  error in evaluating the argument 'y' in selecting a method for function 'intersecting_units':
  #  package slot missing from signature for generic ‘raster’
  # I reinstalled raster. But that didn't help. Then raster's depend sp but that didn't help. Then sp followed by raster
  # again but that didn't help. Then all dependencies but that didn't help. Then lattice too but that didn't help.
  # Then after a few hours, I stumbled on the realization when I reinstalled lattice it got installed in revdeplib rather
  # than R's base library where the recommended packages are; lattice was in two places on .libPaths(). Removing it from
  # revdeplib didn't help. But reinstalling lattice (using sudo R) in /usr/lib/R/library did appear to finally fix prioritizr.
  # Why it was lattice that needed to be reinstalled just beats me, and why do S4 packages need to be reinstalled anyway?
} 

stopifnot(identical(Sys.getenv("_R_CHECK_FORCE_SUGGESTS_"),"true"))
# _R_CHECK_FORCE_SUGGESTS_=true explicitly in .dev/.bash_aliases
# All suggests should be installed for revdep checking. This avoids problems for some packages for which the attempt to run
# run R CMD check without all suggests can fail due to changed behaviour when some of the suggests aren't available;
# e.g. https://github.com/reimandlab/ActivePathways/issues/14

cflags = system("grep \"^[^#]*CFLAGS\" ~/.R/Makevars", intern=TRUE)
cat("~/.R/Makevars contains", cflags)
if (!grepl("^CFLAGS=-O[0-3] *$", cflags)) {
  stop("Some packages have failed to install in the past (e.g. processx and RGtk2) when CFLAGS contains -pedandic, -Wall, and similar. ",
  "So for revdepr keep CFLAGS simple; i.e. -O[0-3] only. Check ~/.R/Makevars.")
} else {
  cat(" ok\n")
}

options(repos = c("CRAN"=c("http://cloud.r-project.org")))
options(repos = BiocManager::repositories())
# Some CRAN packages import Bioc packages; e.g. wilson imports DESeq2. So we need to install DESeq2 from Bioc.
# BiocManager::repositories() includes CRAN in its result (it appends to getOption("repos"). Using the Bioc function
# ensures the latest Bioc version is in the repo path here (their repos have the version number in the path).

options(warn=1)  # warning at the time so we can more easily see what's going on package by package when we scroll through output
cat("options()$timeout==", options()$timeout," set by R_DEFAULT_INTERNET_TIMEOUT in .dev/.bash_aliases revdepsh\n",sep="")
# R's default is 60. Before Dec 2020, we used 300 but that wasn't enough to download Bioc package BSgenome.Hsapiens.UCSC.hg19 (677GB) which is
# suggested by CRAN package CNVScope which imports data.table. From Dec 2020 we use 3600.

if (is.null(utils::old.packages(.libPaths()[2]))) {
  cat("All", length(dir(.libPaths()[2])), "recommended packages supplied with R in", .libPaths()[2], "are the latest version\n")
} else {
  cat("Some recommended packages supplied with R need to be updated ...\n")
  system(paste0(if(R=="R")"sudo ", R, " -e \"utils::update.packages('",.libPaths()[2],"', ask=TRUE, checkBuilt=TRUE)\""))
  # old.packages was called first, to avoid entering password for sudo if, as is most often the case, all recommended packages are already to date
}

require(utils)  # only base is loaded when R_PROFILE_USER runs
update.packages(ask=FALSE, checkBuilt=TRUE)
# if package not found on mirror, try manually a different one:
#   install.packages("<pkg>", repos="http://cran.stat.ucla.edu/")
#   update.packages(ask=FALSE)   # a repeat sometimes does more, keep repeating until none

# Follow: https://bioconductor.org/install
# Ensure no library() call in .Rprofile, such as library(bit64)
# As from October 2020, Matt no longer checks Bioconductor revdeps. After many years of trying, and repeated
# emails to Bioconductor maintainers, there were still too many issues not fixed for too long. The packages
# are big in size and have many warnings which make it hard to find the true problems. The way the Bioc
# devel and release repositories are set up require more work and confuses communication. That doesn't need
# to be done in the better and simpler way that CRAN is setup. 
# require(BiocManager)
# BiocManager::install(ask=FALSE, version="devel", checkBuilt=TRUE)
# BiocManager::valid()

avail = available.packages()  # includes CRAN and Bioc, from getOption("repos") set above

avail = avail[!rownames(avail) %in% c("cplexAPI","Rcplex"), ]
# cplexAPI is suggested by revdeps ivmte and prioritizr. I haven't succeeded to install IBM ILOG CPLEX which requires a license,
# so consider cplexAPI not available when resolving missing suggests at the end of status().
# Update: cplexAPI was removed from CRAN on 5 Nov 2021 so this is now redundant, but leave it in place for future use.
# Update: Rcplex is on CRAN as of 20 Nov 2022 but with install errors, therefore treat it as not available.

# The presence of packages here in revdeplib which no longer exist on CRAN could explain differences to CRAN. A revdep
# could be running tests using that package when available and failing which may be the very reason that package was removed from CRAN.
# When it is removed from revdeplib to match CRAN, then the revdep might then pass as it will skip its tests using that package.
x = installed.packages()
tt = match(rownames(x), rownames(avail))
removed = rownames(x)[is.na(tt) & is.na(x[,"Priority"])]
cat("Removing",length(removed),"packages which are no longer available on CRAN/Bioc:", paste(removed, collapse=","), "\n")
stopifnot(all(x[removed,"LibPath"] == .libPaths()[1]))
oldn = nrow(x)
remove.packages(removed, .libPaths()[1])
x = installed.packages()
stopifnot(nrow(x) == oldn-length(removed))

# Ensure all installed packages were built with this x.y release of R; i.e. that checkBuilt=TRUE worked above
cat("This is R ",R.version$major,".",R.version$minor,"; ",R.version.string,"\n",sep="")
cat("Previously installed packages were built using:\n")
print(tt <- table(x[,"Built"], dnn=NULL))
minorR = paste(strsplit(as.character(getRversion()), split="[.]")[[1]][c(1,2)], collapse=".")
if (any(w<-names(tt)<minorR)) {
  stop(sum(tt[w])," packages built with R<",minorR," failed to update: ", paste(paste0("'",rownames(x)[x[,"Built"]<minorR],"'"), collapse=","), "\n",sep="")
}

deps = tools::package_dependencies("data.table",
  db = available.packages(repos=getOption("repos")["CRAN"]),  # just CRAN revdeps though (not Bioc) from October 2020
  which="all", reverse=TRUE, recursive=FALSE)[[1]]
# exclude = c("TCGAbiolinks")  # too long (>30mins): https://github.com/BioinformaticsFMRP/TCGAbiolinks/issues/240
# deps = deps[-match(exclude, deps)]
table(avail[deps,"Repository"], dnn=NULL)
old = 0
new = 0
if (basename(.libPaths()[1]) != "revdeplib") stop("Must start R with exports as above")
for (p in deps) {
  fn = paste0(p, "_", avail[p,"Version"], ".tar.gz")
  if (!file.exists(fn) ||
      identical(tryCatch(packageVersion(p), error=function(e)FALSE), FALSE) ||
      packageVersion(p) != avail[p,"Version"]) {      
    cat("\n**** Installing revdep:", p, "\n")
    system(paste0("rm -rf ", p, ".Rcheck"))  # Remove last check (of previous version) to move its status() to not yet run

    install.packages(p, dependencies=TRUE)
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

# Remove the tar.gz no longer needed :
for (p in deps) {
  f = paste0(p, "_", avail[p,"Version"], ".tar.gz")  # keep this one
  all = system(paste0("ls ",p,"_*.tar.gz"), intern=TRUE)
  old = all[all!=f]
  for (i in old) {
    cat("Removing",i,"because",f,"is newer\n")
    system(paste0("rm ",i))
  }
}
all = system("ls *.tar.gz", intern=TRUE)
all = sapply(strsplit(all, split="_"),'[',1)
for (i in all[!all %in% deps]) {
  cat("Removing",i,"because it", if (!i %in% rownames(avail)) "has been removed from CRAN\n" else "no longer uses data.table\n")
  system(paste0("rm ",i,"_*.tar.gz"))
}
num_tar.gz = as.integer(system("ls *.tar.gz | wc -l", intern=TRUE))
if (length(deps) != num_tar.gz) stop("num_tar.gz==",num_tar.gz," but length(deps)==",length(deps))

status0 = function(bioc=FALSE) {
  deps = deps[grep("bioc", avail[deps,"Repository"], invert=!bioc)]
  x = unlist(sapply(deps, function(x) {
    fn = paste0("./",x,".Rcheck/00check.log")
    if (file.exists(fn)) {
      v = suppressWarnings(system(paste0("grep 'Status:' ",fn), intern=TRUE))
      if (!length(v)) return("RUNNING")
      return(substr(v, 9L, nchar(v)))
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
  cat(" ERROR   :",sprintf("%4d",length(e)),":",paste(sort(names(x)[e])),"\n",
      "WARNING :",sprintf("%4d",length(w)),":",paste(sort(names(x)[w])),"\n",
      "NOTE    :",sprintf("%4d",length(n)),"\n",  #":",paste(sort(names(x)[n])),"\n",
      "OK      :",sprintf("%4d",length(ok)),"\n",
      "TOTAL   :",sprintf("%4d",length(e)+length(w)+length(n)+length(ok)),"/",length(deps),"\n",
      if (length(r))  paste0("RUNNING       : ",paste(sort(names(x)[r]),collapse=" "),"\n"),
      if (length(ns)) paste0("NOT STARTED   : ",paste(sort(names(x)[head(ns,20)]),collapse=" "), if(length(ns)>20)paste(" +",length(ns)-20,"more"), "\n"),
      "\n"
      )
  assign(if (bioc) ".fail.bioc" else ".fail.cran", c(sort(names(x)[e]), sort(names(x)[w])), envir=.GlobalEnv) 
  assign(if (bioc) ".running.bioc" else ".running.cran", sort(names(x)[r]), envir=.GlobalEnv)
  # if parallel finished then 'running' means killed; we want to see if status on CRAN (using cran()) shows FAIL with a log showing kill signal (or similar) due to taking too long
  invisible()
}

status = function(bioc=FALSE) {
  cat("\nInstalled data.table to be tested against:",
    as.character(packageVersion("data.table")),
    format(as.POSIXct(packageDescription("data.table")$Packaged, tz="UTC"), tz=""),  # local time
    "\n\nCRAN:\n")
  status0()
  if (bioc) {
    cat("BIOC:\n"); status0(bioc=TRUE)
    cat("TOTAL          :", length(deps), "\n\n")
  }
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
  
  # Now deal with Suggests that are not available. Could have been removed from CRAN/Bioc, or are not installing for some reason like system library not installed.
  tt = system("find . -name '00check.log' -exec grep -zl 'ERROR.Packages* suggested but not available' {} \\;", intern=TRUE)
  if (length(tt)) {
    tt = sort(substring(tt, 3L, nchar(tt)-nchar(".Rcheck/00check.log")))
    installed = installed.packages()
    all_sugg_unavail = c()
    for (pkg in tt) {
      sugg = strsplit(gsub("\n","",avail[pkg,"Suggests"]), split=",")[[1L]]
      sugg = gsub("^ ","",sugg)
      sugg = gsub(" [(].+[)]","",sugg)
      miss = sugg[!sugg %in% rownames(installed)]
      cat("\n",pkg,sep="")
      if (!length(miss)) {
        cat(" 00check.log states that some of its suggests are not installed, but they all appear to be. Inspect and rerun.\n")
        next
      }
      cat(" is missing",paste(miss,collapse=","))
      if (any(tt <- miss %in% rownames(avail))) {
        cat("; some are available, installing ...\n")
        install.packages(miss[which(tt)])  # careful not to ask for unavailable packages here, to avoid the warnings we already know they aren't available
      } else {
        cat("; all unavailable on CRAN/Bioc\n")
        all_sugg_unavail = c(all_sugg_unavail, pkg)
      }
    }
    if (length(all_sugg_unavail)) {
      cat('\nPackages for which all their missing suggests are not available (',length(all_sugg_unavail),'): ', paste(all_sugg_unavail, collapse=" "), "\n", sep="")
      cat('Rerunning them with R_CHECK_FORCE_SUGGESTS=FALSE ...\n')
      run(all_sugg_unavail, R_CHECK_FORCE_SUGGESTS=FALSE, ask=FALSE)
      # the main run() ran with TRUE in an attempt to check suggests where possible in case data.table usage is there. It does that as OS level using
      # parallel linux command of `R CMD check`. Therefore it would be awkward to rerun with TRUE in that step. Instead we trigger the rerun from
      # here in status() afterwards once we know all such packages and can count and log them (the cat() just above). Since run() is
      # asynchronous we do have to wait again and run status() again when perfbar shows finished. However, currently there are only
      # 37/1315 like this so it only takes a few minutes with my default of 6 at a time.
      return(TRUE) # to indicate status() started a run() so that the run() afterwards can be avoided which would otherwise see these as not started and run them simultaneously but with _SUGGESTS=TRUE
    }
    # Otherwise, inspect manually each result in fail.log written by log()
  }
  invisible(FALSE)
}

cran = function()  # reports CRAN status of the .fail.cran packages
{
  x = c(.fail.cran, .running.cran)
  if (!length(x)) {
    cat("No CRAN revdeps in error, warning or running status\n")
    return(invisible())
  }
  require(data.table)
  p = proc.time()
  db <<- setDT(tools::CRAN_check_results())
  cat("tools::CRAN_check_results() returned",prettyNum(nrow(db), big.mark=","),"rows in",timetaken(p),"\n")
  ans = db[Package %chin% x,
    .(ERROR=sum(Status=="ERROR", na.rm=TRUE),
      WARN =sum(Status=="WARN", na.rm=TRUE),
      cran =paste(unique(Version),collapse=";"),
      local=as.character(tryCatch(packageVersion(.BY[[1]]), error=function(e)"error"))),
    keyby=Package]
  ans[local==cran, c("cran","local"):=""]
  ans[, "right_click_in_bash":=paste0("https://cran.r-project.org/web/checks/check_results_",Package,".html")]
  setkey(ans, Package)[x,]
}

run = function(pkgs=NULL, R_CHECK_FORCE_SUGGESTS=TRUE, choose=NULL, ask=TRUE) {
  if (length(pkgs)==1) pkgs = strsplit(pkgs, split="[, ]")[[1]]
  if (anyDuplicated(pkgs)) stop("pkgs contains dups")
  if (!length(pkgs)) {
    opts = c("not.started","cran.fail","bioc.fail","both.fail","rerun.cran","rerun.bioc","rerun.all")
    w = if (is.null(choose)) {
      cat(paste0(1:length(opts),": ",opts)  , sep="\n")
      suppressWarnings(as.integer(readline("Enter option: ")))
    } else choose
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
    } else if (which=="rerun.cran") {
      pkgs = deps[ !grepl("bioconductor", avail[deps,"Repository"]) ]
    } else if (which=="rerun.bioc") {
      pkgs = deps[ grepl("bioconductor", avail[deps,"Repository"]) ]
    } else {
      pkgs = NULL
      if (which=="not.started") pkgs = deps[!file.exists(paste0("./",deps,".Rcheck"))]  # those that haven't run
      if (which %in% c("cran.fail","both.fail")) pkgs = union(pkgs, .fail.cran)  # .fail.* were written to .GlobalEnv by status0()
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
  if (ask && is.null(choose)) {
    cat("Proceed? (ctrl-c or enter)\n")
    scan(quiet=TRUE)
  }
  if (!identical(pkgs,"_ALL_")) for (i in pkgs) system(paste0("rm -rf ./",i,".Rcheck"))
  SUGG = paste0("_R_CHECK_FORCE_SUGGESTS_=",tolower(R_CHECK_FORCE_SUGGESTS))
  cmd = paste0("ls -1 *.tar.gz ", filter, "| TZ='UTC' OMP_THREAD_LIMIT=2 ",SUGG," parallel --max-procs 50% --timeout 1200 ",R," CMD check")
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

log = function(bioc=FALSE, fnam="~/fail.log", app="gedit") {
  x = c(.fail.cran, if (bioc) .fail.bioc)
  cat("Writing 00check.log for",length(x),"packages to",fnam,":\n")
  cat(paste(x,collapse=" "), "\n")
  require(BiocManager)  # to ensure Bioc version is included in attached packages sessionInfo. It includes the minor version this way; e.g. 1.30.4
  cat(capture.output(sessionInfo()), "\n", file=fnam, sep="\n")
  for (i in x) {
    system(paste0("ls | grep '",i,"_.*tar.gz' >> ",fnam))
    if (bioc && i %in% .fail.bioc) {
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
  system(paste(app, fnam), wait=FALSE)
  invisible()
}

inst()
if (!status()) run(choose=1)  # run pkgs in not-started status; i.e. updates to and new revdeps. Unless status() found any all-suggests-unavail which it then started run() for with _SUGGESTS=FALSE

# Now R prompt is ready to fix any problems with CRAN or Bioconductor updates.
# Then run run(), status() and log() as per section in CRAN_Release.cmd

