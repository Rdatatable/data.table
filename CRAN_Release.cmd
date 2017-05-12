###############################################
#  Basic checks
###############################################

sudo apt-get update
sudo apt-get upgrade
R
update.packages(ask=FALSE)
# See here for getting R.oo to install: https://github.com/HenrikBengtsson/R.utils/issues/29
q("no")

# Ensure latest version of R otherwise problems with CRAN not finding dependents that depend on latest R
# e.g. mirror may have been disabled in sources.list when upgrading ubuntu

# Ensure no non-ASCII, other than in README.md is ok
# tests.Rraw in particular have failed CRAN Solaris (only) due to this.
# No unicode either. Put these tests in DtNonAsciiTests package.
grep -RI --exclude-dir=".git" --exclude="*.md" --exclude="*~" --color='auto' -P -n "[\x80-\xFF]" data.table/
grep -RI --exclude-dir=".git" --exclude="*.md" --exclude="*~" --color='auto' -n "[\]u[0-9]" data.table/

# Ensure no calls to omp_set_num_threads() [to avoid affecting other packages and base R]
grep --exclude="data.table/src/openmp-utils.c" omp_set_num_threads data.table/src/*

# Endure no calls to omp_get_max_threads() also since access should be via getDTthreads()
grep --exclude="data.table/src/openmp-utils.c" omp_get_max_threads data.table/src/*

# Ensure all #pragama omp parallel directives include a num_threads() clause
grep "pragma omp parallel" data.table/src/*.c

# Ensure all .Call's first argument are unquoted.  TODO - change to use INHERITS()
grep "[.]Call(\"" data.table/R/*.R

# Ensure no Rprintf in init.c
grep "Rprintf" data.table/src/init.c

# workaround for IBM AIX - ensure no globals named 'nearest' or 'class'.
# See https://github.com/Rdatatable/data.table/issues/1351
grep "nearest *=" data.table/src/*.c  # none
grep "class *=" data.table/src/*.c    # quite a few but none global

# No undefined type punning of the form:  *(long long *)&REAL(column)[i]
# Failed clang 3.9.1 -O3 due to this, I think.
grep "&REAL" data.table/src/*.c

# seal leak potential where two unprotected API calls are passed to the same
# function call, usually involving install() or mkChar()
# Greppable thanks to single lines and wide screens
# See comments in init.c
cd data.table/src
grep install.*alloc *.c   --exclude init.c
grep install.*Scalar *.c
grep alloc.*install *.c   --exclude init.c
grep Scalar.*install *.c
grep mkChar.*alloc *.c
grep mkChar.*Scalar *.c
grep alloc.*mkChar *.c
grep Scalar.*mkChar *.c
grep "getAttrib.*mk" *.c   # use sym_* in getAttrib calls
grep "PROTECT *( *getAttrib" *.c  # attributes are already protected
grep "\"starts\"" *.c     --exclude init.c
grep "setAttrib(" *.c      # scan all setAttrib calls manually as a double-check
grep "install(" *.c       --exclude init.c   # TODO: perhaps in future pre-install all constants

# ScalarInteger and ScalarString allocate and must be PROTECTed unless i) returned (which protects),
# or ii) passed to setAttrib (which protects, providing leak-seals above are ok)
# ScalarLogical in R now returns R's global TRUE but from R 3.1.0; Apr 2014. Before that it allocates.
# Aside: ScalarInteger may return globals for small integers in future version of R.
grep ScalarInteger *.c   # Check all Scalar* either PROTECTed, return-ed or passed to setAttrib.
grep ScalarLogical *.c   # When we move R dependency to 3.1.0+, no need to protect ScalarLogical
grep ScalarString *.c

cd
R
cc(clean=TRUE)  # to compile with -pedandic
q("no")
R CMD build data.table
R CMD check data.table_1.10.1.tar.gz --as-cran
R CMD INSTALL data.table_1.10.1.tar.gz
R
require(data.table)
test.data.table()
test.data.table(verbose=TRUE)  # since main.R no longer tests verbose mode

# Upload to win-builder, both release and dev



###############################################
#  R 3.0.0 (stated dependency)
###############################################

### ONE TIME BUILD
sudo apt-get -y build-dep r-base
cd ~/build
wget http://cran.stat.ucla.edu/src/base/R-3/R-3.0.0.tar.gz
tar xvf R-3.0.0.tar.gz
cd R-3.0.0
./configure --without-recommended-packages
make
alias R300=~/build/R-3.0.0/bin/R
cd ..
R300
install.packages("chron")
q("no")
### END ONE TIME BUILD

R300 CMD INSTALL ~/data.table_1.10.1.tar.gz
R300
require(data.table)
test.data.table()


###############################################
#  valgrind
###############################################

vi ~/.R/Makevars  # make the -O0 -g line active, for info on source lines with any problems
R --vanilla CMD INSTALL data.table_1.10.5.tar.gz
R -d "valgrind --tool=memcheck --leak-check=full --show-leak-kinds=definite" --vanilla
require(data.table)
require(bit64)
test.data.table()

gctorture(TRUE)   # very very slow, though. Don't run with suggests tests.
test.data.table()

# Investigated and ignore :
# Tests 648 and 1262 (see their comments) have single precision issues under valgrind that don't occur on CRAN, even Solaris.
# Ignore all "set address range perms" warnings :
#   http://stackoverflow.com/questions/13558067/what-does-this-valgrind-warning-mean-warning-set-address-range-perms
# Ignore heap summaries around test 1705 and 1707/1708 due to the fork() test opening/closing, I guess.
# Tests 1729.4, 1729.8, 1729.11, 1729.13 again have precision issues under valgrind only.
# Leaks for tests 1738.5, 1739.3 but no data.table .c lines are flagged, rather libcairo.so
#   and libfontconfig.so via GEMetricInfo and GEStrWidth in libR.so

vi ~/.R/Makevars  # make the -O3 line active again

###############################################
#  R-devel with UBSAN and ASAN on too
###############################################

cd ~/build
wget -N ftp://ftp.stat.math.ethz.ch/pub/Software/R/R-devel.tar.gz
rm -rf R-devel
tar xvf R-devel.tar.gz
cd R-devel
# Following R-exts#4.3.3
# (clang 3.6.0 works but gcc 4.9.2 fails in R's distance.c:256 error: ‘*.Lubsan_data0’ not specified in enclosing parallel)
./configure CC="clang -std=gnu99 -fsanitize=undefined,address" CFLAGS="-fno-omit-frame-pointer -O0 -g -Wall -pedantic -mtune=native" --without-recommended-packages --disable-byte-compiled-packages  
make
alias Rdevel='~/build/R-devel/bin/R --vanilla'
Rdevel
install.packages("bit64")  # important to run tests using integer64
# Skip compatibility tests with other Suggests packages; unlikely UBSAN/ASAN problems there.
q("no")
Rdevel CMD INSTALL ~/data.table_1.10.1.tar.gz
# Check UBSAN and ASAN flags appear in compiler output above. Rdevel was compiled with
# them (above) so should be passed through to here
Rdevel
require(data.table)
require(bit64)
test.data.table()     # slower than usual of course due to UBSAN and ASAN. Too slow to run R CMD check.
# Throws /0 errors on R's summary.c (tests 648 and 1185.2) but ignore those: https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=16000
q("no")


###############################################
#  Single precision e.g. CRAN's Solaris Sparc
###############################################

# Adding --disable-long-double (see R-exts) in the same configure as ASAN/UBSAN fails.  Hence separately.

cd ~/build
rm -rf R-devel    # 'make clean' isn't enough: results in link error, oddly.
tar xvf R-devel.tar.gz
cd R-devel
./configure CC="gcc -std=gnu99" CFLAGS="-O0 -g -Wall -pedantic -ffloat-store -fexcess-precision=standard" --without-recommended-packages --disable-byte-compiled-packages --disable-long-double
make
Rdevel
install.packages("bit64")
q("no")
Rdevel CMD INSTALL ~/data.table_1.9.9.tar.gz
Rdevel
.Machine$sizeof.longdouble   # check 0
require(data.table)
require(bit64)
test.data.table()
q("no")


###############################################
#  QEMU to emulate big endian
###############################################

# http://www.aurel32.net/info/debian_sparc_qemu.php
# https://people.debian.org/~aurel32/qemu/powerpc/

### IF NOT ALREADY DONE
sudo apt-get -y install qemu
sudo apt-get -y install openbios-ppc
cd ~/build
wget https://people.debian.org/~aurel32/qemu/powerpc/debian_wheezy_powerpc_standard.qcow2
### ENDIF

qemu-system-ppc -hda debian_wheezy_powerpc_standard.qcow2 -nographic
root, root
lscpu  # confirm big endian
wget http://cran.r-project.org/src/base/R-latest.tar.gz
tar xvf R-latest.tar.gz
cd R-3.2.1
apt-get update
apt-get build-dep r-base
uptime   # the first time, several hours.  But it saves the disk image in the .qcow2 file so be sure not to wipe it
./configure CFLAGS="-O0 -g" --without-recommended-packages --disable-byte-compiled-packages
# -O0 to compile quicker. Keeping -g in case needed to debug
make
alias R=$PWD/bin/R
cd ~
wget -N https://github.com/Rdatatable/data.table/archive/master.zip   # easiest way; avoids setting up share between host and qemu
apt-get install unzip
unzip master.zip
mv data.table-master data.table
R CMD build --no-build-vignettes data.table
# R CMD check requires many packages in Suggests. Too long under emulation. Instead run data.table's test suite directly
R
options(repos = "http://cran.stat.ucla.edu")
install.packages("chron")  # takes a minute or so to start download and install.
install.packages("bit64")  # important to test data.table with integer64 on big endian
q("no")
R CMD INSTALL data.table_1.9.5.tar.gz
R
.Platform$endian
require(data.table)
test.data.table()
q()
shutdown now   # doesn't return you to host prompt properly so just kill the window.  http://comments.gmane.org/gmane.linux.embedded.yocto.general/3410


###############################################
#  Downstream dependencies
###############################################

# IF NOT ALREADY INSTALLED
sudo apt-get update
sudo apt-get -y install htop
sudo apt-get -y install r-base r-base-dev
sudo apt-get -y build-dep r-base-dev     
sudo apt-get -y build-dep qpdf
sudo apt-get -y build-dep r-cran-rgl
sudo apt-get -y build-dep r-cran-rmpi
sudo apt-get -y build-dep r-cran-cairodevice
sudo apt-get -y build-dep r-cran-tkrplot
sudo apt-get -y install libcurl4-openssl-dev    # needed by devtools
sudo apt-get -y install xorg-dev x11-common libgdal-dev libproj-dev mysql-client libcairo2-dev libglpk-dev
sudo apt-get -y install texlive texlive-latex-extra texlive-bibtex-extra texlive-science texinfo fonts-inconsolata latex-xcolor
sudo apt-get -y install libv8-dev
sudo apt-get -y install gsl-bin libgsl0-dev
sudo apt-get -y install libgtk2.0-dev netcdf-bin
sudo apt-get -y install libcanberra-gtk-module
sudo apt-get -y install git
sudo apt-get -y install openjdk-7-jdk
sudo apt-get -y install libnetcdf-dev udunits-bin libudunits2-dev
sudo apt-get -y install tk8.6-dev
sudo apt-get -y install clustalo  # for package LowMACA
sudo apt-get -y install texlive-xetex   # for package optiRum
sudo apt-get -y install texlive-pstricks  # for package GGtools
sudo apt-get -y install libfftw3-dev  # for package fftwtools
sudo apt-get -y install libgsl-dev
sudo apt-get -y install octave liboctave-dev
sudo apt-get -y install jags
sudo apt-get -y install libmpfr-dev
sudo apt-get -y install bwidget
sudo apt-get -y install librsvg2-dev  # for rsvg
sudo apt-get -y install libboost-all-dev libboost-locale-dev  # for textTinyR
sudo R CMD javareconf
# ENDIF

cd ~/build/revdeplib/
export R_LIBS=~/build/revdeplib/
export R_LIBS_SITE=none
export _R_CHECK_FORCE_SUGGESTS_=false         # in my profile so always set
R
.libPaths()   # should be just 2 items: revdeplib and the base R package library
update.packages(ask=FALSE)
# if package not found on mirror, try manually a different one:
install.packages("MIAmaxent", repos="http://cloud.r-project.org/")
update.packages(ask=FALSE)   # a repeat sometimes does more, keep repeating until none

# Follow: https://bioconductor.org/install/#troubleshoot-biocinstaller
# Ensure no library() call in .Rprofile, such as library(bit64)
source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite()   # keep repeating until returns with nothing left to do
# biocLite("BiocUpgrade")
# This error means it's up to date: "Bioconductor version 3.4 cannot be upgraded with R version 3.3.2"

avail = available.packages(repos=c(getOption("repos"), BiocInstaller::biocinstallRepos()[["BioCsoft"]]))
deps = tools::package_dependencies("data.table", db=avail, which="most", reverse=TRUE, recursive=FALSE)[[1]]
table(avail[deps,"Repository"])
length(deps)
old = 0
new = 0
for (p in deps) {
   fn = paste0(p, "_", avail[p,"Version"], ".tar.gz")
   if (!file.exists(fn) || 
       identical(tryCatch(packageVersion(p), error=function(e)FALSE), FALSE) ||
       packageVersion(p) != avail[p,"Version"]) {
     system(paste0("rm -f ", p, "*.tar.gz"))  # Remove previous *.tar.gz.  -f to be silent if not there (i.e. first time seeing this package)
     system(paste0("rm -rf ", p, ".Rcheck"))  # Remove last check (of previous version)
     if (!length(grep("bioc",avail[p,"Repository"]))) {
       install.packages(p, dependencies=TRUE)  # To install its dependencies really.
     } else {
       biocLite(p,suppressUpdates=TRUE)        # To install its dependencies really.
     }
     download.packages(p, destdir="~/build/revdeplib", contriburl=avail[p,"Repository"])   # So R CMD check can run on these
     if (!file.exists(fn)) stop("Still does not exist!:", fn)
     new = new+1
   } else {
     old = old+1
   }
}
cat("New downloaded:",new," Already had latest:", old, " TOTAL:", length(deps), "\n")
table(avail[deps,"Repository"])

# To identify and remove the tar.gz no longer needed :
for (p in deps) {
   f = paste0(p, "_", avail[p,"Version"], ".tar.gz")
   system(paste0("mv ",f," ",f,"_TEMP"))
}
system("rm *.tar.gz")
for (p in deps) {
   f = paste0(p, "_", avail[p,"Version"], ".tar.gz")
   system(paste0("mv ",f,"_TEMP ",f))
}

status = function(which="both") {
  if (which=="both") {
     cat("CRAN:\n"); status("cran")
     cat("BIOC:\n"); status("bioc")
     cat("Oldest 00check.log (to check no old stale ones somehow missed):\n")
     system("find . -name '00check.log' | xargs ls -lt | tail -1")
     cat("\n")
     if (file.exists("/tmp/started.flag")) {
       system("ls -lrt /tmp/*.flag")
       tt = as.POSIXct(file.info(c("/tmp/started.flag","/tmp/finished.flag"))$ctime)
       if (is.na(tt[2])) tt[2] = Sys.time()
       print(diff(tt))
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
      "RUNNING :",sprintf("%3d",length(r)),":",paste(sort(names(x)[r])),"\n",
      if (length(ns)==0) "\n" else paste0("NOT STARTED (first 20 of ",length(ns),") : ",paste(sort(names(x)[head(ns,20)]),collapse="|"),"\n")
      )
  invisible()
}

status()

run = function(all=FALSE) {
  numtgz = as.integer(system("ls -1 *.tar.gz | wc -l", intern=TRUE))
  stopifnot(numtgz==length(deps))
  cat("Installed data.table to be tested against:",as.character(packageVersion("data.table")),"\n")
  if (!all) {
    x = sapply(paste0("./",deps,".Rcheck"), file.exists)
    x = deps[!x]
    if (!length(x)) { cat("All package checks have already run. To rerun all: run(all=TRUE).\n"); return(); }
    cat("Running checks for",length(x),"packages\n")
    cmd = paste0("ls -1 *.tar.gz | grep -E '", paste0(x,collapse="|"),"' | parallel R CMD check") 
  } else {
    cmd = "rm -rf *.Rcheck ; ls -1 *.tar.gz | parallel R CMD check"
    # apx 2.5 hrs for 313 packages on my 4 cpu laptop with 8 threads
  }
  cat("Command:",cmd,"\n")
  if (as.integer(system("ps -a | grep perfbar | wc -l", intern=TRUE)) < 1) system("perfbar",wait=FALSE)
  cat("Proceed? (ctrl-c or enter)\n")
  scan(quiet=TRUE)
  system("touch /tmp/started.flag ; rm -f /tmp/finished.flag")
  system(paste(cmd,">/dev/null 2>&1 && touch /tmp/finished.flag"),wait=FALSE)
  #                                 ^^ must be && and not ; otherwise wait doesn't wait
}

run()
system("R CMD INSTALL ~/data.table_1.10.4.tar.gz")      # ** ensure latest version installed into revdeplib **


# Investigate and fix the fails ...
# For RxmSim: export JAVA_HOME=/usr/lib/jvm/java-8-oracle
more <failing_package>.Rcheck/00check.log
R CMD check <failing_package>.tar.gz
R CMD INSTALL ~/data.table_1.9.6.tar.gz   # CRAN version to establish if fails are really due to data.table
R CMD check <failing_package>.tar.gz
ls -1 *.tar.gz | grep -E 'Chicago|dada2|flowWorkspace|LymphoSeq' | parallel R CMD check

# Warning: replacing previous import robustbase::sigma by stats::sigma when loading VIM
# Reinstalling robustbase fixed this warning. Even though it was up to date, reinstalling made a difference.



###############################################
#  Release to CRAN
###############################################

Bump versions in DESCRIPTION and NEWS to even release number
Do not push to GitHub. Prevents even a slim possibility of user getting premature version. install_github() should only ever fetch odd releases at all times. Even release numbers must have been obtained from CRAN and only CRAN. (Too many support problems in past before this procedure brought in.)
R CMD build data.table
R CMD check --as-cran data.table_1.10.4.tar.gz
Resubmit to winbuilder (both R-release and R-devel)
Submit to CRAN
Bump version in DESCRIPTION to next ODD dev version
Add new heading in NEWS for the next dev version
Push to GitHub so dev can continue
Bump dev badge on homepage
Cross fingers accepted first time. If not, push changes to devel and backport locally
Close milestone
** If on EC2, shutdown instance. Otherwise get charged for potentially many days/weeks idle time with no alerts **

Submit message template:
Have rechecked the 339 CRAN packages using data.table.
Either ok or have liaised with maintainers in advance.


