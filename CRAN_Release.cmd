###############################################
#  Basic checks
###############################################

sudo apt-get update
sudo apt-get upgrade

sudo apt-get install pandoc
# v1.12 as on Ubuntu repo Sep-2015 had 'no header_extensions' problem. Needed manual update to v1.15
# Ubuntu now has v1.16 which is fine.

R
update.packages()
# See here for getting R.oo to install: https://github.com/HenrikBengtsson/R.utils/issues/29
q()

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

cc(clean=TRUE)  # to compile with -pedandic
R CMD build data.table
R CMD check data.table_1.9.9.tar.gz --as-cran
R CMD INSTALL data.table_1.9.9.tar.gz
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

R300 CMD INSTALL ~/data.table_1.9.9.tar.gz
R300
require(data.table)
test.data.table()


###############################################
#  valgrind
###############################################

vi .R/Makevars  # make the -O0 -g line active, for info on source lines with any problems
R --vanilla CMD INSTALL data.table_1.9.9.tar.gz
R -d "valgrind --tool=memcheck --leak-check=full --show-leak-kinds=definite" --vanilla
require(data.table)
require(bit64)
test.data.table()

# Investigated and ignore :
# Tests 648 and 1262 (see their comments) have single precision issues under valgrind that don't occur on CRAN, even Solaris.
# Ignore all "set address range perms" warnings :
#   http://stackoverflow.com/questions/13558067/what-does-this-valgrind-warning-mean-warning-set-address-range-perms
# Ignore heap summaries around test 1705 and 1707/1708 due to the fork() test opening/closing, I guess.
# Tests 1729.4, 1729.8, 1729.11, 1729.13 again have precision issues under valgrind only.

vi .R/Makevars  # make the -O3 line active again

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
Rdevel CMD INSTALL ~/data.table_1.9.9.tar.gz
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
sudo R CMD javareconf
# ENDIF

cd ~/build/revdeplib/
export R_LIBS=~/build/revdeplib/
export R_LIBS_SITE=none
R
.libPaths()   # should be just 2 items: revdeplib and the base R package library
update.packages(ask=FALSE)

# Follow: https://bioconductor.org/install/#troubleshoot-biocinstaller
# Ensure no library() call in .Rprofile, such as library(bit64)
source("http://bioconductor.org/biocLite.R")
biocLite()
# biocLite("BiocUpgrade")
# This error means it's up to date: "Bioconductor version 3.4 cannot be upgraded with R version 3.3.2"

avail = available.packages(repos=c(getOption("repos"), BiocInstaller::biocinstallRepos()[["BioCsoft"]]))
deps = tools::package_dependencies("data.table", db=avail, which="most", reverse=TRUE, recursive=FALSE)[[1]]
table(avail[deps,"Repository"])
old = 0
new = 0
for (p in deps) {
   fn = paste0(p, "_", avail[p,"Version"], ".tar.gz")
   if (!file.exists(fn) || identical(tryCatch(packageVersion(p), error=function(e)FALSE), FALSE)) {
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
   if (packageVersion(p) != avail[p,"Version"])
       stop("Installed version of ",p," doesn't equal the previously downloaded tar.gz")
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

cd ~/build/revdeplib/
export R_LIBS=~/build/revdeplib/
export R_LIBS_SITE=none
export _R_CHECK_FORCE_SUGGESTS_=false         # in my profile so always set
R CMD INSTALL ~/data.table_1.10.1.tar.gz      # ** ensure latest version installed into revdeplib **
rm -rf *.Rcheck                               # [optional] delete all previous .Rcheck directories
ls -1 *.tar.gz | wc -l                        # check this equals length(deps) above
time ls -1 *.tar.gz | parallel R CMD check    # apx 2.5 hrs for 313 packages on my 4 cpu laptop with 8 threads
ls -1 *.tar.gz | grep -E 'pk1|pk2|...' | parallel R CMD check

status = function(which="both") {
  if (which=="both") {
     cat("CRAN:\n"); status("cran")
     cat("BIOC:\n"); status("bioc")
     cat("Oldest 00check.log (to check no old stale ones somehow missed):\n")
     system("find . -name '00check.log' | xargs ls -lt | tail -1")
     return(invisible())
  }
  if (which=="cran") deps = deps[grep("cran",avail[deps,"Repository"])]
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
      if (length(ns)==0) "\n" else paste0("NOT STARTED (first 5 of ",length(ns),") : ",paste(sort(names(x)[head(ns,5)]),collapse=" "),"\n")
      )
  invisible()
}

status()

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
R CMD check --as-cran data.table_1.10.0.tar.gz
Resubmit to winbuilder (both R-release and R-devel)
Submit to CRAN
Bump version in DESCRIPTION to next ODD dev version
Add new heading in NEWS for the next dev version
Push to GitHub so dev can continue
Cross fingers accepted first time. If not, push changes to devel and backport locally
Close milestone
** If on EC2, shutdown instance. Otherwise get charged for potentially many days/weeks idle time with no alerts **

Submit message template:
Have rechecked the 229 CRAN packages using data.table.
Either ok or have liaised with maintainers in advance.


