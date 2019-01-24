###############################################
#  Basic checks
###############################################

sudo apt-get update
sudo apt-get upgrade
R
update.packages(ask=FALSE)
q("no")

# Ensure latest version of R otherwise problems with CRAN not finding dependents that depend on latest R
# e.g. mirror may have been disabled in sources.list when upgrading ubuntu

cd ~/GitHub/data.table
rm ./src/*.so
rm ./src/*.o
rm -rf ./data.table.Rcheck

# Ensure no non-ASCII, other than in README.md is ok
# tests.Rraw in particular have failed CRAN Solaris (only) due to this.
grep -RI --exclude-dir=".git" --exclude="*.md" --exclude="*~" --color='auto' -P -n "[\x80-\xFF]" ./

# Unicode is now ok. This unicode in tests.Rraw is passing on CRAN.
grep -RI --exclude-dir=".git" --exclude="*.md" --exclude="*~" --color='auto' -n "[\]u[0-9]" ./

# Ensure no calls to omp_set_num_threads() [to avoid affecting other packages and base R]
# Only comments referring to it should be in openmp-utils.c
grep omp_set_num_threads ./src/*

# Ensure no calls to omp_get_max_threads() also since access should be via getDTthreads()
grep --exclude="./src/openmp-utils.c" omp_get_max_threads ./src/*

# Ensure all #pragama omp parallel directives include a num_threads() clause
grep "pragma omp parallel" ./src/*.c

# Ensure all .Call's first argument are unquoted.  TODO - change to use INHERITS()
grep "[.]Call(\"" ./R/*.R

# Ensure no Rprintf in init.c
grep "Rprintf" ./src/init.c

# workaround for IBM AIX - ensure no globals named 'nearest' or 'class'.
# See https://github.com/Rdatatable/data.table/issues/1351
grep "nearest *=" ./src/*.c  # none
grep "class *=" ./src/*.c    # quite a few but none global

# No undefined type punning of the form:  *(long long *)&REAL(column)[i]
# Failed clang 3.9.1 -O3 due to this, I think.
grep "&REAL" ./src/*.c

# No [UN]PROTECT_PTR, #3232
grep "PROTECT_PTR" ./src/*.c

# No use of long long, instead use int64_t. TODO
# grep "long long" ./src/*.c

# No tabs in C or R code (sorry, Richard Hendricks)
grep -P "\t" ./R/*.R
grep -P "\t" ./src/*.c

# No T or F symbols in tests.Rraw. 24 valid F (quoted, column name or in data) and 1 valid T at the time of writing
grep -n "[^A-Za-z0-9]T[^A-Za-z0-9]" ./inst/tests/tests.Rraw
grep -n "[^A-Za-z0-9]F[^A-Za-z0-9]" ./inst/tests/tests.Rraw

# No system.time in main tests.Rraw. Timings should be in benchmark.Rraw
grep -n "system[.]time" ./inst/tests/tests.Rraw

# seal leak potential where two unprotected API calls are passed to the same
# function call, usually involving install() or mkChar()
# Greppable thanks to single lines and wide screens
# See comments in init.c
cd ./src
grep install.*alloc *.c   --exclude init.c
grep install.*Scalar *.c
grep alloc.*install *.c   --exclude init.c
grep Scalar.*install *.c
grep mkChar.*alloc *.c
grep mkChar.*Scalar *.c
grep alloc.*mkChar *.c
grep Scalar.*mkChar *.c
grep "getAttrib.*mk" *.c   # use sym_* in getAttrib calls
grep "\"starts\"" *.c     --exclude init.c
grep "setAttrib(" *.c      # scan all setAttrib calls manually as a double-check
grep "install(" *.c       --exclude init.c   # TODO: perhaps in future pre-install all constants
grep mkChar *.c            # see comment in bmerge.c about passing this grep. I'm not sure char_ is really necessary, though.

# ScalarInteger and ScalarString allocate and must be PROTECTed unless i) returned (which protects),
# or ii) passed to setAttrib (which protects, providing leak-seals above are ok)
# ScalarLogical in R now returns R's global TRUE from R 3.1.0; Apr 2014. Before that it allocated.
# Aside: ScalarInteger may return globals for small integers in future version of R.
grep ScalarInteger *.c   # Check all Scalar* either PROTECTed, return-ed or passed to setAttrib.
grep ScalarLogical *.c   # Now we depend on 3.1.0+, check ScalarLogical is NOT PROTECTed.
grep ScalarString *.c

# Inspect missing PROTECTs
# To pass this grep is why we like SET_VECTOR_ELT(,,var=allocVector()) style on one line.
# If a PROTECT is not needed then a comment is added explaining why and including "PROTECT" in the comment to pass this grep
grep allocVector *.c | grep -v PROTECT | grep -v SET_VECTOR_ELT | grep -v setAttrib | grep -v return

cd ..
R
cc(clean=TRUE)  # to compile with -pedandic. Also use very latest gcc (currently gcc-7) as CRAN does
saf = options()$stringsAsFactors
options(stringsAsFactors=!saf)    # check tests (that might be run by user) are insensitive to option, #2718
test.data.table()
install.packages("xml2")   # to check the 150 URLs in NEWS.md under --as-cran below
q("no")
R CMD build .
R CMD check data.table_1.12.1.tar.gz --as-cran
R CMD INSTALL data.table_1.12.1.tar.gz

# Test C locale doesn't break test suite (#2771)
echo LC_ALL=C > ~/.Renviron
R
Sys.getlocale()=="C"
q("no")
R CMD check data.table_1.12.1.tar.gz
rm ~/.Renviron

R
remove.packages("xml2")    # we checked the URLs; don't need to do it again (many minutes)
require(data.table)
test.data.table()
test.data.table(with.other.packages=TRUE)
test.data.table(verbose=TRUE)   # since main.R no longer tests verbose mode
gctorture2(step=50)
system.time(test.data.table())  # apx 75min

# Upload to win-builder: release, dev & old-release


###############################################
#  R 3.1.0 (stated dependency)
###############################################

### ONE TIME BUILD
sudo apt-get -y build-dep r-base
cd ~/build
wget http://cran.stat.ucla.edu/src/base/R-3/R-3.1.0.tar.gz
tar xvf R-3.1.0.tar.gz
cd R-3.1.0
./configure --without-recommended-packages
make
alias R310=~/build/R-3.1.0/bin/R
### END ONE TIME BUILD

cd ~/GitHub/data.table
R310 CMD INSTALL ./data.table_1.12.1.tar.gz
R310
require(data.table)
test.data.table()


############################################
#  Check compiles ok when OpenMP is disabled
############################################
vi ~/.R/Makevars
# Make line SHLIB_OPENMP_CFLAGS= active to remove -fopenmp
R CMD build .
R CMD INSTALL data.table_1.12.1.tar.gz   # ensure that -fopenmp is missing and there are no warnings
R
require(data.table)   # observe startup message about no OpenMP detected
test.data.table()
q("no")
vi ~/.R/Makevars
# revert change above
R CMD build .
R CMD check data.table_1.12.1.tar.gz

#####################################################
#  R-devel with UBSAN, ASAN and strict-barrier on too
#####################################################

cd ~/build
wget -N https://stat.ethz.ch/R/daily/R-devel.tar.gz
rm -rf R-devel
rm -rf R-devel-strict
tar xvf R-devel.tar.gz
mv R-devel R-devel-strict
cd R-devel-strict    # important to change directory name before building not after because the path is baked into the build, iiuc
# Following R-exts#4.3.3

./configure --without-recommended-packages --disable-byte-compiled-packages --disable-openmp --enable-strict-barrier CC="gcc -fsanitize=undefined,address -fno-sanitize=float-divide-by-zero -fno-omit-frame-pointer" CFLAGS="-O0 -g -Wall -pedantic" LIBS="-lpthread"
# For ubsan, disabled openmp otherwise gcc fails in R's distance.c:256 error: ‘*.Lubsan_data0’ not specified in enclosing parallel
# UBSAN gives direct line number under gcc but not clang it seems. clang-5.0 has been helpful too, though.
# If use later gcc-8, add F77=gfortran-8
# LIBS="-lpthread" otherwise ld error about DSO missing
# -fno-sanitize=float-divide-by-zero, otherwise /0 errors on R's summary.c (tests 648 and 1185.2) but ignore those:
#   https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=16000

# without ubsan, openmp can be on :
./configure --without-recommended-packages --disable-byte-compiled-packages --enable-strict-barrier CC="gcc -fsanitize=address -fno-sanitize=float-divide-by-zero -fno-omit-frame-pointer" CFLAGS="-O0 -g -Wall -pedantic"

make
alias Rdevel-strict='~/build/R-devel-strict/bin/R --vanilla'
cd ~/GitHub/data.table
Rdevel-strict CMD INSTALL data.table_1.12.1.tar.gz
# Check UBSAN and ASAN flags appear in compiler output above. Rdevel was compiled with them so should be passed through to here
Rdevel-strict
install.packages(c("bit64","xts","nanotime","R.utils"), repos="http://cloud.r-project.org")  # minimum packages needed to not skip any tests in test.data.table()
require(data.table)
test.data.table()      # 7 mins (vs 1min normally) under UBSAN, ASAN and --strict-barrier
# If any problems, edit ~/.R/Makevars and activate "CFLAGS=-O0 -g" to trace. Rerun 'Rdevel-strict CMD INSTALL' and rerun tests.
for (i in 1:10) if (!test.data.table()) break  # try several runs maybe even 100; e.g a few tests generate data with a non-fixed random seed
# gctorture(TRUE)      # very slow, many days
gctorture2(step=100)   # [12-18hrs] under ASAN, UBSAN and --strict-barrier
print(Sys.time()); started.at<-proc.time(); try(test.data.table()); print(Sys.time()); print(timetaken(started.at))

## In case want to ever try again with 32bit on 64bit Ubuntu for tracing any 32bit-only problems
## dpkg --add-architecture i386
## apt-get update
## apt-get install libc6:i386 libstdc++6:i386 gcc-multilib g++-multilib gfortran-multilib libbz2-dev:i386 liblzma-dev:i386 libpcre3-dev:i386 libcurl3-dev:i386 libstdc++-7-dev:i386
## sudo apt-get purge libcurl4-openssl-dev    # cannot coexist, it seems
## sudo apt-get install libcurl4-openssl-dev:i386
## cd ~/build/32bit/R-devel
## ./configure --without-recommended-packages --disable-byte-compiled-packages --disable-openmp --without-readline --without-x CC="gcc -m32" CXX="g++ -m32" F77="gfortran -m32" FC=${F77} OBJC=${CC} LDFLAGS="-L/usr/local/lib" LIBnn=lib LIBS="-lpthread" CFLAGS="-O0 -g -Wall -pedantic"
##


###############################################
#  valgrind in R-devel (see R-exts$4.3.2)
###############################################

cd ~/build
rm -rf R-devel    # easiest way to remove ASAN from compiled packages in R-devel/library
                  # to avoid "ASan runtime does not come first in initial library list" error; no need for LD_PRELOAD
tar xvf R-devel.tar.gz
cd R-devel
./configure --without-recommended-packages --disable-byte-compiled-packages --disable-openmp --with-valgrind-instrumentation=1 CC="gcc" CFLAGS="-O0 -g -Wall -pedantic" LIBS="-lpthread"
make
cd ~/GitHub/data.table
vi ~/.R/Makevars  # make the -O0 -g line active, for info on source lines with any problems
Rdevel CMD INSTALL data.table_1.12.1.tar.gz
Rdevel -d "valgrind --tool=memcheck --leak-check=full --track-origins=yes --show-leak-kinds=definite"
# gctorture(TRUE)      # very slow, many days
# gctorture2(step=100)
print(Sys.time()); require(data.table); print(Sys.time()); started.at<-proc.time(); try(test.data.table()); print(Sys.time()); print(timetaken(started.at))
# 3m require; 62m test

# Investigated and ignore :
# Tests 648 and 1262 (see their comments) have single precision issues under valgrind that don't occur on CRAN, even Solaris.
# Old comment from gsumm.c ...  // long double usage here used to result in test 648 failing when run under valgrind
                                // http://valgrind.org/docs/manual/manual-core.html#manual-core.limits"
# Ignore all "set address range perms" warnings :
#   http://stackoverflow.com/questions/13558067/what-does-this-valgrind-warning-mean-warning-set-address-range-perms
# Ignore heap summaries around test 1705 and 1707/1708 due to the fork() test opening/closing, I guess.
# Tests 1729.4, 1729.8, 1729.11, 1729.13 again have precision issues under valgrind only.
# Leaks for tests 1738.5, 1739.3 but no data.table .c lines are flagged, rather libcairo.so
#   and libfontconfig.so via GEMetricInfo and GEStrWidth in libR.so

vi ~/.R/Makevars  # make the -O3 line active again


#############################################################################
# TODO: recompile without USE_RINTERNALS and recheck write barrier under ASAN
#############################################################################
There are some things to overcome to achieve compile without USE_RINTERNALS, though.


########################################################################
#  Single precision e.g. CRAN's Solaris-Sparc when it was alive on CRAN.
#  Not Solaris-x86 which is still on CRAN and easier.
########################################################################
#
# Adding --disable-long-double (see R-exts) in the same configure as ASAN/UBSAN fails.  Hence separately.
## cd ~/build
## rm -rf R-devel    # 'make clean' isn't enough: results in link error, oddly.
## tar xvf R-devel.tar.gz
## cd R-devel
## ./configure CC="gcc -std=gnu99" CFLAGS="-O0 -g -Wall -pedantic -ffloat-store -fexcess-precision=standard" --without-recommended-packages --disable-byte-compiled-packages --disable-long-double
## make
## Rdevel
## install.packages("bit64")
## q("no")
## Rdevel CMD INSTALL ~/data.table_1.12.1.tar.gz
## Rdevel
## .Machine$sizeof.longdouble   # check 0
## require(data.table)
## require(bit64)
## test.data.table()
## q("no")

########################################################################
#  rchk : https://github.com/kalibera/rchk
########################################################################
cd ~/build/rchk/trunk
. ../scripts/config.inc
. ../scripts/cmpconfig.inc
# edit ~/.R/Makevars to set CFLAGS=-O0 -g so that rchk can provide source line numbers
echo 'install.packages("~/GitHub/data.table/data.table_1.12.1.tar.gz",repos=NULL)' | ./bin/R --slave
../scripts/check_package.sh data.table
cat packages/lib/data.table/libs/*check
# keep running and rerunning locally until all problems cease.
#   rchk has an internal stack which can exhaust. Clearing the current set of problems (e.g. as displayed
#   on CRAN) is not sufficient because new problems can be found because it didn't get that far before.
#   Hence repeating locally until clear is necessary.


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
sudo apt-get -y install aptitude
sudo aptitude build-dep r-cran-rgl   # leads to libglu1-mesa-dev
sudo apt-get -y build-dep r-cran-rmpi
sudo apt-get -y build-dep r-cran-cairodevice
sudo apt-get -y build-dep r-cran-tkrplot
sudo apt-get -y install libcurl4-openssl-dev    # needed by devtools
sudo apt-get -y install xorg-dev x11-common libgdal-dev libproj-dev mysql-client libcairo2-dev libglpk-dev
sudo apt-get -y install texlive texlive-latex-extra texlive-bibtex-extra texlive-science texinfo texlive-fonts-extra
sudo apt-get -y install libv8-dev
sudo apt-get -y install gsl-bin libgsl0-dev
sudo apt-get -y install libgtk2.0-dev netcdf-bin
sudo apt-get -y install libcanberra-gtk-module
sudo apt-get -y install git
sudo apt-get -y install openjdk-8-jdk
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
sudo apt-get -y install libsndfile1-dev  # for seewave
sudo apt-get -y install libpoppler-cpp-dev  # for pdftools
sudo apt-get -y install libapparmor-dev  # for sys
sudo apt-get -y install libmagick++-dev  # for magick
sudo apt-get -y install libjq-dev libprotoc-dev libprotobuf-dev and protobuf-compiler   # for protolite
sudo apt-get -y install python-dev  # for PythonInR
sudo apt-get -y install gdal-bin libgeos-dev  # for rgdal/raster tested via lidR
sudo apt-get build-dep r-cran-rsymphony   # for Rsymphony: coinor-libcgl-dev coinor-libclp-dev coinor-libcoinutils-dev coinor-libosi-dev coinor-libsymphony-dev
sudo apt-get -y install libtesseract-dev libleptonica-dev tesseract-ocr-eng   # for tesseract
sudo apt-get -y install libssl-dev libsasl2-dev
sudo apt-get -y install biber   # for ctsem
sudo R CMD javareconf
# ENDIF

cd ~/build/revdeplib/
export R_LIBS=~/build/revdeplib/
export R_LIBS_SITE=none
export _R_CHECK_FORCE_SUGGESTS_=false         # in my profile so always set
R
.libPaths()   # should be just 2 items: revdeplib and the base R package library
options(repos = c("CRAN"=c("http://cloud.r-project.org")))
update.packages(ask=FALSE)
# if package not found on mirror, try manually a different one:
install.packages("<pkg>", repos="http://cran.stat.ucla.edu/")
update.packages(ask=FALSE)   # a repeat sometimes does more, keep repeating until none

# Follow: https://bioconductor.org/install
# Ensure no library() call in .Rprofile, such as library(bit64)
BiocManager::install()  # rerun to confirm
BiocManager::valid()

avail = available.packages(repos=BiocManager::repositories())  # includes CRAN at the end from getOption("repos"). And ensure latest Bioc version is in repo path here.
deps = tools::package_dependencies("data.table", db=avail, which="most", reverse=TRUE, recursive=FALSE)[[1]]
exclude = c("TCGAbiolinks")  # too long (>30mins): https://github.com/BioinformaticsFMRP/TCGAbiolinks/issues/240
deps = deps[-match(exclude, deps)]
table(avail[deps,"Repository"])
length(deps)
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
length(deps)
update.packages(repos=BiocManager::repositories())  # double-check all dependencies are latest too

# Remove the tar.gz no longer needed :
system("ls *.tar.gz | wc -l")
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
system("ls *.tar.gz | wc -l")
length(deps)

status = function(which="both") {
  if (which=="both") {
    cat("Installed data.table to be tested against:",as.character(packageVersion("data.table")),"\n")
    cat("CRAN:\n"); status("cran")
    cat("BIOC:\n"); status("bioc")
    cat("TOTAL          :", length(deps), "\n\n")
    cat("Oldest 00check.log (to check no old stale ones somehow missed):\n")
    system("find . -name '00check.log' | xargs ls -lt | tail -1")
    cat("\n")
    tt = length(system('ps -aux | grep "parallel.*R CMD check"', intern=TRUE))>2L
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

status()

run = function(which=c("not.started","cran.fail","bioc.fail","both.fail","rerun.all")) {
  cat("Installed data.table to be tested against:",as.character(packageVersion("data.table")),"\n")
  if (length(which)>1) which = which[1L]
  cat("which == ",which,"\n", sep="")
  numtgz = as.integer(system("ls -1 *.tar.gz | wc -l", intern=TRUE))
  stopifnot(numtgz==length(deps))
  if (which=="rerun.all") {
    cmd = "rm -rf *.Rcheck ; ls -1 *.tar.gz | TZ='UTC' parallel R CMD check"
    # TZ='UTC' because some packages have failed locally for me but not on CRAN or for their maintainer, due to sensitivity of tests to timezone
    cat("WIPE ALL CHECKS:",cmd,"\n")
    cat("Proceed? (ctrl-c or enter)\n")
    scan(quiet=TRUE)
    # apx 7.5 hrs for 582 packages on my 4 cpu laptop with 8 threads
  } else {
    if (!which %in% c("not.started","cran.fail","bioc.fail","both.fail")) {
      x = which   # one package manually
    } else {
      x = NULL
      if (which=="not.started") x = deps[!file.exists(paste0("./",deps,".Rcheck"))]  # those that haven't run
      if (which %in% c("cran.fail","both.fail")) x = union(x, .fail.cran)  # .fail.* were written to .GlobalEnv by status()
      if (which %in% c("bioc.fail","both.fail")) x = union(x, .fail.bioc)
    }
    if (length(x)==0) { cat("No packages to run\n"); return(invisible()); }
    cat("Running",length(x),"packages:", paste(x), "\n")
    cat("Proceed? (ctrl-c or enter)\n")
    scan(quiet=TRUE)
    for (i in x) system(paste0("rm -rf ./",i,".Rcheck"))
    cmd = paste0("ls -1 *.tar.gz | grep -E '", paste0(paste0(x,"_"),collapse="|"),"' | TZ='UTC' parallel R CMD check")
  }
  if (as.integer(system("ps -e | grep perfbar | wc -l", intern=TRUE)) < 1) system("perfbar",wait=FALSE)
  system("touch /tmp/started.flag ; rm -f /tmp/finished.flag")
  system(paste("((",cmd,">/dev/null 2>&1); touch /tmp/finished.flag)"), wait=FALSE)
}

# ** ensure latest version installed into revdeplib **
system("R CMD INSTALL ~/GitHub/data.table/data.table_1.12.1.tar.gz")
run("rerun.all")

out = function(fnam="~/fail.log") {
  x = c(.fail.cran, .fail.bioc)
  cat("Writing 00check.log for",length(x),"packages to",fnam,":\n")
  cat(paste(x,collapse=" "), "\n")
  cat(capture.output(sessionInfo()), "\n", file=fnam, sep="\n")
  cat("> BiocManager::install()\n", file=fnam, append=TRUE)
  cat(capture.output(BiocManager::install(), type="message"), file=fnam, sep="\n", append=TRUE)
  cat("> BiocManager::valid()\n", file=fnam, append=TRUE)
  cat(isTRUE(BiocManager::valid()), "\n\n\n", file=fnam, append=TRUE)
  for (i in x) {
    system(paste0("ls | grep '",i,".*tar.gz' >> ",fnam))
    system(paste0("grep -H . ./",i,".Rcheck/00check.log >> ",fnam))
    cat("\n\n", file=fnam, append=TRUE)
  }
}

# Once all issues resolved with CRAN packages, tackle long-term unfixed bioconductor packages as follows.
# 1. Note down all error and warning bioc packages
# 2. Revert to CRAN data.table :
install.packages("data.table")
run("bioc.fail")
# 3. Email bioc maintainers again asking them to fix existing errors and warnings (usually unrelated to data.table)
emails = gsub(">$","",gsub(".*<","", sapply(.fail.bioc, maintainer)))
cat(emails,sep=";")
# 4. Investigate the packages that reverting to CRAN data.table solved (diff between 1 and 2).

# Old commands before run() was expanded ...
find . -name 00check.log -exec grep -H -B 20 "Status:.*ERROR" {} \;
find . -name 00check.log | grep -E 'AFM|easycsv|...|optiSel|xgboost' | xargs grep -H . > /tmp/out.log
# For RxmSim: export JAVA_HOME=/usr/lib/jvm/java-8-oracle
more <failing_package>.Rcheck/00check.log
TZ='UTC' R CMD check <failing_package>.tar.gz
R CMD INSTALL ~/data.table_1.9.6.tar.gz   # CRAN version to establish if fails are really due to data.table
TZ='UTC' R CMD check <failing_package>.tar.gz
ls -1 *.tar.gz | grep -E 'Chicago|dada2|flowWorkspace|LymphoSeq' | TZ='UTC' parallel R CMD check &


###############################################
#  Release to CRAN
###############################################

Bump version to even release number in 3 places :
  1) DESCRIPTION
  2) NEWS (without 'on CRAN date' text as that's not yet known)
  3) dllVersion() at the end of init.c
DO NOT push to GitHub. Prevents even a slim possibility of user getting premature version. Even release numbers must have been obtained from CRAN and only CRAN. There were too many support problems in the past before this procedure was brought in.
R CMD build .
R CMD check --as-cran data.table_1.12.0.tar.gz
Resubmit to winbuilder (R-release, R-devel and R-oldrelease)
Submit to CRAN. Message template :
------------------------------------------------------------
Hello,
Happy New Year and thanks for everything!
595 CRAN + 135 BIOC rev deps checked ok
2 already updated on CRAN in advance and should not break: behavr, maditr
3 will break: checkmate, rENA and SpaDES.core. Maintainers aware and standing ready.
3 newly in error/warning status on CRAN in the last day or so for non-data.table reasons: segregation, xlm, prediction
NoLD 'additional issue' resolved
Requests from Luke Tierney and Tomas Kalibera included, noted and thanked in NEWS.
Best, Matt
------------------------------------------------------------
DO NOT commit or push to GitHub. Leave 4 files (CRAN_Release.cmd, DESCRIPTION, NEWS and init.c) edited and not committed. Include these in a single and final bump commit below.
DO NOT even use a PR. Because PRs build binaries and we don't want any binary versions of even release numbers available from anywhere other than CRAN.
Leave milestone open with a 'final checks' issue open. Keep updating status there.
** If on EC2, shutdown instance. Otherwise get charged for potentially many days/weeks idle time with no alerts **
Sleep.
If any issues arise, backport locally. Resubmit the same even version to CRAN.
When on CRAN :
1. Close milestone
2. Check that 'git status' shows 4 files in modified and uncommitted state: DESCRIPTION, NEWS.md, init.c and this CRAN_Release.cmd
2. Bump version in DESCRIPTION to next odd number. Note that DESCRIPTION was in edited and uncommitted state so even number never appears in git.
3. Add new heading in NEWS for the next dev version. Add "(date)" on the released heading.
4. Bump 3 version numbers in Makefile
5. Bump dllVersion() in init.c
6. Push to master with this consistent commit message: "1.12.0 on CRAN. Bump to 1.12.1"
7. Take sha from step 6 and run `git tag 1.12.0 34796cd1524828df9bf13a174265cb68a09fcd77` then `git push origin 1.12.0` (not `git push --tags` according to https://stackoverflow.com/a/5195913/403310)

