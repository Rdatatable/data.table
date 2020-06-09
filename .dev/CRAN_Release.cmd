###############################################
#  Updating translations
###############################################

# 1) Update messages for new release
## (a) Update C template file: src/data.table.pot
##     ideally, we are including _() wrapping in
##     new PRs throughout dev cycle, and this step
##     becomes about tying up loose ends
## Appending _() char array wrapping to all messages
##   that might be shown to the user. This step is slightly
##   too greedy, as it includes too many msg, some of which
##   need not be translated [more work to do here to make
##   this less manual] some things to watch out for:
##     * quote embedded (and escaped) within message [could be fixed with smarter regex]
##     * multi-line implicit-concat arrays (in C, `"a" "b"` is the same as `"ab"`) should be wrapped "on the outside" not individually
##     * `data.table` shares some of its `src` with `pydatatable`, so the requirement to `#include <R.h>` before the `#define _` macro meant we need to be careful about including this macro only in the R headers for these files (hence I created `po.h`)
##     * Can't use `_()` _inside_ another functional macro. Only wrap the string passed to the macro later.
for MSG in error warning DTWARN DTPRINT Rprintf STOP Error;
  do for SRC_FILE in src/*.c;
    # no inplace -i in default mac sed
    do sed -E "s/$MSG[(]("[^"]*")/$MSG(_(\1)/g" $SRC_FILE > out;
    mv out $SRC_FILE;
  done
done

## checking for other lines calling these that didn't get _()-wrapped
for MSG in error warning DTWARN DTPRINT Rprintf STOP Error;
  do grep -Er "\b$MSG[(]" src --include=*.c | grep -v _ | grep -Ev "(?://|[*]).*$MSG[(]"

## similar, but a bit more manual to check snprintf usage

## look for char array that haven't been covered yet
grep -Er '"[^"]+"' src --include=*.c | grep -Fv '_("' | grep -v "#include" | grep -v '//.*".*"'

## look for lines starting with a char array (likely continued from prev line & can be combined)
grep -Er '^\s*"' src/*.c

## Now extract these messages with xgettext
cd src
xgettext --keyword=_ -o data.table.pot *.c
cd ..

## (b) Update R template file: src/R-data.table.pot
## much easier, once the update_pkg_po bug is fixed
R --no-save
## a bug fix in R still hadn't made the 2019-12-12 release,
##   so run the following to source the corrected function manually
STEM='https://raw.githubusercontent.com/wch/r-source/trunk/src/library/tools/R'
source(file.path(STEM, 'utils.R'))
source(file.path(STEM, 'xgettext.R'))
source(file.path(STEM, 'translations.R'))
## shouldn't be any errors from this...
update_pkg_po('.')
q()

# 2) Open a PR with the new templates & contact the translators
#   * zh_CN:
## Translators to submit commits with translations to this PR
##   [or perhaps, if we get several languages, each to open
##    its own PR and merge to main translation PR]

## 3) Check validity
##   update_pkg_po('.') to be run again for the PR
##     [can this be done via Travis?]

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

checkbashisms ./configure  # for portability; e.g. Solaris 10 running Bourne shell; #3964

# Ensure no non-ASCII, other than in README.md is ok
# tests.Rraw in particular have failed CRAN Solaris (only) due to this.
grep -RI --exclude-dir=".git" --exclude="*.md" --exclude="*~" --color='auto' -P -n "[\x80-\xFF]" ./

# Unicode is now ok. This unicode in tests.Rraw is passing on CRAN.
grep -RI --exclude-dir=".git" --exclude="*.md" --exclude="*~" --color='auto' -n "[\]u[0-9]" ./

# Ensure no calls to omp_set_num_threads() [to avoid affecting other packages and base R]
# Only comments referring to it should be in openmp-utils.c
grep omp_set_num_threads ./src/*

# Ensure no calls to omp_set_nested() as i) it's hard to fully honor OMP_THREAD_LIMIT as required by CRAN, and
#                                        ii) a simpler non-nested approach is always preferable if possible, as has been the case so far
grep omp_set_nested ./src/*.c

# Ensure no calls to omp_get_max_threads() also since access should be via getDTthreads()
grep --exclude="./src/openmp-utils.c" omp_get_max_threads ./src/*

# Ensure all #pragama omp parallel directives include a num_threads() clause
grep "pragma omp parallel" ./src/*.c | grep -v getDTthreads

# Update documented list of places where openMP parallelism is used: c.f. ?openmp
grep -Elr "[pP]ragma.*omp" src | sort

# Ensure all .Call's first argument are unquoted.
grep "[.]Call(\"" ./R/*.R

# Make sure all \dots calls in the manual are parsed as ...
Rscript -e "setwd('man'); unlist(sapply(list.files('.'), function(rd) rapply(tools::parse_Rd(rd), grep, pattern='dots', fixed = TRUE, value=TRUE)))"

# No unused macros in the manual (e.g. code{} instead of \code{})
grep -Enr "\b[^\a-z][a-z]+\{[^}]+\}" man

# Ensure no Rprintf in init.c
grep "Rprintf" ./src/init.c

# workaround for IBM AIX - ensure no globals named 'nearest' or 'class'.
# See https://github.com/Rdatatable/data.table/issues/1351
grep "nearest *=" ./src/*.c  # none
grep "class *=" ./src/*.c    # quite a few but none global

# ensure no use of z_const from zconf.h; #3939
grep "z_const" ./src/*.[hc]  # none other than the comment

# No undefined type punning of the form:  *(long long *)&REAL(column)[i]
# Failed clang 3.9.1 -O3 due to this, I think.
grep "&REAL" ./src/*.c

# No [UN]PROTECT_PTR, #3232
grep "PROTECT_PTR" ./src/*.c

# No use of long long, instead use int64_t. TODO
# grep "long long" ./src/*.c

// No use of llu, lld, zd or zu
grep -nE "(llu|lld|zd|zu)" src/*.[hc]
// Comment moved here from fread.c on 19 Nov 2019
// [Moved from fread.c on 19 Nov 2019] On Windows variables of type `size_t` cannot be printed
// with "%zu" in the `snprintf()` function. For those variables we used to cast them into
// `unsigned long long int` before printing, and defined (llu) to make the cast shorter.
// We're now observing warnings from gcc-8 with -Wformat-extra-args, #4062. So
// now we're more strict and cast to [u]int64_t and use PRIu64/PRId64 from <inttypes.h>
// In many cases the format specifier is passed to our own macro (e.g. DTPRINT) or to Rprintf(),
// error() etc, and even if they don't call sprintf() now, they could in future.

# No tabs in C or R code (sorry, Richard Hendricks)
grep -P "\t" ./R/*.R
grep -P "\t" ./src/*.c

# No T or F symbols in tests.Rraw. 24 valid F (quoted, column name or in data) and 1 valid T at the time of writing
grep -n "[^A-Za-z0-9]T[^A-Za-z0-9]" ./inst/tests/tests.Rraw
grep -n "[^A-Za-z0-9]F[^A-Za-z0-9]" ./inst/tests/tests.Rraw

# All integers internally should have L suffix to avoid lots of one-item coercions
# Where 0 numeric is intended we should perhaps use 0.0 for clarity and make the grep easier
# 1) tolerance=0 usages in setops.R are valid numeric 0, as are anything in strings
# 2) leave the rollends default using roll>=0 though; comments in PR #3803
grep -Enr "^[^#]*(?:\[|==|>|<|>=|<=|,|\(|\+)\s*[-]?[0-9]+[^0-9L:.e]" R | grep -Ev "stop|warning|tolerance"

# Never use ifelse. fifelse for vectors when necessary (nothing yet)
 grep -Enr "\bifelse" R

# No system.time in main tests.Rraw. Timings should be in benchmark.Rraw
grep -n "system[.]time" ./inst/tests/tests.Rraw

# All % in *.Rd should be escaped otherwise text gets silently chopped
grep -n "[^\]%" ./man/*.Rd

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
grep ScalarInteger *.c | grep -v PROTECT | grep -v setAttrib | grep -v return  # Check all Scalar* either PROTECTed, return-ed or passed to setAttrib.
grep ScalarString *.c  | grep -v PROTECT | grep -v setAttrib | grep -v return
grep ScalarLogical *.c   # Now we depend on 3.1.0+, check ScalarLogical is NOT PROTECTed.

# Inspect missing PROTECTs
# To pass this grep is why we like SET_VECTOR_ELT(,,var=allocVector()) style on one line.
# If a PROTECT is not needed then a comment is added explaining why and including "PROTECT" in the comment to pass this grep
grep allocVector *.c | grep -v PROTECT | grep -v SET_VECTOR_ELT | grep -v setAttrib | grep -v return
grep coerceVector *.c | grep -v PROTECT | grep -v SET_VECTOR_ELT | grep -v setAttrib | grep -v return
grep asCharacter *.c | grep -v PROTECT | grep -v SET_VECTOR_ELT | grep -v setAttrib | grep -v return

cd ..
R
cc(test=TRUE, clean=TRUE, CC="gcc-8")  # to compile with -pedandic -Wall, latest gcc as CRAN: https://cran.r-project.org/web/checks/check_flavors.html
saf = options()$stringsAsFactors
options(stringsAsFactors=!saf)    # check tests (that might be run by user) are insensitive to option, #2718
test.data.table()
install.packages("xml2")   # to check the 150 URLs in NEWS.md under --as-cran below
q("no")
R CMD build .
R CMD check data.table_1.12.9.tar.gz --as-cran
R CMD INSTALL data.table_1.12.9.tar.gz --html

# Test C locale doesn't break test suite (#2771)
echo LC_ALL=C > ~/.Renviron
R
Sys.getlocale()=="C"
q("no")
R CMD check data.table_1.12.9.tar.gz
rm ~/.Renviron

# Test non-English does not break test.data.table() due to translation of messages; #3039, #630
LANGUAGE=de R
require(data.table)
test.data.table()
q("no")

R
remove.packages("xml2")    # we checked the URLs; don't need to do it again (many minutes)
require(data.table)
test.data.table(script="other.Rraw")
test.data.table(script="*.Rraw")
test.data.table(verbose=TRUE)   # since main.R no longer tests verbose mode
gctorture2(step=50)
system.time(test.data.table(script="*.Rraw"))  # apx 8h = froll 3h + nafill 1m + main 5h

# Upload to win-builder: release, dev & old-release
# Turn on Travis OSX; it's off in dev until it's added to GLCI (#3326) as it adds 17min after 11min Linux.
# Turn on r-devel in Appveyor; it may be off in dev for similar dev cycle speed reasons


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
R310 CMD INSTALL ./data.table_1.12.9.tar.gz
R310
require(data.table)
test.data.table(script="*.Rraw")


############################################
#  Check compiles ok when OpenMP is disabled
############################################
vi ~/.R/Makevars
# Make line SHLIB_OPENMP_CFLAGS= active to remove -fopenmp
R CMD build .
R CMD INSTALL data.table_1.12.9.tar.gz   # ensure that -fopenmp is missing and there are no warnings
R
require(data.table)   # observe startup message about no OpenMP detected
test.data.table()
q("no")
vi ~/.R/Makevars
# revert change above
R CMD build .
R CMD check data.table_1.12.9.tar.gz


#####################################################
#  R-devel with UBSAN, ASAN, strict-barrier, and noLD
#####################################################

cd ~/build
wget -N https://stat.ethz.ch/R/daily/R-devel.tar.gz
rm -rf R-devel
rm -rf R-devel-strict-*
tar xvf R-devel.tar.gz
mv R-devel R-devel-strict-gcc
tar xvf R-devel.tar.gz
mv R-devel R-devel-strict-clang
tar xvf R-devel.tar.gz

cd R-devel  # used for revdep testing: .dev/revdep.R.
# important to change directory name before building not after because the path is baked into the build, iiuc
./configure CFLAGS="-O2 -Wall -pedantic"
make

# use latest available `apt-cache search gcc-` or `clang-`
cd ~/build/R-devel-strict-clang
./configure --without-recommended-packages --disable-byte-compiled-packages --disable-openmp --enable-strict-barrier --disable-long-double CC="clang-10 -fsanitize=undefined,address -fno-sanitize=float-divide-by-zero -fno-omit-frame-pointer"
make

cd ~/build/R-devel-strict-gcc
# gcc-10 (in dev currently) failed to build R, so using regular gcc-9 (9.3.0 as per focal/Pop!_OS 20.04)
./configure --without-recommended-packages --disable-byte-compiled-packages --disable-openmp --enable-strict-barrier --disable-long-double CC="gcc-9 -fsanitize=undefined,address -fno-sanitize=float-divide-by-zero -fno-omit-frame-pointer"
make

# See R-exts#4.3.3
# CFLAGS an LIBS seem to be ignored now by latest R-devel/gcc, and it works without : CFLAGS="-O0 -g -Wall -pedantic" LIBS="-lpthread"
# Adding --disable-long-double (see R-exts) in the same configure as ASAN/UBSAN used to fail, but now works. So now noLD is included in this strict build.
# Other flags used in the past: CC="gcc -std=gnu99" CFLAGS="-O0 -g -Wall -pedantic -DSWITCH_TO_REFCNT -ffloat-store -fexcess-precision=standard"
# For ubsan, disabled openmp otherwise gcc fails in R's distance.c:256 error: ‘*.Lubsan_data0’ not specified in enclosing parallel
# UBSAN gives direct line number under gcc but not clang it seems. clang-5.0 has been helpful too, though.
# If use later gcc-8, add F77=gfortran-8
# LIBS="-lpthread" otherwise ld error about DSO missing
# -fno-sanitize=float-divide-by-zero, otherwise /0 errors on R's summary.c (tests 648 and 1185.2) but ignore those:
#   https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=16000
# without ubsan, openmp can be on :
# ./configure --without-recommended-packages --disable-byte-compiled-packages --enable-strict-barrier CC="gcc -fsanitize=address -fno-sanitize=float-divide-by-zero -fno-omit-frame-pointer" CFLAGS="-O0 -g -Wall -pedantic -DSWITCH_TO_REFCNT"

# already in ~/.bash_aliases
alias Rdevel-strict-gcc='~/build/R-devel-strict-gcc/bin/R --vanilla'
alias Rdevel-strict-clang='~/build/R-devel-strict-clang/bin/R --vanilla'

cd ~/GitHub/data.table
Rdevel-strict-gcc CMD INSTALL data.table_1.12.9.tar.gz
Rdevel-strict-clang CMD INSTALL data.table_1.12.9.tar.gz
# Check UBSAN and ASAN flags appear in compiler output above. Rdevel was compiled with them so should be passed through to here
Rdevel-strict-gcc
Rdevel-strict-clang  # repeat below with clang and gcc
isTRUE(.Machine$sizeof.longdouble==0)  # check noLD is being tested
options(repos = "http://cloud.r-project.org")
install.packages(c("bit64","xts","nanotime","R.utils","yaml")) # minimum packages needed to not skip any tests in test.data.table()
# install.packages(c("curl","knitr"))                          # for `R CMD check` when not strict. Too slow to install when strict
require(data.table)
test.data.table(script="*.Rraw") # 7 mins (vs 1min normally) under UBSAN, ASAN and --strict-barrier
                                 # without the fix in PR#3515, the --disable-long-double lumped into this build does now work and correctly reproduces the noLD problem
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
Rdevel CMD INSTALL data.table_1.12.9.tar.gz
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
#  rchk : https://github.com/kalibera/rchk
########################################################################
# if rebuilding, ~/build/rchk/scripts/config.inc :
# export WLLVM=/home/mdowle/.local/bin
# export LLVM=/usr/lib/llvm-7
# export RCHK=/home/mdowle/build/rchk
cd ~/build/rchk/trunk
. ../scripts/config.inc
. ../scripts/cmpconfig.inc
vi ~/.R/Makevars   # set CFLAGS=-O0 -g so that rchk can provide source line numbers
echo 'install.packages("~/GitHub/data.table/data.table_1.12.9.tar.gz",repos=NULL)' | ./bin/R --slave
# objcopy warnings (if any) can be ignored: https://github.com/kalibera/rchk/issues/17#issuecomment-497312504
. ../scripts/check_package.sh data.table
cat packages/lib/data.table/libs/*check
# keep running and rerunning locally until all problems cease.
#   rchk has an internal stack which can exhaust. Clearing the current set of problems (e.g. as displayed
#   on CRAN) is not sufficient because new problems can be found because it didn't get that far before.
#   Hence repeat locally until all clear is necessary.


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
sudo aptitude -y build-dep r-cran-rgl   # leads to libglu1-mesa-dev
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
sudo apt-get -y install openjdk-11-jdk   # solves "fatal error: jni.h: No such file or directory"; change 11 to match "java --version"
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
sudo apt-get -y build-dep r-cran-rsymphony   # for Rsymphony: coinor-libcgl-dev coinor-libclp-dev coinor-libcoinutils-dev coinor-libosi-dev coinor-libsymphony-dev
sudo apt-get -y install libtesseract-dev libleptonica-dev tesseract-ocr-eng   # for tesseract
sudo apt-get -y install libssl-dev libsasl2-dev
sudo apt-get -y install biber   # for ctsem
sudo apt-get -y install libopenblas-dev  # for ivmte (+ local R build with default ./configure to pick up shared openblas)
sudo apt-get -y install libhiredis-dev  # for redux used by nodbi
sudo apt-get -y install libzmq3-dev   # for rzmq
sudo apt-get -y install libimage-exiftool-perl   # for camtrapR
sudo apt-get -y install parallel   # for revdepr.R
sudo apt-get -y install pandoc-citeproc   # for basecallQC
sudo R CMD javareconf
# ENDIF

revdepr  # see top of revdep.R for this alias to put in ~/.bash_aliases
inst()   # *** ensure latest dev version of data.table installed into revdeplib ***
run()    # prints menu of options
status() # includes timestamp of installed data.table that is being tested.
log()    # cats all fail logs to ~/fail.log

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
du -k inst/tests                # 1.5MB before
bzip2 inst/tests/*.Rraw         # compress *.Rraw just for release to CRAN; do not commit compressed *.Rraw to git
du -k inst/tests                # 0.75MB after
R CMD build .
R CMD check data.table_1.12.8.tar.gz --as-cran
#
bunzip2 inst/tests/*.Rraw.bz2  # decompress *.Rraw again so as not to commit compressed *.Rraw to git
#
Resubmit to winbuilder (R-release, R-devel and R-oldrelease)
Submit to CRAN. Message template :
------------------------------------------------------------
Hello,
779 CRAN revdeps checked. No status changes.
All R-devel issues resolved.
New gcc10 warnings resolved.
Solaris is not resolved but this release will write more output upon that error so I can trace the problem.
Many thanks!
Best, Matt
------------------------------------------------------------
DO NOT commit or push to GitHub. Leave 4 files (.dev/CRAN_Release.cmd, DESCRIPTION, NEWS and init.c) edited and not committed. Include these in a single and final bump commit below.
DO NOT even use a PR. Because PRs build binaries and we don't want any binary versions of even release numbers available from anywhere other than CRAN.
Leave milestone open with a 'final checks' issue open. Keep updating status there.
** If on EC2, shutdown instance. Otherwise get charged for potentially many days/weeks idle time with no alerts **
If it's evening, SLEEP.
It can take a few days for CRAN's checks to run. If any issues arise, backport locally. Resubmit the same even version to CRAN.
CRAN's first check is automatic and usually received within an hour. WAIT FOR THAT EMAIL.
When CRAN's email contains "Pretest results OK pending a manual inspection" (or similar), or if not and it is known why not and ok, then bump dev.
###### Bump dev
0. Close milestone to prevent new issues being tagged with it. The final 'release checks' issue can be left open in a closed milestone.
1. Check that 'git status' shows 4 files in modified and uncommitted state: DESCRIPTION, NEWS.md, init.c and this .dev/CRAN_Release.cmd
2. Bump version in DESCRIPTION to next odd number. Note that DESCRIPTION was in edited and uncommitted state so even number never appears in git.
3. Add new heading in NEWS for the next dev version. Add "(submitted to CRAN on <today>)" on the released heading.
4. Bump dllVersion() in init.c
5. Bump 3 version numbers in Makefile
6. Search and replace this .dev/CRAN_Release.cmd to update 1.12.7 to 1.12.9, and 1.12.6 to 1.12.8 (e.g. in step 8 and 9 below)
7. Another final gd to view all diffs using meld. (I have `alias gd='git difftool &> /dev/null'` and difftool meld: http://meldmerge.org/)
8. Push to master with this consistent commit message: "1.12.8 on CRAN. Bump to 1.12.9"
9. Take sha from step 8 and run `git tag 1.12.8 34796cd1524828df9bf13a174265cb68a09fcd77` then `git push origin 1.12.8` (not `git push --tags` according to https://stackoverflow.com/a/5195913/403310)
######
