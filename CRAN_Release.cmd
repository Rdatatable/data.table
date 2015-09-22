###############################################
#  Basic checks
###############################################

sudo apt-get update
sudo apt-get upgrade
sudo apt-get install pandoc  # but use .deb from pandoc homepage (v1.15) otherwise 'no header_extensions' with v1.12 on Ubuntu repo as of Sep-2015
R
update.packages()
q()

# Ensure no non-ASCII, other than in README.md is ok
# tests.Rraw in particular have failed CRAN Solaris (only) due to this.
# No unicode either. Put these tests in DtNonAsciiTests package.
grep -RI --exclude-dir=".git" --exclude="*.md" --exclude="*~" --color='auto' -P -n "[\x80-\xFF]" data.table/
grep -RI --exclude-dir=".git" --exclude="*.md" --exclude="*~" --color='auto' -n "[\]u[0-9]" data.table/

# workaround for IBM AIX - ensure no globals named 'nearest' or 'class'. See https://github.com/Rdatatable/data.table/issues/1351
grep "nearest *=" data.table/src/*.c  # none
grep "class *=" data.table/src/*.c    # quite a few but none global

R CMD build data.table
R CMD check --as-cran data.table_1.9.7.tar.gz

# Upload to win-builder, both release and dev


###############################################
#  R 2.14.1 (stated dependency)
###############################################

### IF NOT ALREADY DONE
sudo apt-get -y build-dep r-base
cd ~/build
wget http://cran.stat.ucla.edu/src/base/R-2/R-2.14.1.tar.gz
tar xvf R-2.14.1.tar.gz
cd R-2.14.1
./configure --without-recommended-packages
make
alias R2141=~/build/R-2.14.1/bin/R
cd ..
R2141
install.packages("chron")
q()
### ENDIF

R2141 CMD INSTALL data.table_1.9.5.tar.gz
R2141
require(data.table)
test.data.table()
test.data.table(verbose=TRUE)


###############################################
#  valgrind
###############################################

R --vanilla CMD INSTALL data.table_1.9.5.tar.gz     # ensure compiles with -g here for info on source lines with any problems
R -d "valgrind --tool=memcheck --leak-check=full" --vanilla
require(data.table)
require(bit64)
test.data.table()
test.data.table(verbose=TRUE)

# Tests 648 and 1262 have single precision issues under valgrind. See comments next to those tests.
# Ignore 'set address range perms' warnings :
#   http://stackoverflow.com/questions/13558067/what-does-this-valgrind-warning-mean-warning-set-address-range-perms


###############################################
#  UBSAN and ASAN
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
alias Rdevel=~/build/R-devel/bin/R
Rdevel
install.packages("chron")   # data.table imports (and thus depends on) this one recommended package (we excluded recommended above for speed of building)
install.packages("bit64")   # good to UBSAN/ASAN check with bit64 as well.
                            # Skip tests using Suggests packages (CRAN will do that) - unlikely to be UBSAN/ASAN problems there.
q("no")
Rdevel CMD INSTALL ../data.table_1.9.5.tar.gz   # Check UBSAN and ASAN flags appear in compiler output here (Rdevel was compiled with them above so should be passed through to here)
Rdevel --vanilla
require(data.table)
require(bit64)
test.data.table()
# Throws /0 errors on R's summary.c (tests 648 and 1185.2) but ignore those: https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=16000
test.data.table(verbose=TRUE)
q()


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
install.packages("chron")
install.packages("bit64")
q("no")
Rdevel CMD INSTALL ../data.table_1.9.5.tar.gz
Rdevel --vanilla
.Machine$sizeof.longdouble   # check 0
require(data.table)
require(bit64)
test.data.table()
test.data.table(verbose=TRUE)
q()


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
sudo apt-get -y install texlive texlive-latex-extra texlive-bibtex-extra texinfo fonts-inconsolata latex-xcolor
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
sudo R CMD javareconf
# ENDIF

cd ~/build
# if on EC2 spot
git clone https://github.com/Rdatatable/data.table.git
# end if

# do not build, install, require() or library() data.table. revdep_check will read ~/build/data.table and install from there
# NB: repos <- c(CRAN="http://cran.rstudio.com/") is hard-coded into devtools::check_cran
# NB: revdep_check moves the tar.gz of directly dependent packages into revdeplib afterwards. The packages those packages depened on are installed in revdeplib without their tar.gz

export R_LIBS=~/build/revdeplib/
R
require(devtools) 
options(devtools.revdep.libpath = "~/build/revdeplib/")
  # essential as the first-time download can be several hours, mainly BSgenome with its 10 * 500MB+ files.
  # revdep_check(..., ignore="BSgenome") is ignored and BSgenome still downloads, I guess only direct dependencies can be ignored
options(Ncpus = 6)
res <- revdep_check("data.table", bioconductor=TRUE)
revdep_check_save_summary(res)
revdep_check_save_logs(res)

# wait for it to finish before fixing; any R CMD INSTALL may affect running tests which run under a new R --slave for each one

# CAGEr & GGtools fail due to not finding KernSmooth. It's so there in /usr/lib/R/library/ alongside other recommended packages that it's not funny. Tried installing KernSmooth in "~/build/revdeplib/" as well and ensured R_LIBS set and present in .libPaths() and they still complain can't find KernSmooth. Searched online and found others with similar problems with KernSmooth too but no clear solution.
# Similarly, pwOmics fails due to not finding RUnit but that too is so installed it's not funny
# Similarly, VanillaICE fails fue to not finding BSgenome.Hsapiens.UCSC.hg18, which again is so there
# Running these 4 manually outside of devtools::revdep_check() pass fine ...
cd ~/build
export R_LIBS=~/build/revdeplib/
R CMD check revdeplib/CAGEr_1.10.0.tar.gz
R CMD check revdeplib/GGtools_5.4.0.tar.gz
R CMD check revdeplib/pwOmics_1.1.8.tar.gz
R CMD check revdeplib/VanillaICE_1.30.1.tar.gz

# Now tackle the real fails ...
cd ~/build
export R_LIBS=~/build/revdeplib/
R CMD check revdeplib/bedr_1.0.1.tar.gz
R CMD INSTALL data.table_1.9.4.tar.gz    # revert to previous version
R CMD check revdeplib/bedr_1.0.1.tar.gz  # if still fails then it isn't data.table causing fail. Check for missing Ubuntu packages and similar. 
# fix data.table
R CMD build data.table
R CMD INSTALL data.table_1.9.5.tar.gz
R CMD check revdeplib/bedr_1.0.1.tar.gz
# iterate

repeat revdep_check() until passes cleanly

# Check for warnings
cd /tmp/RtmpNRWfYa/check_crana4e6458d6c8/
find . -name "00check.log" -exec grep -iH warning {} \;
find . -name "00check.log" -exec grep -iH "replacing previous import" {} \;


###############################################
#  Release to CRAN
###############################################

Bump versions in README and DESCRIPTION to even release number
Do not push to GitHub. Prevents even a slim possibility of user getting premature version. install_github() should only ever fetch odd releases at all times. Even release numbers must have been obtained from CRAN and only CRAN. (Too many support problems in past before this procedure brought in.)
R CMD build data.table
R CMD check --as-cran data.table_1.9.6.tar.gz
Resubmit to winbuilder (both R-release and R-devel)
Submit to CRAN
Bump version in DESCRIPTION to next odd dev version
Add new heading in README for the next dev version
Push to GitHub so dev can continue
Cross fingers accepted first time. If not, push changes to devel and backport locally
Close milestone
** If on EC2, shutdown instance. Otherwise get charged for potentially many days/weeks idle time with no alerts **

Submit message template:
"
1. I've seen today's change in R-devel that has just started to cause data.table to fail on CRAN R-devel on Windows. This update will pass.
2. 3 out of 131 dependents will fail: bedr, repra and GenomicInteractions. Maintainers notified and are ok to submit revision.
3. Passes winbuilder both r-release and r-devel.
4. Passes emulated big endian using QEMU so should pass Solaris Sparc.
5. Checked on 64-bit Ubuntu with UBSAN, ASAN and valgrind.
6. My email address has changed.
Many thanks!
"

