# Matt's ~/.bash_aliases is a link to this file ~/GitHub/data.table/.dev/.bash_aliases

# One off configure meld as difftool:
# git config --global diff.tool meld
# git config --global difftool.prompt false
alias gd='git difftool &> /dev/null'
alias gdm='git difftool master &> /dev/null'
# If meld has scrolling issues, turn off GTK animation which I don't need:
#   https://gitlab.gnome.org/GNOME/meld/-/issues/479#note_866040

alias perfbar=~/build/gtk_perfbar/linux_perfbar   # revdep.R; https://github.com/tomkraljevic/gtk_perfbar

alias Rdevel='~/build/R-devel/bin/R --vanilla'
alias Rdevel-strict-gcc='~/build/R-devel-strict-gcc/bin/R --vanilla'
alias Rdevel-strict-clang='~/build/R-devel-strict-clang/bin/R --vanilla'
alias Rdevel-valgrind='~/build/R-devel-valgrind/bin/R --vanilla'
alias Rdevel32='~/build/32bit/R-devel/bin/R --vanilla'
alias R310='~/build/R-3.1.0/bin/R --vanilla'

alias revdepsh='cd ~/build/revdeplib/ && export TZ=UTC && export R_LIBS_SITE=NULL && export R_LIBS=~/build/revdeplib/ && export _R_CHECK_FORCE_SUGGESTS_=true'
alias revdepr='revdepsh; R_PROFILE_USER=~/GitHub/data.table/.dev/revdep.R R'
# use ~/build/R-devel/bin/R at the end of revdepr to use R-devel instead of R-release.
# If so, doing a `rm -rf *` in revdeplib first to rebuild everything is easiest way to avoid potential problems later. A full rebuild is a good idea periodically anyway. Packages in
# revdeplib may have been compiled many months ago, but the .so libraries they link to may have been updated in the meantime, or multiple packages may use the same .so libary, or
# switches inside the package's code may behave differently when R-devel is used instead of R-release, etc. I use R-release for revdepr, unless R-devel contains significant changes
# that we really need to test revdeps under.

export R_PROFILE_USER='~/.Rprofile'
# there's a .Rprofile in ~/GitHub/data.table/ so Matt sets R_PROFILE_USER here to always use ~/.Rprofile
# even when starting R in ~/GitHub/data.table
# Matt's ~/.Rprofile as a link to ~/GitHub/data.table/.dev/.Rprofile

export R_DEFAULT_INTERNET_TIMEOUT=3600
# increase from R's default 60, always not just in revdep testing, to help --as-cran

export TEST_DATA_TABLE_WITH_OTHER_PACKAGES=true
# R CMD check in dev should always run other.Rraw

