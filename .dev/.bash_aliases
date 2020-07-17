# Matt's ~/.bash_aliases is a link to this file ~/GitHub/data.table/.dev/.bash_aliases

# One off configure meld as difftool:
# git config --global diff.tool meld
# git config --global difftool.prompt false
alias gd='git difftool &> /dev/null'
alias gdm='git difftool master &> /dev/null'
# If meld has scrolling issues, turn off GTK animation (which I don't need anyway):
#   https://gitlab.gnome.org/GNOME/meld/-/issues/479#note_866040

alias Rdevel='~/build/R-devel/bin/R --vanilla'
alias Rdevel-strict-gcc='~/build/R-devel-strict-gcc/bin/R --vanilla'
alias Rdevel-strict-clang='~/build/R-devel-strict-clang/bin/R --vanilla'
alias Rdevel32='~/build/32bit/R-devel/bin/R --vanilla'
alias R310='~/build/R-3.1.0/bin/R --vanilla'
alias revdepsh='cd ~/build/revdeplib/ && export TZ=UTC && export R_LIBS_SITE=none && export R_LIBS=~/build/revdeplib/ && export _R_CHECK_FORCE_SUGGESTS_=false'
alias revdepr='revdepsh; R_PROFILE_USER=~/GitHub/data.table/.dev/revdep.R ~/build/R-devel/bin/R'

export R_PROFILE_USER='~/.Rprofile'
# there's a .Rprofile in ~/GitHub/data.table/ so Matt sets R_PROFILE_USER here to always use ~/.Rprofile
# even when starting R in ~/GitHub/data.table
# Matt's ~/.Rprofile as a link to ~/GitHub/data.table/.dev/.Rprofile

