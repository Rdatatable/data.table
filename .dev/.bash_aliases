# Matt's ~/.bash_aliases is a link to this file ~/GitHub/data.table/.dev/.bash_aliases

# One off configure meld as difftool:
# git config --global diff.tool meld
# git config --global difftool.prompt false
alias gd='git difftool &> /dev/null'
alias gdm='git difftool master &> /dev/null'

alias Rdevel='~/build/R-devel/bin/R --vanilla'
alias Rdevel-strict-gcc='~/build/R-devel-strict-gcc/bin/R --vanilla'
alias Rdevel-strict-clang='~/build/R-devel-strict-clang/bin/R --vanilla'
alias Rdevel32='~/build/32bit/R-devel/bin/R --vanilla'
alias R310='~/build/R-3.1.0/bin/R --vanilla'
alias revdepsh='cd ~/build/revdeplib/ && export TZ=UTC && export R_LIBS_SITE=none && export R_LIBS=~/build/revdeplib/ && export _R_CHECK_FORCE_SUGGESTS_=false'
alias revdepr='revdepsh; R_PROFILE_USER=~/GitHub/data.table/.dev/revdep.R ~/build/R-devel/bin/R'

export R_PROFILE_USER='~/.Rprofile'  # ignore the .Rprofile now in ~/GitHub/data.table/ 
