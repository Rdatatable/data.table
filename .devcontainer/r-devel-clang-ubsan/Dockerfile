FROM registry.gitlab.com/jangorecki/dockerfiles/r-devel-clang

RUN apt-get -qq update \
  && apt-get install -y --no-install-recommends git

# install dependencies without ubsan flags
RUN Rscript -e 'install.packages(c("bit64", "bit", "R.utils", "xts", "zoo", "yaml", "knitr", "markdown"))'

# setup cc()
WORKDIR /workspaces/data.table

RUN ln -s /workspaces ~/GitHub
RUN echo 'Sys.setenv(PROJ_PATH="/workspaces/data.table")\nsource(".dev/cc.R")' >> /workspaces/data.table/.Rprofile

# set ubsan flags
RUN mkdir -p ~/.R \
  && echo 'CC=clang -fsanitize=undefined -fno-omit-frame-pointer' > ~/.R/Makevars \
  && echo 'CXX=clang++ -stdlib=libc++ -fsanitize=undefined -fno-omit-frame-pointer' >> ~/.R/Makevars \
  && echo 'LDFLAGS=-L/usr/local/clang/lib64 -L/usr/local/lib64 -lubsan' >> ~/.R/Makevars
