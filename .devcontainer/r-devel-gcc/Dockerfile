FROM registry.gitlab.com/jangorecki/dockerfiles/r-devel-gcc

RUN apt-get -qq update \
  && apt-get install -y --no-install-recommends git

# install dependencies without ubsan flags
RUN Rscript -e 'install.packages(c("bit64", "bit", "R.utils", "xts", "zoo", "yaml", "knitr", "markdown"))'

# setup cc()
WORKDIR /workspaces/data.table

RUN ln -s /workspaces ~/GitHub
RUN echo 'Sys.setenv(PROJ_PATH="/workspaces/data.table")\nsource(".dev/cc.R")' >> /workspaces/data.table/.Rprofile
