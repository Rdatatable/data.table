FROM rocker/r-devel-ubsan-clang:latest

RUN apt-get -qq update \
  && apt-get install -y --no-install-recommends git

COPY DESCRIPTION .

# install dependencies without ubsan flags
RUN Rscript -e '                                          \ 
read.dcf("DESCRIPTION", c("Imports", "Suggests")) |>      \
  tools:::.split_dependencies() |>                        \
  names() |>                                              \
  setdiff(tools:::.get_standard_package_names()$base) |>  \
  install.packages()                                      \
'

# setup cc()
WORKDIR /root
COPY .devcontainer/.Rprofile .

# set ubsan flags
WORKDIR .R
COPY .devcontainer/r-devel-clang-ubsan/Makevars .
