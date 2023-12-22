FROM registry.gitlab.com/jangorecki/dockerfiles/r-devel-clang

RUN apt-get -qq update \
  && apt-get install -y --no-install-recommends git

# install dependencies without ubsan flags
RUN Rscript -e '                                          \ 
read.dcf("DESCRIPTION", c("Imports", "Suggests")) |>      \
  tools:::.split_dependencies() |>                        \
  names() |>                                              \
  setdiff(tools:::.get_standard_package_names()$base) |>  \
  install.packages()                                      \
'

# setup cc()
WORKDIR /workspaces/data.table

RUN ln -s /workspaces ~/GitHub
COPY .devcontainer/r-devel-clang-ubsan/.Rprofile /workspaces/data.table

# set ubsan flags
RUN mkdir -p ~/.R
COPY .devcontainer/r-devel-clang-ubsan/Makevars ~/.R
