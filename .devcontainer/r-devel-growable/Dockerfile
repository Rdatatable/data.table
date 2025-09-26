FROM registry.gitlab.com/rdatatable/dockerfiles/r-devel-growable

COPY DESCRIPTION .
RUN /usr/local/bin/Rscript -e '                               \
read.dcf("DESCRIPTION", c("Imports", "Suggests")) |>         \
  tools:::.split_dependencies() |>                           \
  names() |>                                                 \
  setdiff(tools:::.get_standard_package_names()$base) |>     \
  install.packages()                                         \
'

WORKDIR /root
COPY .devcontainer/.Rprofile .