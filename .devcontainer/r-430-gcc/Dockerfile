FROM registry.gitlab.com/rdatatable/dockerfiles/r-4.3.0

RUN apt-get -qq update \
  && apt-get install -y --no-install-recommends git

COPY DESCRIPTION .

RUN Rscript -e '                                          \ 
read.dcf("DESCRIPTION", c("Imports", "Suggests")) |>      \
  tools:::.split_dependencies() |>                        \
  names() |>                                              \
  setdiff(tools:::.get_standard_package_names()$base) |>  \
  install.packages(repos="https://cloud.r-project.org")   \
'

# setup cc()
WORKDIR /root
COPY .devcontainer/.Rprofile .
