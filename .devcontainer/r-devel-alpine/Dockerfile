FROM docker.io/rhub/r-minimal:devel

RUN apk update \
  && apk add --no-cache \
     gcc git musl-dev openmp pkgconf zlib-dev \
  && echo 'options("repos"="https://cloud.r-project.org")' >> /usr/local/lib/R/etc/Rprofile.site

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
