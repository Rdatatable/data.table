FROM registry.gitlab.com/jangorecki/dockerfiles/r-3.3.0

RUN apt-get -qq update \
  && apt-get install -y --no-install-recommends git

COPY DESCRIPTION .

RUN Rscript -e '                                        \ 
dcf = read.dcf("DESCRIPTION", c("Imports", "Suggests")) \
deps = names(tools:::.split_dependencies(dcf))          \
standard_pkgs = tools:::.get_standard_package_names()   \
install.packages(setdiff(deps, standard_pkgs$base))     \
'

# setup cc()
WORKDIR /root
COPY .devcontainer/.Rprofile .
