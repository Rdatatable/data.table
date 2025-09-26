FROM registry.gitlab.com/rdatatable/dockerfiles/r-devel-gcc

RUN apt-get -qq update \
  && apt-get install -y --no-install-recommends \
    git \
    patch \
    subversion \
    rsync \
    texinfo \
    texlive-latex-base \
    texlive-latex-recommended \
    texlive-fonts-recommended \
  && rm -rf /var/lib/apt/lists/*

COPY .devcontainer/r-devel-growable/growable-api.patch /opt/growable-api.patch

WORKDIR /opt/R-source
RUN svn checkout https://svn.r-project.org/R/trunk R-devel && \
    cd R-devel && \
    patch -p0 < /opt/growable-api.patch

# Build patched R
WORKDIR /opt/R-source/R-devel
RUN ./configure \
    --prefix=/usr/local \
    --enable-R-shlib \
    --enable-memory-profiling \
    --with-blas \
    --with-lapack \
    --with-readline \
    --with-cairo \
    --with-libpng \
    --with-jpeglib \
    --with-libtiff \
    --disable-nls && \
    make -j$(nproc) && \
    make install

RUN echo 'Testing growable vectors...' && \
    /usr/local/bin/R --slave -e '.Internal(growvec_alloc(13L, 0, 10))'

ENV PATH=/usr/local/bin:$PATH
ENV R_HOME=/usr/local/lib/R

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