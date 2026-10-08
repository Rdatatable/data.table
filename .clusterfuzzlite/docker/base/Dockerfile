# Base image for the ClusterFuzzLite setup: an OSS-Fuzz builder image with an
# instrumented R already compiled into /opt/r-prebuilt.
#
# Adapted from https://github.com/r-devel/r-oss-fuzz/blob/main/docker/base/Dockerfile.
#
# By default, .clusterfuzzlite/Dockerfile pulls ghcr.io/r-devel/r-oss-fuzz/r-base:address
# directly so data.table's CI runs do not need to maintain a separate R trunk build.
# This Dockerfile is provided so Rdatatable/data.table (or a fork) can also build and
# publish its own ghcr.io/<owner>/data.table/r-base:address image via
# .github/workflows/base-image.yml if desired:
#
#   docker build -f .clusterfuzzlite/docker/base/Dockerfile \
#     -t ghcr.io/rdatatable/data.table/r-base:address .

FROM gcr.io/oss-fuzz-base/base-builder

RUN apt-get update && apt-get install -y --no-install-recommends \
        gfortran \
        libbz2-dev \
        libcurl4-openssl-dev \
        liblzma-dev \
        libpcre2-dev \
        libreadline-dev \
        pkg-config \
        subversion \
        texinfo \
        zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

COPY .clusterfuzzlite/docker/base/checkout-r.sh /opt/checkout-r.sh
RUN /opt/checkout-r.sh

COPY .clusterfuzzlite/ossfuzz.sh /opt/ossfuzz.sh
COPY .clusterfuzzlite/docker/base/build-r.sh /opt/build-r.sh

RUN /opt/build-r.sh && rm -rf $SRC/r-source/.svn
