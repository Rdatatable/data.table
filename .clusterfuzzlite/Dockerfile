# ClusterFuzzLite build image for data.table.
#
# Uses the prebuilt instrumented R base image (built from R trunk with
# AddressSanitizer, libFuzzer coverage instrumentation, and
# --enable-strict-barrier; see .clusterfuzzlite/docker/base/).
# This layer only installs data.table and compiles the C harnesses, so CI
# builds complete in ~1-2 minutes instead of ~45 minutes.

ARG BASE_IMAGE=ghcr.io/r-devel/r-oss-fuzz/r-base:address
FROM ${BASE_IMAGE}

RUN apt-get update && apt-get install -y --no-install-recommends \
        pkg-config \
        zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

COPY . $SRC/data.table
COPY .clusterfuzzlite/build.sh $SRC/build.sh
WORKDIR $SRC/data.table
