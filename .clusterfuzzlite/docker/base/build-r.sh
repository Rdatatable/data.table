#!/bin/bash -eu
#
# Build an instrumented R into /opt/r-prebuilt inside the base image.
# Adapted from https://github.com/r-devel/r-oss-fuzz/blob/main/docker/base/build-r.sh.

FLAGS_VAR="SANITIZER_FLAGS_${SANITIZER}"
SANITIZER_FLAGS="${!FLAGS_VAR-}"
if [ -z "$SANITIZER_FLAGS" ]; then
    echo "build-r.sh: no flags known for SANITIZER=$SANITIZER" >&2
    exit 1
fi

export CFLAGS="$CFLAGS $SANITIZER_FLAGS $COVERAGE_FLAGS"
export CXXFLAGS="$CXXFLAGS $SANITIZER_FLAGS $COVERAGE_FLAGS"

export R_BUILD_ONLY=1
export R_PREFIX="${R_PREFIX:-/opt/r-prebuilt}"

/opt/ossfuzz.sh

cat > "$R_PREFIX/.fuzz-build-info" <<EOF
sanitizer=$SANITIZER
engine=$FUZZING_ENGINE
EOF
