#!/bin/bash -eu
#
# ClusterFuzzLite build entrypoint for data.table.
#
# R is pre-compiled in the base image at /opt/r-prebuilt (see
# .clusterfuzzlite/docker/base/), so this step only verifies that the base
# image matches the requested sanitizer/engine pair and delegates to
# .clusterfuzzlite/ossfuzz.sh to build data.table and the fuzz harnesses.

R_PREBUILT="${R_PREBUILT:-/opt/r-prebuilt}"
INFO="$R_PREBUILT/.fuzz-build-info"

if [ ! -f "$INFO" ]; then
    echo "build.sh: $INFO is missing -- the base image is malformed or" >&2
    echo "          predates the build stamp. Rebuild it." >&2
    exit 1
fi

built_sanitizer=$(sed -n 's/^sanitizer=//p' "$INFO")
built_engine=$(sed -n 's/^engine=//p' "$INFO")

if [ "$built_sanitizer" != "$SANITIZER" ] || [ "$built_engine" != "$FUZZING_ENGINE" ]; then
    echo "build.sh: base image contains R built for" >&2
    echo "            sanitizer=$built_sanitizer engine=$built_engine" >&2
    echo "          but this build requests" >&2
    echo "            sanitizer=$SANITIZER engine=$FUZZING_ENGINE" >&2
    echo "          Rebuild the base image for that combination first." >&2
    exit 1
fi

export DEFERRED_TARGETS=""
export R_PREBUILT
exec "$SRC/data.table/.clusterfuzzlite/ossfuzz.sh"
