#!/bin/bash -eu
#
# Check out R trunk into $SRC/r-source with retry logic for transient SVN drops.
# Adapted from https://github.com/r-devel/r-oss-fuzz/blob/main/docker/base/checkout-r.sh.

URL=https://svn.r-project.org/R/trunk
DEST="$SRC/r-source"
ATTEMPTS=5
DELAY=30

attempt() {
    local n
    for n in $(seq 1 "$ATTEMPTS"); do
        if "$@"; then
            return 0
        fi
        echo "checkout-r.sh: '$*' failed (attempt $n of $ATTEMPTS)" >&2
        if [ "$n" -lt "$ATTEMPTS" ]; then
            sleep "$DELAY"
        fi
    done
    return 1
}

resume_checkout() {
    if [ -d "$DEST/.svn" ]; then
        svn cleanup "$DEST"
    fi
    svn checkout --depth=infinity -r "$REV" "$URL" "$DEST"
}

REV=$(attempt svn info --show-item revision "$URL") || exit 1
echo "checkout-r.sh: checking out $URL at r$REV"

attempt resume_checkout || exit 1

svnversion "$DEST" > /opt/r-svn-revision
