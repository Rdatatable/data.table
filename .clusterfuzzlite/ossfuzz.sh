#!/bin/bash -eu
#
# OSS-Fuzz / ClusterFuzzLite build script for data.table fuzz targets.
#
# Modeled directly on https://github.com/r-devel/r-oss-fuzz/blob/main/ossfuzz.sh.
#
# Expected environment (provided by the OSS-Fuzz base-builder image):
#   $CC, $CXX, $CFLAGS, $CXXFLAGS   compiler + sanitizer + coverage flags
#   $LIB_FUZZING_ENGINE             fuzzing engine library to link against
#   $SRC, $WORK, $OUT               source / scratch / output directories
#
# Optional knobs:
#   R_PREBUILT=<dir>   use an already-installed R at <dir>, skip building R
#   R_BUILD_ONLY=1     build and install R into $R_PREFIX, then exit
#   R_SOURCE=<dir>     R trunk checkout when building R from source

FUZZ_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$FUZZ_DIR/.." && pwd)"
R_SOURCE="${R_SOURCE:-${SRC:-/src}/r-source}"
LIBDEFLATE_VERSION="${LIBDEFLATE_VERSION:-1.26}"
R_PREFIX="${R_PREFIX:-$WORK/r-install}"

########################################################################
# 1. Build and install R (or locate prebuilt R)
########################################################################
if [ -n "${R_PREBUILT:-}" ]; then
    echo "ossfuzz.sh: using prebuilt R at $R_PREBUILT"
    R_PREFIX="$R_PREBUILT"
    if [ ! -x "$R_PREFIX/bin/Rscript" ]; then
        echo "ossfuzz.sh: no R install found at $R_PREFIX" >&2
        exit 1
    fi
else
    cd "$R_SOURCE"

    LIBDEFLATE_PREFIX="$WORK/libdeflate"
    if [ ! -f "$LIBDEFLATE_PREFIX/lib/libdeflate.a" ]; then
        curl -fL --retry 3 --retry-delay 5 \
            "https://github.com/ebiggers/libdeflate/archive/refs/tags/v${LIBDEFLATE_VERSION}.tar.gz" \
            -o "$WORK/libdeflate.tar.gz"
        rm -rf "$WORK/libdeflate-src"
        mkdir -p "$WORK/libdeflate-src"
        tar -xzf "$WORK/libdeflate.tar.gz" --strip-components=1 -C "$WORK/libdeflate-src"
        cmake -S "$WORK/libdeflate-src" -B "$WORK/libdeflate-build" \
            -DCMAKE_INSTALL_PREFIX="$LIBDEFLATE_PREFIX" \
            -DCMAKE_INSTALL_LIBDIR=lib \
            -DCMAKE_C_COMPILER="$CC" \
            -DCMAKE_C_FLAGS="$CFLAGS -fno-omit-frame-pointer -fPIC" \
            -DLIBDEFLATE_BUILD_SHARED_LIB=OFF \
            -DLIBDEFLATE_BUILD_GZIP=OFF \
            -DLIBDEFLATE_BUILD_TESTS=OFF
        cmake --build "$WORK/libdeflate-build" -j"$(nproc)"
        cmake --install "$WORK/libdeflate-build"
        rm -rf "$WORK/libdeflate-src" "$WORK/libdeflate-build" "$WORK/libdeflate.tar.gz"
    fi

    ./configure \
        CC="$CC" \
        CXX="$CXX" \
        CFLAGS="$CFLAGS -fno-omit-frame-pointer" \
        CXXFLAGS="$CXXFLAGS -fno-omit-frame-pointer" \
        CPPFLAGS="-I$LIBDEFLATE_PREFIX/include" \
        FFLAGS="" \
        FCFLAGS="" \
        LDFLAGS="$CFLAGS -lgfortran -L$LIBDEFLATE_PREFIX/lib" \
        --prefix="$R_PREFIX" \
        --enable-R-shlib \
        --with-x=no \
        --disable-java \
        --enable-strict-barrier \
        --without-recommended-packages

    make -j"$(nproc)"
    make install
fi

R_HOME="$R_PREFIX/lib/R"
R_INCLUDE="$R_HOME/include"
R_LIB_DIR="$R_HOME/lib"

# Copy Fortran and OpenMP runtime libraries into $R_LIB_DIR so the minimal
# base-runner image can resolve them via DT_RPATH alongside libR.so.
for lib in libgfortran.so.5 libquadmath.so.0 libomp.so libomp.so.5 libgomp.so.1; do
    src=$(find /usr /lib /lib64 -name "$lib" \( -type l -o -type f \) 2>/dev/null | head -1 || true)
    if [ -n "$src" ]; then
        cp -f "$(readlink -f "$src")" "$R_LIB_DIR/$lib"
    fi
done

if [ -n "${R_BUILD_ONLY:-}" ]; then
    echo "ossfuzz.sh: R_BUILD_ONLY set -- R installed at $R_PREFIX, stopping"
    exit 0
fi

########################################################################
# 2. Build and install instrumented data.table into $OUT/r-install
########################################################################
# Stage a copy of $R_PREFIX into $OUT/r-install first and install data.table
# there so $R_PREBUILT stays untouched across builds.
rm -rf "$OUT/r-install"
cp -a "$R_PREFIX" "$OUT/r-install"

OUT_R_PREFIX="$OUT/r-install"
R_HOME="$OUT_R_PREFIX/lib/R"
R_INCLUDE="$R_HOME/include"
R_LIB_DIR="$R_HOME/lib"

export R_HOME
export LD_LIBRARY_PATH="$R_LIB_DIR:${LD_LIBRARY_PATH:-}"

mkdir -p "$WORK"
MAKEVARS_FUZZ="$WORK/Makevars.fuzz"
cat > "$MAKEVARS_FUZZ" <<EOF
CC = $CC
CFLAGS = $CFLAGS -fno-omit-frame-pointer -O2 -g
SHLIB_LDFLAGS = -shared $CFLAGS
EOF

echo "ossfuzz.sh: installing instrumented data.table from $REPO_ROOT"
R_MAKEVARS_USER="$MAKEVARS_FUZZ" \
    "$OUT_R_PREFIX/bin/R" CMD INSTALL \
    --no-lock --no-test-load --no-byte-compile --no-help --no-docs \
    -l "$R_HOME/library" "$REPO_ROOT"

# Keep the git working tree clean after R CMD INSTALL.
rm -f "$REPO_ROOT"/src/*.o "$REPO_ROOT"/src/*.so "$REPO_ROOT"/src/Makevars

# If data.table.so linked against an OpenMP runtime not yet in $R_LIB_DIR,
# copy its resolved shared object into $R_LIB_DIR so base-runner finds it.
DT_SO="$R_HOME/library/data.table/libs/data.table.so"
if [ -f "$DT_SO" ]; then
    ldd "$DT_SO" | awk '/=> \// {print $1, $3}' | while read -r soname fullpath; do
        case "$soname" in
            libomp*|libgomp*)
                if [ -f "$fullpath" ] && [ ! -f "$R_LIB_DIR/$soname" ]; then
                    cp -f "$(readlink -f "$fullpath")" "$R_LIB_DIR/$soname"
                fi
                ;;
        esac
    done
fi

########################################################################
# 3. Stage seed corpora
########################################################################
SEED_STAGE="$WORK/seeds"
rm -rf "$SEED_STAGE"
mkdir -p "$SEED_STAGE"
if [ -d "$FUZZ_DIR/seeds" ]; then
    cp -a "$FUZZ_DIR/seeds/." "$SEED_STAGE/"
fi

mkdir -p \
    "$SEED_STAGE/fread" \
    "$SEED_STAGE/fread_file" \
    "$SEED_STAGE/fwrite" \
    "$SEED_STAGE/forder" \
    "$SEED_STAGE/bmerge" \
    "$SEED_STAGE/reshape" \
    "$SEED_STAGE/froll"

# Stage small CSV / delimited test files from inst/tests/ into fread and
# fread_file seed corpora, prefixing a selector byte so data + 1 holds the
# complete test file.
if [ -d "$REPO_ROOT/inst/tests" ]; then
    idx=0
    for f in "$REPO_ROOT"/inst/tests/*.csv "$REPO_ROOT"/inst/tests/*.txt; do
        [ -f "$f" ] || continue
        sz=$(wc -c < "$f")
        if [ "$sz" -gt 0 ] && [ "$sz" -le 16384 ]; then
            base=$(basename "$f")
            slot_char=$(printf "\\$(printf '%03o' $((idx % 20)))")
            { printf "%b" "$slot_char"; cat "$f"; } > "$SEED_STAGE/fread/test_${idx}_${base}"
            { printf "%b" "$slot_char"; cat "$f"; } > "$SEED_STAGE/fread_file/test_${idx}_${base}"
            idx=$((idx + 1))
        fi
    done
fi

# Generate structured seed inputs using the freshly built R + data.table.
"$OUT_R_PREFIX/bin/Rscript" --vanilla -e '
  write_seed <- function(dir, name, slot, payload) {
    raw_payload <- if (is.raw(payload)) payload else charToRaw(paste(payload, collapse = "\n"))
    writeBin(c(as.raw(slot), raw_payload), file.path(dir, name))
  }

  stage <- Sys.getenv("SEED_STAGE")

  # fread seeds across slots
  write_seed(file.path(stage, "fread"), "basic_csv.bin", 0L,
             c("a,b,c", "1,2.5,hello", "3,-4.0,\"world,x\"", "NA,Inf,"))
  write_seed(file.path(stage, "fread"), "tsv.bin", 2L,
             c("x\ty\tz", "10\t20\t30", "-1\t0\t999999999999"))
  write_seed(file.path(stage, "fread"), "eu_dec.bin", 8L,
             c("a;b", "1,25;3,50", "-0,75;NA"))
  write_seed(file.path(stage, "fread"), "logical_zeros.bin", 13L,
             c("l1,l2,z", "0,Y,0012", "1,N,0000", "NA,Y,099"))

  # fread_file seeds with BOM and embedded NUL bytes
  write_seed(file.path(stage, "fread_file"), "utf8_bom.bin", 0L,
             c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw("a,b\n1,2\n3,4\n")))
  write_seed(file.path(stage, "fread_file"), "embedded_nul.bin", 1L,
             c(charToRaw("a,b\n1,x"), as.raw(0L), charToRaw("y\n2,z\n")))

  # Line-based seeds for fwrite, forder, bmerge, reshape, froll
  line_samples <- list(
    numeric = c("1", "-1", "0", "-0.0", "3.14159", "NA", "NaN", "Inf", "-Inf",
                "1e-300", "1e308", "2147483647", "-2147483647", "42"),
    strings = c("alpha", "beta", "alpha", "", "NA", "hello,world", "\"quoted\"",
                "123", "-45.6", "\xc3\xa9", "\xe2\x82\xac", "omega"),
    mixed   = c("10", "20", "10", "5", "-3", "NA", "0", "100", "50", "20")
  )
  for (target in c("fwrite", "forder", "bmerge", "reshape", "froll")) {
    tdir <- file.path(stage, target)
    for (s in seq_along(line_samples)) {
      nm <- names(line_samples)[s]
      for (slot in 0:11) {
        write_seed(tdir, sprintf("%s_slot%02d.bin", nm, slot), slot, line_samples[[s]])
      }
    }
  }
' SEED_STAGE="$SEED_STAGE"

########################################################################
# 4. Compile, link, and package each fuzz target
########################################################################
DEFERRED_TARGETS="${DEFERRED_TARGETS:-}"

for src in "$FUZZ_DIR"/harnesses/*.c; do
    name=$(basename "$src" .c)

    case " $DEFERRED_TARGETS " in
        *" $name "*)
            echo "ossfuzz.sh: target $name: deferred -- skipping"
            continue
            ;;
    esac

    echo "ossfuzz.sh: building target $name"
    $CC $CFLAGS -fno-omit-frame-pointer \
        -I"$R_INCLUDE" \
        -c "$src" -o "$WORK/${name}.o"

    $CXX $CXXFLAGS -fno-omit-frame-pointer \
        "$WORK/${name}.o" \
        -o "$OUT/$name" \
        $LIB_FUZZING_ENGINE \
        -L"$R_LIB_DIR" -lR \
        -Wl,--disable-new-dtags \
        -Wl,-rpath,\$ORIGIN/r-install/lib/R/lib \
        -Wl,-rpath-link,"$R_LIB_DIR" \
        -rdynamic \
        -lm -lpthread -ldl

    if [ -f "$FUZZ_DIR/dictionaries/${name}.dict" ]; then
        cp "$FUZZ_DIR/dictionaries/${name}.dict" "$OUT/${name}.dict"
    fi

    if [ -f "$FUZZ_DIR/options/${name}.options" ]; then
        cp "$FUZZ_DIR/options/${name}.options" "$OUT/${name}.options"
    fi

    if [ -d "$SEED_STAGE/${name}" ] && [ -n "$(ls -A "$SEED_STAGE/${name}" 2>/dev/null)" ]; then
        ( cd "$SEED_STAGE/${name}" && zip -q -j "$OUT/${name}_seed_corpus.zip" ./* )
    fi
done
