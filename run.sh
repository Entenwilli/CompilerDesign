#!/usr/bin/env sh
BIN_DIR="$(dirname "$0")/target/release"
"$BIN_DIR"/compiler_design "$@"
