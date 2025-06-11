#!/usr/bin/env fish
cargo build --release
nix run github:I-Al-Istannen/crow#client -- run-tests --test-dir tests/ --compiler-run ./run.sh
