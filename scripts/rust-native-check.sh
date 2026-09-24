#!/bin/sh
# specs/polyglot-rust.md stage 2, the LIVE check of the Scala Native road:
# build the argon2 crate's STATICLIB offline, link it into okay-rust's Native
# test binary (OKAY_RUST_ARGON2_LIB, read by build.sbt's nativeSettings), and
# run the suite holding it to the bytes BouncyCastle pinned. Not part of the
# default gate, which has no cargo. Prints `rust-native: GREEN` or `: RED`.
set -eu
here=$(cd "$(dirname "$0")/.." && pwd)
work=$(mktemp -d "${TMPDIR:-/tmp}/okay-rust-native.XXXXXX")
red() { echo "rust-native: RED — $1"; exit 1; }
command -v cargo >/dev/null 2>&1 || red "cargo is not installed"

(cd "$here/okay-rust/kernels/argon2" && cargo build --offline --release --target-dir "$work/target" > "$work/cargo.log" 2>&1) \
  || { tail -20 "$work/cargo.log"; red "the crate did not build"; }
lib="$work/target/release/libokay_argon2.a"
[ -f "$lib" ] || red "no staticlib at $lib"

OKAY_RUST_ARGON2_LIB="$lib" sh "$here/scripts/gate.sh" "okayRustNative/test" > "$work/gate.log" 2>&1 || true
grep '^gate:' "$work/gate.log"
grep -q '^gate: GREEN' "$work/gate.log" || red "the Native suite did not pass (log: $work/gate.log)"
echo "rust-native: GREEN — the staticlib through @extern gives BouncyCastle's bytes on Scala Native"
