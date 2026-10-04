#!/bin/sh
# hsdis-link.sh — keep HotSpot's disassembler (hsdis) in EVERY JDK on this box, so `-XX:+PrintAssembly`
# and JMH's `-prof perfasm`/`dtraceasm` work whatever JDK a fork runs on (state-foreign-shape, 2026-10-04).
#
# The one real copy lives OUTSIDE any JDK, at $HSDIS (default ~/Library/hsdis/hsdis-<arch>.dylib), so a JDK
# upgrade cannot take it away; every JDK under sdkman gets a symlink in lib/server, where HotSpot looks first.
# Run it after installing a JDK. It never overwrites a real file and says what it did.
#
#   sh scripts/hsdis-link.sh            link every sdkman JDK
#   sh scripts/hsdis-link.sh --check    say which JDKs lack it (exit 1 if any)
set -eu
arch=$(uname -m); [ "$arch" = arm64 ] && arch=aarch64
lib="hsdis-$arch.dylib"; [ "$(uname -s)" = Linux ] && lib="hsdis-$arch.so"
HSDIS=${HSDIS:-$HOME/Library/hsdis/$lib}
JDKS=${JDKS:-$HOME/.sdkman/candidates/java}
if [ ! -f "$HSDIS" ]; then
  # adopt a copy a JDK already carries (Temurin builds may ship it)
  found=$(find "$JDKS" -path '*/lib/server/'"$lib" -type f 2>/dev/null | head -1 || true)
  [ -n "$found" ] || { echo "hsdis-link: no $lib at $HSDIS nor in any JDK under $JDKS; build or fetch one first" >&2; exit 2; }
  mkdir -p "$(dirname "$HSDIS")"; cp "$found" "$HSDIS"; echo "hsdis-link: kept $found as $HSDIS"
fi
missing=0
for j in "$JDKS"/*/; do
  j=${j%/}; [ -L "$j" ] && continue; [ -d "$j/lib/server" ] || continue
  t="$j/lib/server/$lib"
  if [ -e "$t" ]; then [ "${1:-}" = --check ] || echo "hsdis-link: has    $t"
  elif [ "${1:-}" = --check ]; then echo "hsdis-link: MISSING $t"; missing=1
  else ln -s "$HSDIS" "$t"; echo "hsdis-link: linked $t"; fi
done
exit $missing
