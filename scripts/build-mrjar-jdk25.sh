#!/usr/bin/env bash
#
# Compiles jdk25/Scoped.scala against a REAL JDK 25+ toolchain --
# java.lang.ScopedValue does not exist on any older JDK's own runtime
# classes, and no `-release` flag can grant a compiler access to a
# newer platform's API than the JVM it itself runs on. So this runs
# the Scala 3 compiler's own main class through a JDK 25+ `java`
# binary, found rather than assumed.
#
# Output: jdk25/target/classes/okay/*.class, picked up by build.sbt's
# `okay` crossProject .jvmSettings (Compile/packageBin/mappings) IF
# this has been run -- never a hard dependency. A machine that never
# runs this script builds the exact same jar it always has. See
# specs/script-scoped-state-mrjar.md.
#
# Usage: scripts/build-mrjar-jdk25.sh
#   OKAY_JDK25_HOME=/path/to/jdk25   -- overrides auto-discovery
set -eu

root="$(cd "$(dirname "$0")/.." && pwd)"
src="$root/jdk25/Scoped.scala"
out="$root/jdk25/target/classes"

jdk25_home() {
  if [ -n "${OKAY_JDK25_HOME:-}" ]; then
    echo "$OKAY_JDK25_HOME"
    return
  fi
  # highest 25.* or newer under sdkman's Temurin candidates
  candidates_dir="$HOME/.sdkman/candidates/java"
  [ -d "$candidates_dir" ] || return 1
  best=""
  best_major=0
  for d in "$candidates_dir"/*/; do
    name="$(basename "$d")"
    major="${name%%.*}"
    case "$major" in
      ''|*[!0-9]*) continue ;;
    esac
    if [ "$major" -ge 25 ] && [ "$major" -gt "$best_major" ]; then
      best_major="$major"
      best="${d%/}"
    fi
  done
  [ -n "$best" ] || return 1
  echo "$best"
}

home="$(jdk25_home || true)"
if [ -z "${home:-}" ] || [ ! -x "$home/bin/java" ]; then
  echo "build-mrjar-jdk25: no JDK 25+ toolchain found (set OKAY_JDK25_HOME, or install one under ~/.sdkman/candidates/java/) -- skipping, okay's jvm package will be the base-only jar" >&2
  exit 0
fi

found_version="$("$home/bin/java" -version 2>&1 | head -1)"
echo "build-mrjar-jdk25: compiling with $home ($found_version)"

command -v cs >/dev/null 2>&1 || { echo "build-mrjar-jdk25: coursier (cs) not found on PATH -- skipping" >&2; exit 0; }

scala_version="$(sed -n 's/^ThisBuild \/ scalaVersion := "\(.*\)"/\1/p' "$root/build.sbt" | head -1)"
[ -n "$scala_version" ] || { echo "build-mrjar-jdk25: could not read scalaVersion from build.sbt -- skipping" >&2; exit 0; }

cp="$(cs fetch --classpath "org.scala-lang:scala3-compiler_3:$scala_version")"

rm -rf "$out"
mkdir -p "$out"

"$home/bin/java" -cp "$cp" dotty.tools.dotc.Main -classpath "$cp" -d "$out" "$src"

echo "build-mrjar-jdk25: wrote $(find "$out" -name '*.class' | wc -l | tr -d ' ') class file(s) to $out"
