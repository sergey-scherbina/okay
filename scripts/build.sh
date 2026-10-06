#!/bin/sh
# Managed platform builds: fresh sbt processes, JVM -> JS -> Native.
set -eu
if [ "$#" -gt 2 ]; then
  echo 'usage: scripts/build.sh [jvm|js|native|all] [task]' >&2
  exit 2
fi
platform="${1:-jvm}"
case "$platform" in jvm|js|native|all) : ;; *) echo "build: unknown platform: $platform" >&2; exit 2 ;; esac
task="${2:-test}"
case "$task" in ''|*[!a-zA-Z0-9_/-]*) echo 'build: task must be a single sbt task name' >&2; exit 2 ;; esac
root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root"
exec sh scripts/gate.sh "family $platform $task"
