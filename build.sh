#!/usr/bin/env bash
# The one build/test entry point for this project. Full detail in
# WORKFLOW.md; the policy behind it (why gate.sh, not bare sbt) is in
# AGENTS.md's "single-path-verification".
#
#   ./build.sh                      full gate: sbt test, through gate.sh
#   ./build.sh test [sbt-command]   the same gate, or any other sbt
#                                    command through it — a single
#                                    module's test, a probe, anything
#   ./build.sh compile              a cold-safe compile check (Test/compile)
#   ./build.sh package <module-id> <module-dir> [image:tag]
#                                    build one deployable's fat jar
#                                    (+ Docker image if a daemon answers)
#
# EVERY sbt invocation here goes through scripts/gate.sh — never bare
# `sbt` — so a single test gets the same warning check, stall
# watchdog and native lost-process rerun as a full landing gate. There
# is exactly one path; this script is a shorter name for it.
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$ROOT"

cmd="${1:-test}"
shift || true

case "$cmd" in
  test)
    exec bash scripts/gate.sh "${1:-test}"
    ;;
  compile)
    exec bash scripts/gate.sh "Test/compile"
    ;;
  package)
    exec okay-deploy/bin/okay-package.sh "$@"
    ;;
  help|-h|--help)
    sed -n '2,16p' "$0" | sed 's/^# \{0,1\}//'
    ;;
  *)
    echo "build.sh: unknown command '$cmd' — try test, compile, package, or help" >&2
    exit 1
    ;;
esac
