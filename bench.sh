#!/usr/bin/env bash
# The one benchmark entry point for this project. Full detail in
# WORKFLOW.md; the numbers live in docs/benchmarks.md, the measuring
# discipline in the `performance` skill
# (.agents/plugins/performance/commands/performance.md) — alternate
# A/B, same session, record refutations too, never trust one run.
#
#   ./bench.sh run <pattern> [sbt-module]   JMH in one module (default: okayJVM)
#   ./bench.sh compare <pattern>            JMH in the `compare` module (ecosystem lanes)
#   ./bench.sh wroclaw [days] [rounds] [fraction]
#                                            the Wrocław streaming benchmark (docs/benchmarks.md §20)
#   ./bench.sh ab <name>                    an A/B that decides a default (scripts/ab-defaults.sh)
#   ./bench.sh history [grep-pattern]       the benchmark history (src/jmh/history.d + the archive), tabulated
#   (record a measurement: scripts/history.sh new <measure>, then write its rows)
#
# `run`/`compare` go through scripts/gate.sh, same as build.sh: a JMH
# fork that hangs on a lock (a real, documented failure — see
# AGENTS.md's "the orphans are the idle reaper's leftovers") is a
# silent, near-0%-CPU process tree exactly like a stalled compile, so
# the same watchdog catches it and prints WHERE it dumped a thread
# stack instead of leaving you to notice the box went quiet.
# `wroclaw`/`ab` are their own multi-stage drivers with their own
# timing and are run directly, unwrapped.
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$ROOT"

cmd="${1:-help}"
shift || true

case "$cmd" in
  run)
    pattern="${1:?usage: ./bench.sh run <pattern> [sbt-module]}"
    module="${2:-okayJVM}"
    # generate and run are two sbt calls (JMH's benchmark list comes
    # from its own annotation processor, run at Jmh/compile — a single
    # `Jmh/run` on a stale or absent list answers "No matching
    # benchmarks" even though the pattern is fine)
    bash scripts/gate.sh "${module}/Jmh/compile"
    exec bash scripts/gate.sh "${module}/Jmh/run ${pattern}"
    ;;
  compare)
    pattern="${1:?usage: ./bench.sh compare <pattern>}"
    bash scripts/gate.sh "compare/Jmh/compile"
    exec bash scripts/gate.sh "compare/Jmh/run ${pattern}"
    ;;
  wroclaw)
    exec sh scripts/wroclaw-bench.sh "$@"
    ;;
  ab)
    exec bash scripts/ab-defaults.sh "$@"
    ;;
  history)
    # the archive and src/jmh/history.d together, oldest first (history-d)
    tab="$(printf '\t')"
    if [ -n "${1:-}" ]; then
      sh scripts/history.sh "$1" | column -t -s "$tab"
    else
      sh scripts/history.sh | { head -1; tail -30; } | column -t -s "$tab"
    fi
    ;;
  help|-h|--help)
    sed -n '2,17p' "$0" | sed 's/^# \{0,1\}//'
    ;;
  *)
    echo "bench.sh: unknown command '$cmd' — try run, compare, wroclaw, ab, history, or help" >&2
    exit 1
    ;;
esac
