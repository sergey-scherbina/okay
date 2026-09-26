#!/bin/sh
# bench-window.sh — gates and benchmarks share the box by PROTOCOL
# (specs/bench-window.md). SOURCED by scripts/gate.sh and
# scripts/jmh-lane.sh, so both sides speak one vocabulary (quiet.sh's
# precedent); executed with --status it prints who holds what.
#
# Readers–writers with writer preference: a gate is a reader (any
# number at once) and holds a TOKEN while it runs; a JMH lane is the
# writer and files a REQUEST while it is queued. A gate that starts
# while a request is live waits (at most OKAY_BENCH_GATE_MAX_WAIT
# seconds, then starts anyway and says so); running gates are never
# touched, they finish and their tokens go; the lane runs when no
# token is live. Nothing changes while nobody is benchmarking.
#
# A token or request is a file named by its owner's pid. A dead pid's
# file is ignored and removed by whoever reads it, so a crashed gate
# or lane blocks nobody.
#
#   sh scripts/bench-window.sh --status
BW_DIR="${OKAY_BENCH_DIR:-${TMPDIR:-/tmp}/okay-bench}"
BW_POLL="${OKAY_BENCH_POLL:-10}"
BW_GATE_MAX_WAIT="${OKAY_BENCH_GATE_MAX_WAIT:-900}"

# the live pids filed under $BW_DIR/<gates|want>, one per line; a dead
# owner's file is removed on the way
bw_live() {
  _d="$BW_DIR/$1"
  [ -d "$_d" ] || return 0
  for _f in "$_d"/*; do
    [ -e "$_f" ] || continue
    _p=$(basename "$_f")
    if kill -0 "$_p" 2>/dev/null; then echo "$_p"; else rm -f "$_f"; fi
  done
}

# a gate's entry. The token is written FIRST and the requests read
# SECOND; a lane does the mirror image (request, then tokens), so a
# gate and a lane arriving together cannot both go — at least one of
# them sees the other.
bw_gate_enter() {
  mkdir -p "$BW_DIR/gates" "$BW_DIR/want"
  _waited=0; _said=""
  while :; do
    : > "$BW_DIR/gates/$$"
    [ "${OKAY_BENCH_WINDOW:-on}" = off ] && return 0
    _w=$(bw_live want | tr '\n' ' ')
    [ -z "$_w" ] && return 0
    if [ "$_waited" -ge "$BW_GATE_MAX_WAIT" ]; then
      echo "gate: bench window: held ${_waited}s for benchmark(s) ${_w}— starting anyway (OKAY_BENCH_GATE_MAX_WAIT)"
      return 0
    fi
    rm -f "$BW_DIR/gates/$$"
    if [ -z "$_said" ]; then
      echo "gate: bench window: a benchmark is queued (pid ${_w}) — holding this gate's start until it has run, at most ${BW_GATE_MAX_WAIT}s (OKAY_BENCH_WINDOW=off to skip)"
      _said=1
    fi
    sleep "$BW_POLL"
    _waited=$((_waited + BW_POLL))
  done
}

bw_gate_leave() { rm -f "$BW_DIR/gates/$$"; }

bw_want() { mkdir -p "$BW_DIR/want" "$BW_DIR/gates"; : > "$BW_DIR/want/$$"; }
bw_unwant() { rm -f "$BW_DIR/want/$$"; }

case "$(basename "$0" 2>/dev/null)" in
  bench-window.sh)
    if [ "${1:-}" = "--status" ]; then
      g=$(bw_live gates | tr '\n' ' '); w=$(bw_live want | tr '\n' ' ')
      echo "bench-window: gates running: ${g:-none}"
      echo "bench-window: benchmarks queued: ${w:-none}"
      for p in $g $w; do ps -p "$p" -o pid=,etime=,args= 2>/dev/null | cut -c1-160; done
      exit 0
    fi
    ;;
esac
