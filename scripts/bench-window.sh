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
# STAGE 2, DEMOTE INSTEAD OF WAIT (bench-window-demote-measure,
# 2026-09-27). Measured: a single-threaded JMH lane beside 14 CPU
# burners on this 10P+4E box read 69-84 us against 44 alone — and 44.4 /
# 42.7 when the same burners ran in the BACKGROUND QoS class
# (`taskpolicy -b`), which macOS keeps on the efficiency cores. So by
# request (OKAY_BENCH_DEMOTE=on — NOT the default, see BW_DEMOTE below) a gate that meets a benchmark does not wait: it demotes itself
# (its sbt and every fork inherit the class) and runs on, slower; the
# lane demotes the gates already running before each attempt and puts
# them all back (`taskpolicy -B`) when it is done. The default, and any
# box without `taskpolicy`, is the stage-1 protocol: wait.
#
# A token or request is a file named by its owner's pid. A dead pid's
# file is ignored and removed by whoever reads it, so a crashed gate
# or lane blocks nobody.
#
#   sh scripts/bench-window.sh --status
BW_DIR="${OKAY_BENCH_DIR:-${TMPDIR:-/tmp}/okay-bench}"
BW_POLL="${OKAY_BENCH_POLL:-10}"
BW_GATE_MAX_WAIT="${OKAY_BENCH_GATE_MAX_WAIT:-900}"
# A HELD GATE SAYS IT IS ALIVE (bench-window-hold-reads-as-stall,
# 2026-09-26): gate-retry kills a gate whose log has not grown for
# GATE_STALL_MIN minutes (10), and a hold is up to 15 — one line for
# the whole hold was read as a stall, three attempts, no verdict, and
# the ci-runner's gate goes through the same road. A line every 30 s,
# half the watchdog's one-minute growth window, so no window is empty.
BW_HEARTBEAT="${OKAY_BENCH_HEARTBEAT:-30}"
# OPT-IN since 2026-09-27 (bench-window-demote-opt-in): ON by default it
# ran whole gates on the 4 efficiency cores for as long as the JMH queue
# stayed non-empty — a relay-forward-same-inject gate ran its entire
# affected matrix demoted and nine timeout-bound tests failed across
# seven modules (TestGenerate's 1M: 243 s against its 120 s limit). A
# lane is quiet either way; a gate's tests with timeouts are not.
BW_DEMOTE="${OKAY_BENCH_DEMOTE:-off}"
command -v taskpolicy > /dev/null 2>&1 || BW_DEMOTE=off

# a pid and every descendant (a process tree, bounded by the box)
bw_tree() {
  echo "$1"
  for _c in $(pgrep -P "$1" 2>/dev/null); do bw_tree "$_c"; done
}

# a gate's tree into the background QoS class, marked for the restore
bw_demote_pid() {
  mkdir -p "$BW_DIR/demoted"
  for _p in $(bw_tree "$1"); do taskpolicy -b -p "$_p" 2>/dev/null; done
  : > "$BW_DIR/demoted/$1"
}

# the lane's side: every live gate demoted before an attempt
bw_demote_gates() {
  [ "$BW_DEMOTE" = on ] || return 0
  for _g in $(bw_live gates); do bw_demote_pid "$_g"; done
}

# the lane's side, when it is done: every marked gate's tree back
bw_restore_gates() {
  [ -d "$BW_DIR/demoted" ] || return 0
  for _f in "$BW_DIR/demoted"/*; do
    [ -e "$_f" ] || continue
    _p=$(basename "$_f")
    if kill -0 "$_p" 2>/dev/null; then
      for _q in $(bw_tree "$_p"); do taskpolicy -B -p "$_q" 2>/dev/null; done
    fi
    rm -f "$_f"
  done
}

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
    _w=$(bw_live want | tr '\n' ' '); _w="${_w% }"
    [ -z "$_w" ] && return 0
    if [ "$BW_DEMOTE" = on ]; then
      bw_demote_pid $$
      echo "gate: bench window: a benchmark is queued or running (pid ${_w}) — this gate runs on the efficiency cores meanwhile (taskpolicy -b; OKAY_BENCH_DEMOTE=off to wait instead)"
      return 0
    fi
    if [ "$_waited" -ge "$BW_GATE_MAX_WAIT" ]; then
      echo "gate: bench window: held ${_waited}s for benchmark(s) ${_w} — starting anyway (OKAY_BENCH_GATE_MAX_WAIT)"
      return 0
    fi
    rm -f "$BW_DIR/gates/$$"
    if [ -z "$_said" ]; then
      echo "gate: bench window: a benchmark is queued (pid ${_w}) — holding this gate's start until it has run, at most ${BW_GATE_MAX_WAIT}s (OKAY_BENCH_WINDOW=off to skip)"
      _said=1; _beat=0
    elif [ $((_waited - _beat)) -ge "$BW_HEARTBEAT" ]; then
      echo "gate: bench window: still holding for benchmark(s) ${_w}, ${_waited}s of ${BW_GATE_MAX_WAIT}s"
      _beat=$_waited
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
