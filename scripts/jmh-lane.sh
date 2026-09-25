#!/bin/sh
# jmh-lane.sh — ONE JMH lane at a time, gated on a quiet box at both
# ends (AGENTS.md, "RUN ONLY WHAT THIS CHANGE NEEDS"; ci-staged skill,
# A-7). A benchmark measures wall-clock behaviour, so a sibling's gate
# starting mid-run does not just slow it down, it makes the NUMBER
# WRONG, silently — a pass/fail test result cannot be corrupted that
# way, which is why this needs its own tool and not just gate.sh.
#
#   scripts/jmh-lane.sh "<project>/Jmh/run <Pattern> <jmh-args...>" [attempts]
#
#   sh scripts/jmh-lane.sh "compare/Jmh/run GenBenchmark.take -f 2 -wi 3 -i 5"
#
# WHAT IT DOES: takes a lock (one lane at a time, on THIS box, across
# every agent — a JMH fork and a sibling's own JMH fork corrupt each
# other exactly as a sibling's test gate would), waits for `quiet.sh`
# before starting (same box-wide threshold `gate-retry.sh` uses — one
# vocabulary, not two), runs the ONE sbt command given, and — the part
# a whole-build test gate does not need — CHECKS QUIET AGAIN at the
# end. If the box was quiet at the start but got busy DURING the run,
# the result is discarded and the SAME lane is retried (default 5
# attempts, matching the incident this fixes: three whole-class rounds
# came back ±50-110% apart until a sibling's gate starting mid-run
# stopped silently corrupting the number — per-lane-gated-jmh,
# generators-jmh, 2026-09-23). Record what actually landed with
# `scripts/history.sh new <measure>`, per the `performance` skill; a
# discarded round belongs there too, marked discarded, not silently
# dropped.
#
# Separate lock from scripts/ci-runner.sh's `.work/ci/lock` on purpose:
# a JMH lane and a whole-build gate are different operations that both
# want the box quiet, and `quiet()` itself is what keeps them from
# running together (a live gate is a busy-sbt process quiet() sees) —
# a shared lock would only add a wait a JMH lane a live gate already
# forces via CPU, and would block two DIFFERENT things (a test gate, a
# benchmark) from ever running in true parallel on a box with room for
# only one anyway, which is what quiet() already decides on its own.
set -u
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/.." && pwd)"
cd "$root"
. "$here/quiet.sh"
# sbt on .sdkmanrc's JDK, as gate.sh runs it (jdk-pin.sh; jmh-lane-jdk-pin)
JDK_PIN_ROOT="$here/.." . "$here/jdk-pin.sh"

LOCKDIR="$root/.work/jmh/lock"
mkdir -p "$root/.work/jmh"

CMD="${1:?usage: jmh-lane.sh \"<sbt command>\" [attempts]}"
ATTEMPTS="${2:-5}"
# a bare sbt call, matching scripts/ab-defaults.sh's own precedent for
# Jmh/run: gate.sh's pass/fail parsing is tuned for a test summary
# line ("Passed: Total N"), not a JMH result table, and its own RED
# detection would not recognise a JMH failure shape either way — a
# JMH run's own sbt exit code is what this script trusts
SBT="${SBT:-sbt}"

take_lock() {
  if mkdir "$LOCKDIR" 2>/dev/null; then
    echo $$ > "$LOCKDIR/pid"
    return 0
  fi
  holder=$(cat "$LOCKDIR/pid" 2>/dev/null || echo "")
  if [ -n "$holder" ] && ps -p "$holder" >/dev/null 2>&1; then
    echo "jmh-lane: lock held by pid $holder — another lane is running on this box"
    return 1
  fi
  echo "jmh-lane: lock dir exists but its pid ($holder) is dead — taking it over"
  rm -rf "$LOCKDIR"
  mkdir "$LOCKDIR" 2>/dev/null || { echo "jmh-lane: lost the race for the lock"; return 1; }
  echo $$ > "$LOCKDIR/pid"
  return 0
}
release_lock() { rm -rf "$LOCKDIR"; }

take_lock || exit 1
trap release_lock EXIT INT TERM

# up to 30 minutes waiting for quiet, same cap as gate-retry.sh, then
# go anyway — a lane that never starts is worse than one run once on a
# box that stayed stubbornly at load 16. BEFORE EVERY ATTEMPT, not just
# the first: a contamination-triggered retry must not blindly re-run
# into the same busy box that just ruined the previous one.
wait_for_quiet() {
  w=0
  while [ "$w" -lt 60 ]; do
    quiet && return
    [ $((w % 4)) -eq 0 ] && echo "jmh-lane: waiting for a quiet box, $((w / 2)) min: busy-sbt=$H load=$L freeGB=$F"
    sleep 30
    w=$((w + 1))
  done
}

i=1
while [ "$i" -le "$ATTEMPTS" ]; do
  wait_for_quiet
  echo "jmh-lane: attempt $i/$ATTEMPTS — $CMD"
  # shellcheck disable=SC2086
  $SBT -batch "$CMD"
  rc=$?
  if quiet; then
    if [ "$rc" -eq 0 ]; then
      echo "jmh-lane: done, box stayed quiet throughout — trust this number"
      exit 0
    else
      echo "jmh-lane: the run itself failed (exit $rc), box was quiet — a real failure, not contention"
      exit "$rc"
    fi
  fi
  echo "jmh-lane: the box got busy DURING this lane (busy-sbt=$H load=$L freeGB=$F) — the number is CONTAMINATED, discarding and retrying"
  i=$((i + 1))
done
echo "jmh-lane: gave up after $ATTEMPTS attempts, the box never stayed quiet through a whole lane"
exit 99
