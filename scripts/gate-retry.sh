#!/bin/sh
#
# Run scripts/gate.sh, and RETRY when the BOX kills the run.
#
# Seven full-matrix gates died in one session on 2026-09-11, and every
# one of them had started on an idle machine: the spike arrived after
# the run did. A sibling's build lands, the launchd RAM guard picks the
# heaviest JVM, and the heaviest JVM is always the run furthest along
# -- so waiting for a quiet box, which is what every agent here already
# does by hand, protects only the first minute of a forty-minute
# matrix. The retry loop is the part that was missing.
#
# WHAT IT NEVER DOES IS RETRY A RED. The distinction it turns on is the
# one gate.sh already makes: a run that reaches a verdict (`gate: RED`
# or `gate: GREEN`) has SAID something about the tree, and its word
# stands -- the exit code is passed straight through. Only a run that
# said nothing, or said `gate: KILLED`, is started again. Retrying a
# red would be a machine for landing broken trees, which is the
# opposite of what a gate is for.
#
# `gate: KILLED` is why this comment changed on the day it was written.
# A killed sbt does not always die before gate.sh can speak: it exits
# 143, gate.sh saw no `==> X`, and the FIRST version of this pair
# called that "RED -- a failure this script does not recognise". The
# loop then read a verdict and passed 143 through, refusing the one
# case it exists for. gate.sh names a signal now, and a signal is not
# a verdict.
#
# On the kills themselves see AGENTS.md ("THE 143, SOLVED") and
# scripts/gate-sentinels.sh, which tells an external signal apart from
# a process-group kill by blast radius.
#
# Usage:
#   scripts/gate-retry.sh <worktree> <log> [attempts] [cmd]   default 6, "test"
#   scripts/gate-retry.sh --probe                       read the box now
#   scripts/gate-retry.sh --read <log>                  what it would do
#                                                       with a log you have
#
# [cmd] is what ci-runner.sh (specs/ci-staged.md, stage B) needs this
# for: "family all", the whole build, run in the MAIN checkout (a
# perfectly good <worktree>) with the same quiet-wait, stall watchdog
# and kill-retry a lane already gets for free. Default "test" so every
# existing call, none of which named a fourth argument, is unchanged.
#
# Exit: the gate's own code on a verdict; 99 if none in <attempts>.
#
set -e

# The box is quiet when no heavy JVM is running (a gate's sbt sits at
# several GB; anything under 1 GB is a language server or a launcher),
# the load average is low, and there is enough free memory that a
# matrix will not page. The free figure counts inactive and
# speculative pages: on Darwin those are reclaimable, and "Pages free"
# alone reads near zero on a healthy machine.
# WHAT "QUIET" MEANS, and both halves were measured wrong before
# (gate-quiet-realistic, 2026-09-17, operator: "I need to work, not
# wait"). Every gate this session waited the FULL 30 minutes and then
# started anyway, which is the loop announcing that its condition is
# unsatisfiable rather than that the box is busy.
#
# HEAVY is a JVM that is BURNING CPU, not one that is merely resident.
# The old test counted any sbt with RSS > 1 GB, and an idle sbt server
# is the normal state of this machine -- there is one in the main
# checkout that has been up for two days, and on 2026-09-17 a 1.1 GB
# sbt in another project sat at 0.0% CPU for 21 minutes and made the
# box "busy" by itself. AGENTS.md already draws this distinction for
# JMH forks ("a fork at 0% for minutes is asleep, not measuring"); it
# just had not reached this line.
#
# FREE was 16 GB, which this box does not reach while anybody is
# logged in: measured 14.6 GB free with nothing but an idle sbt and
# the operator's VM. The replacement is DERIVED from the guard that
# would kill the run, not guessed -- `io.scalascript.build-ram-guard`
# is loaded and ticks every 20 s, and its own constants are:
#
#   REAP_FLOOR_MB=8192   below this (or any thrashing) it starts
#                        reclaiming -- orphaned and idle servers
#   SHED_FLOOR_MB=3072   below this AND thrashing it may kill LIVE
#                        work: the heaviest build JVM, which during a
#                        gate is the gate (AGENTS.md, THE 143)
#
# sbt here takes 6 GB (.jvmopts). Starting at 8 GB free would put the
# host at ~2 GB once the heap is up -- UNDER the shed floor, with the
# gate as the heaviest JVM, which is the 143 this project already
# spent three days blaming on its own test suite. 10 GB is the first
# number that keeps the host above the shed floor with the gate's own
# footprint accounted for, and the machine reaches it: measured 12-16
# GB available through this session's runs.
#
# (An earlier version of this comment said 8, and it was wrong for
# exactly the reason written above; it was caught by reading the
# guard's script rather than by a kill.)
# KILL A PROCESS TREE BY PID, never by name (AGENTS.md is explicit,
# and the incident it comes from cost a full matrix). Depth first, so
# a child cannot be reparented away while its parent is still alive.
kill_tree() {
  for c in $(pgrep -P "$1" 2>/dev/null); do kill_tree "$c"; done
  kill "$1" 2>/dev/null
}

quiet() {
  L=$(sysctl -n vm.loadavg | awk '{print int($2)}')
  H=$(ps -eo pcpu,rss,args | grep "sbt.script" | grep -v grep | awk '$1 > 20 && $2 > 1000000' | wc -l | tr -d ' ')
  F=$(vm_stat | awk '/Pages free|Pages inactive|Pages speculative/ {gsub("\\.","",$NF); s+=$NF} END {print int(s*16384/1073741824)}')
  [ "$H" -eq 0 ] && [ "$L" -lt 15 ] && [ "$F" -ge 10 ]
}

# What one attempt's log says. This is the whole safety property, so it
# is one function and `--read` runs exactly it: `done` means the gate
# reached a verdict and nobody may start it again, `retry` means the
# attempt said nothing at all, which is what a killed run looks like.
verdict() {
  if grep -q "gate: GREEN" "$1"; then echo green
  elif grep -q "gate: RED" "$1"; then echo red
  elif grep -q "gate: KILLED" "$1"; then echo killed
  elif grep -q "gate: STALLED" "$1"; then echo stalled
  else echo none
  fi
}

if [ "${1:-}" = "--read" ]; then
  L="${2:?--read needs a log}"
  case "$(verdict "$L")" in
    green)  echo "$L: gate: GREEN — done, exit 0" ;;
    red)    echo "$L: gate: RED — done, the gate's exit code stands; NOT retried" ;;
    killed) echo "$L: gate: KILLED — a signal, not a verdict; retry" ;;
    stalled) echo "$L: gate: STALLED — the watchdog killed a silent, idle run; retry, and READ the dump beside the log first" ;;
    none)   echo "$L: no verdict — the box took it; retry" ;;
  esac
  exit 0
fi

if [ "${1:-}" = "--probe" ]; then
  if quiet; then v=quiet; else v=busy; fi
  echo "box: $v  (busy-sbt=$H load=$L freeGB=$F; wants busy-sbt=0 load<15 free>=10)"
  exit 0
fi

WT="${1:?usage: gate-retry.sh <worktree> <log> [attempts] [cmd]}"
LOG="${2:?usage: gate-retry.sh <worktree> <log> [attempts] [cmd]}"
N="${3:-6}"
GATE_CMD="${4:-test}"
# Minutes of SILENCE that mean a hang rather than a long compile.
# Ten, because the longest legitimate quiet stretch here is one big
# module's compile and the measured hang was 28 minutes and counting.
# THE BACKSTOP, not the first line of defence any more (gate-watchdog,
# 2026-09-18). `gate.sh` now watches itself at 8 minutes of silence
# WITH an idle process tree, and takes a jcmd dump before it kills —
# so in the ordinary case that one fires first and this never runs.
# This stays for the case it still covers: gate.sh's own loop wedged,
# or a stall in something gate.sh does not run under its watchdog.
# Keep it ABOVE gate.sh's threshold or the diagnosing layer never gets
# to look.
STALL="${GATE_STALL_MIN:-10}"

: > "$LOG"
i=1
while [ "$i" -le "$N" ]; do
  # Wait up to 30 minutes for quiet, then go anyway: a box that stays
  # busy that long is the normal state of this machine, and a gate
  # that never starts is worse than one that may be killed.
  # SAY SO WHILE WAITING. The log used to stay empty for up to half an
  # hour, which from outside is indistinguishable from a hung gate --
  # three watchers expired over an empty file on 2026-09-17 before
  # anybody thought to look at the process list.
  w=0
  while [ "$w" -lt 60 ]; do
    quiet && break
    if [ $((w % 4)) -eq 0 ]; then
      echo "== waiting for a quiet box, $((w / 2)) min: busy-sbt=$H load=$L freeGB=$F" >> "$LOG"
    fi
    sleep 30
    w=$((w + 1))
  done

  echo "== attempt $i at $(date +%H:%M) load $(sysctl -n vm.loadavg)" >> "$LOG"
  # BASH, not `sh`: gate.sh is `#!/usr/bin/env bash` and uses process
  # substitution, and `sh` here is bash in POSIX mode where `<(...)` is
  # a syntax error. It only shows on the branch that compares the
  # failed projects against the known-lost ones, which is why it went
  # unseen — that branch runs exactly when a gate has already gone
  # wrong.
  # THE STALL WATCHDOG (gate-stall-watchdog, 2026-09-17). A gate that
  # HANGS is invisible to this loop as it was written: it reads the
  # log only after gate.sh returns, and a hung gate never returns.
  # Measured the same day: a run reached 80 of 81 modules and then sat
  # for 28 minutes with sbt's main thread parked in
  # ExecutorCompletionService.take and ~200 forked native/node runners
  # at 0.0% CPU -- a runner handshake that never completed. Killing it
  # by hand and rerunning took half an hour, twice.
  #
  # So: run it in the background, and watch the log GROW. A healthy
  # gate writes something every few seconds; a compile of one big
  # module is the longest legitimate silence, which is why the
  # threshold is minutes and not seconds. A stall is killed by PID,
  # tree first, and counted as "no verdict" -- which this loop already
  # knows how to retry.
  # A SENTINEL FILE, not `kill -0`: a finished background child is a
  # ZOMBIE until the shell reaps it, and `kill -0` on a zombie
  # SUCCEEDS. The first cut of this watchdog used it and never noticed
  # a gate finishing -- caught by the test that a HEALTHY gate must
  # not be killed, which is the test worth writing first.
  rcfile="$LOG.rc"
  rm -f "$rcfile"
  # `set +e` INSIDE the subshell, and it is not decoration: this
  # script runs under `set -e`, a subshell inherits it, and a RED gate
  # exits non-zero -- so the first cut died ON gate.sh and never
  # reached the sentinel. Every red gate then sat the full stall
  # timeout before this loop noticed anything, which is the opposite
  # of the bug the watchdog was written for. Caught in production the
  # same afternoon: gate-drv.log has the verdict and, ten minutes
  # later, "STALLED".
  ( set +e; cd "$WT"; bash scripts/gate.sh "$GATE_CMD"; echo $? > "$rcfile" ) >> "$LOG" 2>&1 &
  gpid=$!
  stalled=0
  quietmin=0
  size=$(wc -c < "$LOG")
  ticks=0
  while [ ! -f "$rcfile" ]; do
    sleep 10
    ticks=$((ticks + 1))
    [ $((ticks % 6)) -ne 0 ] && continue          # the growth check is per MINUTE
    now=$(wc -c < "$LOG")
    if [ "$now" -gt "$size" ]; then
      size=$now
      quietmin=0
    else
      quietmin=$((quietmin + 1))
      if [ "$quietmin" -ge "$STALL" ]; then
        echo "== attempt $i STALLED: nothing written for $STALL min; killing by pid" >> "$LOG"
        kill_tree "$gpid"
        stalled=1
        break
      fi
    fi
  done
  # `|| true`, and the script is `set -e`: a killed child makes `wait`
  # answer 143, which under `set -e` ENDS THIS SCRIPT — the retry loop
  # would never run, the log would stop mid-sentence, and the exit
  # code would be a signal. That is exactly what the first cut did,
  # and the stall test is what showed it (the log ended at "killing by
  # pid" and the script exited 143).
  wait "$gpid" 2>/dev/null || true
  if [ "$stalled" -eq 1 ]; then rc=99
  elif [ -f "$rcfile" ]; then rc=$(cat "$rcfile")
  else rc=99
  fi
  rm -f "$rcfile"

  case "$(verdict "$LOG")" in
    green)
      # A GREEN whose warning check was BLIND says so. gate.sh checks
      # warnings only in a run that compiled something, and a worktree
      # somebody already built by hand recompiles nothing -- measured
      # 2026-09-17: delim-forward-not-throw landed two unused imports
      # through a gate that said "no compile warnings", because the
      # session had run testOnly in that worktree first. The tests
      # still passed, so this stays exit 0; what it must not do is let
      # silence read as cleanliness.
      if grep -q "warnings NOT checked" "$LOG"; then
        echo "gate: GREEN, but the WARNING CHECK WAS BLIND — this worktree was already built, so nothing recompiled. Compile it cold before trusting 'no warnings'." >> "$LOG"
      fi
      echo "GATE EXIT=0"   >> "$LOG"; exit 0 ;;
    red)   echo "GATE EXIT=$rc" >> "$LOG"; exit "$rc" ;;
  esac

  echo "== attempt $i produced no verdict (rc=$rc) — the box took it; retrying" >> "$LOG"
  i=$((i + 1))
done

echo "GATE EXIT=99  (no verdict in $N attempts)" >> "$LOG"
exit 99
