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
#   scripts/gate-retry.sh <worktree> <log> [attempts]   default 6
#   scripts/gate-retry.sh --probe                       read the box now
#   scripts/gate-retry.sh --read <log>                  what it would do
#                                                       with a log you have
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
# the operator's VM. The number that matters is not "plenty" but
# "enough not to trip the RAM guard mid-run": the scalascript launchd
# agent kills the heaviest JVM when available memory falls under 3 GB
# with pageouts (AGENTS.md, THE 143), and sbt here takes 6 GB. 8 GB is
# the heap plus headroom above the guard's line, and it is a number
# this machine actually reaches.
quiet() {
  L=$(sysctl -n vm.loadavg | awk '{print int($2)}')
  H=$(ps -eo pcpu,rss,args | grep "sbt.script" | grep -v grep | awk '$1 > 20 && $2 > 1000000' | wc -l | tr -d ' ')
  F=$(vm_stat | awk '/Pages free|Pages inactive|Pages speculative/ {gsub("\\.","",$NF); s+=$NF} END {print int(s*16384/1073741824)}')
  [ "$H" -eq 0 ] && [ "$L" -lt 15 ] && [ "$F" -ge 8 ]
}

# What one attempt's log says. This is the whole safety property, so it
# is one function and `--read` runs exactly it: `done` means the gate
# reached a verdict and nobody may start it again, `retry` means the
# attempt said nothing at all, which is what a killed run looks like.
verdict() {
  if grep -q "gate: GREEN" "$1"; then echo green
  elif grep -q "gate: RED" "$1"; then echo red
  elif grep -q "gate: KILLED" "$1"; then echo killed
  else echo none
  fi
}

if [ "${1:-}" = "--read" ]; then
  L="${2:?--read needs a log}"
  case "$(verdict "$L")" in
    green)  echo "$L: gate: GREEN — done, exit 0" ;;
    red)    echo "$L: gate: RED — done, the gate's exit code stands; NOT retried" ;;
    killed) echo "$L: gate: KILLED — a signal, not a verdict; retry" ;;
    none)   echo "$L: no verdict — the box took it; retry" ;;
  esac
  exit 0
fi

if [ "${1:-}" = "--probe" ]; then
  if quiet; then v=quiet; else v=busy; fi
  echo "box: $v  (busy-sbt=$H load=$L freeGB=$F; wants busy-sbt=0 load<15 free>=8)"
  exit 0
fi

WT="${1:?usage: gate-retry.sh <worktree> <log> [attempts]}"
LOG="${2:?usage: gate-retry.sh <worktree> <log> [attempts]}"
N="${3:-6}"

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
  ( cd "$WT" && bash scripts/gate.sh ) >> "$LOG" 2>&1 && rc=0 || rc=$?

  case "$(verdict "$LOG")" in
    green) echo "GATE EXIT=0"   >> "$LOG"; exit 0 ;;
    red)   echo "GATE EXIT=$rc" >> "$LOG"; exit "$rc" ;;
  esac

  echo "== attempt $i produced no verdict (rc=$rc) — the box took it; retrying" >> "$LOG"
  i=$((i + 1))
done

echo "GATE EXIT=99  (no verdict in $N attempts)" >> "$LOG"
exit 99
