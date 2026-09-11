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
# produced NO verdict, which is what a killed run looks like, is worth
# starting again. Retrying a red would be a machine for landing broken
# trees, which is the opposite of what a gate is for.
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
quiet() {
  L=$(sysctl -n vm.loadavg | awk '{print int($2)}')
  H=$(ps -eo rss,args | grep "sbt.script" | grep -v grep | awk '$1 > 1000000' | wc -l | tr -d ' ')
  F=$(vm_stat | awk '/Pages free|Pages inactive|Pages speculative/ {gsub("\\.","",$NF); s+=$NF} END {print int(s*16384/1073741824)}')
  [ "$H" -eq 0 ] && [ "$L" -lt 15 ] && [ "$F" -ge 16 ]
}

# What one attempt's log says. This is the whole safety property, so it
# is one function and `--read` runs exactly it: `done` means the gate
# reached a verdict and nobody may start it again, `retry` means the
# attempt said nothing at all, which is what a killed run looks like.
verdict() {
  if grep -q "gate: GREEN" "$1"; then echo green
  elif grep -q "gate: RED" "$1"; then echo red
  else echo none
  fi
}

if [ "${1:-}" = "--read" ]; then
  L="${2:?--read needs a log}"
  case "$(verdict "$L")" in
    green) echo "$L: gate: GREEN — done, exit 0" ;;
    red)   echo "$L: gate: RED — done, the gate's exit code stands; NOT retried" ;;
    none)  echo "$L: no verdict — the box took it; retry" ;;
  esac
  exit 0
fi

if [ "${1:-}" = "--probe" ]; then
  if quiet; then v=quiet; else v=busy; fi
  echo "box: $v  (heavy-jvm=$H load=$L freeGB=$F; wants heavy=0 load<15 free>=16)"
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
  w=0
  while [ "$w" -lt 60 ]; do
    quiet && break
    sleep 30
    w=$((w + 1))
  done

  echo "== attempt $i at $(date +%H:%M) load $(sysctl -n vm.loadavg)" >> "$LOG"
  ( cd "$WT" && sh scripts/gate.sh ) >> "$LOG" 2>&1 && rc=0 || rc=$?

  case "$(verdict "$LOG")" in
    green) echo "GATE EXIT=0"   >> "$LOG"; exit 0 ;;
    red)   echo "GATE EXIT=$rc" >> "$LOG"; exit "$rc" ;;
  esac

  echo "== attempt $i produced no verdict (rc=$rc) — the box took it; retrying" >> "$LOG"
  i=$((i + 1))
done

echo "GATE EXIT=99  (no verdict in $N attempts)" >> "$LOG"
exit 99
