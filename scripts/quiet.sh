#!/bin/sh
# quiet.sh — "is the box quiet enough to start something heavy" — sourced
# by scripts/gate-retry.sh and scripts/jmh-lane.sh, so the ONE box-specific
# threshold set lives in ONE place (policy's own P-6: duplicated logic
# needs one vocabulary on both sides, or it is two guards, not one).
#
#   . "$(dirname "$0")/quiet.sh"
#   if quiet; then ...           # sets $L $H $F as a side effect
#
#   sh scripts/quiet.sh --probe   # prints the reading and exits 0/1
#
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
# box "busy" by itself. This is also the fix a JMH benchmark run needs
# for the identical reason (per-lane-gated-jmh, 2026-09-23): the
# 1-minute load average LAGS a back-to-back gate by minutes and never
# bottoms out between them, so counting it alone either never fires or
# fires into the next storm — count only sbt processes ACTUALLY
# burning CPU right now (`pcpu > 20`), never the load average alone.
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
quiet() {
  L=$(sysctl -n vm.loadavg | awk '{print int($2)}')
  H=$(ps -eo pcpu,rss,args | grep "sbt.script" | grep -v grep | awk '$1 > 20 && $2 > 1000000' | wc -l | tr -d ' ')
  F=$(vm_stat | awk '/Pages free|Pages inactive|Pages speculative/ {gsub("\\.","",$NF); s+=$NF} END {print int(s*16384/1073741824)}')
  [ "$H" -eq 0 ] && [ "$L" -lt 15 ] && [ "$F" -ge 10 ]
}

# KILL A PROCESS TREE BY PID, never by name (AGENTS.md is explicit,
# and the incident it comes from cost a full matrix). Depth first, so
# a child cannot be reparented away while its parent is still alive.
kill_tree() {
  for c in $(pgrep -P "$1" 2>/dev/null); do kill_tree "$c"; done
  kill "$1" 2>/dev/null
}

# Only when EXECUTED directly, never when SOURCED — `$0` stays the
# sourcing script's name across a `. path/quiet.sh`, so this must key
# off `$0`, not `$1` (which is the sourcing script's own argument and
# has nothing to do with this file). Sourced by gate-retry.sh, whose
# OWN `$1` is legitimately `--probe` for an unrelated reason.
case "$(basename "$0" 2>/dev/null)" in
  quiet.sh)
    if [ "${1:-}" = "--probe" ]; then
      if quiet; then v=quiet; else v=busy; fi
      echo "box: $v  (busy-sbt=$H load=$L freeGB=$F; wants busy-sbt=0 load<15 free>=10)"
      [ "$v" = quiet ]
      exit $?
    fi
    ;;
esac
