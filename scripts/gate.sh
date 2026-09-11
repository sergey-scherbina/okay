#!/usr/bin/env bash
#
# The full matrix, with the one false failure it is known to produce
# told apart from a real one.
#
# TWICE on 2026-09-09 a matrix ended red with NO failed test: a Native
# module reported `Error: Total 110, Failed 0, Errors 1` and named the
# suite that happened to be running, having run fewer tests than that
# module has. Alone, the module passed. Both times the box was paging
# under sibling builds. It is a LOST TEST PROCESS, and the runner says
# nothing else about it: no exception, no output, no stack.
#
# What is ruled out, so nobody re-runs these (native-runner-error):
#   - the RAM guard: `killed=0` in its log at both minutes
#   - an OS kill: the kernel's memorystatus log for that window has
#     only idle-exit of system daemons, nothing of ours
#   - CPU pressure alone: 13 Native modules in parallel under 42
#     burners, four rounds, all green; and 5 modules under 28, green
#
# It takes two shapes, both handled below; the third occurrence was of
# the second, and this script missed it until it was taught.
#
# So this script does not pretend to fix it. It makes the gate SAY it:
# a failure that carries this signature and nothing else is re-run for
# the affected projects ALONE, and the outcome is printed either way.
#
# It never hides a real failure. A single `==> X` anywhere, or a
# failure whose module reported `Failed` above zero, is final: no
# rerun, and the exit code stands.
#
# Usage: scripts/gate.sh [sbt-command]          (default: test)
#        scripts/gate.sh --read <log>           read a gate log that
#          already exists and say what it would have done — which is
#          how the three branches below are tested without waiting for
#          the failure to happen again
set -uo pipefail

replay=""
if [ "${1:-}" = "--read" ]; then replay="${2:?--read needs a log}"; fi

if [ -n "$replay" ]; then
  log="$replay"
  # A log does not carry the exit status of the process that wrote it,
  # and one of the branches below is keyed on exactly that (a signal).
  # `GATE_STATUS=143 scripts/gate.sh --read <log>` is how that branch
  # is exercised without waiting to be killed again.
  status="${GATE_STATUS:-$(grep -cE "^\[error\]" "$log" > /dev/null && echo 1 || echo 0)}"
  echo "gate: reading $log (no sbt run)"
else
  cmd="${1:-test}"
  log="${GATE_LOG:-$(mktemp -t okay-gate)}"
  echo "gate: sbt $cmd  (log: $log)"
  sbt "$cmd" > "$log" 2>&1
  status=$?
fi
# ONE stripped copy, then plain greps over the FILE. Not a pipeline:
# `set -o pipefail` plus `grep -q` reports failure even on a match,
# because grep exits early and the writer takes a SIGPIPE — which is
# how the first cut of this script called every real failure
# "unrecognised" (native-runner-error, 2026-09-09).
clean="${log}.clean"
perl -pe 's/\e\[[0-9;]*m//g' "$log" > "$clean"
strip() { cat "$clean"; }

tests=$(awk '/Passed: Total|Error: Total/ {for (i=1;i<=NF;i++) if ($i=="Total") {gsub(",","",$(i+1)); s+=$(i+1)}} END {print s+0}' "$clean")
echo "gate: sbt exited $status, $tests test results"

# ORDER MATTERS HERE, and each step earns its place.
#
#   1  a failed test is final, whatever else happened afterwards
#   2  a SIGNAL is not a verdict, and its log is TRUNCATED — so it is
#      answered before anything reads that log for absences
#   3  warnings, which only a log of a finished run can be trusted on
#   4  green

# 1. A REAL TEST FAILURE ENDS IT, whatever the exit status was: a
# suite that failed and was then killed is red, not killed.
if grep -q "==> X" "$clean"; then
  echo "gate: RED — tests failed:"
  grep "==> X" "$clean" | head -20
  exit "${status:-1}"
fi

# 2. A SIGNAL IS NOT A VERDICT.
#
# 143 is SIGTERM and 137 is SIGKILL, and on this box they come from
# launchd's RAM guard and idle reaper, not from anything about the
# tree (AGENTS.md, "THE 143, SOLVED"; scripts/gate-sentinels.sh tells
# an external signal from a process-group kill by blast radius).
#
# This mattered more than a wrong word. `scripts/gate-retry.sh` retries
# a run that produced NO VERDICT and passes a `gate: RED` straight
# through, deliberately — a loop that re-rolls a red is a machine for
# landing broken trees. Calling a kill RED therefore disabled the
# retry in exactly the case it was written for: measured 2026-09-11, a
# matrix died at 147 module compiles with zero `==> X`, zero `[error]`
# lines and the log simply stopping mid-suite, and the loop reported it
# as a failure of the tree.
#
# Nothing else may read this log for an ABSENCE, which is why this
# comes before the warning check: half a matrix that warned about
# nothing has not told you the tree is clean.
if [ "$status" -eq 143 ] || [ "$status" -eq 137 ]; then
  echo "gate: KILLED — sbt took signal $((status - 128)) and no test failed"
  echo "gate: this is NOT a verdict about the tree; run it again on a quiet box"
  exit "$status"
fi

# 3. WARNINGS, which this script did not look at until 2026-09-11 and
# which AGENTS.md has required all along ("no warnings, ever"). Three
# unused imports in okay-openapi and one in the core's own tests had
# ridden through every green gate.
#
# Two facts decide the shape. A warning is a COMPILE diagnostic, so a
# warm run emits none and its silence is not evidence — the script
# says which case it is in rather than letting a warm pass look like a
# clean one. And a lane's gate runs in a fresh worktree, where nothing
# is compiled yet, so the run that decides a landing is exactly the run
# that sees them.
#
# The signature is dotty's own diagnostic header, `[warn] -- [Exxx]`,
# and not any line sbt happens to call a warning: a resolution note or
# "multiple main classes" is not what the rule is about.
#
# ONE FALSE POSITIVE IS KNOWN, and deleting the import it names breaks
# the build. E198 "unused import" fired on `import okay.RowLift.{at as
# liftAt, plus}` in the core's own tests, where `liftAt` IS used —
# removing it failed with E008 "value liftAt is not a member of". A
# RENAMED import reached only in extension-selection position is not
# counted as used. The fix is to drop the RENAME, not the import
# (`{at, plus}` and `.at[...]`, which compiles clean), and it is
# written here so the next person does not delete a line the compiler
# pointed at and then wonder why nothing builds.
warns=$(grep -cE "^\[warn\] -- " "$clean")
compiled=$(grep -cE "^\[info\] compiling " "$clean")
if [ "$compiled" -eq 0 ]; then
  echo "gate: warnings NOT checked — nothing was compiled (a warm run says nothing about them)"
elif [ "$warns" -gt 0 ]; then
  echo "gate: RED — $warns compile warning(s) over $compiled module compile(s); 'no warnings, ever' (AGENTS.md):"
  grep -E "^\[warn\] -- " "$clean" | sed 's/^/  /' | head -20
  exit 1
else
  echo "gate: no compile warnings ($compiled module compile(s) looked at)"
fi

# 4. GREEN
[ "$status" -eq 0 ] && { echo "gate: GREEN"; exit 0; }

# THE TWO SHAPES A LOST TEST PROCESS TAKES (native-runner-error).
#
#   A  the process ran some tests and then went: the module reports
#      `Error: Total N, Failed 0, Errors 1` and names the suite that
#      was in flight
#   B  it went before it said anything at all, so there is no report
#      to speak of — only `(<m> / Test / executeTests)` carrying
#      scala-native's `RunTerminatedException` (its RPC channel closed
#      under it). Filed by the `failing-over` gate, 2026-09-09 18:55,
#      after this script called it "unrecognised" and re-ran nothing.
#
# Conservative on purpose: a project that failed in NEITHER shape
# means we do not understand this red, so nothing is re-run.
lost_a=$(grep -E "^\[error\] \(.*Test / test\) sbt.TestsFailedException" "$clean" \
      | sed -E 's/^\[error\] \(([^ ]+) \/ Test \/ test\).*/\1/' | sort -u)
lost_b=$(grep -E "^\[error\] \(.*Test / executeTests\).*(RunTerminatedException|RPCCore\$ClosedException)" "$clean" \
      | sed -E 's/^\[error\] \(([^ ]+) \/ Test \/ executeTests\).*/\1/' | sort -u)
# shape A must carry its own report line, or it is not shape A
if [ -n "$lost_a" ] && ! grep -qE "^\[error\] Error: Total [0-9]+, Failed 0, Errors [1-9]" "$clean"; then
  lost_a=""
fi
lost=$(printf '%s\n%s\n' "$lost_a" "$lost_b" | grep -v '^$' | sort -u)
failed_projects=$(grep -E "^\[error\] \([^)]*Test / (test|executeTests)\)" "$clean" \
      | sed -E 's/^\[error\] \(([^ ]+) \/ Test \/ (test|executeTests)\).*/\1/' | sort -u)
unknown=$(comm -23 <(printf '%s\n' "$failed_projects" | grep -v '^$') <(printf '%s\n' "$lost" | grep -v '^$'))
if [ -z "$lost" ] || [ -n "$unknown" ]; then
  echo "gate: RED — a failure this script does not recognise; read $log"
  [ -n "$unknown" ] && { echo "gate: these failed in no known shape:"; echo "$unknown" | sed 's/^/  /'; }
  grep -E "^\[error\]" "$clean" | head -20
  exit $status
fi

echo "gate: no test failed, and these modules lost a test process:"
echo "$lost" | sed 's/^/  /'
if [ -n "$replay" ]; then
  echo "gate: (--read) would re-run those alone and report the outcome"
  exit 0
fi
echo "gate: re-running exactly those, alone, to say which it was"
rerun=""
for p in $lost; do rerun="$rerun $p/test"; done
rlog="${log}.rerun"
# shellcheck disable=SC2086
sbt $rerun > "$rlog" 2>&1
rstatus=$?
if [ $rstatus -eq 0 ]; then
  echo "gate: GREEN AFTER RERUN — the matrix's only failure was a lost process in:"
  echo "$lost" | sed 's/^/  /'
  echo "gate: (rerun log: $rlog) — record the recurrence in BACKLOG's native-runner-error entry"
  exit 0
fi
echo "gate: RED — the rerun failed too, so this is not the known signature:"
perl -pe 's/\e\[[0-9;]*m//g' "$rlog" > "${rlog}.clean"
grep -E "==> X|^\[error\]" "${rlog}.clean" | head -20
exit $rstatus
