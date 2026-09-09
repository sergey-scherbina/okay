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
  status=$(grep -cE "^\[error\]" "$log" > /dev/null && echo 1 || echo 0)
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
[ $status -eq 0 ] && { echo "gate: GREEN"; exit 0; }

# a real test failure ends it here
if grep -q "==> X" "$clean"; then
  echo "gate: RED — tests failed:"
  grep "==> X" "$clean" | head -20
  exit $status
fi

# the signature: modules that errored with nothing failed
lost=$(grep -E "^\[error\] \(.*Test / test\) sbt.TestsFailedException" "$clean" \
      | sed -E 's/^\[error\] \(([^ ]+) \/ Test \/ test\).*/\1/' | sort -u)
if [ -z "$lost" ] || ! grep -qE "^\[error\] Error: Total [0-9]+, Failed 0, Errors [1-9]" "$clean"; then
  echo "gate: RED — a failure this script does not recognise; read $log"
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
