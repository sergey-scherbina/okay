#!/usr/bin/env bash
#
# The watchdog in `gate.sh`, exercised in seconds instead of in an
# hour — and in BOTH directions, because a guard that fires on
# everything is worse than no guard.
#
#   1  a build that goes SILENT AND IDLE must be killed, with the
#      evidence written beside its log, and reported as STALLED
#   2  a build that is silent because it is WORKING must survive, and
#      the run must reach its ordinary verdict
#   3  the stall must be killed by PID all the way down: the fake's
#      `sleep` child must be gone afterwards
#
# Run it after touching anything in gate.sh's watchdog:
#   scripts/gate-selftest.sh
set -uo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
# BOTH INVOCATIONS, because they are not the same shell and the repo
# uses the second: AGENTS.md says `sh scripts/gate.sh`, while the
# shebang says bash. The first cut of the watchdog passed through the
# shebang and died under /bin/sh on a `case` inside a command
# substitution, which is the whole reason this loop exists.
: "${GATE_SELFTEST_SHELL:=}"
run_gate() { if [ -n "$GATE_SELFTEST_SHELL" ]; then "$GATE_SELFTEST_SHELL" "$here/gate.sh" "$@";
             else "$here/gate.sh" "$@"; fi; }
tmp="$(mktemp -d -t gate-selftest)"
fail=0
say() { printf '%s\n' "$*"; }
ok()  { say "  ok   — $*"; }
bad() { say "  FAIL — $*"; fail=1; }

# ---------------------------------------------------------------- 1. the stall
say "1. a silent, idle build is STALLED, killed, and leaves evidence"
out="$tmp/stall.out"
GATE_SBT="$here/fake-sbt-stall.sh" GATE_STALL_SECS=6 GATE_TICK_SECS=2 GATE_STALL_CPU=5 \
  GATE_LOG="$tmp/stall.log" run_gate test > "$out" 2>&1
rc=$?
[ "$rc" -eq 124 ] && ok "exit 124" || bad "exit was $rc, expected 124"
grep -q "gate: STALLED" "$out" && ok "said STALLED" || bad "no STALLED line"
grep -q "gate: RED" "$out"    && bad "said RED — gate-retry would refuse to retry it" || ok "did not say RED"
grep -q "gate: KILLED" "$out" && bad "said KILLED — that is the signal branch" || ok "did not say KILLED"
[ -s "$tmp/stall.log.stall.ps" ] && ok "wrote the process tree" || bad "no .stall.ps evidence"
# the fake's `sleep` must be gone: the kill walked the tree
if pgrep -f "$here/fake-sbt-stall.sh" > /dev/null 2>&1; then
  bad "the fake build is still alive"
else ok "the tree was killed by pid"; fi

# ---------------------------------------------------------------- 2. the control
say "2. a silent but BUSY build survives the same thresholds"
out2="$tmp/busy.out"
GATE_SBT="$here/fake-sbt-busy.sh" GATE_STALL_SECS=6 GATE_TICK_SECS=2 GATE_STALL_CPU=3 \
  GATE_LOG="$tmp/busy.log" run_gate test > "$out2" 2>&1
rc2=$?
grep -q "gate: STALLED" "$out2" && bad "killed a working build — the CPU signal did not hold" \
  || ok "not stalled"
[ "$rc2" -eq 0 ] && ok "reached a verdict (exit 0)" || bad "exit was $rc2, expected 0"
grep -q "gate: GREEN" "$out2" && ok "said GREEN" || bad "no GREEN line"

say ""
# and now the same suite under the OTHER shell, once
if [ -z "$GATE_SELFTEST_SHELL" ] && [ "$fail" -eq 0 ]; then
  say ""
  say "3. the whole suite again under /bin/sh, the way AGENTS.md invokes it"
  if GATE_SELFTEST_SHELL=/bin/sh "$0" > "$tmp/sh.out" 2>&1; then
    ok "passes under /bin/sh too"
  else
    bad "FAILS under /bin/sh — output:"; sed 's/^/      /' "$tmp/sh.out"
  fi
fi

[ "$fail" -eq 0 ] && { say "gate-selftest: PASS"; exit 0; }
say "gate-selftest: FAIL — output kept in $tmp"; exit 1
