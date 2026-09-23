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
# GATE_STALL_CPU=0 — "burned ANY cpu at all", not "burned 3 seconds".
# The threshold that matters in production is proportional (5 s over
# 8 minutes, ~1%); asserting 3 s over a 6 s window is asserting about
# the SCHEDULER, and it failed the first time this suite was run
# beside another gate — the busy fake was real but starved. The
# distinction the watchdog actually makes is stalled=0 against
# working>0, and that is what this checks.
GATE_SBT="$here/fake-sbt-busy.sh" GATE_STALL_SECS=6 GATE_TICK_SECS=2 GATE_STALL_CPU=0 \
  GATE_LOG="$tmp/busy.log" run_gate test > "$out2" 2>&1
rc2=$?
grep -q "gate: STALLED" "$out2" && bad "killed a working build — the CPU signal did not hold" \
  || ok "not stalled"
[ "$rc2" -eq 0 ] && ok "reached a verdict (exit 0)" || bad "exit was $rc2, expected 0"
grep -q "gate: GREEN" "$out2" && ok "said GREEN" || bad "no GREEN line"

# ------------------------------------------ 4. a busy host over idle children
say "4. idle CHILDREN under a sbt that still burns a little CPU are STALLED"
# gate-watchdog-idle-sbt-cpu: the whole-tree sum counted sbt's own
# idle overhead (~3 s/min) as work, so a hang of every child read as
# "still working" for ever. The host's CPU has its own, higher bar.
out4="$tmp/idlekids.out"
GATE_SBT="$here/fake-sbt-idle-children.sh" GATE_STALL_SECS=6 GATE_TICK_SECS=2 \
  GATE_STALL_CPU=0 GATE_STALL_HOST_CPU=1000 FAKE_SPIN=100000000 \
  GATE_LOG="$tmp/idlekids.log" run_gate test > "$out4" 2>&1
rc4=$?
[ "$rc4" -eq 124 ] && ok "exit 124" || bad "exit was $rc4, expected 124 — the host's own CPU hid the hang"
grep -q "gate: STALLED" "$out4" && ok "said STALLED" || bad "no STALLED line"
if pgrep -f "$here/fake-sbt-idle-children.sh" > /dev/null 2>&1; then
  bad "the fake build is still alive"
else ok "the tree was killed by pid"; fi

say "5. the same shape with the host WORKING (a cold compile: sbt busy, no children busy) survives"
out5="$tmp/busyhost.out"
GATE_SBT="$here/fake-sbt-idle-children.sh" GATE_STALL_SECS=6 GATE_TICK_SECS=2 \
  GATE_STALL_CPU=0 GATE_STALL_HOST_CPU=0 \
  GATE_LOG="$tmp/busyhost.log" run_gate test > "$out5" 2>&1
rc5=$?
grep -q "gate: STALLED" "$out5" && bad "killed a compiling host" || ok "not stalled"
[ "$rc5" -eq 0 ] && ok "reached a verdict (exit 0)" || bad "exit was $rc5, expected 0"

say "6. a \";\"-chained command reaches sbt as SEPARATE commands, none dropped"
# gate-command-chain: `gate.sh "a; b"` handed sbt ONE argument and sbt
# ran `a` alone — "0 test results" in the verdict was the only tell.
out6="$tmp/chain.out"
GATE_SBT="$here/fake-sbt-args.sh" GATE_LOG="$tmp/chain.log" \
  run_gate "okayJVM/testOnly A; ; okayJS/testOnly B ;okayNative/test" > "$out6" 2>&1
rc6=$?
got=$(grep -o 'fake-sbt-arg: <[^>]*>' "$tmp/chain.log" | tr '\n' '|')
want='fake-sbt-arg: <okayJVM/testOnly A>|fake-sbt-arg: <okayJS/testOnly B>|fake-sbt-arg: <okayNative/test>|'
[ "$got" = "$want" ] && ok "three commands, in order, trimmed" || bad "sbt was handed: $got"
[ "$rc6" -eq 0 ] && ok "exit 0" || bad "exit was $rc6, expected 0"
out7="$tmp/single.out"
GATE_SBT="$here/fake-sbt-args.sh" GATE_LOG="$tmp/single.log" \
  run_gate "okayJVM/testOnly A" > "$out7" 2>&1
got=$(grep -o 'fake-sbt-arg: <[^>]*>' "$tmp/single.log" | tr '\n' '|')
[ "$got" = 'fake-sbt-arg: <okayJVM/testOnly A>|' ] && ok "a plain command is untouched" || bad "sbt was handed: $got"

say "7. a chain with NO command in it is refused, not handed to sbt"
# With zero arguments real sbt opens its INTERACTIVE shell and the gate
# waits on it for ever — found by trying `gate.sh "; ;"` on this lane.
out8="$tmp/empty.out"
GATE_SBT="$here/fake-sbt-args.sh" GATE_LOG="$tmp/empty.log" run_gate " ; ; " > "$out8" 2>&1
rc8=$?
[ "$rc8" -eq 2 ] && ok "exit 2" || bad "exit was $rc8, expected 2"
grep -q "Passed: Total" "$tmp/empty.log" 2>/dev/null && bad "sbt was started anyway" || ok "sbt was not started"

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
