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
# the fakes compile nothing real: the stack-recursion guard (gate.sh 3b)
# has no classes to read here, and is exercised by recscan-check.sh itself
export GATE_RECSCAN=0
run_gate() { if [ -n "$GATE_SELFTEST_SHELL" ]; then "$GATE_SELFTEST_SHELL" "$here/gate.sh" "$@";
             else "$here/gate.sh" "$@"; fi; }
tmp="$(mktemp -d -t gate-selftest)"
# the fixture's own bench window (specs/bench-window.md): a JMH lane
# really queued on this box must not hold the selftest's gates
export OKAY_BENCH_DIR="$tmp/bench" OKAY_BENCH_POLL=1
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

say "8. \`affected <ref> staged\` is two phases with the order on both; four arguments pass through"
# ci-staged: the pre-merge gate's order is sbt's FOURTH argument, so the
# JVM-first split has to carry it on each phase — a first cut matched
# `*" staged"` alone and would have rewritten `affected master test all
# staged` into `affected master test all test jvm staged`.
GATE_SBT="$here/fake-sbt-args.sh" GATE_LOG="$tmp/staged.log" run_gate "affected master staged" > "$tmp/staged.out" 2>&1
got=$(grep -o 'fake-sbt-arg: <[^>]*>' "$tmp/staged.log" | tr '\n' '|')
want='fake-sbt-arg: <affected master test jvm staged>|fake-sbt-arg: <affected master test rest staged>|'
[ "$got" = "$want" ] && ok "two phases, JVM first, order on both" || bad "sbt was handed: $got"
GATE_SBT="$here/fake-sbt-args.sh" GATE_LOG="$tmp/staged4.log" run_gate "affected master test all staged" > "$tmp/staged4.out" 2>&1
got=$(grep -o 'fake-sbt-arg: <[^>]*>' "$tmp/staged4.log" | tr '\n' '|')
[ "$got" = 'fake-sbt-arg: <affected master test all staged>|' ] && ok "four arguments pass through untouched" || bad "sbt was handed: $got"
GATE_SBT="$here/fake-sbt-args.sh" GATE_LOG="$tmp/closed.log" run_gate "affected master" > "$tmp/closed.out" 2>&1
got=$(grep -o 'fake-sbt-arg: <[^>]*>' "$tmp/closed.log" | tr '\n' '|')
[ "$got" = 'fake-sbt-arg: <affected master test jvm>|fake-sbt-arg: <affected master test rest>|' ] && ok "the plain form is unchanged" || bad "sbt was handed: $got"

say "9. the lost-process classifier, over real logs, in both directions"
# native-accept-timeout: shape C (a Native binary that never connected
# within ComRunner's 40 s accept, killed by the adapter) was read as
# "a failure this script does not recognise", and the ci-runner reverted
# a green lane for it. The fixtures are the verbatim lines of real logs.
fx="$here/gate-fixtures"
read_gate() { run_gate --read "$fx/$1" > "$tmp/$1.out" 2>&1; echo $?; }
rc=$(read_gate shape-c-accept-timeout.log)
grep -q "lost a test process" "$tmp/shape-c-accept-timeout.log.out" && grep -q "okayChainNative" "$tmp/shape-c-accept-timeout.log.out" \
  && [ "$rc" -eq 0 ] && ok "shape C is a lost process, and would be re-run alone" || bad "shape C: rc=$rc, $(grep '^gate:' "$tmp/shape-c-accept-timeout.log.out" | tail -1)"
rc=$(read_gate shape-c-beside-a-real-failure.log)
grep -q "gate: RED — tests failed" "$tmp/shape-c-beside-a-real-failure.log.out" && grep -q "==> X okay.foo.TestBar" "$tmp/shape-c-beside-a-real-failure.log.out" \
  && ! grep -q "lost a test process" "$tmp/shape-c-beside-a-real-failure.log.out" \
  && [ "$rc" -ne 0 ] && ok "shape C beside a real ==> X stays RED, naming the real one" || bad "a real failure was excused: rc=$rc"
rc=$(read_gate loaded-frameworks-no-accept-timeout.log)
grep -q "does not recognise" "$tmp/loaded-frameworks-no-accept-timeout.log.out" && [ "$rc" -ne 0 ] \
  && ok "loadedTestFrameworks WITHOUT the accept timeout stays RED" || bad "an unexplained shape was excused: rc=$rc"
rc=$(read_gate shape-b-run-terminated.log)
grep -q "lost a test process" "$tmp/shape-b-run-terminated.log.out" && grep -q "okayLexNative" "$tmp/shape-b-run-terminated.log.out" \
  && [ "$rc" -eq 0 ] && ok "shape B is still a lost process" || bad "shape B regressed: rc=$rc"
# the whole path, not --read: a box busy enough to starve the matrix's
# binary starves the rerun's too, and that is KILLED (retried), not RED
# (which ci-runner bisects and reverts)
GATE_SBT="$here/fake-sbt-accept-timeout.sh" GATE_LOG="$tmp/accept.log" run_gate test > "$tmp/accept.out" 2>&1
rc=$?
grep -q "gate: KILLED" "$tmp/accept.out" && ! grep -q "gate: RED" "$tmp/accept.out" && [ "$rc" -eq 137 ] \
  && ok "a rerun lost to the same accept timeout is KILLED, not RED" || bad "rc=$rc, $(grep '^gate:' "$tmp/accept.out" | tail -1)"
[ "$(sh "$here/gate-retry.sh" --read "$tmp/accept.out")" = "$tmp/accept.out: gate: KILLED — a signal, not a verdict; retry" ] \
  && ok "and gate-retry retries it" || bad "gate-retry reads it as: $(sh "$here/gate-retry.sh" --read "$tmp/accept.out")"

say "10. the bench window: a queued benchmark holds the gate's start; a --read takes no token"
# a request whose owner lives 3 s: the gate waits for it, then runs
sleep 3 & lane=$!
mkdir -p "$OKAY_BENCH_DIR/want"; : > "$OKAY_BENCH_DIR/want/$lane"
start=$(date +%s)
GATE_SBT="$here/fake-sbt-args.sh" GATE_LOG="$tmp/bw.log" OKAY_BENCH_GATE_MAX_WAIT=30 \
  run_gate "okayJVM/testOnly A" > "$tmp/bw.out" 2>&1
rc9=$?; took=$(( $(date +%s) - start ))
grep -q "a benchmark is queued (pid $lane" "$tmp/bw.out" && ok "said it was held for the benchmark" || bad "did not: $(cat "$tmp/bw.out")"
[ "$took" -ge 2 ] && ok "started after the benchmark (${took}s)" || bad "started at once (${took}s)"
[ "$rc9" -eq 0 ] && grep -q "fake-sbt-arg" "$tmp/bw.log" && ok "then ran" || bad "did not run (exit $rc9)"
[ -z "$(ls "$OKAY_BENCH_DIR/gates" 2>/dev/null)" ] && ok "its token is gone" || bad "token left behind"
run_gate --read "$tmp/bw.log" > /dev/null 2>&1
[ -z "$(ls "$OKAY_BENCH_DIR/gates" 2>/dev/null)" ] && ok "a --read took no token" || bad "--read left a token"

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
