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
# and its own ci lock (ci-runner-lock-bypass): a whole build (`test`,
# `family …`) takes .work/ci/lock, and a real runner on this box must
# neither hold these fakes back nor be held back by them
export OKAY_CI_LOCK_DIR="$tmp/cilock"
fail=0
say() { printf '%s\n' "$*"; }
ok()  { say "  ok   — $*"; }
bad() { say "  FAIL — $*"; fail=1; }

# ---------------------------------------------------------------- 1. the stall
say "1. a silent, idle build is STALLED, killed, and leaves evidence"
out="$tmp/stall.out"
GATE_SBT="$here/fake-sbt-stall.sh" GATE_STALL_SECS=6 GATE_TICK_SECS=2 GATE_STALL_CPU=5 \
  GATE_LOG="$tmp/stall.log" run_gate "okayJVM/test" > "$out" 2>&1
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
  GATE_LOG="$tmp/busy.log" run_gate "okayJVM/test" > "$out2" 2>&1
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
  GATE_LOG="$tmp/idlekids.log" run_gate "okayJVM/test" > "$out4" 2>&1
rc4=$?
[ "$rc4" -eq 124 ] && ok "exit 124" || bad "exit was $rc4, expected 124 — the host's own CPU hid the hang"
grep -q "gate: STALLED" "$out4" && ok "said STALLED" || bad "no STALLED line"
if pgrep -f "$here/fake-sbt-idle-children.sh" > /dev/null 2>&1; then
  bad "the fake build is still alive"
else ok "the tree was killed by pid"; fi

say "5. the same shape with the host WORKING (a cold compile: sbt busy, no children busy) survives"
out5="$tmp/busyhost.out"
FAKE_SPIN="${GATE_SELFTEST_SPIN:-3000000}" GATE_SBT="$here/fake-sbt-idle-children.sh" GATE_STALL_SECS=6 GATE_TICK_SECS=2 \
  GATE_STALL_CPU=0 GATE_STALL_HOST_CPU=0 \
  GATE_LOG="$tmp/busyhost.log" run_gate "okayJVM/test" > "$out5" 2>&1
rc5=$?
grep -q "gate: STALLED" "$out5" && bad "killed a compiling host" || ok "not stalled"
[ "$rc5" -eq 0 ] && ok "reached a verdict (exit 0)" || bad "exit was $rc5, expected 0"

say "5b. a host burning well under a second per window is still WORKING (CPU counted below whole seconds)"
out5b="$tmp/lighthost.out"
GATE_SBT="$here/fake-sbt-light-host.sh" GATE_STALL_SECS=6 GATE_TICK_SECS=2 \
  GATE_STALL_CPU=0 GATE_STALL_HOST_CPU=0 \
  GATE_LOG="$tmp/lighthost.log" run_gate "okayJVM/test" > "$out5b" 2>&1
rc5b=$?
grep -q "gate: STALLED" "$out5b" && bad "killed a lightly working host: $(grep STALLED "$out5b")" || ok "not stalled"
[ "$rc5b" -eq 0 ] && ok "reached a verdict (exit 0)" || bad "exit was $rc5b, expected 0"

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

say "6b. \`affected <ref> [staged]\` inside a chain is expanded IN PLACE, the rest of the chain intact"
# gate-affected-short-form-in-chain: the expansion matched the WHOLE
# argument, so inside a chain `affected master staged` reached sbt raw
# and was refused ("Not a valid key: staged") — foreign-one-r, 2026-09-26
GATE_SBT="$here/fake-sbt-args.sh" GATE_LOG="$tmp/chain-affected.log" \
  run_gate "affected master staged; okayDeploy/testOnly X; affected master" > "$tmp/chain-affected.out" 2>&1
rc6b=$?
got=$(grep -o 'fake-sbt-arg: <[^>]*>' "$tmp/chain-affected.log" | tr '\n' '|')
want='fake-sbt-arg: <affected master test jvm staged>|fake-sbt-arg: <affected master test js staged>|fake-sbt-arg: <affected master test native staged>|fake-sbt-arg: <okayDeploy/testOnly X>|fake-sbt-arg: <affected master test jvm>|fake-sbt-arg: <affected master test js>|fake-sbt-arg: <affected master test native>|'
[ "$got" = "$want" ] && ok "both affected parts expanded, in order, around the plain one" || bad "sbt was handed: $got"
[ "$rc6b" -eq 0 ] && ok "exit 0" || bad "exit was $rc6b, expected 0"

say "7. a chain with NO command in it is refused, not handed to sbt"
# With zero arguments real sbt opens its INTERACTIVE shell and the gate
# waits on it for ever — found by trying `gate.sh "; ;"` on this lane.
out8="$tmp/empty.out"
GATE_SBT="$here/fake-sbt-args.sh" GATE_LOG="$tmp/empty.log" run_gate " ; ; " > "$out8" 2>&1
rc8=$?
[ "$rc8" -eq 2 ] && ok "exit 2" || bad "exit was $rc8, expected 2"
grep -q "Passed: Total" "$tmp/empty.log" 2>/dev/null && bad "sbt was started anyway" || ok "sbt was not started"

say "8. \`affected <ref> staged\` is three platforms with the order on each; four arguments pass through"
# ci-staged: the pre-merge gate's order is sbt's FOURTH argument, so the
# JVM-first split has to carry it on each phase — a first cut matched
# `*" staged"` alone and would have rewritten `affected master test all
# staged` into `affected master test all test jvm staged`.
GATE_SBT="$here/fake-sbt-args.sh" GATE_LOG="$tmp/staged.log" run_gate "affected master staged" > "$tmp/staged.out" 2>&1
got=$(grep -o 'fake-sbt-arg: <[^>]*>' "$tmp/staged.log" | tr '\n' '|')
want='fake-sbt-arg: <affected master test jvm staged>|fake-sbt-arg: <affected master test js staged>|fake-sbt-arg: <set Global / concurrentRestrictions := Seq(Tags.limitAll(1))>|fake-sbt-arg: <affected master test native staged>|'
[ "$got" = "$want" ] && ok "three platforms, JVM first, Native tasks bounded" || bad "sbt was handed: $got"
GATE_SBT="$here/fake-sbt-args.sh" GATE_LOG="$tmp/staged4.log" run_gate "affected master test all staged" > "$tmp/staged4.out" 2>&1
got=$(grep -o 'fake-sbt-arg: <[^>]*>' "$tmp/staged4.log" | tr '\n' '|')
[ "$got" = 'fake-sbt-arg: <affected master test all staged>|' ] && ok "four arguments pass through untouched" || bad "sbt was handed: $got"
GATE_SBT="$here/fake-sbt-args.sh" GATE_LOG="$tmp/closed.log" run_gate "affected master" > "$tmp/closed.out" 2>&1
got=$(grep -o 'fake-sbt-arg: <[^>]*>' "$tmp/closed.log" | tr '\n' '|')
[ "$got" = 'fake-sbt-arg: <affected master test jvm>|fake-sbt-arg: <affected master test js>|fake-sbt-arg: <set Global / concurrentRestrictions := Seq(Tags.limitAll(1))>|fake-sbt-arg: <affected master test native>|' ] && ok "the plain form is unchanged" || bad "sbt was handed: $got"

say "8b. managed full builds use fresh platform heaps and stop at the first failure"
GATE_SBT="$here/fake-sbt-args.sh" FAKE_SBT_REQUIRE_LOCK=1 GATE_LOG="$tmp/platform-all.log" run_gate "family all" > "$tmp/platform-all.out" 2>&1
rc=$?
[ "$rc" -eq 0 ] && ok "full split gate passes" || bad "split gate exit $rc"
pids=$(grep 'fake-sbt-pid:' "$tmp/platform-all.log" | sort -u | wc -l | tr -d ' ')
[ "$pids" -eq 3 ] && ok "three distinct sbt processes" || bad "process count $pids"
got=$(grep -o 'fake-sbt-arg: <[^>]*>' "$tmp/platform-all.log" | tr '\n' '|')
want='fake-sbt-arg: <family jvm>|fake-sbt-arg: <family js>|fake-sbt-arg: <set Global / concurrentRestrictions := Seq(Tags.limitAll(1))>|fake-sbt-arg: <family native>|'
[ "$got" = "$want" ] && ok "platform order and Native limit" || bad "full gate arguments: $got"
grep -q '3 test results' "$tmp/platform-all.out" && ok "all stage results retained" || bad "lost stage output"
FAKE_SBT_FAIL_COMMAND='family js' GATE_SBT="$here/fake-sbt-args.sh" GATE_LOG="$tmp/platform-red.log" run_gate "family all" > "$tmp/platform-red.out" 2>&1
rc=$?
[ "$rc" -ne 0 ] && ok "failed JS stops the gate" || bad "JS failure went green"
grep -q 'fake-sbt-arg: <family native>' "$tmp/platform-red.log" && bad "Native ran after failed JS" || ok "Native never started"
grep -q 'fake-sbt-arg: <family jvm>' "$tmp/platform-red.log" && ok "earlier stage retained on failure" || bad "lost earlier log"
FAKE_SBT_FAIL_COMMAND='family jvm' GATE_SBT="$here/fake-sbt-args.sh" GATE_LOG="$tmp/platform-jvm-red.log" run_gate test > "$tmp/platform-jvm-red.out" 2>&1
rc=$?
[ "$rc" -ne 0 ] && ! grep -q 'fake-sbt-arg: <family js>' "$tmp/platform-jvm-red.log" && ok "bare test stops after JVM failure" || bad "bare test did not fail fast"
GATE_SBT="$here/fake-sbt-args.sh" GATE_LOG="$tmp/build-entry.log" sh "$here/build.sh" all Test/compile > "$tmp/build-entry.out" 2>&1
rc=$?
[ "$rc" -eq 0 ] && grep -q 'fake-sbt-arg: <family native Test/compile>' "$tmp/build-entry.log" && ok "build entry point carries compile task" || bad "build entry failed $rc"
GATE_SBT="$here/fake-sbt-args.sh" GATE_LOG="$tmp/build-invalid.log" sh "$here/build.sh" all 'test; compile' > "$tmp/build-invalid.out" 2>&1
rc=$?
[ "$rc" -eq 2 ] && [ ! -f "$tmp/build-invalid.log" ] && ok "malformed task refused before sbt" || bad "invalid task ran"

GATE_SBT="$here/fake-sbt-args.sh" GATE_LOG="$tmp/build-default.log" sh "$here/build.sh" > "$tmp/build-default.out" 2>&1
rc=$?
got=$(grep -o 'fake-sbt-arg: <[^>]*>' "$tmp/build-default.log" | tr '\n' '|')
[ "$rc" -eq 0 ] && [ "$got" = 'fake-sbt-arg: <family jvm test>|' ] && ok "build defaults to JVM only" || bad "build default changed: $got"
GATE_SBT="$here/fake-sbt-args.sh" GATE_LOG="$tmp/test-default.log" run_gate test > "$tmp/test-default.out" 2>&1
rc=$?
got=$(grep -o 'fake-sbt-arg: <[^>]*>' "$tmp/test-default.log" | tr '\n' '|')
[ "$rc" -eq 0 ] && [ "$got" = 'fake-sbt-arg: <family jvm>|' ] && ok "managed bare test is JVM only" || bad "test default changed: $got"

holders=$(grep 'fake-lock-holder:' "$tmp/platform-all.log" | sort -u | wc -l | tr -d ' ')
[ "$holders" -eq 1 ] && [ ! -d "$OKAY_CI_LOCK_DIR" ] && ok "one lock held across platforms and released afterwards" || bad "platform lock lifetime changed"
mkdir "$tmp/other-build"
( cd "$tmp/other-build"
  GATE_SBT="$here/fake-sbt-args.sh" GATE_LOG="$tmp/other-test.log" run_gate test > "$tmp/other-test.out" 2>&1
)
rc=$?
got=$(grep -o 'fake-sbt-arg: <[^>]*>' "$tmp/other-test.log" | tr '\n' '|')
[ "$rc" -eq 0 ] && [ "$got" = 'fake-sbt-arg: <test>|' ] && ok "other builds retain raw test" || bad "non-family build was rewritten: $got"

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

say "11. a DEMOTED run whose only failures are munit timeouts is no verdict; anything else stays RED"
# gate-demote-timeouts: nine timeouts in seven modules on a tree that was
# green undemoted (2026-09-27); the run's own marker line is the key
rc=$(read_gate demoted-timeouts.log)
grep -q "gate: DEMOTED" "$tmp/demoted-timeouts.log.out" && ! grep -q "gate: RED" "$tmp/demoted-timeouts.log.out" \
  && [ "$rc" -eq 122 ] && ok "demoted + only timeouts: DEMOTED, exit 122, not RED" || bad "rc=$rc, $(grep '^gate:' "$tmp/demoted-timeouts.log.out" | head -1)"
[ "$(sh "$here/gate-retry.sh" --read "$tmp/demoted-timeouts.log.out")" = "$tmp/demoted-timeouts.log.out: gate: DEMOTED — a run on the efficiency cores that lost only to munit timeouts; not a verdict; retry" ] \
  && ok "and gate-retry retries it" || bad "gate-retry reads it as: $(sh "$here/gate-retry.sh" --read "$tmp/demoted-timeouts.log.out")"
rc=$(read_gate demoted-beside-a-real-failure.log)
grep -q "gate: RED — tests failed" "$tmp/demoted-beside-a-real-failure.log.out" && ! grep -q "gate: DEMOTED" "$tmp/demoted-beside-a-real-failure.log.out" \
  && [ "$rc" -ne 0 ] && ok "a real failure beside the timeouts stays RED" || bad "a real failure was excused by the demotion: rc=$rc"
grep -v "demoted to the efficiency cores" "$fx/demoted-timeouts.log" > "$tmp/undemoted-timeouts.log"
run_gate --read "$tmp/undemoted-timeouts.log" > "$tmp/undemoted.out" 2>&1; rc=$?
grep -q "gate: RED — tests failed" "$tmp/undemoted.out" && ! grep -q "gate: DEMOTED" "$tmp/undemoted.out" && [ "$rc" -ne 0 ] \
  && ok "the same timeouts in a run that was NOT demoted are RED" || bad "timeouts were excused without a demotion: rc=$rc"

say "12. a whole build takes the checkout's ci lock; a scoped command takes nothing"
# ci-runner-lock-bypass: a hand-run `family all` raced a `ci-runner.sh
# once` in the same checkout, two sbts on one target/ tree (2026-09-25)
sleep 30 & other=$!
mkdir -p "$OKAY_CI_LOCK_DIR"; echo "$other" > "$OKAY_CI_LOCK_DIR/pid"
GATE_SBT="$here/fake-sbt-args.sh" GATE_LOG="$tmp/lock-foreign.log" run_gate "family all" > "$tmp/lock-foreign.out" 2>&1; rc=$?
[ "$rc" -eq 3 ] && grep -q "gate: LOCKED" "$tmp/lock-foreign.out" && ok "held by another live run: LOCKED, exit 3" || bad "rc=$rc: $(cat "$tmp/lock-foreign.out")"
grep -q "held by pid $other" "$tmp/lock-foreign.out" && ok "named the holder's pid" || bad "did not name the pid"
grep -q "fake-sbt-arg" "$tmp/lock-foreign.log" 2>/dev/null && bad "sbt was started beside the holder" || ok "sbt was not started"
[ "$(sh "$here/gate-retry.sh" --read "$tmp/lock-foreign.out")" = "$tmp/lock-foreign.out: gate: LOCKED — a whole build already holds this checkout's ci lock; done, the gate's exit code stands; NOT retried" ] \
  && ok "and gate-retry does not retry it" || bad "gate-retry reads it as: $(sh "$here/gate-retry.sh" --read "$tmp/lock-foreign.out")"
GATE_SBT="$here/fake-sbt-args.sh" GATE_LOG="$tmp/lock-scoped.log" run_gate "okayJVM/testOnly A" > "$tmp/lock-scoped.out" 2>&1; rc=$?
[ "$rc" -eq 0 ] && grep -q "fake-sbt-arg" "$tmp/lock-scoped.log" && ok "a scoped command runs beside the holder" || bad "a scoped command was held: rc=$rc"
kill "$other" 2>/dev/null; wait "$other" 2>/dev/null
# the holder is THIS shell, an ancestor of the gate: the runner's own gate
echo $$ > "$OKAY_CI_LOCK_DIR/pid"
GATE_SBT="$here/fake-sbt-args.sh" GATE_LOG="$tmp/lock-ours.log" run_gate test > "$tmp/lock-ours.out" 2>&1; rc=$?
[ "$rc" -eq 0 ] && grep -q "own ancestor" "$tmp/lock-ours.out" && ok "held by an ancestor: it is ours, the gate runs" || bad "rc=$rc: $(grep '^gate:' "$tmp/lock-ours.out" | head -2)"
[ "$(cat "$OKAY_CI_LOCK_DIR/pid" 2>/dev/null)" = "$$" ] && ok "and the ancestor's lock is left in place" || bad "the gate released a lock it did not take"
# a dead holder is taken over, and the lock is gone when the run ends
deadpid=99999; while kill -0 "$deadpid" 2>/dev/null; do deadpid=$((deadpid + 1)); done
echo "$deadpid" > "$OKAY_CI_LOCK_DIR/pid"
GATE_SBT="$here/fake-sbt-args.sh" GATE_LOG="$tmp/lock-dead.log" run_gate "family jvm" > "$tmp/lock-dead.out" 2>&1; rc=$?
[ "$rc" -eq 0 ] && grep -q "taking it over" "$tmp/lock-dead.out" && ok "a dead holder's lock is taken over" || bad "rc=$rc: $(grep '^gate:' "$tmp/lock-dead.out" | head -2)"
[ ! -d "$OKAY_CI_LOCK_DIR" ] && ok "and released when the run ends" || bad "lock left behind: $(cat "$OKAY_CI_LOCK_DIR/pid" 2>/dev/null)"

say "10. the bench window: a queued benchmark holds the gate's start; a --read takes no token"
# a request whose owner lives 3 s: the gate waits for it, then runs
sleep 3 & lane=$!
mkdir -p "$OKAY_BENCH_DIR/want"; : > "$OKAY_BENCH_DIR/want/$lane"
start=$(date +%s)
GATE_SBT="$here/fake-sbt-args.sh" GATE_LOG="$tmp/bw.log" OKAY_BENCH_GATE_MAX_WAIT=30 OKAY_BENCH_DEMOTE=off \
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
