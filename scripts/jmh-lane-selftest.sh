#!/bin/sh
# jmh-lane-selftest.sh — scripts/jmh-lane.sh against a FIXTURE (a
# throwaway directory with a fake scripts/quiet.sh and a fake sbt),
# never the real box's load and never a real JMH run. The fake
# quiet.sh pops one word ("quiet"/"busy") off a queue file per call,
# so a specific sequence of quiet-before / busy-after-run / quiet-again
# is reproducible without depending on real system load — exactly the
# thing AGENTS.md's "no flaky tests" rule forbids depending on.
#
#   sh scripts/jmh-lane-selftest.sh
set -u
here="$(cd "$(dirname "$0")" && pwd)"
fail=0
say() { printf '%s\n' "$1"; }
ok()  { say "  ok   — $1"; }
bad() { say "  FAIL — $1"; fail=1; }

new_fixture() {
  tmp=$(mktemp -d)
  mkdir -p "$tmp/scripts" "$tmp/.work"
  export JMH_LANE_LOCK="$tmp/.work/jmh/lock"   # the fixture's own lock, not the box's
  export OKAY_BENCH_DIR="$tmp/.work/bench"     # and its own bench window
  export JMH_LANE_LOCK_POLL=1 JMH_LANE_QUIET_POLL=1
  cp "$here/jmh-lane.sh" "$tmp/scripts/jmh-lane.sh"
  cp "$here/bench-window.sh" "$tmp/scripts/bench-window.sh"
  cp "$here/jdk-pin.sh" "$tmp/scripts/jdk-pin.sh"   # sourced by jmh-lane.sh; no .sdkmanrc in the fixture, so it pins nothing
  chmod +x "$tmp/scripts/jmh-lane.sh"
  cat > "$tmp/scripts/fake-sbt.sh" <<'EOF'
#!/bin/sh
echo "[info] fake jmh run: $*"
exit 0
EOF
  chmod +x "$tmp/scripts/fake-sbt.sh"
  # the fake quiet.sh: pops one word off $tmp/.work/queue per call to
  # `quiet`; an empty/exhausted queue defaults to quiet, so a test that
  # does not care about a given call does not have to pad the queue
  cat > "$tmp/scripts/quiet.sh" <<'EOF'
#!/bin/sh
Q="$(cd "$(dirname "$0")/.." && pwd)/.work/queue"
quiet() {
  L=1; H=0; F=99
  if [ -s "$Q" ]; then
    line=$(head -1 "$Q")
    tail -n +2 "$Q" > "$Q.tmp" 2>/dev/null && mv "$Q.tmp" "$Q"
    [ "$line" = "quiet" ]
  else
    return 0
  fi
}
kill_tree() { :; }
EOF
  : > "$tmp/.work/queue"
}
queue() { # one word per line
  for w in "$@"; do printf '%s\n' "$w" >> "$tmp/.work/queue"; done
}
run() { ( cd "$tmp" && SBT="$tmp/scripts/fake-sbt.sh" sh scripts/jmh-lane.sh "$@" ); }

say "1. quiet throughout: one attempt, exit 0"
new_fixture
out=$(run "Bench.thing" 5 2>&1); rc=$?
[ "$rc" -eq 0 ] && ok "exit 0" || bad "exit $rc: $out"
printf '%s\n' "$out" | grep -q "attempt 1/5" && ok "ran attempt 1" || bad "did not: $out"
! printf '%s\n' "$out" | grep -q "attempt 2" && ok "no second attempt" || bad "retried when it should not have: $out"
rm -rf "$tmp"

say "2. box got busy DURING the run: discarded, retried, then succeeds"
new_fixture
# call order per attempt: [pre-wait] then [after-run]. Attempt 1:
# quiet before, BUSY after (contaminated) -> retry. Attempt 2: quiet
# before, quiet after -> success.
queue quiet busy quiet quiet
out=$(run "Bench.thing" 5 2>&1); rc=$?
[ "$rc" -eq 0 ] && ok "exit 0 eventually" || bad "exit $rc: $out"
printf '%s\n' "$out" | grep -q "CONTAMINATED" && ok "said the first result was contaminated" || bad "did not flag contamination: $out"
printf '%s\n' "$out" | grep -q "attempt 2/5" && ok "retried as attempt 2" || bad "did not retry: $out"
printf '%s\n' "$out" | grep -q "trust this number" && ok "the retry's own result is trusted" || bad "did not confirm the good run: $out"
rm -rf "$tmp"

say "3. box never stays quiet through a whole lane: gives up at the attempt cap"
new_fixture
# every attempt: quiet before, busy after -- 3 attempts, so 6 entries
queue quiet busy quiet busy quiet busy
out=$(run "Bench.thing" 3 2>&1); rc=$?
[ "$rc" -eq 99 ] && ok "exit 99" || bad "exit $rc, wanted 99: $out"
printf '%s\n' "$out" | grep -q "attempt 3/3" && ok "used all 3 attempts" || bad "did not: $out"
printf '%s\n' "$out" | grep -q "gave up after 3 attempts" && ok "said it gave up" || bad "did not say so: $out"
rm -rf "$tmp"

say "4. JMH_LANE_LOCK_WAIT=0: a second lane while one holds the lock refuses, naming the pid"
new_fixture
mkdir -p "$tmp/.work/jmh/lock"; echo $$ > "$tmp/.work/jmh/lock/pid"
out=$(JMH_LANE_LOCK_WAIT=0 run "Bench.thing" 5 2>&1); rc=$?
[ "$rc" -ne 0 ] && ok "refused (nonzero exit)" || bad "ran anyway"
printf '%s\n' "$out" | grep -q "held by pid $$" && ok "named the holder's pid" || bad "did not name the pid: $out"
[ -z "$(ls "$tmp/.work/bench/want" 2>/dev/null)" ] && ok "its request is gone after it gave up" || bad "left a request behind"
rm -rf "$tmp"

say "4b. by default a lane QUEUES behind a held lock and runs when it frees (bench-window)"
new_fixture
sleep 3 & holder=$!
mkdir -p "$tmp/.work/jmh/lock"; echo "$holder" > "$tmp/.work/jmh/lock/pid"
out=$(run "Bench.thing" 5 2>&1); rc=$?
[ "$rc" -eq 0 ] && ok "ran and exited 0" || bad "exit $rc: $out"
printf '%s\n' "$out" | grep -q "queued behind it" && ok "said it was queued" || bad "did not queue: $out"
printf '%s\n' "$out" | grep -q "trust this number" && ok "then ran the lane" || bad "did not run: $out"
rm -rf "$tmp"

say "4c. a live gate token holds the lane's start; it runs once the gate is gone (bench-window)"
new_fixture
sleep 3 & gatepid=$!
mkdir -p "$tmp/.work/bench/gates"; : > "$tmp/.work/bench/gates/$gatepid"
start=$(date +%s)
out=$(run "Bench.thing" 5 2>&1); rc=$?
took=$(( $(date +%s) - start ))
[ "$rc" -eq 0 ] && ok "exit 0" || bad "exit $rc: $out"
printf '%s\n' "$out" | grep -q "gates=$gatepid" && ok "named the gate it waited for" || bad "did not wait on the gate: $out"
[ "$took" -ge 2 ] && ok "started only after the gate ended (${took}s)" || bad "started while the gate ran (${took}s)"
rm -rf "$tmp"

say "4d. while a lane is queued its request is filed; after it, the request is gone"
new_fixture
cat > "$tmp/scripts/fake-sbt.sh" <<'EOF2'
#!/bin/sh
ls "$OKAY_BENCH_DIR/want" > "$OKAY_BENCH_DIR/seen-during-run"
exit 0
EOF2
chmod +x "$tmp/scripts/fake-sbt.sh"
out=$(run "Bench.thing" 5 2>&1); rc=$?
[ -s "$tmp/.work/bench/seen-during-run" ] && ok "request filed while the lane ran" || bad "no request during the run: $out"
[ -z "$(ls "$tmp/.work/bench/want" 2>/dev/null)" ] && ok "request removed after" || bad "request left behind"
rm -rf "$tmp"

say "5. a lock whose pid is dead is taken over"
new_fixture
deadpid=99999
while kill -0 "$deadpid" 2>/dev/null; do deadpid=$((deadpid + 1)); done
mkdir -p "$tmp/.work/jmh/lock"; echo "$deadpid" > "$tmp/.work/jmh/lock/pid"
out=$(run "Bench.thing" 5 2>&1); rc=$?
[ "$rc" -eq 0 ] && ok "ran and exited 0" || bad "exit $rc: $out"
printf '%s\n' "$out" | grep -q "taking it over" && ok "said it took the dead lock over" || bad "did not say so: $out"
rm -rf "$tmp"

say "6. the lock is released after a run, successful or not"
new_fixture
run "Bench.thing" 5 >/dev/null 2>&1
[ ! -d "$tmp/.work/jmh/lock" ] && ok "lock released after success" || bad "lock left behind"
rm -rf "$tmp"

say "7. another JMH holds JMH's own lock: contention, not a failure — waited out and retried"
new_fixture
# the fake sbt fails ONCE the way JMH does when a run outside jmh-lane
# holds $TMPDIR/jmh.lock (jmh-lane-foreign-jmh-lock), then runs
cat > "$tmp/scripts/fake-sbt.sh" <<'EOF2'
#!/bin/sh
n="$(cd "$(dirname "$0")/.." && pwd)/.work/sbt-calls"
c=$(cat "$n" 2>/dev/null || echo 0); c=$((c + 1)); echo "$c" > "$n"
if [ "$c" -eq 1 ]; then
  echo "[error] ERROR: org.openjdk.jmh.runner.RunnerException: ERROR: Another JMH instance might be running. Unable to acquire the JMH lock (/tmp/jmh.lock), exiting. Use -Djmh.ignoreLock=true to forcefully continue."
  exit 1
fi
echo "[info] fake jmh run: $*"
exit 0
EOF2
chmod +x "$tmp/scripts/fake-sbt.sh"
out=$(JMH_LANE_FOREIGN_WAIT=0 run "Bench.thing" 5 2>&1); rc=$?
[ "$rc" -eq 0 ] && ok "exit 0 after the retry" || bad "exit $rc: $out"
printf '%s\n' "$out" | grep -q "another JMH holds" && ok "named the foreign lock as contention" || bad "did not: $out"
printf '%s\n' "$out" | grep -q "attempt 2/5" && ok "retried as attempt 2" || bad "did not retry: $out"
! printf '%s\n' "$out" | grep -q "a real failure" && ok "not called a real failure" || bad "called it a real failure: $out"
rm -rf "$tmp"

say "8. a real failure is still final: a nonzero run without the lock message is not retried"
new_fixture
printf '#!/bin/sh\necho "[error] a compile error"\nexit 1\n' > "$tmp/scripts/fake-sbt.sh"; chmod +x "$tmp/scripts/fake-sbt.sh"
out=$(run "Bench.thing" 5 2>&1); rc=$?
[ "$rc" -eq 1 ] && ok "exit 1 passed through" || bad "exit $rc: $out"
printf '%s\n' "$out" | grep -q "a real failure" && ok "called a real failure" || bad "did not: $out"
! printf '%s\n' "$out" | grep -q "attempt 2/" && ok "no retry" || bad "retried a real failure: $out"
rm -rf "$tmp"

say "9. a quiet box but NOISY rows: discarded as contaminated, retried, the tight run trusted"
new_fixture
# jmh-lane-error-gate: a sibling's gate can start and end INSIDE a lane,
# so quiet-at-both-ends passes while the number is +-60%. The fake sbt
# prints JMH's result table: noisy on call 1, tight on call 2.
cat > "$tmp/scripts/fake-sbt.sh" <<'EOF2'
#!/bin/sh
n="$(cd "$(dirname "$0")/.." && pwd)/.work/sbt-calls"
c=$(cat "$n" 2>/dev/null || echo 0); c=$((c + 1)); echo "$c" > "$n"
echo "[info] Benchmark                  (producers)  Mode  Cnt     Score     Error  Units"
if [ "$c" -eq 1 ]; then
  echo "[info] ManyProducers.default_elem            1  avgt   10  1502.945 ±  904.618  us/op"
else
  echo "[info] ManyProducers.default_elem            1  avgt   10   492.767 ±   19.824  us/op"
fi
exit 0
EOF2
chmod +x "$tmp/scripts/fake-sbt.sh"
out=$(run "Bench.thing" 5 2>&1); rc=$?
[ "$rc" -eq 0 ] && ok "exit 0 after the retry" || bad "exit $rc: $out"
printf '%s\n' "$out" | grep -q "too NOISY" && ok "named the noisy row" || bad "did not flag noise: $out"
printf '%s\n' "$out" | grep -q "attempt 2/5" && ok "retried as attempt 2" || bad "did not retry: $out"
printf '%s\n' "$out" | grep -q "trust this number" && ok "the tight run is trusted" || bad "did not trust it: $out"
rm -rf "$tmp"

say "10. JMH_LANE_MAX_ERR=0 turns the error gate off"
new_fixture
printf '#!/bin/sh\necho "[info] ManyProducers.default_elem  1  avgt  10  1502.945 ±  904.618  us/op"\nexit 0\n' > "$tmp/scripts/fake-sbt.sh"; chmod +x "$tmp/scripts/fake-sbt.sh"
out=$(JMH_LANE_MAX_ERR=0 run "Bench.thing" 5 2>&1); rc=$?
[ "$rc" -eq 0 ] && ok "exit 0" || bad "exit $rc: $out"
! printf '%s\n' "$out" | grep -q "attempt 2/" && ok "no retry" || bad "retried with the gate off: $out"
rm -rf "$tmp"

say "11. a noisy SECONDARY metric (:gc...) does not reject a tight primary row"
new_fixture
cat > "$tmp/scripts/fake-sbt.sh" <<'EOF2'
#!/bin/sh
echo "[info] Gen.take                          avgt   10   21.960 ±   0.410  us/op"
echo "[info] Gen.take:gc.count                 avgt   10    3.000 ±   9.000  counts"
exit 0
EOF2
chmod +x "$tmp/scripts/fake-sbt.sh"
out=$(run "Bench.thing" 5 2>&1); rc=$?
[ "$rc" -eq 0 ] && ok "exit 0" || bad "exit $rc: $out"
! printf '%s\n' "$out" | grep -q "attempt 2/" && ok "no retry for secondary noise" || bad "retried on a secondary metric: $out"
rm -rf "$tmp"

say ""
if [ "$fail" -eq 0 ]; then say "jmh-lane-selftest: PASS"; else say "jmh-lane-selftest: FAIL"; fi
exit "$fail"
