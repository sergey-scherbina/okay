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
  cp "$here/jmh-lane.sh" "$tmp/scripts/jmh-lane.sh"
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
printf '%s\n' "$out" | grep -qv "attempt 2" && ok "no second attempt" || bad "retried when it should not have: $out"
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

say "4. a second lane while one holds the lock refuses, naming the pid"
new_fixture
mkdir -p "$tmp/.work/jmh/lock"; echo $$ > "$tmp/.work/jmh/lock/pid"
out=$(run "Bench.thing" 5 2>&1); rc=$?
[ "$rc" -ne 0 ] && ok "refused (nonzero exit)" || bad "ran anyway"
printf '%s\n' "$out" | grep -q "held by pid $$" && ok "named the holder's pid" || bad "did not name the pid: $out"
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

say ""
if [ "$fail" -eq 0 ]; then say "jmh-lane-selftest: PASS"; else say "jmh-lane-selftest: FAIL"; fi
exit "$fail"
