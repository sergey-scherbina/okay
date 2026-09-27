#!/bin/sh
# bench-window-selftest.sh — scripts/bench-window.sh's protocol against
# a FIXTURE directory (OKAY_BENCH_DIR), never the box's own, with one-
# second polls: the gate's side of specs/bench-window.md. The lane's
# side is in jmh-lane-selftest.sh (4-4d). Runs under sh and bash, as
# the gate does (memory sh-not-bash-test-both).
#
#   sh scripts/bench-window-selftest.sh
set -u
here="$(cd "$(dirname "$0")" && pwd)"
fail=0
say() { printf '%s\n' "$1"; }
ok()  { say "  ok   — $1"; }
bad() { say "  FAIL — $1"; fail=1; }

for shell in sh bash; do
  command -v "$shell" > /dev/null || continue
  say "== under $shell"
  export OKAY_BENCH_DIR="$(mktemp -d -t bench-window-selftest)"
  export OKAY_BENCH_POLL=1
  # a "gate": sources the library, enters, reports, leaves on EXIT
  gate() { "$shell" -c ". '$here/bench-window.sh'; bw_gate_enter; echo entered; ls \"\$OKAY_BENCH_DIR/gates\" | grep -qx \$\$ && echo token; trap bw_gate_leave EXIT; $1"; }
  deadpid() { d=99999; while kill -0 "$d" 2>/dev/null; do d=$((d + 1)); done; echo "$d"; }

  say "1. no request: a gate enters at once, holds a token, drops it on exit"
  out=$(OKAY_BENCH_GATE_MAX_WAIT=30 gate "true"); 
  printf '%s\n' "$out" | grep -q entered && ok "entered" || bad "did not: $out"
  printf '%s\n' "$out" | grep -q token && ok "held a token while running" || bad "no token: $out"
  [ -z "$(ls "$OKAY_BENCH_DIR/gates")" ] && ok "token gone after exit" || bad "token left: $(ls "$OKAY_BENCH_DIR/gates")"

  say "2. a live request holds the gate; it enters as soon as the request goes"
  sleep 3 & lane=$!
  : > "$OKAY_BENCH_DIR/want/$lane"
  start=$(date +%s)
  out=$(OKAY_BENCH_GATE_MAX_WAIT=30 gate "true")
  took=$(( $(date +%s) - start ))
  printf '%s\n' "$out" | grep -q "a benchmark is queued (pid $lane" && ok "said why it waited" || bad "did not say: $out"
  [ "$took" -ge 2 ] && [ "$took" -lt 15 ] && ok "entered after the lane ended (${took}s)" || bad "took ${took}s"
  ! printf '%s\n' "$out" | grep -q "starting anyway" && ok "not by the cap" || bad "used the cap: $out"

  say "3. the cap: a request that never goes holds a gate no longer than OKAY_BENCH_GATE_MAX_WAIT"
  sleep 30 & lane=$!
  : > "$OKAY_BENCH_DIR/want/$lane"
  start=$(date +%s)
  out=$(OKAY_BENCH_GATE_MAX_WAIT=2 gate "true")
  took=$(( $(date +%s) - start ))
  printf '%s\n' "$out" | grep -q "starting anyway" && ok "said it started anyway" || bad "did not: $out"
  [ "$took" -lt 10 ] && ok "held ${took}s, cap 2" || bad "held ${took}s"
  kill "$lane" 2>/dev/null; wait "$lane" 2>/dev/null

  say "4. OKAY_BENCH_WINDOW=off enters at once but still holds a token"
  sleep 30 & lane=$!
  : > "$OKAY_BENCH_DIR/want/$lane"
  out=$(OKAY_BENCH_WINDOW=off OKAY_BENCH_GATE_MAX_WAIT=30 gate "true")
  printf '%s\n' "$out" | grep -q token && ok "token held" || bad "no token: $out"
  ! printf '%s\n' "$out" | grep -q "queued" && ok "did not wait" || bad "waited: $out"
  kill "$lane" 2>/dev/null; wait "$lane" 2>/dev/null
  rm -f "$OKAY_BENCH_DIR/want/"*

  say "5. a dead pid's request and token are ignored and removed"
  d=$(deadpid)
  : > "$OKAY_BENCH_DIR/want/$d"; : > "$OKAY_BENCH_DIR/gates/$d"
  out=$(OKAY_BENCH_GATE_MAX_WAIT=30 gate "true")
  ! printf '%s\n' "$out" | grep -q "queued" && ok "the dead request held nobody" || bad "waited on a dead pid: $out"
  live=$("$shell" -c ". '$here/bench-window.sh'; bw_live gates")
  [ -z "$live" ] && ok "the dead token reads as no gate" || bad "reads live: $live"
  [ ! -e "$OKAY_BENCH_DIR/gates/$d" ] && [ ! -e "$OKAY_BENCH_DIR/want/$d" ] && ok "both files removed" || bad "left behind"

  say "6. a gate killed by a signal leaves a token that reads as dead"
  "$shell" -c ". '$here/bench-window.sh'; bw_gate_enter; trap bw_gate_leave EXIT; sleep 30" & g=$!
  sleep 1
  live=$("$shell" -c ". '$here/bench-window.sh'; bw_live gates")
  [ "$live" = "$g" ] && ok "live while it runs" || bad "reads '$live', wanted $g"
  kill -9 "$g"; wait "$g" 2>/dev/null
  live=$("$shell" -c ". '$here/bench-window.sh'; bw_live gates")
  [ -z "$live" ] && ok "dead after SIGKILL" || bad "still reads '$live'"

  say "7. a held gate writes a heartbeat while it waits (OKAY_BENCH_HEARTBEAT)"
  sleep 4 & lane=$!
  : > "$OKAY_BENCH_DIR/want/$lane"
  out=$(OKAY_BENCH_HEARTBEAT=1 OKAY_BENCH_GATE_MAX_WAIT=30 gate "true")
  beats=$(printf '%s\n' "$out" | grep -c "still holding")
  [ "$beats" -ge 2 ] && ok "$beats heartbeat lines over a ~4 s hold" || bad "only $beats heartbeat lines: $out"

  rm -rf "$OKAY_BENCH_DIR"
done

# 8. THE DEFECT (bench-window-hold-reads-as-stall, found 2026-09-26 on a
# sibling's gate): gate-retry kills a gate whose log has not grown for
# GATE_STALL_MIN minutes, and a gate held by the window wrote ONE line
# for the whole hold — so a hold longer than that was killed as STALLED,
# three attempts, no verdict, and the ci-runner goes through the same
# road. A fixture: gate-retry, gate.sh and this library copied beside a
# quiet.sh that is always quiet, a fake sbt, a request whose owner lives
# 150 s, GATE_STALL_MIN=1 — longer than TWO of gate-retry's minute
# windows, since the hold's own first line counts as growth in the first
# (a 75 s hold passed on the defective code: the first cut of this case
# did not reproduce). The held gate must reach its verdict. Slow
# (~2.5 min), so under one shell only.
say "8. gate-retry over a gate held longer than GATE_STALL_MIN reaches its verdict"
fx=$(mktemp -d -t bench-window-retry)
mkdir -p "$fx/scripts" "$fx/wt" "$fx/bench/want"
for f in gate-retry.sh gate.sh bench-window.sh jdk-pin.sh fake-sbt-args.sh; do cp "$here/$f" "$fx/scripts/$f"; done
cat > "$fx/scripts/quiet.sh" <<'QEOF'
quiet() { L=0; H=0; F=99; return 0; }
kill_tree() { for c in $(pgrep -P "$1" 2>/dev/null); do kill_tree "$c"; done; kill "$1" 2>/dev/null; }
QEOF
sleep 150 & lane=$!
: > "$fx/bench/want/$lane"
( cd "$fx/wt" && OKAY_BENCH_DIR="$fx/bench" OKAY_BENCH_POLL=5 GATE_STALL_MIN=1 GATE_RECSCAN=0 \
    GATE_SBT="$fx/scripts/fake-sbt-args.sh" sh "$fx/scripts/gate-retry.sh" "$fx/wt" "$fx/retry.log" 1 "okayJVM/testOnly A" > "$fx/retry.out" 2>&1 )
rc=$?
kill "$lane" 2>/dev/null; wait "$lane" 2>/dev/null
! grep -q "STALLED" "$fx/retry.log" && ok "not killed as STALLED" || bad "killed as STALLED: $(grep STALLED "$fx/retry.log")"
grep -q "gate: GREEN" "$fx/retry.log" && ok "reached its verdict (GREEN)" || bad "no verdict (rc $rc): $(tail -5 "$fx/retry.log")"
rm -rf "$fx"

say ""
if [ "$fail" -eq 0 ]; then say "bench-window-selftest: PASS"; else say "bench-window-selftest: FAIL"; fi
exit "$fail"
