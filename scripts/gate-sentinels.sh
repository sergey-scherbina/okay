#!/usr/bin/env bash
#
# Who sends the SIGTERM that ends a full matrix with 143?
#
# BACKLOG's `matrix-kill-by-process-group` blames a suite that kills a
# process GROUP rather than a pid. CHANGELOG:8155 records the same
# signature with a different, ADMITTED cause: "three full-matrix runs
# were SIGTERM-killed externally (a sibling pkill, admitted in the
# room)". Neither has been verified. This script tells them apart by
# BLAST RADIUS, which is the one thing they do not share.
#
# Three sentinels run beside the gate, each heartbeating a timestamp to
# its own file once a second so its death is timed to the second:
#
#   group   same session as this script, so a group-directed signal
#           reaches it; nothing matchable in its command line
#   named   its OWN session (os.setsid), but carries "sbt-launch" IN
#           its command line, so `pkill -f sbt` reaches it
#   plain   its own session, neutral command line; only an
#           indiscriminate sweep reaches it
#
# Read the verdict off which ones died:
#
#   none          the gate was signalled directly, or died on its own
#   group only    a process-group kill -- the entry's hypothesis holds
#   named         somebody ran a pkill matching "sbt"
#   all three     an indiscriminate sweep (killall java, or user-wide)
#
# The `named` sentinel carries "sbt-launch" ON PURPOSE. That means a
# sibling's quiet-box check (`pgrep -f sbt-launch`) will see it while
# this runs. That is honest -- the box IS busy then -- but it is also
# why this script is for investigating a 143, not for routine gating.
#
# Usage: scripts/gate-sentinels.sh ["clean; test"]
#
set -u

SBT_CMD="${1:-clean; test}"
OUT="${OKAY_SENTINEL_DIR:-${TMPDIR:-/tmp}/okay-gate-sentinels-$$}"
mkdir -p "$OUT"

# What the `named` sentinel puts in its command line. The default is
# the widest catch: it matches `pkill -f sbt` AND `pkill -f sbt-launch`.
# It therefore also shows up in a sibling's `pgrep -f sbt-launch` quiet
# check. Narrow it (OKAY_SENTINEL_TAG=sbt-sentinel) to stay out of that
# check at the cost of missing a pkill that matched the longer name.
TAG="${OKAY_SENTINEL_TAG:-sbt-launch-sentinel}"

# One source for all three; it must contain no "sbt" and no "java", so
# that only the argument appended after it decides what matches.
HEARTBEAT='
import os, sys, time
if sys.argv[1] == "own":
    try:
        os.setsid()
    except OSError:
        pass
path = sys.argv[2]
while True:
    with open(path, "w") as f:
        f.write("%.3f" % time.time())
    time.sleep(1)
'

start_sentinel() {           # name session extra-argv...
  local name="$1" session="$2"; shift 2
  # stdout/stderr MUST be redirected: a background child that inherits
  # them holds the caller's pipe open, and the run never returns.
  python3 -c "$HEARTBEAT" "$session" "$OUT/$name.hb" "$@" >/dev/null 2>&1 &
  echo $!
}

PID_GROUP=$(start_sentinel group same)
PID_NAMED=$(start_sentinel named own "$TAG")
PID_PLAIN=$(start_sentinel plain own okay-gate-sentinel)

# What else is on the box, sampled through the run: at a 143 this is
# the only record of who else was alive at the time.
(
  while true; do
    { date +%s; ps -eo pid,ppid,pgid,etime,pcpu,args | grep -E "[s]bt-launch|[j]ava|[p]ython3" | head -30; echo "--"; } >> "$OUT/ps.log"
    sleep 5
  done
) >/dev/null 2>&1 &
PID_PS=$!

cleanup() {
  for p in "$PID_GROUP" "$PID_NAMED" "$PID_PLAIN" "$PID_PS"; do
    kill "$p" 2>/dev/null      # by pid, explicitly -- never pkill
  done
}
trap cleanup EXIT

echo "sentinels: group=$PID_GROUP named=$PID_NAMED plain=$PID_PLAIN"
echo "artifacts: $OUT"
echo "load at start: $(uptime | sed 's/.*averages: //')"
echo "running: sbt \"$SBT_CMD\""

sbt "$SBT_CMD" > "$OUT/gate.log" 2>&1
GATE=$?
DIED=$(date +%s)

echo
echo "GATE EXIT -> $GATE  ($( [ "$GATE" -eq 143 ] && echo 'SIGTERM -- this is the case under investigation' || echo 'not a 143' ))"

verdict=""
for s in group named plain; do
  case "$s" in
    group) pid="$PID_GROUP" ;;
    named) pid="$PID_NAMED" ;;
    plain) pid="$PID_PLAIN" ;;
  esac
  if kill -0 "$pid" 2>/dev/null; then
    printf '  %-6s ALIVE\n' "$s"
  else
    last=$(cat "$OUT/$s.hb" 2>/dev/null || echo 0)
    printf '  %-6s DIED  (last heartbeat %ss before the gate returned)\n' \
      "$s" "$(awk -v a="$DIED" -v b="$last" 'BEGIN{printf "%.0f", a-b}')"
    verdict="$verdict $s"
  fi
done

echo
case "$(echo $verdict | tr -d ' ')" in
  "")                echo "VERDICT: no sentinel died -- the gate was signalled DIRECTLY, or exited on its own." ;;
  "group")           echo "VERDICT: only the same-session sentinel died -- a PROCESS-GROUP kill, as the entry claims." ;;
  *named*plain*|*plain*named*) echo "VERDICT: sentinels in their own sessions died -- an INDISCRIMINATE sweep, not a group kill." ;;
  *named*)           echo "VERDICT: the sbt-named sentinel died -- somebody ran a PKILL MATCHING \"sbt\"." ;;
  *)                 echo "VERDICT: unexpected combination:$verdict -- read $OUT/ps.log." ;;
esac

echo
# anywhere in the line, not anchored: parallel modules interleave
# their stdout, and a summary glued to another suite's output line
# loses its `[info]` prefix -- the anchored form read 2423 on a run
# whose real total was 2441, and looked like eighteen tests vanished
echo "tests counted: $(grep -oE 'Passed: Total [0-9]+' "$OUT/gate.log" | awk '{s+=$3} END {print s+0}')"
echo "warnings:      $(grep -cE '^\[warn\]' "$OUT/gate.log")"
echo "last suite in the log: $(grep -oE '^[a-z0-9.]+(Test|Spec)[A-Za-z]*:|^okay\.[a-z.]+\.Test[A-Za-z]*:' "$OUT/gate.log" | tail -1)"
exit "$GATE"
