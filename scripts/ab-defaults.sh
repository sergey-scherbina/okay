#!/usr/bin/env bash
# ab-defaults.sh — the A/B that decides a DEFAULT, on a quiet box.
#
# WHY THIS EXISTS. Two defaults in this library ship the slower of two
# mechanisms it already owns, and both decisions are blocked on one
# measurement each:
#
#   channel   Channel.apply builds a single ring. The adaptive buffer
#             reads 870/1042 at 4x4 and 16x16 against ZIO's 3106/6325,
#             and all four of its races are fixed and lawed
#             (adaptive-p-x-c-deadlock, 2026-09-07). Channel.scala names
#             what is missing: "the A/B of every single-producer path the
#             default feeds -- buffer, bufferChunked, merge -- since its
#             capacity is PER PART and a lone producer must not pay for
#             parts it never opens."
#   scheduler the default is Loom, a fiber IS a virtual thread.
#             Schedulers.own reads 750us per 10 000 fork/joins against
#             kyo's 880 where that default reads 2715.
#
# WHY IT IS A SCRIPT AND NOT A RUN. The previous attempt at the channel
# arm was thrown away, and the reason is the whole design here: its
# control lane (zioChunked, which touches nothing either arm changes)
# moved 8x between arms -- 73 -> 620. That arm was taken under page-outs,
# so its headline numbers (427, 2509) measured the host, not the buffer.
# A verdict is only worth reading if the run can prove the box held
# still while it was taken. So:
#
#   * both arms run in ONE invocation, alternating A/B/A/B, not two
#     sessions an hour apart;
#   * a CONTROL lane runs in every arm, and the disqualification
#     threshold is written down HERE, before any number exists;
#   * the verdict prints DISQUALIFIED, not a ranking, when the control
#     moves -- there is no way to read a ratio out of a bad arm later.
#
# USAGE
#   scripts/ab-defaults.sh channel      # ring vs adaptive
#   scripts/ab-defaults.sh scheduler    # loom vs own vs adaptive
#   scripts/ab-defaults.sh both
#
#   ROUNDS=3        alternating rounds per arm (default 3)
#   CONTROL_PCT=15  max control drift, in percent, before an arm is
#                   disqualified (default 15)
#   JMH_ARGS='-f2 -wi 3 -i 5'   passed through
#
# Run it on a QUIET box. It refuses to start above a load threshold for
# the same reason the control exists.
#
# WHAT THIS SCRIPT CANNOT DECIDE. The channel arm has a semantics half
# that no benchmark settles, and Queues.scala states it: the adaptive
# buffer's laws promise each producer's own order, no loss and no
# duplication, and say NOTHING about the order BETWEEN producers.
# Nothing ever promised that, but under a single ring it happened to
# hold, and making adaptive the default would take it away from code
# that may have come to rely on it. So a win here is a NECESSARY
# condition for changing the default, never a sufficient one.

set -uo pipefail

MODE="${1:-both}"
ROUNDS="${ROUNDS:-3}"
CONTROL_PCT="${CONTROL_PCT:-15}"
JMH_ARGS="${JMH_ARGS:--f2 -wi 3 -i 5}"
MAX_LOAD="${MAX_LOAD:-2.0}"
SBT="${SBT:-sbt}"

cd "$(dirname "$0")/.." || exit 2
OUT="target/ab-defaults"
mkdir -p "$OUT"
STAMP="$(date +%Y%m%d-%H%M%S)"

declare -A VERIFY=(
  [ring]="SentinelChannel\[Ring\]"
  [adaptive]="SentinelChannel\[AdaptiveFifo\]"
  [loom]="SCHED.*loom"
  [own]="Owned"
)

load_now() { awk '{print $1}' /proc/loadavg 2>/dev/null || echo 0; }

LOAD="$(load_now)"
if awk -v l="$LOAD" -v m="$MAX_LOAD" 'BEGIN{exit !(l>m)}'; then
  echo "REFUSING TO START: load average is $LOAD, threshold $MAX_LOAD."
  echo "A default decided on a busy box is the mistake this script exists to prevent."
  echo "Override with MAX_LOAD=... only if you mean it."
  exit 2
fi
echo "host load at start: $LOAD  (threshold $MAX_LOAD)"
echo "rounds per arm: $ROUNDS   control drift allowed: ${CONTROL_PCT}%"
echo "jmh: $JMH_ARGS"
echo

# ---------------------------------------------------------------- lanes
#
# Each arm runs the lanes the DEFAULT ITSELF feeds, plus the P x C lanes
# the change is FOR, plus a control the change cannot touch.

CHANNEL_LANES='.*ChunkFlushBenchmark.(bufferPerElement|bufferDrained|okayChunked|zioChunked).*|.*ChannelGranularityBenchmark.(okayChunked|zioChunked).*|.*IdiomaticApiBenchmark.okayChannelForeach_chunkNative_runForeach.*|.*AdversarialBenchmark.manyToMany_okay.*'
CHANNEL_CONTROL='zioChunked'

SCHED_LANES='.*AdversarialBenchmark.(forkJoin10k_okay|cancel1k_okay|manyToMany_okay).*|.*AsyncBenchmark.*'
SCHED_CONTROL='zio|cats'

# JMH FORKS ITS OWN JVM, so a -D given to sbt never reaches the code
# under measurement: both arms would run the shipped default and the
# script would report an honest-looking "no difference". Found by
# probing rather than by reading (the sbt -D arm printed "(unset)").
# The property goes through JMH's own -jvmArgsAppend, and
# verify_arm below proves it arrived BEFORE any measuring starts.
run_arm() { # $1 = tag, $2 = -Dprop, $3 = lane regex, $4 = round
  local tag="$1" prop="$2" lanes="$3" round="$4"
  local f="$OUT/$STAMP.$tag.r$round.txt"
  echo "--- arm=$tag round=$round  load=$(load_now)" >&2
  $SBT -batch "compare/Jmh/run $JMH_ARGS -jvmArgsAppend $prop -rf text -rff $f.jmh $lanes" \
    > "$f" 2>&1
  local rc=$?
  [ $rc -ne 0 ] && echo "    (sbt exit $rc — see $f)" >&2
  echo "$f"
}

# Prove the arm reaches the JVM. Without this the script's worst
# failure is silent: two identical arms, a clean-looking verdict.
verify_arm() { # $1 = tag, $2 = -Dprop, $3 = expected marker
  local tag="$1" prop="$2" want="$3"
  local got
  got=$($SBT -batch 'set ThisBuild / Test / fork := true' \
        "set ThisBuild / Test / javaOptions ++= Seq(\"$prop\")" \
        "okayJVM/Test/runMain okay.AbSwitchProbe" 2>&1 \
        | grep -E '^\[info\] (BUFFER|SCHED)' | tr -d '\r')
  if echo "$got" | grep -q "$want"; then
    echo "  arm $tag reaches the JVM: $(echo "$got" | head -2 | tr '\n' ' ')"
  else
    echo "  ARM $tag DID NOT TAKE EFFECT — expected '$want', probe said:"
    echo "$got" | sed 's/^/    /'
    echo "  Refusing to measure two identical arms."
    exit 3
  fi
}

# collect "Benchmark<TAB>Score" pairs out of a JMH text result
scores_of() {
  awk '/^[A-Za-z].*avgt/ {print $1"\t"$4}' "$1" 2>/dev/null
}

ab() { # $1 = subject, $2 = lanes, $3 = control regex, shift 3 = arms "tag=props"
  local subject="$1" lanes="$2" control="$3"; shift 3
  local arms=("$@")
  echo "================ $subject ================"
  for a in "${arms[@]}"; do
    verify_arm "${a%%=*}" "${a#*=}" "${VERIFY[${a%%=*}]:-.}"
  done
  echo
  local files=()
  for r in $(seq 1 "$ROUNDS"); do
    for a in "${arms[@]}"; do
      local tag="${a%%=*}" props="${a#*=}"
      files+=("$(run_arm "$tag" "$props" "$lanes" "$r" | tail -1)")
    done
  done
  echo
  echo "raw results under $OUT/$STAMP.*"
  echo
  python3 - "$control" "$CONTROL_PCT" "${files[@]}" <<'PY'
import re, sys, os, statistics as st
control_re, pct = sys.argv[1], float(sys.argv[2])
files = sys.argv[3:]
by_arm = {}
for f in files:
    m = re.search(r'\.([a-z0-9]+)\.r\d+\.txt$', f)
    if not m: continue
    arm = m.group(1)
    jmh = f + '.jmh'
    src = jmh if os.path.exists(jmh) else f
    try: text = open(src, errors='replace').read()
    except OSError: continue
    for line in text.splitlines():
        mm = re.match(r'^\s*(?:\[info\]\s*)?([\w.]+\.\w+)\s+(?:\S+\s+)?avgt\s+\d+\s+([\d.,]+)', line)
        if mm:
            name, score = mm.group(1), float(mm.group(2).replace(',', ''))
            by_arm.setdefault(arm, {}).setdefault(name, []).append(score)

if not by_arm:
    print("NO PARSED RESULTS — read the raw files; do not guess a verdict."); raise SystemExit(1)

def med(xs): return st.median(xs)

# 1. THE CONTROL GATE, applied before any comparison is printed.
controls = {}
for arm, lanes in by_arm.items():
    vals = [med(v) for n, v in lanes.items() if re.search(control_re, n, re.I)]
    if vals: controls[arm] = med(vals)
print("control lanes (median per arm):")
for arm, v in controls.items(): print(f"  {arm:12s} {v:12.2f}")
ok = True
if len(controls) >= 2:
    lo, hi = min(controls.values()), max(controls.values())
    drift = (hi - lo) / lo * 100 if lo else 999
    print(f"  control drift: {drift:.1f}%  (allowed {pct}%)")
    if drift > pct:
        ok = False
        print()
        print("VERDICT: DISQUALIFIED — the control moved more than the threshold.")
        print("The box did not hold still, so NOTHING below is a measurement of the")
        print("default. Re-run on a quieter host. Do not read the ratios.")
else:
    ok = False
    print("VERDICT: NO CONTROL — the control lane did not run in both arms.")

print()
print("per-lane medians:")
arms = sorted(by_arm)
names = sorted({n for a in by_arm.values() for n in a})
w = max(len(n) for n in names) + 2
print(" " * w + "".join(f"{a:>14s}" for a in arms))
for n in names:
    row = "".join(f"{med(by_arm[a][n]):14.2f}" if n in by_arm[a] else f"{'-':>14s}" for a in arms)
    print(f"{n:<{w}}{row}")

if ok:
    print()
    print("VERDICT: the control held. The table above is a measurement of the")
    print("default. A default changes only if the arm wins the lanes it is FOR")
    print("and loses nothing the current default feeds.")
PY
}

case "$MODE" in
  channel)
    ab "CHANNEL DEFAULT — ring vs adaptive" "$CHANNEL_LANES" "$CHANNEL_CONTROL" \
       "ring=-Dokay.channel.buffer=ring" "adaptive=-Dokay.channel.buffer=adaptive" ;;
  scheduler)
    ab "SCHEDULER DEFAULT — loom vs own vs adaptive" "$SCHED_LANES" "$SCHED_CONTROL" \
       "loom=-Dokay.scheduler=loom" "own=-Dokay.scheduler=own" "adaptive=-Dokay.scheduler=adaptive" ;;
  both)
    "$0" channel; echo; "$0" scheduler ;;
  *)
    echo "usage: $0 [channel|scheduler|both]"; exit 2 ;;
esac

echo
echo "host load at end: $(load_now)"
echo "Record the verdict in src/jmh/history.tsv with the load column filled in."
