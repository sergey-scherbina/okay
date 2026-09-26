#!/bin/sh
# Run the five-way headline lanes for every runtime, okay included, in a
# local clone patched by apply.py. His JMH settings (run_five_way.py's
# "measured" profile: -f 3 -wi 3 -i 3 -w 1s -r 1s -t 1, 2 GiB G1), but
# ONE benchmark x runtime per sbt call, each started on a quiet box and
# re-run if the box got busy during it — okay's jmh-lane.sh protocol,
# with the same box-wide lock, because this machine is shared.
#
#   sh compare/five-way/run.sh <clone> <out-dir> [runtimes...]
#
# JDK: $FIVE_WAY_JAVA_HOME if set, else sdkman's 25 (his run was 25.0.3).
# Deliberately NOT $JAVA_HOME: the ambient one here is 17, and his
# sources need JDK 21+ APIs (virtual threads) to compile at all.
set -u
clone="$(cd "${1:?clone}" && pwd)"
out="${2:?out-dir}"
shift 2
runtimes="${*:-ce kyo loom ox gears okay okayOwn okayAdaptive}"
here="$(cd "$(dirname "$0")" && pwd)"
okay_root="$(cd "$here/../.." && pwd)"
. "$okay_root/scripts/quiet.sh"
JAVA_HOME="${FIVE_WAY_JAVA_HOME:-$HOME/.sdkman/candidates/java/25.0.4.1-tem}"
export JAVA_HOME PATH="$JAVA_HOME/bin:$PATH"
mkdir -p "$out"
"$JAVA_HOME/bin/java" -version 2>&1 | head -1 > "$out/jdk.txt"
LOCKDIR="${JMH_LANE_LOCK:-${TMPDIR:-/tmp}/okay-jmh-lane.lock}"

take_lock() {
  while ! mkdir "$LOCKDIR" 2>/dev/null; do
    holder=$(cat "$LOCKDIR/pid" 2>/dev/null)
    if [ -n "$holder" ] && ! kill -0 "$holder" 2>/dev/null; then rm -rf "$LOCKDIR"; continue; fi
    sleep 30
  done
  echo $$ > "$LOCKDIR/pid"
}
release_lock() { rm -rf "$LOCKDIR"; }
trap release_lock EXIT INT TERM

wait_quiet() { n=0; until quiet; do n=$((n + 1)); [ $n -ge 60 ] && break; sleep 30; done; }

JMH="-f 3 -wi 3 -i 3 -w 1s -r 1s -t 1 -foe true -prof gc -bm thrpt -rf json"
lane() { # name pattern extra-params runtime
  name=$1 pattern=$2 params=$3 rt=$4
  for attempt in 1 2 3; do
    wait_quiet
    take_lock
    log="$out/$name-$rt.log"
    (cd "$clone" && sbt -batch "ioBench/Jmh/run $JMH -rff $out/$name-$rt.json -p runtime=$rt $params $pattern") > "$log" 2>&1
    rc=$?
    release_lock
    if grep -q "Compilation failed" "$log"; then echo "$name $rt: COMPILATION FAILED, see $log" >> "$out/summary.txt"; exit 1; fi
    if quiet; then echo "$name $rt rc=$rc attempt=$attempt"; return; fi
    echo "$name $rt attempt=$attempt DISCARDED: the box got busy during the run" >> "$out/discarded.txt"
  done
  echo "$name $rt rc=$rc attempt=3 (kept after three busy attempts)"
}

for rt in $runtimes; do
  lane workers    'bench.direct.ParallelBench.workers'   '-p size=4096 -p parallelism=8' "$rt"
  lane spawnJoin  'bench.direct.PrimitivesBench.spawnJoin' '-p ops=1000' "$rt"
  lane entry      'bench.direct.RunnerBench.entry'       '' "$rt"
  # okay's `own` scheduler runs fibers on platform workers and okay's
  # docs say not to block in it (a blocked worker strands the fibers it
  # forked); his TCP validation's parallel-worker gate fails it on the
  # blocking transport, so `okayOwn` runs the callback transport only
  if [ "$rt" = okayOwn ]; then transport='-p transport=nonblocking'; else transport=''; fi
  lane tcp        'bench.io.IoBench.requests'            "-p parallelism=64 -p size=256 -p delayMicros=1000 $transport" "$rt"
done >> "$out/summary.txt"
echo DONE >> "$out/summary.txt"
