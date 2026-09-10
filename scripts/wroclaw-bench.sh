#!/bin/sh
# The Wrocław event-time benchmark (docs/benchmarks.md §20): one JVM per
# lane, every lane's answer checked against okay's before its number is
# printed, and one table at the end.
#
#   scripts/wroclaw-bench.sh [days] [rounds] [fraction]
#
# `days` is how many service days of the feed to replay (8 ≈ 2.4M
# events), `rounds` how many times each lane runs (the best is kept),
# and `fraction` the denominator of the prefix (1 = all, 4 = a quarter —
# the size the JDK's `groupingBy` road fits in).
#
# WHY A SCRIPT AND NOT A TEST. Every lane lives in its own interop
# module and runs in its own forked JVM, so no lane inherits another's
# heap, JIT state or garbage — the first version of this table, one JVM
# for everything, read Flink 3.6x slower at the bottom than at the top.
# It is also the only arrangement in which Spark can be measured at
# all: its `SparkSession` needs a two-stdlib classpath that breaks the
# compilation of anything inlining okay's core.
set -e

DAYS=${1:-8}
ROUNDS=${2:-3}
PART=${3:-1}
OUT=$(mktemp -t wroclaw-bench)
: > "$OUT"

echo "wroclaw: days=$DAYS rounds=$ROUNDS fraction=1/$PART — a JVM per lane"

# ONE sbt PER MODULE, and not one invocation with seven commands: sbt
# abandons the commands after a failing one, so the JDK's `groupingBy`
# road dying of an OutOfMemoryError at full size — which is that road's
# whole finding — used to take Spark's lanes down with it, silently.
for lane in \
  "compare/runMain okay.wroclaw.OkayBench" \
  "okayJava/Test/runMain okay.java.wroclaw.JavaBench" \
  "okayFs2/Test/runMain okay.fs2.wroclaw.Fs2Bench" \
  "okayZio/Test/runMain okay.zio.wroclaw.ZioBench" \
  "okayKyo/Test/runMain okay.kyo.wroclaw.KyoBench" \
  "okayFlink/Test/runMain okay.flink.wroclaw.FlinkBench" \
  "okaySpark/Test/runMain okay.spark.wroclaw.SparkBench"
do
  echo "  -> $lane"
  sbt -batch "$lane $DAYS $ROUNDS $PART" 2>&1 | tee -a "$OUT" | grep -E "^\[info\] (ROW|SKIP)" || true
done

echo
echo "lane                                            cores        ev/s      wall   B/event  peak heap"
grep -o "ROW	.*" "$OUT" | sort -t'	' -k6 -rn | awk -F'\t' '{
  printf "%-46s %5s %11s %7s ms %9s %7s MB  %s\n", $2, $3, $6, $5, $7, $8, $9
}'
echo
echo "(full log: $OUT)"
