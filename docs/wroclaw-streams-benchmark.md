# The Wrocław streams benchmark

Okay against eight other ways of running the same streaming job on
one machine — Apache Flink 1.20, Spark 4.0, `java.util.stream`, fs2,
zio-streams, kyo, and a bare `while` loop with `Thread`s — plus okay's
own distributed coordinator over four OS processes.

This page is a curated entry point: the job, the headline result, the
main tables, and links to every line of code that produced them. It
adds nothing that [docs/benchmarks.md §20](benchmarks.md) does not
already say — that section is 1100+ lines of derivation, corrections
and refuted first attempts, kept because the wrong readings are as
useful as the right ones. This page exists so a reader does not have
to find it inside a 4000-line file first.

## The job

Wrocław's public transport publishes its timetable as GTFS
(`open-data.cui.wroclaw.pl`, snapshot 131): 1 158 821 scheduled stop
departures, 41 962 trips, 138 routes, 2 482 stops. A timetable is a
PLAN, not a stream, so the benchmark turns it into one deterministically
— one splitmix hash of trip, stop-sequence and day, no `Random`, no
clock — adding a delay (worse in rush hour), an event time that is not
arrival order, and an arrival jitter kept below the watermark bound so
every engine sees complete windows and the two answers can be asserted
EQUAL rather than "close". Eight service days replay as 2 414 119
events.

Five stages, one definition (`Job`), run on every engine:

| stage | what | what it exercises |
|---|---|---|
| 1 | enrich: map-side join against the 138-row routes table, drop unknown routes | closure-shipped table |
| 2 | tumbling 5 min per route: count, sum, max delay | the shared `Aggregator` |
| 3 | sliding 15 min every 5 per stop | the same aggregator, 3 panes per event |
| 4 | bunching: two departures of one route from one stop < 2 min apart | keyed STATE |
| 5 | top-5 tram routes by mean delay, per window | `Aggregator.topK` on stage 2's output |

**Why the comparison is unusually fair.** An okay `Aggregator` IS a
Flink `AggregateFunction` field for field (`init/add/merge/present`
against `createAccumulator/add/merge/getResult`) and IS a JDK
`Collector` the same way (`okay-java`'s `Collect.collector`). So the
same VALUE — `Job.stats` — is handed to three engines rather than
reimplemented three times, and what separates the numbers is the
engine, not the arithmetic.

**Correctness first.** Every lane's run produces eleven
order-independent and order-sensitive checksums, asserted EQUAL to
okay's before a single number is timed. A row that computed something
else never reaches a table.

## Methodology, in six rules

1. **A JVM per lane** — every library lives in its own module with its
   own `main`; the first version of this benchmark shared one JVM and
   read Flink 3.6x slower at the bottom of a run than at the top.
2. **A floor** — a bare `while` loop, no stream machinery, so every
   other row reads as a multiple of the work itself.
3. **Bytes per event**, from `getTotalThreadAllocatedBytes`, beside
   the time — steadier than wall clock on a shared box, and it
   explains the time.
4. **Cores as a column**, so N-against-N is visible rather than
   reconstructed from separate tables.
5. **The answer first** — the eleven checksums, every run.
6. **Each competitor on its OWN operators and its OWN arithmetic.** No
   lane is handed okay's window operator or aggregator. Handing Flink
   `Job.stats`'s Scala tuple accumulator was found to force it through
   Kryo serialization on every pane — a handicap, not a favour, that
   nobody had noticed for three runs of this benchmark.

## The headline: core scaling, 1/2/4/8 cores

The one result worth seeing before any other: okay is the only
in-process library that keeps climbing as cores are added. Every
other one plateaus around 1.6–1.7x from one core to eight; the
plain-JVM thread lane regresses past four.

Each cell: wall clock / events per second.

| lane | 1 core | 2 cores | 4 cores | 8 cores | 1→8 |
|---|---:|---:|---:|---:|---:|
| **okay** (merge) | 563 ms / 4,287,955 | 317 ms / 7,615,517 | 189 ms / 12,773,116 | **107 ms / 22,561,859** | **5.3x** |
| flink | 3,101 ms / 778,496 | 1,960 ms / 1,231,693 | 1,582 ms / 1,525,991 | 1,437 ms / 1,679,971 | 2.2x |
| java.util.stream | 561 ms / 4,303,242 | 458 ms / 5,271,002 | 396 ms / 6,096,260 | 337 ms / 7,163,557 | 1.7x |
| zio-streams | 583 ms / 4,140,855 | 417 ms / 5,789,254 | 357 ms / 6,762,238 | 347 ms / 6,957,115 | 1.7x |
| kyo | 610 ms / 3,957,572 | 512 ms / 4,715,076 | 419 ms / 5,761,620 | 380 ms / 6,352,944 | 1.6x |
| fs2 | 579 ms / 4,169,462 | 472 ms / 5,114,658 | 377 ms / 6,403,498 | 368 ms / 6,560,105 | 1.6x |
| plain JVM, threads | 574 ms / 4,205,782 | 471 ms / 5,125,518 | 398 ms / 6,065,625 | 409 ms / 5,902,491 | 1.4x, and 8 is WORSE than 4 |

At one core every lane but Flink is within a few percent of the
others — inside the measurement's own 9% noise floor (two rows in the
full table below are the SAME code under two method names and read 9%
apart, minutes apart). Nothing at one core is a finding. What
separates the lanes is the SLOPE: okay fans the job across fibres
joined by `merge`; the others parallelize a single
`Stream`/`foreachPar`/`parEvalMap` and hit its fan-in cost before they
run out of cores.

## The full table — every lane, one run

Host: 14 cpus (10 performance, 4 efficiency), 36 GB, JVM 21, macOS
arm64. 2 414 119 events, best of 3 rounds per lane, one JVM per lane,
every lane's eleven checksums asserted before its number was taken —
`scripts/wroclaw-bench.sh 8 3 1`, ONE run of the script, every engine
(`wroclaw-table-refresh`, 2026-09-11).

| lane | cores | ev/s | wall | B/event |
|---|---:|---:|---:|---:|
| okay, 8 fibres (merge) | 8 | **22 561 859** | **107 ms** | 789 |
| okay, 4 fibres (merge) | 4 | 12 773 116 | 189 ms | 781 |
| okay, 2 fibres (merge) | 2 | 7 615 517 | 317 ms | 775 |
| java.util.stream, 8 cores | 8 | 7 163 557 | 337 ms | 824 |
| zio-streams, 8 cores (`foreachPar`) | 8 | 6 957 115 | 347 ms | 508 |
| zio-streams, 4 cores | 4 | 6 762 238 | 357 ms | 481 |
| fs2, 8 cores (`parEvalMap`) | 8 | 6 560 105 | 368 ms | 517 |
| fs2, 4 cores | 4 | 6 403 498 | 377 ms | 491 |
| kyo, 8 cores (`Async.parallel`) | 8 | 6 352 944 | 380 ms | 638 |
| java.util.stream, 4 cores | 4 | 6 096 260 | 396 ms | 740 |
| plain JVM, 4 threads | 4 | 6 065 625 | 398 ms | 468 |
| plain JVM, 8 threads | 8 | 5 902 491 | 409 ms | 495 |
| zio-streams, 2 cores | 2 | 5 789 254 | 417 ms | 438 |
| kyo, 4 cores | 4 | 5 761 620 | 419 ms | 611 |
| java.util.stream, 2 cores | 2 | 5 271 002 | 458 ms | 684 |
| plain JVM, 2 threads | 2 | 5 125 518 | 471 ms | 425 |
| fs2, 2 cores | 2 | 5 114 658 | 472 ms | 447 |
| okay, 1 thread, packed + mutable cell | 1 | 4 770 986 | 506 ms | 590 |
| kyo, 2 cores | 2 | 4 715 076 | 512 ms | 568 |
| **java.util.stream, 1 core** | 1 | **4 303 242** | **561 ms** | 338 |
| **okay, 1 thread (`Chunks`)** | 1 | **4 287 955** | **563 ms** | 751 |
| the floor (a while loop, okay's operator) | 1 | 4 220 487 | 572 ms | 737 |
| plain JVM, while loop | 1 | 4 205 782 | 574 ms | 338 |
| okay, 1 thread, mutable-cell aggregator | 1 | 4 198 467 | 575 ms | 604 |
| fs2, 1 core (pure) | 1 | 4 169 462 | 579 ms | 360 |
| zio-streams, 1 core | 1 | 4 140 855 | 583 ms | 351 |
| kyo, 1 core | 1 | 3 957 572 | 610 ms | 477 |
| okay, 1 thread, `Aggregator.summary` (same code as row 21) | 1 | 3 931 789 | 614 ms | 869 |
| okay, 1 thread, packed-key windows | 1 | 3 180 657 | 759 ms | 1 004 |
| okay, 1 thread, `count zip sum zip max` | 1 | 2 781 243 | 868 ms | 1 136 |
| java.util.stream + okay's windowed collector | 1 | 2 703 380 | 893 ms | 1 333 |
| flink, parallelism 8 | 8 | 1 679 971 | 1 437 ms | 1 773 |
| flink, parallelism 4 | 4 | 1 525 991 | 1 582 ms | 1 767 |
| flink, p4 + checkpoints every 5 s | 4 | 1 524 065 | 1 584 ms | 1 771 |
| flink, p4, object reuse off | 4 | 1 252 786 | 1 927 ms | 1 968 |
| flink, parallelism 2 | 2 | 1 231 693 | 1 960 ms | 1 766 |
| flink, parallelism 1 | 1 | 778 496 | 3 101 ms | 1 761 |
| spark, local[4], batch RDD | 4 | 338 918 | 7 123 ms | 13 787 |
| spark, local[4], structured streaming | 4 | 264 445 | 9 129 ms | 3 870 |

Full derivation, the Flink-serialization correction, and the
Spark-comparison caveats: [§20](benchmarks.md).

## Four findings worth their own table

### 1. Fixed cost vs marginal rate

A Flink job pays a fixed cost before it sees an event (a MiniCluster
starts, a job graph is built and deployed); a benchmark over a fixed
dataset charges that to the events, which makes a small dataset look
worse than Flink's real jobs ever would. Each lane was also run over
prefixes of the stream and the two costs separated by a least-squares
fit:

| lane | fixed cost | marginal |
|---|---:|---:|
| okay, 1 thread | 19 ms | 2 721 230 ev/s |
| okay, 4 fibres (merge) | 3 ms | 8 941 185 ev/s |
| flink, parallelism 1 | 428 ms | 738 909 ev/s |
| flink, parallelism 4 | 433 ms | 1 870 582 ev/s |

Four okay fibres are 4.8x four Flink tasks, PER EVENT — the fair
comparison, four cores against four cores. Flink's ~0.43 s fixed cost
is the price of being an engine: a job graph, task deployment, a state
backend. What that buys and this benchmark cannot show: the job
survives a machine dying, state is checkpointed, a rescale
re-partitions it. Any comparison that leaves that out prices two
different products as if they were one.

### 2. Parallelism with no shuffle at all

okay's multi-fibre rows are not a parallel stream library: `Aggregator`
already carries `merge`, so the arrival order is cut into P contiguous
slices, each folds independently, and only the accumulators that
straddle a slice boundary — a few thousand, not millions — come back
to be merged. Nothing is serialized, nothing crosses a socket. The
suite asserts the parallel answer equals the sequential one at 2, 4
and 8 slices before any number is trusted.

| fibres | throughput | wall | of one thread |
|---|---:|---:|---:|
| 1 | 2 905 077 ev/s | 831 ms | 1.00x |
| 2 | 5 499 132 ev/s | 439 ms | 1.89x |
| 4 | 9 179 159 ev/s | 263 ms | 3.16x |
| 8 | 15 778 555 ev/s | 153 ms | 5.43x |

A real defect surfaced on the way, worth knowing for any stateful job:
the first parallel run disagreed with the sequential one on the
bunching stage (451 detections against 913), because a single-
partition stream rebalanced round-robin across a filter breaks
per-key ordering — Flink's own p1 and p4 runs disagreed with each
other for the identical reason. Pinning stage 1 to the source's own
parallelism fixed it. Full mechanism: [§20](benchmarks.md), "Four cores without a shuffle".

### 3. One core loses, eight cores win — decomposed

Handed NO okay types at all — a plain `HashMap`/`Thread` fold, exactly
what a user of each library would write — the hand-written JDK stream
beats okay's own lane by 1.4x on one core. On eight cores it reverses
hard: okay reaches 15.78M ev/s, the best of the other five reaches
6.7M.

The one-core gap, decomposed by what changes between rows:

| | pane map | accumulator | evicts | wall | B/event |
|---|---|---|---|---:|---:|
| `run` | general (`HashMap`) | tuple-tree zip | yes | 694 ms | 955 |
| `packed` | packed key, one map | tuple-tree zip | yes | 635 ms | 1 005 |
| `runCells` | general (`HashMap`) | mutable cell | yes | 582 ms | 608 |
| `packedCells` | packed key, one map | mutable cell | yes | **520 ms** | 625 |
| plain-JVM loop | packed key, one map | mutable cell | **no** | **520 ms** | 338 |

Eviction is FREE — the bottom two rows are the same code but for the
watermark sweep, and they read within a millisecond of each other. The
gap is the ARITHMETIC (a zip-composed accumulator against a mutable
cell, now fixed in the core as `Aggregator.summary`, a flat
four-field accumulator) and the general pane map (any key type against
one packed `Long`) — not the event-time machinery itself, which does
not pay for existing here.

Why eight cores flip it: the plain-JVM parallel run's own instrumentation
shows the fold itself scales fine (135 ms across 8 threads), and then
spends 382 ms — three quarters of the run — merging 3 426 483 pane
cells in one thread, because a map fold with no watermark never evicts
anything. okay's operator holds 4 918 panes at once, exactly, because
the watermark closed the rest as the run went. Full decomposition and
the correction of an earlier, wrongly-attributed reading:
[§20](benchmarks.md), "Every in-process library on its OWN operators".

### 4. `java.util.stream`: two roads through the same seam

`Collectors.groupingBy` has no event time, so a "window" is only a key
and every pane of the run stays live until the terminal operation
finishes — the parallel lane dies with an `OutOfMemoryError` on an 8 GB
heap at the full 2.4M events. `okay.java.Windowed` makes an
`okay.Windows` fold into a `Collector` instead, evicting a pane the
moment its watermark closes it:

| the JDK lane, sequential, 603 529 events | throughput | wall | peak heap | full feed? |
|---|---:|---:|---:|---|
| `Collectors.groupingBy` | 1 160 632 ev/s | 520 ms | 652 MB | no — OOM at 2.4M |
| `okay.java.Windowed` | 2 493 921 ev/s | 242 ms | 403 MB | **yes: 992 ms, 825 MB** |

2.15x faster, 38% less heap, and it finishes the job the other road
cannot. It is also, deliberately, sequential: a parallel `collect`
splits by position and each split's watermark would close panes that
an earlier split's elements still belong to, so the combiner REFUSES
rather than return a plausible wrong answer — the sharpest statement
in this benchmark of what a coordinator buys that a fold cannot.
`parallel()` on the honest `groupingBy` road, separately, was 4.9x
SLOWER and cost 5.7x the memory: high-cardinality grouping is not
what parallel streams are for. Full story: [§20](benchmarks.md), "The third engine: java.util.stream, through the Collector interop".

### 5. Across four OS processes

Every number above is one JVM. This road runs the identical job
through okay's coordinator over four real operating-system processes
(four service days, 1 255 298 events, eight partitions, best of five
interleaved rounds):

| road | ev/s | vs the in-JVM fan |
|---|---:|---:|
| 8 fibres, one JVM (`Flows.fan`) | 14 944 023 | 1.00x |
| 8 partitions, coordinator, workers in-JVM | 9 030 920 | 1.65x |
| 8 partitions, over sockets, one JVM | 7 472 011 | 2.00x |
| 8 partitions, over 4 OS processes | 8 151 285 | 1.83x |

The protocol and CBOR cost 1.65x; sockets take it to 2.00x; four
separate processes cost NOTHING FURTHER beyond the sockets — within
the run-to-run noise of the transported roads themselves. The
distributed road's cost is fixed (about 65 ms once — two round trips,
a plan built on every worker), not marginal: its per-event rate stays
within a tenth of the in-JVM fan. Full table and what crosses the wire
in bytes: [§20](benchmarks.md), "The engine at a DISTANCE".

## Source

`scripts/wroclaw-bench.sh` invokes the Bench class in each row below;
Lane is where that class's logic lives.

| lane | Bench (what the script runs) | Lane (the logic) |
|---|---|---|
| the job itself | [Job.scala](../compare/src/main/scala/okay/wroclaw/Job.scala), [Gtfs.scala](../compare/src/main/scala/okay/wroclaw/Gtfs.scala) | [Bench.scala](../compare/src/main/scala/okay/wroclaw/Bench.scala) (shared harness) |
| okay (merge) | [OkayBench.scala](../compare/src/main/scala/okay/wroclaw/OkayBench.scala) | [OkayLane.scala](../compare/src/main/scala/okay/wroclaw/OkayLane.scala) |
| plain JVM | [JvmBench.scala](../compare/src/main/scala/okay/wroclaw/JvmBench.scala) | [JvmLane.scala](../compare/src/main/scala/okay/wroclaw/JvmLane.scala) |
| java.util.stream | [JavaBench.scala](../okay-java/src/test/scala/okay/java/wroclaw/JavaBench.scala) | [JavaLane.scala](../okay-java/src/test/scala/okay/java/wroclaw/JavaLane.scala) |
| fs2 | [Fs2Bench.scala](../okay-fs2/src/test/scala/okay/fs2/wroclaw/Fs2Bench.scala) | [Fs2Lane.scala](../okay-fs2/src/test/scala/okay/fs2/wroclaw/Fs2Lane.scala) |
| zio-streams | [ZioBench.scala](../okay-zio/src/test/scala/okay/zio/wroclaw/ZioBench.scala) | [ZioLane.scala](../okay-zio/src/test/scala/okay/zio/wroclaw/ZioLane.scala) |
| kyo | [KyoBench.scala](../okay-kyo/src/test/scala/okay/kyo/wroclaw/KyoBench.scala) | [KyoLane.scala](../okay-kyo/src/test/scala/okay/kyo/wroclaw/KyoLane.scala) |
| flink | [FlinkBench.scala](../okay-flink/src/test/scala/okay/flink/wroclaw/FlinkBench.scala) | [FlinkLane.scala](../okay-flink/src/test/scala/okay/flink/wroclaw/FlinkLane.scala) |
| the five libraries' own-operator fold (§20 finding 3) | — | [Native.scala](../compare/src/main/scala/okay/wroclaw/Native.scala) |

Driver: [scripts/wroclaw-bench.sh](../scripts/wroclaw-bench.sh) — one
`sbt` invocation per lane, in its own forked JVM, printing the
`ROW`/`SKIP` lines the tables above are built from. Spark's lanes
(under `okay-spark/src/test/scala/okay/spark/wroclaw/`) and the
distributed/native variants added since this table was taken are a
different, later measurement and are not linked here.

## Reading further

Everything on this page is an excerpt, verified against
[docs/benchmarks.md §20](benchmarks.md), which additionally carries:
the object-reuse and checkpointing-cost measurements, the fan-out and
watermark-cadence asymmetries named rather than buried, what the okay
lane cost to write and what closing that gap cost (three roads, the
producer-vs-operator split), the byte-level accounting of what crosses
the wire in the distributed road, and the corrections this section
made to its own earlier, wrongly-attributed readings — kept visible
because a wrong reading is the shape the next person will reach for
first.
