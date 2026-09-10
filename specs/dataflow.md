# okay-dataflow — distributed data processing, ours

## Overview

The operator's ask: what Flink and Spark do — data processing across
machines — done by us, independently, and better where better can be
shown rather than asserted.

The starting position is not zero and it is not a clean field either,
so both halves are worth stating plainly.

**What is already here.** `Aggregator` (specs/aggregators.md) is the
merge contract as a composable VALUE — init/add/merge/present, which
is field for field Flink's `AggregateFunction` and Spark's
(zero, seqOp, combOp). `Windows` (specs/event-time-windows.md) is
event time, watermarks, panes and lateness as one cross-platform
operator. `Chunks` is a replayable source that is a VALUE, so lineage
is in hand rather than reconstructed. `Pipeline` (specs/staged-
pipelines.md) is already a Catalyst-shaped plan with rewrite rules and
a compiler onto the chunked transformers. `okay-persist` is a
partitioned durable log with offsets, snapshots, election and Raft.
`okay-codec` gives a typed, cross-platform wire. `okay-cluster` ships
chunks over sockets and recomputes a dead worker's chunk on a
survivor.

**What is missing, exactly.** There is no PLAN with keys in it, no
partitioning, no exchange, no worker protocol beyond "send a chunk,
await a partial", and no recovery model for anything stateful.
docs/benchmarks.md §20 says it in its own words — *"okay has no
distributed lane at all yet"* — which is why that table is one
machine per lane and the distributed comparison is not attempted.
The 10x it reports against Flink is a per-EVENT lead in one JVM; it
is not a claim about a cluster and this spec must not quietly turn it
into one.

## The three claims

Every distributed engine's marketing is interchangeable, so this spec
commits instead to three claims that can be FALSIFIED, and names what
would falsify each.

**Claim 1 — the shuffle is optional, and the plan can tell.**
Spark's `reduceByKey` is DEFINED by a shuffle stage; Flink's `keyBy`
moves records to the key's owner before the operator sees them. In
both, the aggregation is a user function the planner cannot reason
about, so the exchange is unconditional. Here the aggregation is an
`Aggregator` value whose `merge` is part of its type, so map-side
combine is always available and the exchange becomes a question about
the FINAL merge only: does `distinct keys × sizeof(Acc)` fit one
node's memory and one node's merge time? When it does, the right plan
moves kilobytes where an engine moves the dataset. When it does not,
the exchange is still there and is chosen.
*Falsified by*: a keyed job where our combine-then-merge finish is
slower than a hash exchange at the same parallelism on the same box,
with the crossover NOT explained by the accumulator count.

**Claim 2 — keyed STATE parallelises without a shuffle more often
than an engine can see.** Flink answers "two departures of one route
from one stop less than two minutes apart" with a
`KeyedProcessFunction` over `ValueState`, and that operator must have
every record of a key on one machine. But the per-key state of that
job has a boundary summary — (first, last, bunches, gap) — which
combines ASSOCIATIVELY over consecutive slices of the arrival order.
An operator whose slice summary is a monoid needs no shuffle at all;
it needs its slices merged in order. This spec makes that a TYPE
(`Sequential`) rather than a trick written by hand in a benchmark,
which is what it is today (`OkayLane.parallel`'s boundary stitch).
*Falsified by*: the type failing to express a keyed-state job that
Flink expresses easily, with no honest way to state its summary — in
which case the general exchange is the answer and Claim 2 shrinks to
"sometimes".

**Claim 3 — nothing ships a closure.** A task is identified by a
registered NAME plus typed parameters carried by `Schema`. There is
no Java serialization, no Kryo, no registration list, and no
`Task not serializable` — the class of failure simply does not
exist. The price is stated rather than hidden: you cannot type a
lambda into a REPL and have it run on the cluster. A plan whose
leaves are anonymous functions runs LOCALLY and is refused at
submission with the offending stage named, which is a compile-time-
shaped failure at the one place a human is watching.
*Falsified by*: the registry making ordinary jobs so awkward that
lanes start smuggling closures through it.

## What this is not

- Not a resource manager. Where workers come from is okay-deploy's
  and the operator's business (specs/deploy.md, specs/ops.md).
- Not SQL. A relational front end over this plan is a separate arc;
  `Sql` is a seam to databases, not to this engine.
- Not exactly-once EXECUTION. The persistence arc settled this
  already (specs/persist.md, `Saga`): the guarantee is exactly-once
  OUTCOME through idempotent sinks and journaled intent, and this
  engine will say the same words.
- Not a Spark/Flink API clone. okay-spark and okay-flink stay what
  they are — bridges, and the honest measuring stick.

## Interface

### The plan

`Flow` lives in okay-cluster (a distributed concern; the core stays
local and dependency-free) and embeds the local plan below every
exchange, so `Pipeline`'s fusion rules keep applying underneath.

```scala
enum Flow[A]:
  /** a PARTITIONED source: one replayable Chunks per partition, in
   * the input's own order — which is what lets a Sequential finish
   * merge slices in arrival order */
  case Src[A](parts: Vector[Chunks[A]]) extends Flow[A]

  /** anything that is per-partition and stateless-across-partitions:
   * map, filter, flatMap, take — held as a Chunks transformer so the
   * local optimizer owns this level */
  case Local[A, B](in: Flow[A], name: String, f: Chunks[A] => Chunks[B]) extends Flow[B]

  /** a keyed aggregation: THE node the engine exists for */
  case Keyed[A, K, Acc, O](in: Flow[A], key: A => K,
                           agg: Aggregator[A, Acc, O],
                           finish: Finish) extends Flow[(K, O)]

  /** a keyed, event-time windowed aggregation */
  case Windowed[A, K, Acc, O](in: Flow[A], size: Long, slide: Long, lateness: Long,
                              key: A => K, at: A => Long,
                              agg: Aggregator[A, Acc, O],
                              finish: Finish) extends Flow[Pane[K, O]]

  /** repartition by key hash — the shuffle, when it is really needed */
  case Exchange[A](in: Flow[A], parts: Int, by: A => Int) extends Flow[A]
```

### How a keyed stage finishes

```scala
enum Finish:
  case Merge      // per-worker partials, combined by the coordinator
  case Shuffle    // per-worker partials, hash-partitioned to owners
  case Auto       // Merge until the partial map crosses a stated size
```

The three roads for a keyed stage, in cost order, and the rule that
picks between them:

| the operator's type | road | what crosses a boundary |
|---|---|---|
| `Aggregator` (commutative merge) | combine, then merge anywhere | one accumulator per (key, window) per worker |
| `Sequential` (associative only) | combine, then merge IN SLICE ORDER | the same, plus the order constraint |
| a general keyed process | exchange | every record |

### The distinction that makes the table true

`Aggregator`'s merge is COMMUTATIVE by contract — specs/cluster.md
already relies on it ("order-free by the P1 contract") and a hash
exchange finishing in arbitrary arrival order relies on it harder.
A keyed state machine's slice summary is usually associative and NOT
commutative. Conflating the two is how a distributed answer silently
stops matching the single-threaded one, so the two get different
types:

```scala
/** an aggregation whose merge is associative but NOT commutative:
 * `merge(a, b)` means "the slice summarised by a, then the slice
 * summarised by b", so the merge tree must respect the input's
 * order. This is what lets a keyed STATE MACHINE parallelise with
 * no shuffle. */
trait Sequential[-In, Acc, +Out] extends Aggregator[In, Acc, Out]
```

A `Sequential` may not be finished by `Finish.Shuffle` unless the
exchange preserves slice order per key; the plan checks this rather
than trusting the author.

## Stages

- **0 — this spec.** What the engine is, what it refuses, and the
  three falsifiable claims.
- **1 — the plan and the local runtime.** DONE. `Flow`, its rewrite rules
  (combine below exchange, local fusion below everything), and an
  executor over N fibres in one process. Acceptance: the existing
  Wrocław checksums, at every parallelism.
- **2 — the exchange.** DONE. A real hash partition with combine
  below it, `Finish.Auto`, and the crossover measured rather than
  assumed.
- **3 — one pass, many sinks.** DONE. A job is several sinks over one
  source and the pass is single. This
  is what §20 already names as the asymmetry: a fan-out in one JVM is
  three method calls, in Flink it is three shuffles.
- **4 — across processes.** DONE. 4a: what crosses, and its Schema.
  4b: the worker protocol — jobs by name, typed parameters, framed
  transport, partials back — and four real processes.
  Acceptance: the full Wrocław `Result` across four OS processes.
- **5 — failure.** DONE for the batch half: a lost partition is
  recomputed on a survivor, under seeded schedules and with a real
  process killed mid-run. The streaming half — barrier checkpoints
  into okay-persist with source offsets — belongs to stage 6, where
  there is an unbounded source to checkpoint.
- **6 — streaming, properly.** 6a DONE (the epoch loop, the
  coordinator as a fold, the watermark as the minimum over the
  partitions) and 6b DONE (a dying worker's partition is replayed on
  a survivor). 6c: exactly-once OUTCOME at the sink.
- **7 — the numbers.** A distributed lane in docs/benchmarks.md §20,
  measured against Flink and Spark in the mode they are built for,
  with what their fixed costs buy named where it belongs.

## Behavior

Stage 1 (TestFlow in okay-cluster; TestWroclawFlow in compare —
`Live`-tagged, so it runs under `integrationTest` and NOT in the
default gate, and the boxes below were checked against a run of it,
not against its existence):
- [x] a `Flow` of source/local/keyed evaluates to the same answer at
      parallelism 1 and N, for every N tested (1, 2, 3, 4, 7, 8, 13,
      16, 17 across the suite)
- [x] the Wrocław tumbling-window stage through the engine equals
      `OkayLane`'s route checksums exactly — and its top-5 ranking
      with them, at 1, 2, 4 and 8 partitions
- [x] the Wrocław sliding-window stage equals its stop checksums
- [x] bunching, expressed as a `Sequential`, equals `OkayLane`'s
      bunch count AND gap sum — the boundary stitch written by hand
      in the benchmark, now the engine's job. Controlled: with the
      boundary term removed the two tests that rest on it fail at
      every parallelism above 1, and pass at 1.
- [x] all eleven checksums agree at parallelism 8
- [x] a `Sequential`'s merge is demonstrably NOT commutative, and the
      executor joins its partials by partition index rather than by
      readiness
- [x] on a feed WITH late elements, a seeded run drops exactly what
      the single-threaded run drops, and an unseeded one drops fewer
      AND answers differently — the difference is demonstrated, not
      assumed
- [x] a second keyed stage in one flow is refused by name, pointing
      at the exchange that stage 2 owes it
- [ ] the optimizer pushes a combine below an exchange, and the
      rewritten plan computes the same answer (property-tested, the
      `Pipeline.optimize` discipline) — MOVED TO STAGE 2: there is no
      exchange to push below yet, so this box could only have been
      checked by a test that asserts nothing

Stage 2 (TestFlow, TestWroclawFlow, MeasureExchange):
- [x] the exchange answers what the merge answers, at every
      parallelism crossed with every reducer count — keyed and
      windowed
- [x] a `Sequential` KEYED aggregator survives the exchange: a
      reducer owns a hash share and merges its buckets partition by
      partition, so the order it depends on is intact
- [x] a `Sequential` TERMINAL over a keyed stage is refused by name —
      and allowed on a stateless plan, where the engine really does
      hand it the input's order
- [x] `Auto` chooses one reducer under the measured bound and the
      buckets over it, and both roads answer the same
- [x] the crossover is MEASURED, not assumed, and the harness is
      honest about its own noise
- [x] on the Wrocław job the exchange is declined by `Auto`, and the
      reason is arithmetic rather than taste
- [ ] the optimizer pushes a combine below an exchange — STILL NOT
      APPLICABLE: the combine is not an optimization here, it is
      where a keyed stage begins. There is no plan in which the
      records reach the exchange uncombined, so there is nothing to
      push. The box stays open only until a stage-4 plan can express
      one.

Stage 3 (TestFlow, TestWroclawFlow, MeasureWroclawFlow):
- [x] a fan of three sinks answers exactly what three separate runs
      answer, at every parallelism
- [x] the whole Wrocław job is ONE plan over one pass, and all eleven
      checksums hold at 1, 2, 4 and 8 partitions
- [x] the fan seeds each sink's watermark from its OWN event time —
      two windowed sinks over different times in one pass, each
      dropping exactly what its own single-threaded run drops
- [x] a fan over a plan that already has a keyed stage is refused by
      name
- [x] a `Sequential` terminal is refused where the SINK IS BUILT, not
      where it runs
- [x] the engine is measured against the hand-written lane, and the
      number is reported whichever way it comes out

Stage 4a — what crosses (TestFlow):
- [x] `Sink` distinguishes `P`, the partition's working state, from
      `W`, what leaves it — and only `W` has to be a value
- [x] `Wire[A, R]` is a `Sink` plus a `Schema[W]`, composing through
      `and` with the pair's Schema
- [x] every partial forced through its codec answers what the local
      run answers, at every parallelism, on a punctual feed AND on
      one with late elements
- [x] what crosses is a SUMMARY plus the boundary panes, not the
      panes — asserted, not described
- [ ] the worker protocol and four processes — stage 4b

Stage 4b — across processes (TestDistributed):
- [x] `Job[P, R]` is the registry entry: a worker is asked for a NAME
      and Schema-encoded parameters and builds the plan itself
- [x] two requests — the pre-pass and the run — mirroring exactly the
      two passes `Flows.fan` makes in one process
- [x] in-process workers compute what the local fan computes, at
      every partition count crossed with every worker count
- [x] the same over SOCKETS in one JVM: real framing, real bytes
- [x] the same across FOUR REAL OPERATING-SYSTEM PROCESSES, for a
      windowed job and for a fan of three sinks
- [x] the late feed too: a drop count only agrees if every worker
      seeded its watermark from the coordinator's bounds
- [x] a job the build does not know is an ANSWER naming what it does
      know, not a crash
- [ ] a worker that dies mid-run — stage 5

Stage 5 — failure (TestFailure):
- [x] a worker that throws is buried and its partition is computed on
      a survivor; the answer, the drops and the merged count do not
      move
- [x] a worker is buried ONCE, not once per partition that met it
- [x] forty SEEDED failure schedules — a different subset of workers
      dying at a different request each time — and the answer never
      moves
- [x] one survivor is enough
- [x] when every worker is gone the run says so and names the FIRST
      cause rather than an empty-collection error
- [x] a considered refusal (`Resp.Failed`) is returned, not retried
      on every worker in turn
- [x] A REAL WORKER PROCESS killed mid-run, at a chosen request, and
      the job finishes with the same answer
- [ ] a coordinator that dies — see the limits below

Stage 6a — the epoch loop (TestStream, TestPanesOnce):
- [x] `Sink`'s coordinator side is a FOLD — `empty` / `absorb(s, ws,
      watermark)` / `emit` — and `result` is that fold with one epoch
      and a watermark of infinity, so the batch answer is unmoved
- [x] a job streamed in epochs answers what the batch run answers, at
      every partition count crossed with every epoch size
- [x] a fan of three sinks, streamed
- [x] NO WINDOW IS PRESENTED TWICE, asserted directly rather than
      inferred from a checksum
- [x] a pane handed over twice is not counted twice
- [x] the sessions are let go when the stream ends
- [x] the same over sockets, with the operator state living in
      another party's memory
- [x] on a late-bearing feed a stream drops FEWER than the batch run,
      and that is stated as correct rather than asserted away
- [x] 6b: a worker that dies MID-STREAM is replaced and its
      partition replayed — the answer does not move, under seeded
      schedules that kill up to three of four
- [x] a session asked for an epoch it has already answered answers
      the SAME partial (what a retry after a lost reply must get),
      and a FRESH session replayed to that epoch arrives at the same
      place
- [ ] exactly-once outcome at the sink — 6c
- [ ] durable checkpointing, so a COORDINATOR restart can resume —
      `dataflow-coordinator`

Stage 7: written when the stage is claimed.

## The watermark, and why a slice is not a stream

A windowed operator's answer depends on its WATERMARK, and a
watermark depends on the order the operator saw — so a worker holding
a slice of the input is not looking at the same stream the
single-threaded run looked at. Its `maxSeen` starts at nothing, so its
watermark runs BEHIND the global one, so it closes panes later, so it
drops fewer late elements. Two runs of the same job then differ by
exactly the late elements, and the difference is invisible on a feed
that has none.

§20's Wrocław feed has none by construction (25 s of jitter under a
30 s bound), which is what let its parallel lane assert equality — so
the benchmark could not have caught this and does not.

The engine does not leave it as a caveat. A partitioned source is in
the input's order, so the greatest event time of everything BEFORE
partition *i* is a prefix maximum, computable in one cheap pass over
the timestamps alone (`OkayLane.parallel` already computes exactly
this array for a different purpose). Seeding partition *i*'s window
operator with that value makes its watermark equal the global one at
every element, and the distributed answer equal the single-threaded
answer INCLUDING the drops. That makes `Windows` need one new thing:
a way to start from a known mark rather than from nothing.

## Decisions

- **The plan lives in okay-cluster, not in the core.** The core's
  `Pipeline` is a LOCAL plan and stays one; a distributed plan drags
  in partitioning, a wire and a worker registry. `Flow.Local` holds a
  `Chunks` transformer, so everything below the first exchange is
  still optimized by the core's rules — reuse without a dependency
  inversion.
- **`Sequential` goes in the core**, beside `Aggregator`, because it
  is an algebraic distinction (monoid vs commutative monoid) and not
  a distributed one. A single-threaded window can use it too.
- **Refused: shipping closures.** See Claim 3 for the price.
- **`Windows` gains a seeded start.** The alternative — declaring
  "the distributed answer equals the local one when nothing is late"
  — is a caveat where a theorem is available for one prefix-max pass.
- **Refused: a second plan type.** A `Flow` node that is "just a
  local pipeline" holds the local plan rather than re-deriving map,
  filter and take at the distributed level.

## Results

### Stage 1 — the plan, the local runtime, and Claim 2 demonstrated

Landed with `Flow` and `Flows` in okay-cluster, `Sequential` and
`Windows.seed` in the core.

**The Wrocław job runs on the engine and answers what §20's lane
answers** — all eleven checksums, at 1, 2, 4 and 8 partitions,
including the top-5 ranking that only agrees if the map-side join
agreed first. What the lane writes is now the JOB: a partitioned
source, a filter, a map, a window, an aggregator. The fifty lines of
slice stitching that made §20's parallel lane possible — the pre-pass
for the slice maxima, the completeness rule, the boundary walk for
the bunching pairs — are the engine's, and no user writes them again.

**Claim 2 held on the job it was written for.** Stage 4 of the job —
"two departures of one route from one stop under two minutes apart",
which Flink answers with a `KeyedProcessFunction` over `ValueState`
and which therefore needs every record of a key on one machine — is
a `Sequential` here, and it parallelises with no shuffle at all. The
slice summary is (first, last, n, bunches, gap); merging asks one
more question, whether the gap ACROSS the boundary is short. The
count and the gap sum both agree with the serial run at every
parallelism.

**One thing measured rather than assumed.** The boundary term was
REMOVED as a control, and the two tests that rest on it failed at
every parallelism above 1 and passed at 1 — which is the shape a
correct control has, since a single partition has no boundary.

**What stage 1 costs, said plainly.** No exchange means at most one
keyed stage per flow, so Wrocław's three keyed stages are three flows
and the feed is read three times where §20's lane reads it once. No
number is quoted for this lane and none should be until stage 3 makes
the pass single.

**One defect found by writing the executor, not by using it.**
`Flows.collect` built its accumulator with `Aggregator.apply`, whose
zero is taken BY VALUE — so `init` answers one and the same buffer
however often it is asked, and a stateless plan asks once per
PARTITION. Every fibre would have appended to the same
`ArrayBuffer`. Invisible for the immutable accumulators every other
lane uses, which is why it survived the first suite. Controlled: the
test fails on the defect and on nothing else. `Aggregator.apply` now
says so in its own scaladoc, since the trap is the core's and this
engine is only the first caller to reach it.

### Stage 2 — the exchange, and the number that says when to take it

`Finish` (Merge / Shuffle(r) / Auto) on the keyed and windowed nodes.
The map side writes hash buckets, a reducer takes a contiguous RANGE
of them, and that indirection is what lets the number of reducers be
chosen AFTER the partials are in — which is what `Auto` needs and what
a plan-time constant could not give it.

**The asymmetry that justifies the `Sequential` type, in both
directions at once.** A `Sequential` KEYED aggregator survives the
exchange: a reducer owns a hash share of the keys and merges its
buckets partition by partition, so the order it depends on is
untouched. A `Sequential` TERMINAL does not, and is refused — but not
for the reason stage 2 was expected to give. A keyed stage's output
is a hash map's iteration order whether one reducer produced it or
eight, so an order-dependent fold over it was already wrong under
`Finish.Merge`. The rule the engine enforces is therefore about the
KEYED STAGE, not about the exchange, and it is wider than the one
this spec first sketched.

**The crossover, measured.** `MeasureExchange`, one million rows over
eight partitions, count per key (the cheapest accumulator there is),
minimum of seven alternating rounds:

| distinct keys | accumulators | merge ms | shuffle ms | ratio |
|---:|---:|---:|---:|---:|
| 1 000 | 8 000 | 1 | 2 | 0.50x |
| 10 000 | 80 000 | 2 | 3 | 0.67x |
| 20 000 | 160 000 | 4 | 3 | 1.33x |
| 30 000 | 240 000 | 5 | 3 | 1.67x |
| 50 000 | 400 000 | 8 | 4 | 2.00x |
| 100 000 | 800 000 | 20 | 8 | 2.50x |
| 250 000 | 1 000 000 | 43 | 9 | 4.78x |
| 1 000 000 | 1 000 000 | 54 | 21 | 2.57x |

So `autoBound = 100 000` accumulators, which sits inside the bracket
the measurement leaves. A fatter accumulator moves it down (merging
gets dearer, the hand-off does not); fewer partitions move it up.

**The prediction this lane wrote down beforehand was right, and it is
the useful half.** Wrocław's keyed stage has ~10^4 accumulators
against 10^6 events — three orders of magnitude under the crossover —
so `Auto` DECLINES the exchange on the job this engine exists for,
and the suite asserts that it declines it. An engine that shuffles
because shuffling is what engines do would pay that 1.5x for nothing.

**The harness had to be rebuilt before any of this could be believed,
and that is worth recording.** Its first table read 61 ms at 10 000
keys and 10 ms at 20 000 — more work in less time, which is not a
measurement of the work but of the previous row's two million dead
objects in a 1 GB test heap. A lane that swings six-fold cannot price
a two-fold effect. What fixed it: a smaller feed, a settle between
rows, the two roads ALTERNATING round by round so drift hits both,
and the MINIMUM of the rounds reported with the worst round beside it
so a reader can see whether to believe the row.

**One thing found by measuring rather than reasoning.**
`Flow.slices` cut its partitions with `xs.iterator.slice(from, until)`
— and an `Iterator` reaches its start by DROPPING, so the last of
eight partitions stepped through seven eighths of the input and threw
it away. Measured directly: 456 µs against 5 µs for the same eighth
through `view.slice`. It had been there since stage 1, under a lane
that was still correct and still passed everything.

### Stage 3 — one pass, many sinks, and the number that came out badly

`Sink[A, R]` is a keyed or windowed stage together with what its
output is folded into; `and` pairs two of them; `Flows.fan` drives one
pass over each partition through all of them. The shape is
deliberately `Aggregator.zip`'s — the core has computed two statistics
in one pass since P1, and this is that idea one level up, over stages
that carry keys, windows and watermarks instead of scalars. Seeding
survives: a sink declares the event-time functions its windows need,
`and` concatenates them, and one pre-pass computes a prefix maximum
per column, so a fan whose stages window on DIFFERENT times is still
exactly the single-threaded answer, drops included.

The whole Wrocław job is now one plan. All eleven checksums hold at
every parallelism.

**And then the measurement, which is not flattering.** One service
day, 296 000 events, same JVM, minimum of 7 rounds:

| lane | ms | vs best |
|---|---:|---:|
| hand-written, 8 threads (`OkayLane.parallel`) | 22 | 1.00x |
| hand-written, 1 thread (`OkayLane.run`) | 79 | 3.59x |
| engine, 8 partitions, one pass | 121 | 5.50x |
| engine, 8 partitions, three passes (what stage 2 did) | 125 | 5.68x |
| engine, 1 partition, one pass | 218 | 9.91x |

Two things to read off it. **One pass bought 3%**, not the third the
name suggests — the prediction written into the claim beforehand said
it would not be a third, and it was righter than it knew. And the
engine is 5.5x the hand-written lane at the same parallelism, which
is a number this spec has to explain rather than round off.

**Where the time goes, decomposed rather than guessed** (8
partitions, one sink at a time):

| lane | ms |
|---|---:|
| the source alone (count) | 2 |
| route windows only — tumbling, 138 keys | 7 |
| **stop windows only — sliding, 2482 keys x 3 panes** | **93** |
| bunching only — keyed state | 4 |
| all three, one pass | 111 |

The three sinks together cost what they cost apart plus about 5 ms, so
the fan really is one pass. **84% of the run is one sink**, and the
reason is a count this spec can state exactly: the job produces 22 543
route panes and **362 983 stop panes**, and every partition holds most
of the stop panes, so the coordinator merges on the order of 2.9
million accumulators — on one thread.

**That refutes a scope decision made in this lane's own claim.** It
said a fan finishes by merge because "a fan of Wrocław-sized stages is
three orders of magnitude under the crossover". True of the route
stage. False of the stop stage by a factor of about thirty: at ~3x10^5
accumulators per partition it is well ABOVE stage 2's measured 100 000
bound. The exchange was disabled for fans on the strength of an
arithmetic that was only checked against the smaller stage.

**But the exchange is not the best fix available, and the measurement
says why.** `OkayLane.parallel` does not parallelise that merge — it
AVOIDS it. It emits at the slice every pane no other slice can touch,
and hands back only the handful that span a boundary. The engine
already computes the array that rule needs: the prefix maxima it
gathers for seeding are exactly `hi`. Filed as `dataflow-complete-panes`,
with this table as its bar.

### dataflow-complete-panes — the 5.5x, and where it went

A partition's windowed operator now emits a pane itself when no other
partition can touch it: `p.start > hi(i-1)` and
`p.end <= hi(i) - back`. Complete panes are presented and folded into
the terminal AT THE PARTITION; only the boundary panes reach the
coordinator. `hi` was already there — it is the prefix-maximum array
the watermark seeding needs, and the seed IS the lower bound. What
was added is `back`, two more columns in the same pre-pass (each
partition's own backwardness and its least event time, combined with
the exclusive prefix max). That combination OVER-estimates `back`,
which is the safe direction: it declares fewer panes complete, never
more.

**The number, on the instrument this lane had to rebuild** — four
service days, 1 255 298 events, every lane run once per ROUND so
drift hits all of them, best of 7 with the worst beside it:

| lane | ms | (worst) | ev/s | vs best |
|---|---:|---:|---:|---:|
| hand-written, 8 threads (`OkayLane.parallel`) | 85 | 137 | 14 768 211 | 1.00x |
| **engine, 8 partitions, one pass** | **97** | 115 | 12 941 216 | **1.14x** |
| hand-written, 1 thread | 370 | 1050 | 3 392 697 | 4.35x |
| engine, 1 partition, one pass | 468 | 646 | 2 682 260 | 5.51x |
| engine, 8 partitions, three plans via `Flows.run` | 737 | 964 | 1 703 253 | 8.67x |

**5.50x to 1.14x.** The claim predicted the two outcomes it could
have: near the hand-written lane if the merge was the whole cost, or
50-60 ms if half of it was the per-pane tuple key. It was the first —
the merge was essentially all of it. And the engine's WORST round
(115 ms) is better than the hand-written lane's worst (137): the
plan's bar is the tighter of the two, which is not something a
benchmark usually finds in the general machinery's favour.

The mechanism is countable rather than inferred. The job makes
1 734 893 panes; at eight partitions **122 679 accumulators reach the
coordinator** — 7% of what ONE partition holds, and about 1.5% of
what eight of them held before. `Run.merged` reports it and the suite
asserts it drops, so this is not a benchmark's private knowledge.

**The last row is not a measurement of the pass count.** It drives
the single-stage road, `Flows.run`, which has no completeness rule
and still merges every pane — the engine as it was an hour before.
Stage 3 measured one pass against three at 3%; the 7.6x here is the
completeness rule, not the fan. Those two numbers must not be added.

**Two things this leaves open, both filed.** `Flows.run`'s windowed
node keeps the old behaviour, so the single-stage road is now much
the slower of the two and the docs say so; and the decomposition
(source 5 ms, route 18, stop 71, bunching 25) sums to 104 against a
fan of 154, so about a third of the fan's time is not in any of its
sinks. Neither is explained here, and neither is guessed at.

### Stage 4a — what crosses a wire, and its Schema

`Sink` had two ideas behind one type. `P` is the partition's WORKING
state — a live `Windows`, mutable maps, a terminal accumulator being
folded into — and it never leaves. `W` is what LEAVES, and only `W`
has to be a value: immutable, mergeable, and describable. `finish(p):
W` closes the operator and hands over; `result`, `drops` and `merged`
take `W`. `Wire[A, R]` then adds exactly one member, a `Schema[W]`,
and composes through `and` with the pair's Schema.

**What crosses is small, and the completeness rule is why.** A
partition has already folded every pane it could finish alone into
its terminal accumulator, so `Handed` carries ONE value standing for
all of them plus the handful of boundary panes. The Schema
requirement lands on the smallest thing it could: the key, the pane
accumulator, and the terminal's accumulator.

**Claim 3 is now a compile error rather than a promise.** A sink
whose partial nobody can describe has no `Wire`, and that is refused
where the sink is BUILT — not as a `NotSerializableException` inside
a task on another machine.

**Tested without a socket.** `Flows.fanWired` runs the ordinary fan
with every partial encoded and decoded between `finish` and `result`.
That is precisely what a worker and a coordinator do, performed in
one process, so a Schema that loses a field is caught in
milliseconds. It is not a network simulation — nothing is delayed,
dropped or reordered, which is stage 5's business — it pins the one
thing sockets cannot fix: whether the partial is a value.

**A control caught the test being weaker than it looked.** With the
drop count deliberately dropped from the Schema, the first version
of the round-trip test still PASSED, because its feed had no late
elements and both sides read zero. The test now runs a late-bearing
feed too; with that, the same break fails it. A round-trip test on
data that cannot exercise a field is testing the other fields only.

### Stage 4b — the coordinator, the worker, and four real processes

`Job[P, R]` is the registry entry, and it is Claim 3 made into a
type: a worker is asked for a NAME and a Schema-encoded parameter and
BUILDS THE PLAN ITSELF. No function crosses, no lambda is serialized,
no class is shipped, and therefore none of the failure modes that
come with those exist. The price is stated rather than hidden: every
worker runs the same artifact. You deploy a build and submit a name —
the same bargain Flink strikes with a submitted jar, made explicit
instead of hidden behind a serializer that usually works.

**Two requests, and they are a transcription rather than a design.**
`Extent(job, params, part, of)` is the pre-pass; `Run(…, bounds)` is
the fold. They are exactly the two passes `Flows.fan` already makes
over each partition in one process, with the first now data-local on
the worker.

**Why two round trips, when one is obviously cheaper.** Because a
partition's watermark must be the STREAM's watermark at that point —
stage 1's theorem — and a worker cannot know what the partitions
before it saw. The coordinator is the only party that can compute a
prefix maximum, so it must hear from every partition before any of
them may fold. The cheaper protocol is not faster, it is a different
answer.

**The transport** is a four-byte length and CBOR. No line framing and
no base64: a partial is already bytes, and re-encoding it as text to
fit a line-oriented protocol is a cost paid on every accumulator that
crosses.

**A worker is `Req => Resp`** — which is what `Cluster.Worker` has
been since P7 — so the driver cannot tell an in-process worker from a
socket. That is why the tests go in three levels: in-process first,
then sockets in one JVM, then four real processes. A failure at level
three is a failure of the transport and never of the arithmetic,
because the arithmetic was pinned at level one.

**Four JVMs that share nothing but an artifact and a name.** The
acceptance runs a windowed job and a fan of three sinks across them,
and asserts the answer, the drop count AND the merged count equal
what one process computes. The late-bearing feed is in there
deliberately: its drop count only agrees if every worker seeded its
watermark from the coordinator's bounds, so the seeding theorem is
what that assertion is really testing.

**Controlled.** With the children started WITHOUT their registrar the
test fails, naming the empty registry it got back — which is what
proves the work happens over there rather than quietly at home.

**What this stage does not do, and does not pretend to.** A worker
that dies takes the run with it: there is no retry, no reassignment,
no recomputation yet, and a partition being a thunk is what will make
that cheap in stage 5. One request is in flight per connection. The
coordinator is a single point of failure. None of that is hidden
behind a hopeful word.

### Stage 5 — a worker dies and the job does not

A thrown error is a DEAD WORKER: it leaves the rotation and its
partition is asked of a survivor. What makes that nearly free is
structural rather than clever — a partition is a THUNK and its
partial is a pure function of the four things every worker is given
(the parameters, the index, the count, the bounds). There is no
lineage graph to walk and no checkpoint to restore, because nothing
was mutated. `Run.retried` reports the burials, so a suite asserts
that recovery HAPPENED rather than inferring it from the answer being
right.

**A `Resp.Failed` is not retried**, and the reason is worth stating:
it is the worker's considered answer — it decoded the request and
refused — and every worker runs the same build, so asking three more
produces the identical refusal. Retrying a deterministic "no" is not
resilience, it is noise in front of the same message.

**Not exactly-once EXECUTION.** A worker that dies after computing
but before its reply arrives has its partition computed twice, and
that is correct because the coordinator keeps exactly one partial per
partition — exactly-once OUTCOME, the words specs/persist.md already
settled on.

**The failures are seeded, not lucky.** Forty schedules, each dooming
a different subset of workers at a different request. A red run names
the seed that produced it.

**A limit the first version of that test found by asking for it.** A
buried worker never returns, and that is the honest model rather than
a simplification: a `Serve` from `Served.connect` IS a connection, and
a broken connection does not heal. It also means a run cannot survive
a blip on EVERY worker — the first seeded test made all four flaky and
correctly died with "no workers left". Reconnection, and burial only
after k consecutive failures, are a different lane and are named in
the backlog rather than half-built here.

**Two things still fatal, said rather than implied.** The coordinator
is a single point of failure: if it dies, the run dies with it, and
nothing is journaled. And a job longer than the workers' patience has
no checkpoint to resume from — every recovery here is a recompute
from the source. For a batch job over a replayable source that is the
right trade; for an unbounded stream it is not, and that is stage 6.

### Stage 6a — the epoch loop, and three ways to present a pane twice

Everything before this was a BATCH engine that happened to run across
machines: a partition is read to its end, its partials are merged
once, an answer is produced once. Three changes make it a stream, and
their order was forced rather than chosen.

**1. The coordinator side of `Sink` became a fold.** It had
`result(ws): R`, which can only be called when there is nothing left
to come. It now has `empty`, `absorb(s, ws, watermark)` and
`emit(s)`, with `result` defined as that fold over one epoch at a
watermark of infinity — so no existing behaviour moved and the batch
answer stayed the definition of correct. Without it the coordinator
can never retire a pane, and "streaming" would mean holding every
boundary pane for ever.

**2. The watermark is the MINIMUM over the partitions**, minus the
window's DECLARED lateness. Both halves are load-bearing. The minimum,
because a partition that has read less may still produce something
earlier than the others' greatest — and one that has read nothing may
produce anything. The declared lateness rather than the observed
backwardness, because the observed figure is only what has been seen
so far and it grows: it is not a bound on the future, and the user's
`lateness` parameter is the only number here that is.

**3. An epoch**: `Open` / `Advance` / `Close`, with the operator state
living on the worker between rounds. A pane open at an epoch boundary
stays open, which is the whole difference between this and running
the batch driver in a loop.

**THE SAME BUG THREE TIMES, and it is worth the space.** Every one of
these produced identical symptoms — the sums agreed exactly and the
pane COUNT was ten too high, all ten in window 99000, which is where
the two partitions meet. A pane retired before its last contributor
has handed over comes out as two panes whose values add up.

  - the local completeness rule (stage 3's) fired in a stream, where
    `back` is an under-estimate and `upper` is therefore too
    generous. A streaming partition now finishes nothing locally; the
    coordinator retires instead. The rule stays what it is — a batch
    optimisation that needs the whole extent to be legal.
  - the coordinator's watermark used the observed backwardness. See
    (2).
  - and the last one: the sources being EXHAUSTED was treated as the
    stream being OVER. It is not. Every operator still holds the panes
    its own watermark never closed, and those come out on the Close —
    so retiring everything at the last `Advance` throws away the slow
    partition's half of a boundary pane.

Only the third was found by reasoning; the first two were found by
the same failing assertion, and the third by finally printing which
panes differed instead of arguing about which could. `TestPanesOnce`
now asserts the invariant directly, so the next version of this
mistake announces itself as itself.

**One thing that is NOT a bug, and the test says so.** On a feed with
late elements a streamed run drops FEWER than a batch run and
therefore counts more. The batch engine reconstructs one global order
out of its slices — stage 1's seeding theorem — and a stream has no
such order to reconstruct: its partitions are independent channels,
each with its own watermark. Asserting equality there would be
asserting that a stream is a batch.

### Stage 6b — a replacement worker replays rather than restores

6a's worker kept its operator state in memory, so a worker that died
mid-stream took its partition's open panes with it — and did not even
fail cleanly, since the retry reached a survivor that answered "no
session".

**A snapshot is not the only road, and here it is the wrong one.** To
snapshot an operator you must describe its insides — a live
`Windows`, its pane map, its watermark — and every one of those
becomes a wire format that has to survive a version change. The other
road is the one this engine has taken since stage 1: a partition is a
RECIPE, and its epoch partial is a pure function of (parameters,
index, count, epoch size, epoch NUMBER). So a replacement does not
need the state. It rebuilds it.

`Advance` therefore carries the epoch INDEX rather than meaning
"next". A session already at that index re-answers the same partial;
one behind catches up silently by replaying and discarding; a worker
with no session at all is given the job and then does the same.
**Recovery is the ordinary case of one mechanism, not a second one** —
and to keep it that way there is no upfront `Open`: every run's first
`Advance` takes the same path a replacement takes, so the recovery
road is exercised on every run rather than only when something dies.

**The same epoch, both times**, and that is the whole correctness
argument. Asking for the NEXT epoch instead would lose one epoch's
data and fail nothing — which is the mistake this lane's claim
predicted of itself, and which `TestStream`'s idempotency test now
pins from both sides: the same session asked twice, and a fresh
session replayed to the same index.

**The cost, stated rather than buried**: recovery is O(elements
consumed so far), not O(state). For a source that can seek — a topic
offset, a file position — it becomes O(elements since the oldest open
pane), and the seam for that is `Flow.Src`'s thunk. Not built,
because nothing has asked.

**A test was wrong before the code was, again.** The idempotency test
compared two `Resp.Epoch` values with `==`, and `Resp.Epoch` carries
an `Array[Byte]` — whose equality in Scala is REFERENCE equality. It
reported a difference that was not there, and its own failure message
showed the extents identical on both sides. It compares bytes now.

**What 6b still does not do**: if the COORDINATOR dies the run dies
with it. It holds the folded state and journals nothing, and no
amount of worker recovery helps. That is `dataflow-coordinator`, and
it is an assembly of okay-persist's log rather than an invention.

**The seeding is not decoration.** On a feed whose jitter exceeds the
window's lateness, an unseeded parallel run drops FEWER late elements
than the stream does and answers differently — asserted in both
directions rather than described.
