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
- **1 — the plan and the local runtime.** `Flow`, its rewrite rules
  (combine below exchange, local fusion below everything), and an
  executor over N fibres in one process. Acceptance: the existing
  Wrocław checksums, at every parallelism.
- **2 — the exchange.** A real hash partition with combine above it,
  `Finish.Auto`, and the crossover MEASURED rather than assumed.
- **3 — one pass, many sinks.** A job is several flows over one
  source; the shared source is detected and the pass is single. This
  is what §20 already names as the asymmetry: a fan-out in one JVM is
  three method calls, in Flink it is three shuffles.
- **4 — across processes.** The worker protocol: jobs by name, typed
  parameters, chunked framed transport, partial results back.
  Acceptance: the full Wrocław `Result` across four OS processes.
- **5 — failure.** Batch: recompute a lost partition from lineage —
  a `Chunks` partition is a value, so this is nearly free. Streaming:
  barrier checkpoints into okay-persist with source offsets.
  Deterministic under `Sim` seeds, not under luck.
- **6 — streaming, properly.** Unbounded sources, the watermark as
  the minimum over input channels, keyed state in a backend, and
  exactly-once OUTCOME at the sink.
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

Stage 2 and later: written when the stage is claimed.

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

**The seeding is not decoration.** On a feed whose jitter exceeds the
window's lateness, an unseeded parallel run drops FEWER late elements
than the stream does and answers differently — asserted in both
directions rather than described.
