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
- **6 — streaming, properly.** DONE. 6a (the epoch loop, the
  coordinator as a fold, the watermark as the minimum over the
  partitions), 6b (a dying worker's partition is replayed on a
  survivor), 6c (exactly-once OUTCOME at a keyed sink, at-least-once
  execution underneath, and the offers counted rather than promised).
- **11 — the log is the source.** A `Flow` whose partitions are
  okay-persist topic partitions that SEEK by epoch (offset = begin +
  epoch x take), so a resumed run reads from the last epoch instead
  of replaying the source from the start — and a staging sink whose
  output offset commits with the epoch, so the run is exactly-once
  from log to log on the repository's own primitive. Verifiable on
  one machine with `MemoryStore`.
- **12 — the network.** The cross-process harness on machines that
  are not this one, or containers with injected latency and loss.
  BLOCKED on machines; the boxes are written so that the day they
  exist the work is a run and not a design.
- **13 — rescale at an epoch boundary.** Partitions are fixed at
  submission today. With the journal and 6b's replay, a stream can
  change its partition count between two epochs: stop at N, re-cut
  the source, resume at N+1 with the new count. Verifiable on one
  machine.
- **10 — the election.** DONE. `Lease` (three methods, no
  dependency), `Cluster.leading`, and a FENCE on the journal so a
  deposed coordinator stops at its next epoch rather than committing
  over its successor. The seam binds to okay-persist's `Election` as
  it stands.
- **9 — the commit window.** DONE. `Sink.committed(epoch)` and
  `Sink.recovered(epoch)` — the two moments a writer needs — and
  `Sink.staging`, a sink that hands a whole epoch over when that
  epoch is final. The engine cannot close the window alone; a writer
  that records the epoch beside its rows can, and now has what it
  needs to.
- **8 — the coordinator survives.** DONE. `Wire.state` makes the
  coordinator's fold a value, `Checkpoint` is where it writes it
  down, and a second `Cluster.stream` over the same journal picks the
  run up at the next epoch. The store is the caller's; the seam binds
  to okay-persist's compacted log in eight lines.
- **7 — the numbers.** DONE. A distributed lane in
  docs/benchmarks.md §20 — the Wrocław job across four real OS
  processes, its wire weighed, and its FIXED cost separated from its
  marginal one by the same least-squares split that section already
  applies to Flink. What is NOT there, and says so: a cluster number
  for Flink or Spark, which would need a cluster.

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
- [x] a coordinator that dies — stage 8, for a STREAM; a batch
      `Cluster.run` is a two-pass function with nothing to resume
      from, and is restarted

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

Stage 6c — a row that LEAVES the engine (TestOnce):
- [x] a windowed sink whose retired panes are handed to a writer with
      their identity — `(window start, key)` — and which answers how
      many it OFFERED
- [x] on a quiet batch run, offers == panes exactly
- [x] WHEN A REPLY IS LOST — a worker that served the request in full
      and then vanished, so its panes were written and its partial was
      not received — the store holds each (window, key) once with the
      batch value, and the offers EXCEED the panes: 3,598 offers for
      3,204 panes on one loss, 3,981 on two
- [x] every repeated offer carried the SAME value (a counter that
      would see a differing one, asserted at zero, and shown to fire
      when its predicate is inverted — 394 repeats reach it)
- [x] twelve seeded loss schedules, and the store is the batch answer
      in every one
- [x] the run's own answer is the PANE count, not the offer count:
      the lost partial took its count with it and the recomputed one
      replaced it
- [x] streaming writes from the COORDINATOR — offers == panes, at
      every partition count crossed with every epoch size — because a
      streaming partition finishes nothing locally
- [x] exactly-once ACROSS RUNS, once the coordinator journals — stage
      8. The window that remains is one epoch wide and named there
      rather than closed
- [x] durable checkpointing, so a COORDINATOR restart can resume —
      stage 8

Stage 11 — the log is the source (box 1 landed; TestSourceLog):
- [x] a partition IS a topic partition: `Streams.chunks(topic, p,
      from)` is a `Chunks[Record]` and `Flow.of` takes the thunk —
      three lines, no dependency in either direction. A job over a
      MemoryStore topic answers what the fan over the array answers at
      1, 4 and 8 partitions, batch and streamed, `merged` included
- [x] positioned by epoch — box 2, FOR THE SINKS THAT CAN: `Flow.Src`
      thunks take a start, `Resp.Epoch` carries the position, the
      journal carries one per partition, and a session opens AT it —
      on a resume and on a replacement worker mid-run, which is one
      road. `Sink.seekable` says who can: fold and keyed (they hand
      over deltas and clear), never windowed. A keyed job resumed
      after a coordinator death reads exactly `total - Σpositions`
      records, asserted by a counting Topic; a windowed one reads the
      whole topic again. Controlled: opening at zero fails both seek
      tests
- [ ] box 2b — a WINDOWED sink that seeks. Two roads, neither free:
      delta handovers of open panes every epoch (a change to
      `okay.Windows`, and the merge traffic grows from panes-closed to
      panes-open per epoch), or a replay bounded by the window horizon
      (the session records a (position, max event time) pair per
      epoch; a fresh one seeks to the epoch whose maximum is below the
      oldest open pane's start and replays from there, seeded with
      that maximum so late-drop decisions do not move). Measure the
      merge traffic of the first against the replay length of the
      second before choosing
- [ ] a resumed coordinator's workers open at the journal's epoch and
      read from there: recovery is O(elements since the last epoch),
      asserted by counting what the topic was asked for
- [ ] `Sink.stagingTo(topic)`: an epoch's panes appended as records
      with the epoch in the key; `committed(epoch)` is the append and
      `recovered(epoch)` reads the topic's tail to know what already
      landed — the writer's atomicity is the log's append
- [ ] exactly-once from log to log: kill the coordinator between the
      append and the journal commit, resume, and the output topic
      holds every pane once — the stage 9 window, closed by a store
      that IS the journal's kind
- [ ] the same on `KafkaStore` when a broker is available (Live)

Stage 12 — the network (BLOCKED: needs machines that are not this one):
- [ ] the three engines' cross-process harness over a real link, and
      the fixed/marginal split re-measured — the first number this
      repository will have about a wire that is not loopback
- [ ] injected latency and loss (`tc netem`, or containers) against
      the tolerance of `dataflow-reconnect`: at what loss rate does a
      run stop finishing, and is three consecutive failures the right
      count when a failure is a packet
- [ ] a partition of the network between coordinator and a worker:
      the worker is buried, the partition moves, and when the link
      heals the worker's old session is NOT resumed into a run that
      has moved on (the fence, from the worker's side)
- [x] what can be done on ONE machine meanwhile: a `Serve` that loses
      requests by a seeded schedule (TestNetem, in the default gate),
      and the tolerance question answered without a network — see the
      Results entry: on a lossy wire the loss never ends a run, the
      burial policy does; `tolerance` is a parameter of `Cluster.run`
      and `Cluster.stream` now

Stage 13 — rescale at an epoch boundary (not started):
- [ ] a stream running at `parts` stops at epoch N and resumes at N+1
      with `parts'`; the answer equals the batch answer
- [ ] the boundary panes of the old cut are re-bucketed rather than
      re-read: the journal holds them, and the completeness rule
      recomputes with the new extents
- [ ] a worker added mid-stream takes partitions from the next epoch;
      one removed hands its back — the same mechanism as death,
      without the death

Stage 10 — the election (TestElection, TestPersisted):
- [x] `Lease`: `take(): Option[Long]` / `held(term)` / `release(term)`
      — three methods over a term, so the engine can be given a real
      election without depending on one
- [x] `Lease.solitary`, so a run with no second coordinator pays
      nothing for the seam
- [x] `Cluster.leading` takes the seat, fences the journal by the
      term, runs, and gives the seat up; `None` means somebody else
      holds it
- [x] it does NOT wait to be elected — a retry loop needs a clock and
      a backoff that belong to a supervisor, and one attempt composes
      into any of them (the doc shows the loop)
- [x] a leader that dies: the next candidate takes the lapsed seat
      and finishes the job with the batch answer
- [x] THE GHOST IS REFUSED: a coordinator deposed BETWEEN its epochs
      throws at its next commit and the journal still ends at the
      epoch it lost the seat on
- [x] controlled: without the fence, the ghost commits and both ghost
      tests fail
- [x] the successor of a refused ghost still answers the batch answer
- [x] bound to okay-persist's `Election` as it stands (TEST scope):
      `tryTakeover` is the term, `leader` is `held`, `heartbeat` is
      what a leader should do once an epoch anyway
- [x] two nodes, one seat, on the real election: the second is told
      no while the lease is live and takes over when it lapses
- [ ] a compare-and-set commit. The fence is a check before a write,
      so a leader deposed between the two can land one commit; the
      seam permits a conditional write and no store here offers one

Stage 9 — the commit window (TestStaged):
- [x] `Sink.committed(epoch)` / `Sink.recovered(epoch)`, defaulting
      to nothing, forwarded by `and` and by every `Wire` wrapper
- [x] the sink hears the commit BEFORE the journal does — told after,
      a writer would lose every epoch the coordinator died inside
- [x] the final sweep is its own epoch (`round + 1`), or a writer
      that recognises a repeat by its number drops the close's panes
      as a duplicate of the last round's
- [x] a batch run is one epoch and says so: `Flows.fan` and
      `Cluster.run` call `committed(1)` when the fold is done
- [x] `Sink.staging` / `Wire.tumblingStaged`: an epoch's panes handed
      over as one batch, with the epoch number
- [x] under the death that makes a plain writer repeat, a staging
      writer that drops an epoch it has applied writes each pane
      EXACTLY once — and the test asserts the repeat REACHED it
- [x] under a death after the commit, nothing repeats at all
- [x] a staging sink REFUSES a batch run rather than dropping the
      panes the completeness rule wrote on a worker
- [x] both controlled: journalling before telling the writer loses
      panes, and makes the window test say it is not exercising
      anything
- [ ] a writer whose stage is DURABLE, so the two-phase commit
      survives the writer's own death as well as the coordinator's.
      The seam is enough for one; nothing here has asked yet

Stage 8 — the coordinator survives (TestResume, TestPersisted):
- [x] `Wire.state: Schema[S]` — the coordinator's fold is a value,
      by `SIso` where the state is a mutable map
- [x] `Checkpoint`: two methods over bytes, an injectable seam, with
      `Checkpoint.none` as the default so a run that does not ask for
      durability pays nothing
- [x] the journal carries the fold, the per-partition EXTENTS, the
      drop and merge counters and the SESSION IDS — a resumed
      coordinator that forgot the extents would answer a different
      `dropped` for the same stream
- [x] a coordinator that dies AFTER committing an epoch: a successor
      over the same journal answers what the batch run answers, with
      the same `merged` as an uninterrupted stream
- [x] a coordinator that dies BEFORE committing: the epoch is asked
      for again, the session re-answers the same partial, and it is
      absorbed once — `merged` still equals an uninterrupted stream's
- [x] both controlled: resuming one epoch late and one epoch early
      each fail all four resume tests
- [x] the successor inherits the session ids, so a dead coordinator
      strands no sessions on any worker
- [x] the resume survives the workers dying too — sessions reopened
      under the inherited ids and replayed (6b's road)
- [x] a WRITING sink across runs: after a committed death every
      (window, key) is offered exactly once across the restart; after
      an uncommitted one that epoch's panes are offered again, the
      store still holds each once, and the test asserts the repeat
      HAPPENS rather than hoping it does not
- [x] the seam bound to okay-persist's compacted log (TEST scope): a
      stream journalled there answers the batch answer, a successor
      given nothing but the log finishes the job, and the log holds
      every epoch's state in order
- [ ] a batch `Cluster.run` that resumes — it is a two-pass function
      with nothing to resume from, and restarting it is the answer
- [ ] a coordinator ELECTION, so a successor starts by itself.
      okay-persist has `Election`; nothing here asks for it yet

Stage 7 — the numbers (MeasureWroclawCluster in compare, `Live`):
- [x] the Wrocław job as a `Job[Days, R]`: submitted by NAME, its
      plan built on every worker from one `Int`, its three partials
      described by Schemas written once
- [x] the eleven checksums equal on all four roads — eight fibres in
      one JVM, the coordinator with in-JVM workers, over sockets, and
      over four real OS processes — asserted before anything is timed
- [x] the four roads timed, interleaved, best of five rounds: 1.00x /
      1.65x / 2.00x / 1.83x, with the worst beside each
- [x] the FIXED cost split from the marginal one over three feed
      sizes: 14 ms and 18.1M ev/s in one JVM, 79 ms and 15.9M ev/s
      over four processes — the distribution costs a constant, not a
      rate
- [x] what crosses, in BYTES: 6 830 878 for 1 255 298 events, in 16
      requests, with the per-stage breakdown beside it
- [x] Claim 2 priced: stage 4's keyed state crosses as 68 091
      accumulators where a shuffle would move 1 255 298 records
- [x] the codec's share measured on its own (111 ms decode, 67 ms
      encode, single-threaded) — which is why the road adds only 70 ms
- [ ] a real CLUSTER, and Flink and Spark on one too. Not measured
      and not estimated: it needs machines this benchmark does not
      have, and a number for it would be an invention

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
the single-stage road, `Flows.run`, which had no completeness rule
and still merged every pane — the engine as it was an hour before.
Stage 3 measured one pass against three at 3%; the 7.6x here is the
completeness rule, not the fan. Those two numbers must not be added.

**Two things this left open, both since closed by their own lanes.**
`Flows.run`'s windowed node has the rule now
(`dataflow-run-complete-panes`, below: 647 ms to 182 on the same box,
and the two roads' merge counts asserted EQUAL). And the "third of a
fan's time in none of its sinks" did not survive being measured
properly (`dataflow-fan-overhead`, below).

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

**A limit the first version of that test found by asking for it**, and
it held for five stages: a buried worker never returned. A `Serve`
from `Served.connect` IS a connection, a broken connection does not
heal, and a run therefore could not survive a blip on EVERY worker —
the first seeded test made all four flaky and correctly died with "no
workers left". `dataflow-reconnect` closed it with the two halves
that entry named, and the section for it is below.

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
amount of worker recovery helps. That is `dataflow-coordinator` —
taken as stage 8, and it was indeed an assembly of okay-persist's log
rather than an invention.

**The seeding is not decoration.** On a feed whose jitter exceeds the
window's lateness, an unseeded parallel run drops FEWER late elements
than the stream does and answers differently — asserted in both
directions rather than described.

### Stage 6c — a row the engine wrote, and the identity a pane already has

Five stages of recovery rest on one trade: a partition may be
COMPUTED twice, and that is correct because the coordinator keeps
exactly one partial per partition. Nothing had ever LEFT the engine,
so the trade never had to be defended. The moment a pane is written
somewhere the question stops being "is the arithmetic right" and
becomes "did this row land twice".

**The identity was already there, and that is the whole mechanism.** A
retired pane is `(window start, key)`, unique by construction — a
window is a half-open interval and a key is a key. So `Sink.writing`
hands the writer that pair and nothing else is needed: no transaction,
no two-phase commit, no dedup table, no tuning. The terminal is an
ordinary `Aggregator` (`Sink.writes`), because "leaves the engine" is
just what this particular fold does on the way past — the engine grew
no seam for it.

**What is promised, in the two halves that are actually true.** Every
(window, key) a run retires is offered to `write` AT LEAST ONCE, and
every offer of one identity carries the SAME value. A keyed writer
therefore ends with each identity present once, holding the batch
answer: exactly-once OUTCOME, the words specs/persist.md already
settled on.

**It is not exactly-once EXECUTION, and that is measured rather than
conceded.** `TestFailure`'s dying worker throws INSTEAD of serving, so
its partition is never computed and nothing is ever written twice —
which would have let this lane claim anything it liked. `TestOnce`
injects the other failure, the one `Cluster.ask`'s comment has named
since stage 5 and no test had produced: a worker that serves the
request IN FULL — every pane the completeness rule lets it finish is
written — and then loses the reply. The coordinator cannot tell the
two apart, so the outcome has to survive both.

```
                                 panes    offers   rewritten   the run's answer
  batch, nothing dies            3,204     3,204           0             3,204
  batch, one reply lost          3,204     3,598         394             3,204
  batch, two replies lost        3,204     3,981         777             3,204
  streamed, any parts x take     3,204     3,204           0             3,204
```

Three readings, and each is a sentence the field usually blurs:

  - **the offers exceed the panes by 12% on one lost reply.** That is
    the recomputed partition rewriting what it had already written.
    The repeat is not a defect to be removed — every stage since 5
    depends on a partition being recomputable — and the identity is
    what makes it harmless. A writer that COUNTS rather than keys (an
    append, a `+= 1`) gets at-least-once and nothing more, which is
    why the answer is documented as a count of OFFERS.
  - **the run's own answer never moves.** The lost partial took its
    count with it and the recomputed one replaced it, so the engine
    reports 3,204 while the writer saw 3,598. Two different true
    numbers about the same run.
  - **a stream offers exactly once**, and that is not a second
    mechanism: a streaming partition finishes nothing locally (6a), so
    every pane reaches the coordinator as an accumulator and is
    written in one place. The batch run trades an extra offer under
    failure for the 1.7 million panes it never sends.

**Both halves of the claim were controlled.** Keying the store on
`key` alone fails all four tests; inverting the equal-value predicate
fails the two that inject a loss and passes the two that do not —
which is what proves 394 repeated identities actually reach that
check, rather than an assertion that could never fire.

**The boundary is sharp, and the writer runs where the pane retires.**
A pane the completeness rule let a partition finish alone is written
on that WORKER; a boundary pane is written on the COORDINATOR when
the watermark passes it. A `write` is therefore a closure over what
the worker process can reach — a table, a topic, a file — never over
the submitting process's memory. Nothing new: `Job` has built its
sink from parameters on the worker since 4b, so the writer is
constructed there like everything else and no closure crosses the
wire.

**And what this is NOT: exactly-once across runs.** A coordinator that
dies and starts again re-offers everything, because it journals
nothing. Within a run the identity is enough; across runs it needs
the journal, which is `dataflow-coordinator`.

### Stage 7 — the distributed road, weighed

Stages 4 to 6 proved the same answer comes back across processes.
None of them asked what that costs, and the engine's only published
number (1.14x of §20's eight-thread hand-written lane) was measured
inside one JVM. Stage 7 puts the Wrocław job — the whole five-stage
one, eleven checksums — through the coordinator across four real OS
processes and prices every step of the way there.

**The four roads, and the money is where the second one is.**

```
  road                                            |   ms | (worst) |    ev/s | vs best
  ------------------------------------------------|------|---------|---------|--------
  8 fibres, one JVM (Flows.fan)                   |   84 |      89 | 14,944,023 | 1.00x
  8 partitions, coordinator, workers in this JVM  |  139 |     155 |  9,030,920 | 1.65x
  8 partitions, over sockets, one JVM             |  168 |     175 |  7,472,011 | 2.00x
  8 partitions, over 4 OS PROCESSES               |  154 |     170 |  8,151,285 | 1.83x
```

The protocol and the CBOR are 1.65x; the sockets take it to 2.00x;
and four separate processes cost nothing further. Repeated, the rows
read 86, 144, 177 and 135 — the two transported roads swap places
between runs, so what is true is that they are within a tenth of each
other, and that FOUR PROCESSES ARE NOT SLOWER THAN FOUR SOCKETS IN
ONE JVM. The codec measurement says why: encoding all eight partials
on one thread is 67 ms and decoding them 111 ms, so in the one-JVM
lanes the coordinator's decoding and the partitions' encoding share a
heap and a collector, and in the process lane they do not.

**The cost is FIXED, not marginal**, which is the finding this stage
exists to produce:

```
  lane                                  | fixed cost |     marginal
  --------------------------------------|------------|--------------
  okay, 8 fibres, one JVM               |      14 ms | 18,098,329 ev/s
  okay, 8 partitions over 4 processes   |      79 ms | 15,873,168 ev/s
  flink, parallelism 4 (§20's own fit)  |     433 ms |  1,870,582 ev/s
```

Distributing costs about 65 ms ONCE and then a rate within about a
tenth of the in-JVM one. The Flink row is there because the method is
identical — §20 has fitted it the same way for two lanes since the
section was written — and not because the deployments are comparable:
Flink's 433 ms buys checkpointing and rescale in a MiniCluster, and
neither engine here is on a cluster.

**Claim 1, weighed.** 6 830 878 bytes cross for 1 255 298 events —
5.44 per event, 3.94 per pane — in SIXTEEN requests, because a
partition is a recipe: a job name and one `Int` go out, and the worker
builds the plan and reads its own slice. Per stage:

```
  stage                                       | accumulators |     bytes | per event
  --------------------------------------------|--------------|-----------|----------
  2 — tumbling per route (138 keys)           |        1,458 |   265,240 |     0.211
  3 — sliding per stop (2,482 keys x 3 panes) |       53,130 | 2,513,430 |     2.002
  4 — keyed state, no window                  |       68,091 | 4,052,096 |     3.228
```

**Claim 2, priced.** Stage 4 is keyed state that depends on per-key
order — Flink's `KeyedProcessFunction` over `ValueState`, and
therefore a shuffle of all 1 255 298 records. Here it crosses as
68 091 accumulators, 18 times fewer objects, because the question has
an answer that combines.

**A number that looked like a distribution cost and was arithmetic.**
The first version of the route terminal held its ranking in a `Vector`
compacted when it passed a threshold, and the threshold (4 096) was
BELOW the compacted size (1 152 windows x 5 entries) — so it compacted
on every add, and the lane read 973 ms where its own in-JVM reference
reads 84. The fix was not a tuning: the accumulator is now a
`LongMap`, exactly as `OkayLane.Sink`'s is, and it travels as a value
through `Schema.SIso` — the codec's own newtype node, so the wire form
is a case class and nothing in the engine knows the difference.

**What stage 7 does not have, and will not invent**: a cluster. Both
engines here run on one machine; a Flink or Spark number across
machines needs machines, and the section says so where the rows are
rather than in a footnote.

### Stage 8 — the coordinator survives

Every stage from 6a to 7 ends with the same sentence: if the
COORDINATOR dies the run dies with it. The workers have been
recoverable since stage 5 — a partition is a recipe, so a replacement
replays it — and the one party that could not be replaced was the one
holding the fold.

**The whole of it is that the epoch loop is LOCK-STEP.** Every
partition contributes exactly rounds 1..N before the coordinator
folds, so a checkpoint taken after absorbing round N is a consistent
cut BY CONSTRUCTION: nothing is in flight, no partition is half an
epoch ahead, and there is no alignment protocol to write. Flink's
checkpointing is an achievement because its operators run
asynchronously and a barrier has to be threaded through the dataflow;
this one is a `save` call because 6a chose the other shape. That was
not foresight — 6a chose lock-step because it made the WATERMARK
computable — but it is the second thing that shape has now paid for.

**Two members, and neither is new machinery.**

  - `Wire.state: Schema[S]` — the coordinator's running state
    described the way `wire: Schema[W]` already describes a partial.
    Every `S` in `Sink` was a value in disguise: a fold's is its
    accumulator, a keyed sink's is a map of accumulators, a windowed
    sink's is the open panes plus what has been retired. The two
    mutable ones travel through `Schema.SIso` — the codec's newtype
    node, the same one stage 7's terminals use.
  - `Checkpoint` — `save(epoch, bytes)` and `latest`. An injectable
    seam, NOT a dependency: okay-cluster's compile graph stays at
    okay-codec and the store is the caller's. `TestPersisted` binds it
    to okay-persist's compacted keyed topic in eight lines, which is
    what "an assembly, not an invention" was supposed to mean when the
    backlog entry said it.

**What is journalled is more than the fold**, and the extra is not
decoration. The per-partition EXTENTS go too, because the watermark is
computed from them and a successor that forgot them would retire panes
on a watermark of `MinValue`; so do the drop and merge counters, which
are part of the answer a `Run` reports. A resumed coordinator that
answered a different `dropped` for the same stream would be the kind
of "nearly right" this repository counts as wrong.

**And the SESSION IDS.** A successor inherits them rather than minting
new ones, and two things fall out. A worker that survived still holds
its session at the epoch it was last asked for, so the resumed run
continues on it instead of replaying from the start; and a worker that
did not is opened afresh under the same id, which is 6b's road
exactly. Without this, every dead coordinator would strand `parts`
sessions on the workers for ever, because the only party that could
close them is gone — `TestResume` asserts the predecessor's ids are
gone at the end, and it fails without the inheritance.

**The two deaths are not the same death, and both are tested.**

  - AFTER the commit: the epoch is in the journal and the successor
    asks for the next one.
  - BEFORE it: the epoch was computed and lost, so the successor asks
    for it AGAIN — and the worker's session, already there, re-answers
    the same partial. `merged` still equals an uninterrupted stream's,
    which is the sharp form of "nothing was folded twice". This is
    where `Advance` naming an INDEX rather than meaning "next" pays
    for the third time.

Both controlled: resuming at `epoch + 2` (an epoch lost) and at
`epoch` (an epoch folded twice) each fail all four resume tests.

**6c's boundary moved, and did not vanish.** A writing sink now keeps
its exactly-once OUTCOME across a coordinator restart: after a
committed death every (window, key) is offered exactly once across the
restart, asserted. After an UNCOMMITTED one that epoch's panes are
offered again — a pane is written while its epoch is absorbed and the
epoch is committed after, so a death in between loses the record of
writes that happened. The window is one epoch wide, the test asserts
the repeat HAPPENS rather than hoping it does not, and what makes it
harmless is the identity that has made every repeat harmless since 6c.

**What stage 8 does not do.** A batch `Cluster.run` still dies with its
coordinator: it is a two-pass function with nothing to resume from,
and restarting it is the answer rather than a mechanism. And nobody
ELECTS the successor — `Cluster.stream` has to be called again, by
something. okay-persist has `Election`; wiring it here would be a
lane, not a line, and nothing has asked.

### Stage 9 — the commit window, closed by the writer

Stage 8 left one window open and named it. A pane is written while its
epoch is being ABSORBED and the epoch is COMMITTED afterwards, so a
coordinator that dies in between has written panes the journal does
not know about, and its successor writes them again. For a keyed
writer that is harmless — same key, same value, one row — and the
backlog entry said the ways to close it were worse than it is.

**That was true of the two roads named there and both were the wrong
shape.** Write-ahead the pane set and the journal carries the output;
commit before writing and a death loses panes instead of repeating
them, which trades at-least-once for at-most-once. The road not named
is the one every engine actually takes: **the engine cannot close the
window, because the write left the engine — so it hands the writer the
two moments that let the writer close it.**

```scala
def committed(epoch: Int): Unit = ()   // the journal now holds this epoch
def recovered(epoch: Int): Unit = ()   // resuming; anything after this never happened
```

Two no-op defaults on `Sink`, forwarded by `and` and by every `Wire`
wrapper. A sink that only computes ignores them. `Sink.staging`
collects the panes an epoch retires and hands the whole epoch to
`move(epoch, panes)` when it is final.

**THE ORDER IS THE CONTRACT, and it is the one thing here that had to
be argued rather than chosen.** The sink hears the commit BEFORE the
journal records the epoch. Told afterwards, a coordinator that died in
between would leave the journal claiming an epoch that the writer was
never asked for — panes lost, at-most-once, the trade this stage
exists to refuse. Told first, the worst case is being asked for the
same epoch TWICE across a restart, and never for two different epochs
under one number. So a writer that records the epoch beside its rows
in one atomic write recognises the repeat and drops it: exactly-once,
by the same argument stage 6c used for a keyed writer, one level up,
with the epoch as the key.

That is a two-phase-commit sink. It is twenty lines rather than a
framework for the same reason the checkpoint was a `save` call — the
epoch loop is lock-step, so "this epoch is final" is a fact the
coordinator already has.

**The bug this produced immediately, and it is the good kind.** The
final sweep committed under the LAST ROUND'S NUMBER, so the writer
recognised it as a repeat and dropped the panes the close swept out.
The rows came out wrong on the very first quiet run. The sweep is its
own epoch now (`round + 1`), which is also the honest description of
it.

**What a staging sink REFUSES, loudly.** Every pane has to retire in
ONE place for a single stage to see them all. That is true of
`Cluster.stream`, where a partition finishes nothing locally, and
false of a batch run, where the completeness rule finishes panes
inside each partition — on a WORKER, whose stage no coordinator will
ever commit. So `finish` throws rather than dropping them, and names
the alternative: `Sink.writing` with a writer keyed by `(start, key)`,
which is stage 6c's answer and needs no commit at all.

**Both directions controlled.** Journalling before telling the writer
makes the after-the-commit test lose panes, and makes the window test
report that it is no longer exercising anything — which is the
assertion that stops this suite from passing for the wrong reason.

**What is still open**: the writer's stage is in memory here, so the
two-phase commit survives the COORDINATOR's death and not the
WRITER's. A durable stage — a transaction, a temp file per epoch —
closes that too, and the seam is already the right one: `recovered`
says where to resume, `committed` says what to finalize. Nothing has
asked, so nothing is built.

### Stage 10 — the election, and the ghost

Stage 8 made a successor possible and stage 9 made its writes safe.
Nobody started one: `Cluster.stream` had to be called again, by
something, which meant the journal was a recovery story a human ran.
And there was a second hole, the dangerous one — **two coordinators
over one journal is worse than none.** A paused leader that wakes
believing it still leads commits over its successor's state, and the
next resume reads whichever landed last.

**Leadership is a seam, for the same reason the journal is.** `Lease`
is three methods over a TERM — `take(): Option[Long]`, `held(term)`,
`release(term)` — and okay-persist's `Election` answers all three as
it stands: `tryTakeover` returns the epoch that becomes the term,
`leader` says who holds it, and `heartbeat` renews the lease, which is
what a leader should be doing once an epoch anyway. The binding is
eleven lines in test scope, so okay-cluster's compile graph is still
okay-codec.

**The term is a fencing token, and that is why `take` answers a number
rather than a boolean.** `Checkpoint.fenced(term, lease, under)` asks
the lease before every commit and throws `Deposed` instead of writing,
so a predecessor that wakes up mid-run stops at its next epoch. The
test that matters is not a fresh candidate taking a vacant seat — that
would pass with no fence at all — it is a leader deposed BETWEEN its
epochs: it throws at the next commit, and the journal still ends at
the epoch it lost the seat on. Without the fence both ghost tests
fail.

**`Cluster.leading` does not wait to be elected**, and that is a
decision rather than an omission. A retry loop needs a clock, a
backoff and a rule for how long to keep trying, every one of which
belongs to whatever supervises the process. One attempt composes into
all of them, and the doc shows the four-line loop.

**What is not closed, and it is one commit wide.** The fence is a
CHECK before a write, not a compare-and-set: a leader deposed between
the check and the write can still land that write. Closing it needs
the STORE to offer "save this only if the term is still mine", and
the seam already permits one because `save` may throw. What is built
here is what can be built over a store that offers nothing of the
kind.

### dataflow-fan-overhead — the third that was not there

Stage 3's decomposition summed the sinks to 104 ms against a fan of
154 and concluded that a third of a fan's time was in none of its
sinks. The entry has said "decompose before optimising" ever since,
which was the right instinct and the wrong premise.

**The arithmetic did not line up.** It subtracted one source read from
each lane, but the lanes are not the same shape: `Sink.keyed` declares
no event-time function, so the bunching lane ran with NO pre-pass at
all, while the fan's pre-pass computes two columns in one pass. And
the parts have changed under the number since — `Aggregator.topK`
stopped sorting its corpus — so the 154 is not today's fan.

**Re-measured lane for lane** (`MeasureFanOverhead`, Live, 1 255 298
events, 8 partitions, best of 21 interleaved rounds, two runs):

```
  lane                                                     |    ms | (worst)
  source alone (count, no pre-pass)                        |     1 |       6
  + a cheap WINDOW (1 pre-pass column)                     |     7 |      13
  + a second cheap window, same time function (2 columns)  |    11 |      21
  route windows alone                                      |    14 |      26
  route + an arm that only counts                          |    17 |      35
  stop windows alone                                       |    63 |     107
  bunching alone (keyed, no pre-pass)                      |    15 |      35
  THE FAN: route + stop + bunching                         |   101 |     194
  three separate fans, one after another                   |    96 |     160
```

The fan is 101–111 across two runs and its three sinks sum to 90–92.
**The gap is 9–20%, not a third**, and the two candidates that can be
resolved account for part of it: a pre-pass column costs 5–6 ms over
1.25 million events, a second column in the same pass 4–5. The third
candidate, `Sink.and`'s plumbing, reads 0–3 ms across two runs — below
what this instrument can see, which is the honest way to report it
rather than as a number.

**And the finding worth more than the entry: THE FAN IS NOT FASTER
THAN THREE SEPARATE FANS on this feed** — 89–96 against 101–111, in
both runs. One pass saves about 2 ms of source reads, because §20's
source is an in-memory array, and pays more than that for three
operators' state being live at once. What a fan buys is the source
read ONCE — which matters when the source is a file, a topic or a
socket, and this feed is none of those — and no shuffle, which is what
§20 actually compares against Flink. Stage 3's own headline number
(737 ms for "three plans") was never this comparison: that road is
`Flows.run`, which has no completeness rule, and it is
`dataflow-run-complete-panes`.

**What is left open, said rather than closed over**: 9–20% of a fan is
still unattributed, and this instrument's bars are wider than it.
Pricing it needs JMH with forks rather than a wall clock in a suite.
Nothing has asked.

### dataflow-run-complete-panes — the rule the single-stage road never got

The completeness rule was stage 3's and it went into the FAN only.
`Flows.run` — the road every windowed plan that is not a fan takes —
kept merging every pane the job produced, and the gap between the two
roads on the same job was 8.2x. Not a fair 8.2x either: the slower one
is the road a simpler plan takes.

**The obstacle was structural and the way past it was to not go
through it.** To finish a pane is to fold it into the terminal, and a
`Wide` node has no terminal — `job(into)` is built after the node
exists. So a partition that can finish a pane PRESENTS it and appends
it to a plain per-bucket buffer, and only the boundary panes go into
the maps a reducer merges. `out` concatenates its range's buffers with
what it merged. Nothing is threaded down, and the node still does not
know what it is folded into.

**Measured back to back on one box** (the same suite, two worktrees,
the before at the parent commit):

```
  lane                                              | before |  after
  hand-written, 8 threads (OkayLane.parallel)       |     79 |     81
  engine, 8 partitions, one pass (the fan)          |    115 |    111
  engine, 8 partitions, three plans via Flows.run   |    647 |    182
```

**3.6x**, with every other lane inside the noise — which is what makes
it the change rather than the machine.

**And the check is a COUNT, not a clock.** `Run.merged` is reported by
the single-stage road now, and `TestFlow` asserts it equals the FAN's
exactly at 2, 4 and 8 partitions: same rule, same partitions, same
number. A time says the road got faster; the count says it is doing
the same thing the other road does.

**Two things fell out of it.**

  - `prepass` answers an `Extent` rather than one `Long`. The rule
    needs the prefix maximum AND the backwardness, and this road had
    been computing exactly half of that since stage 1 — enough to seed
    a watermark, not enough to finish a pane.
  - **the LAST partition has no upper bound at all.** The two bounds
    guard two different neighbours: `lower` says no EARLIER partition
    can contribute, `upper` says no LATER one can. The last partition
    has no later one. Before this it held back the final `back` of the
    stream for no reason — which is why a run at ONE partition merged
    fifteen panes it was the only side of. It merges nothing now, and
    the suite asserts that rather than describing it. The fan gets the
    same refinement for free: 122 679 accumulators to 122 649.

**Controlled**: declaring every pane complete (`if true`) fails four
of the Wrocław checksums, so the tests can see a rule that fires when
it should not.

### dataflow-auto-for-a-real-accumulator — the weight does not move the crossover

`Finish.Auto` decides from one constant, and that constant was
measured on the cheapest accumulator there is: a Long count. The
comment beside it predicted that a fatter accumulator makes merging
dearer and moves the bound DOWN — a prediction sitting in a comment
with no number under it, which is the shape of claim this repository
is supposed to catch.

**Measured** (MeasureExchange, 1M rows, 8 partitions, minimum of 7
alternating rounds, the ratio is merge/shuffle so above 1.00 the
exchange wins):

```
  accumulators |  count: ratio | tuple: ratio
         4,000 |         0.77x |        0.67x
         8,000 |         0.79x |        0.75x
        20,000 |         0.75x |        0.87x
        40,000 |         0.79x |        0.79x
        80,000 |         0.94x |        0.94x
       160,000 |         1.14x |        1.95x
       320,000 |         1.71x |        1.30x
       640,000 |         2.20x |        1.67x
```

**Both cross between 80 000 and 160 000**, in two runs, and 100 000
sits in that band. The tuple tree — §20's own `count zip sum zip max`,
six objects per add and per merge — does not move the crossing
measurably. What its weight changes is the SLOPE past it, and not even
that consistently: the exchange pulls ahead harder at 160 000 and less
hard at 320 000 than it does for a count, because a fat accumulator
loads the MAP side too and both roads pay that.

So one constant serves both, and the prediction is refuted rather than
confirmed.

**And a methodological finding that cost the first two runs.** The
table was originally computed from a millisecond clock, over lanes
that are 1 to 7 ms at the crossing. It reported the two crossovers as
40 000 and 160 000 — four-fold apart — and every ratio in the band
came out as 0.50, 0.67 or 1.00, which are 1/2, 2/3 and 1/1: the
quantisation of a millisecond clock, not the engine. The ratio is
computed from nanoseconds now, and the four-fold difference vanished.

**What stays unmeasured, and is now labelled as such** rather than
asserted beside a measured number: that fewer partitions move the
bound up. Nobody has run it.

### dataflow-reconnect — a failure is not yet a death

Stage 5 buried a worker on its FIRST throw. Its own first seeded test
asked for a blip on every worker and correctly died with "no workers
left (4 were given)", and that sentence has been a named limit in
three documents ever since. The entry said the fix had two separable
halves and that the second should be measured before the first was
built. It was, and the answer is that both are needed — for different
failures.

**Tolerance.** A worker is buried after three CONSECUTIVE failures,
and any answer clears its count. The partition still moves to a
survivor on every failure: what the count changes is who is asked NEXT
TIME, not who answers now. Three is a judgement rather than a
measurement — one is what the engine did and could not survive a blip,
and a large number keeps asking a corpse — and what makes it cheap is
that the count belongs to the WORKER and the RUN, so a machine that is
really gone costs three attempts once, not three per partition.

The control is the historical failure itself: with the tolerance set
back to one, the new test dies with the sentence stage 5 produced.

**And it forced a distinction that was overdue.** `Run.retried` counts
workers BURIED; `Run.failed` counts attempts LOST. A run can now
recover from a failure without burying anybody, so two tests that
asserted `retried > 0` to mean "the injection fired" were asking the
older question — one of them the four-process kill, whose worker in a
short run is never asked three times. They ask `failed` now, which is
the same claim on the instrument that still means it.

**The socket.** Tolerance is enough for a worker that hiccups and
cannot be enough for a connection, whose failure is permanent by
construction: a broken socket is broken for ever, so tolerance just
keeps asking a corpse three times instead of once. So
`Served.reconnecting(host, port)` dials LAZILY and drops the socket on
any failure — the next request dials again, which is what makes a
RESTARTED worker process usable rather than merely tolerated.

It does NOT retry inside itself, and that is the design rather than an
omission: the coordinator already has a policy for a failed attempt —
move the partition, count it against that worker, bury it if they keep
coming — and a second policy hidden in the transport would fight it.
The transport makes the connection able to heal; `Living` decides when
to give up.

**The test is the two roads side by side.** A server that serves one
request per connection and hangs up — the cheapest honest model of a
worker that restarts, since the port stays reachable and the socket
does not. Handed `Served.connect`, the run dies with "no workers
left"; handed `Served.reconnecting`, it finishes with the same answer
and nobody buried. Same server, same job, same tolerance: the only
difference is which `Serve` the coordinator was given.

### dataflow-durable — what a journal that keeps its history buys

The last two open entries were the same question asked twice.
`dataflow-fenced-commit`: stage 10's fence is a check before a write,
so a leader deposed between the two can land one stale commit.
`dataflow-durable-stage`: stage 9's staging sink keeps its epoch in
memory. Both are about what a real STORE buys that a cell cannot, and
both close here — one of them by being answered smaller than it was
asked, and one by a defect it turned up on the way.

**The term goes into the record, and a resume takes the newest.**
`Folded` carries the term its commit was made under, and
`Checkpoint.newest` picks the highest (term, epoch) out of everything
a journal still holds. A stale commit from a deposed leader is then
shadowed for ever instead of being read back — no compare-and-set
required, and none of the stores here offers one. A store that keeps
only the LAST write has nothing to choose from, and that is the honest
difference between a log and a cell. The test writes the ghost's
record by hand — faking a lease would only prove the fence works, and
the point is what happens when it does not — and reads the same log
two ways: defended, the run picks up where the real leader left off;
naive, it resumes from the ghost and redoes the work.

**And the honest half, which is smaller than the entry implied.** The
rows are right EITHER WAY. A stale resume costs work, not
correctness, as long as the source replays and the writer is keyed by
(window, key) — which is stage 6c's argument, and stages 5 through 8
already rest on it. The fence and the history are what keep a run from
paying for a ghost, not what keep it from being wrong. For a source
that does NOT replay the fence is the only thing between two
coordinators and divergence, and it is still a check, so that window
is real and stays named.

**THE DEFECT THIS TURNED UP, and it falsifies a sentence stage 8
wrote.** That stage claimed a coordinator dying between the last Close
and the answer could resume, ask for one more epoch, be told
everything was drained, and re-answer the same value. It could not.
The fresh sessions a resumed run opens replay the whole source,
DISCARD every pane their catch-up closes, and hand over only what is
still open at the end — so the tail panes were retired a second time
out of one partition's half and overwritten with a partial value. On
the synthetic feed: 62 extra offers and **29 of 3 204 panes wrong**.

The fix is a boolean. A finished run records that it is finished
(`Folded.done`), and a resume that reads it answers from the state
instead of asking anybody — which is also the cheap thing to do.
Controlled: with the flag not set, the two tests that found it fail
again.

**What `dataflow-durable-stage` turned out to need: nothing.** The
staging sink's contract already requires the writer to record the
epoch in ONE write with its rows, and a writer that cannot do that
cannot have exactly-once — which is a property of the store, not of
the engine. The entry asked for a durable stage to survive the
writer's own death; what survives it is atomicity at the writer, and
saying so is the whole of the answer. No mechanism was invented to
justify the lane.

### Stage 11, box 1 — the partition is a topic partition

The repository's thesis is one primitive, the durable log, and until
this box the dataflow engine had never read from it: a worker DERIVED
its partition from parameters, which was the honest choice for a
benchmark and is not a source.

**It cost three lines, and that is the finding.** `Streams.chunks` is
a blocking, iterator-backed `Chunks[Record]` over a topic partition —
okay-persist already sees `Chunks` — and `Flow.of` takes any thunk. No
dependency runs in either direction, and the connector is the user's
one line: `Flow.of(Vector.tabulate(parts)(p => () => chunks(t, p, 0)))`.
Both halves fit because both were built to the same shape, and the
assembly is what the log-as-stream claim was supposed to mean.

**Blocking on purpose.** `Streams.stream` is the effectful producer
for a consumer that composes; a dataflow partition runs on its own
fibre and pulls until the source is dry, and an iterator is the whole
of what it needs. And `TooEarly` is a `DroppedHistory` here rather
than a resume: a partition that silently started later than it was
asked to would answer a different question.

**Asserted equal, not close.** Partition p of the topic holds the
p-th contiguous slice — the cut `Flow.slices` makes — so the two plans
run over the same partitions and `merged` is the same count, not only
the answer. Batch and streamed, 1, 4 and 8 partitions.

**What box 1 does not do, and box 2's design is in the claim so the
next session starts rather than re-derives**: seek. A resumed session
still catches up by replaying from offset zero. The session has to
record the offset it reached beside its extent, and a fresh session
asked for epoch N opens at N-1's — which means `Flow.Src` thunks take
a start. One Long on the wire and one signature; the whole of box 2.

### Stage 11, box 2 — a session opens at its position, and who can

The claim written for this box predicted "one Long on the wire and
one signature". Reading the code before writing it said that was
wrong, and the correction is the finding.

**A windowed operator cannot seek.** Its open panes live INSIDE the
partition and are handed over only when they close. A fresh session
opened at epoch N-1's position has none of them, and their
contributions from before that position reach nobody — the answer
would be silently short. That is why 6b replays from zero, and it was
right to.

**A fold and a keyed sink can**, because what they hand over each
epoch is a DELTA: `peek` empties the map. A session opened at the
position with an empty map is exactly right — its deltas from N on
merge into what the coordinator already holds. So `Sink.seekable` is a
property of the sink (fold, keyed: true; windowed: false; `and`:
both), and the coordinator opens a seekable sink's sessions at the
journalled position and epoch, and a windowed sink's at zero.

**Positions, not offsets.** A position is elements consumed, which the
session already counts; a source that can seek — `Flow.slices` over
an array, `Streams.chunks` over a topic — positions itself, and one
that cannot (`Flow.of` over any thunk) reads and drops, which is the
replay a resumed run always paid, now in one place and named. For a
topic that makes the contiguity of offsets load-bearing, and
`Streams.chunks`'s `DroppedHistory` is what says so when it is not.

**One road for two cases.** Where a session opens is the same
question on a resume and on a replacement worker mid-run, so it is one
function: a keyed job whose worker blips mid-stream has its partition
reopened elsewhere at the position, not at zero — asserted by the
same count.

**Asserted by counting, not by trusting the flag.** A `Topic` that
counts the records it hands out says a keyed job resumed after a
coordinator death reads exactly `total - Σpositions`, and a windowed
one on the same schedule reads at least the whole topic again. With
the seek disabled, both seek assertions fail.

**Box 2b — the windowed case — is designed and not built.** Two roads:
delta handovers of open panes every epoch (a change to `okay.Windows`,
and the merge traffic grows from panes-CLOSED to panes-OPEN per
epoch), or a replay bounded by the window horizon (a (position,
maximum event time) pair recorded per epoch; a fresh session seeks to
the epoch whose maximum is below the oldest open pane's start, replays
from there, and is SEEDED with that maximum so its late-drop decisions
are the original's). The first costs every epoch, the second costs a
horizon on resume; measure both before choosing.

### dataflow-netem — the loss rate at which a run stops finishing, and why

Stage 12 needs machines that are not this one. One of its boxes never
did: `dataflow-reconnect` chose to bury a worker after three
consecutive failures, called the number a judgement, and left the
question that begs — at what LOSS RATE does a run stop finishing? —
unanswered. A wire that loses requests by a seeded schedule answers it
on one machine.

**Four workers, eight partitions, forty schedules per rate, tolerance
3:**

```
  loss rate | finished/40 | lost attempts per finished run | workers buried
       0.00 |          40 |                            0.0 |             0
       0.05 |          40 |                            0.8 |             0
       0.10 |          40 |                            1.6 |             0
       0.20 |          40 |                            3.6 |             2
       0.30 |       36-37 |                            6.0 |          9-13
       0.40 |          31 |                            8.6 |            20
       0.50 |          20 |                           12.4 |            24
       0.70 |           0 |                              — |             —
```

Twenty percent loss is carried with certainty; the knee is at thirty;
half the runs die at fifty; none finish at seventy. And the column
that explains it is the last one: the runs that die are the runs in
which workers were BURIED.

**So the same wire again, with the count as the second dimension**
(finished of twenty):

```
  loss rate | tol 1 | tol 3 | tol 6 | tol 12 | tol 1000
       0.10 |    19 |    20 |    20 |     20 |       20
       0.30 |     1 |    17 |    20 |     20 |       20
       0.50 |     0 |    10 |    20 |     20 |       20
       0.70 |     0 |     0 |     7 |     20 |       20
```

**With burial off, a wire losing seventy percent of requests still
finishes every run.** On a lossy wire the loss never ends a run — the
burial policy does. Tolerance 3 reads three lost packets as a dead
machine, and from thirty percent loss up it is what turns a cluster of
live machines into "no workers left". Tolerance 12 carries seventy.

**What that means for the count.** It couples two failures that are
not the same: a machine that is gone, for which any number of retries
is waste, and a link that drops, for which every retry has the same
chance. Three is a good default for the first and a bad one for the
second, and no single number serves both — which is why `tolerance`
is a parameter of `Cluster.run` and `Cluster.stream` now rather than
a constant, and why the honest sentence is that a deployment which
knows its wire is lossy should say so. Stage 12 proper, with a real
wire and real latency, is where the two failures can be told apart
BY THE ENGINE (a lost packet answers late or not at all; a dead
machine refuses the connection), and that is a design for machines
that exist.

**On the determinism.** The loss is seeded per worker, so the same
seed drops a worker's i-th request every time; which worker a
partition's i-th attempt reaches is the fibres' order. The counts
therefore move by a run or two between runs, the assertions sit far
from any edge, and the sweep lives in the default gate on that basis.
