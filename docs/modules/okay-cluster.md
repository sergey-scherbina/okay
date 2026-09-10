# okay-cluster

> The own small distributed runtime, assembled from existing parts:
> a remote channel that looks local, chunk work distributed with
> per-chunk recompute, the cross-platform acceptance — a JS client
> driving a JVM server with one shared-source program — and, since
> specs/dataflow.md stage 1, the ENGINE: a `Flow` is a distributed
> plan as a value, and `Flows` runs it on a fibre per partition.

Depends on: `okay-codec`. Cross-built: the JVM side holds the
sockets, the JS side the Node client, the shared tree the program
both compile.

## Guide

**Nothing new is invented here** — that is the design. A remote
channel is a `Channel` with a socket underneath; the shipping unit is
the `Chunk`, as everywhere; the wire format is the codec (JSON lines
now, CBOR when its streaming lands on the wire); the cross-node merge
contract is the `Aggregator` triple `(zero, seqOp, combOp)`; the
fault model is the P2 one — a pure chunked source is a VALUE, so a
chunk in hand is lineage, and recompute is just handing it to someone
else.

**The wire forgives.** A damaged frame is dropped as data and the
stream lives; the wire closing closes the channel after the buffered
chunks drain — exactly a local channel's contract, which is why
downstream code cannot tell it is remote.

**Work distribution in one seam.** `Cluster.distribute` sees a
worker as ONE function `Chunk[A] => Acc` — in-process, or a wire
away (send the chunk, await the partial). A dead worker THROWS; that
is the whole protocol: it leaves the rotation and its chunk goes to
a survivor. Partials merge by combOp, which is order-free by the P1
contract — so recompute cannot corrupt the aggregate.

**The acceptance test is the policy.** specs/cross-platform-async.md
promised: one source, platforms interoperate. The test is literal: a
Node-linked JS client (`Client.scala`, driven by `runAsync` through
the event loop — nothing there may block) streams the shared
`Acceptance` object's frames to a JVM fold-server and verifies the
answer against ITS OWN computation of the same shared source.

**The engine: a keyed stage that does not shuffle.** `Flow` is the
plan — a partitioned source, per-partition `map`/`filter`, and a
keyed or event-time-windowed aggregation. `Flows.run` gives one fibre
per partition and puts the pieces together, and the way it puts them
together is the whole point: a keyed stage does not have to move its
RECORDS to the key's owner, it has to move its ACCUMULATORS, and an
`Aggregator` is exactly the value that makes that legal. So every
partition folds every key it happens to see and the coordinator
merges what comes back — one accumulator per (key, window) per
partition, where Spark's `reduceByKey` and Flink's `keyBy` move the
dataset. A second keyed stage in one flow is still refused by name.

**And the exchange, for when that is the wrong plan.** `Finish` picks
the road: `Merge` puts every partial on one coordinator, `Shuffle(r)`
gives each of r reducers a hash share of the keys, `Auto` decides
when the partials are in and their size is known. The map side writes
hash buckets and a reducer takes a contiguous RANGE of them, which is
what lets the number of reducers be chosen after the run has started.

**When to take it, measured.** In one process the exchange saves no
memory — every partial is already in this heap — it lets the final
merge run on several threads instead of one, and the question is
whether there is enough merging to pay for the hand-off. The answer
is a count of ACCUMULATORS: below ~80 000 the merge road wins, above
~160 000 the exchange does, reaching 4.8x against the merge by a
million (`MeasureExchange`, and the table in specs/dataflow.md). A
fatter accumulator moves the bound down; fewer partitions move it up.
On Wrocław's job — ~10^4 accumulators against 10^6 events — `Auto`
DECLINES the exchange, and the suite asserts that it declines it.

**One pass, many sinks.** A `Sink[A, R]` is a keyed or windowed stage
together with what its output is folded into; `and` pairs two;
`Flows.fan` drives one pass over each partition through all of them.
The shape is `Aggregator.zip`'s, one level up — the core has computed
two statistics in one pass since P1. Seeding survives the fan: each
sink declares the event-time functions its windows need, and one
pre-pass computes a prefix maximum per column, so a fan whose stages
window on different times is still exactly the single-threaded answer.
In this process a fan-out is calling three methods with the same
reference; in Flink it is three shuffles.

**A partition finishes what no other partition can touch.** A
windowed pane leaving the operator goes one of two ways: if it starts
after `hi(i-1)` and ends at or before `hi(i) - back` then no other
partition can contribute to it, so it is presented and folded into
the terminal HERE; otherwise it is kept for the coordinator. `hi` is
the prefix-maximum array the seeding already gathers — the seed is
the lower bound — and `back`, the stream's greatest backwardness, is
two more columns in the same pre-pass. An unseeded window finishes
nothing locally: a partition that does not know where the stream
stood may not declare anything closed.

**What the engine costs, measured** (`MeasureWroclawFlow`, four
service days, 1 255 298 events, every lane once per round, best of 7
with the worst beside it). The whole Wrocław job as one plan on 8
partitions is **97 ms (worst 115)** against `OkayLane.parallel`'s
hand-written **85 (worst 137)** — **1.14x**, and the engine's worst
round is the better of the two. It was 5.5x before the completeness
rule, and the whole of that difference was the coordinator: the job
makes 1 734 893 panes and 122 679 accumulators now reach it, 7% of
what one partition holds. `Run.merged` reports that number and the
suite asserts it shrinks.

**The `Sequential` rule cuts both ways, and the second way was a
surprise.** A `Sequential` KEYED aggregator survives the exchange
untouched: a reducer owns a hash share and merges its buckets
partition by partition, so the order it depends on is intact. A
`Sequential` TERMINAL over a keyed stage is refused — and not because
of the exchange. A keyed stage's output is a hash map's iteration
order whether one reducer produced it or eight, so an order-dependent
fold over it was already wrong under `Finish.Merge`. It stays allowed
on a stateless plan, where the engine really does hand the terminal
the input's order.

**Two orders that are not decoration.** Partials are joined BY
PARTITION INDEX, never by which fibre finished first — free for a
plain `Aggregator`, whose merge is commutative, and load-bearing for
a `Sequential`, whose merge is only associative. And a windowed
partition is SEEDED with the greatest event time before it, so its
watermark is the watermark the whole stream would have had at that
point; without that a slice closes panes later than the stream does,
drops fewer late elements, and quietly answers something else at
parallelism 4. Both are tested in both directions.

**Keyed state without a shuffle** (`Sequential`, in the core). A
keyed state machine usually has a slice summary that combines: the
bus-bunching statistic of docs/benchmarks.md §20 summarises a slice
as (first, last, n, bunches, gap), and merging two of them asks one
more question — whether the gap ACROSS the boundary is short. That
question is not the same one in the other order, which is why it is a
`Sequential` and not an `Aggregator`, and why the executor may not
reorder its partials. Flink answers the same stage with a
`KeyedProcessFunction` over `ValueState`, which must have every
record of a key on one machine.

**Across processes.** A `Job[P, R]` is what a worker can be asked for
by NAME: it carries a `Schema` for its parameters and builds the plan
itself, so nothing that crosses is a function. `Cluster.run(job,
params, parts, workers)` is the coordinator, and a worker is
`Req => Resp` — in-process, or a socket away, and the driver cannot
tell. Two requests: the pre-pass (`Extent`) and the fold (`Run`),
which are exactly the two passes the local fan already makes. The
transport is a four-byte length and CBOR; `okay.cluster.WorkerMain`
is a worker process, told its port and the classes whose loading
registers what this build can run.

Tested in three levels — in-process, sockets in one JVM, then four
real operating-system processes — with the same bar at each: the
answer, the drop count and the merged count must EQUAL what
`Flows.fan` computes alone.

**A worker that dies is buried and its partition is recomputed on a
survivor**, which is nearly free because a partition is a thunk and
its partial is a pure function of (parameters, index, count, bounds).
`Run.retried` reports the burials. A `Resp.Failed` is returned rather
than retried — it is the worker's considered answer, and every worker
runs the same build. Tested under forty seeded failure schedules and
with a real worker process killed mid-run.

Two limits, named rather than implied: a buried worker never returns
(a `Serve` is a connection, and a broken one does not heal), so a
blip on EVERY worker still ends the run — that one is in the backlog,
`dataflow-reconnect`. The second, the coordinator being a single
point of failure, held until stage 8: a STREAM can journal now, so a
successor picks the run up (see "And the COORDINATOR can be replaced
too", below). A BATCH `Cluster.run` still dies with its coordinator,
and it is a two-pass function with nothing to resume from — restart
it.

**As a stream.** `Cluster.stream(job, params, parts, workers, take)`
runs the job epoch by epoch: every round advances each partition by up
to `take` elements, the workers keep their operator state between
rounds (so a pane open at an epoch boundary stays open), and the
coordinator retires what the global watermark has closed. That
watermark is the MINIMUM over the partitions minus the window's
declared lateness — the minimum because a partition that has read
less may still produce something earlier, and the declared lateness
because the observed backwardness is only what has been seen so far
and is no bound on the future.

A streaming partition finishes nothing locally: the completeness rule
of `Flows.fan` needs the whole extent to be legal, which a stream does
not have. And note the honest asymmetry — on a feed with late
elements a streamed run drops FEWER than a batch run, because the
batch engine reconstructs one global order out of its slices and a
stream's partitions are independent channels.

**A worker that dies mid-stream is replaced, and the replacement
REPLAYS rather than restores.** `Advance` carries the epoch index, so
a session behind it catches up by replaying and discarding, and a
worker with no session is given the job and does the same. There is no
snapshot of an operator's insides anywhere in this — a partition is a
recipe, and its epoch partial is a pure function of (parameters,
index, count, epoch size, epoch number). Recovery costs O(elements
consumed so far) rather than O(state); a seekable source would make it
O(elements since the oldest open pane), and the seam for that is
`Flow.Src`'s thunk.

**And the COORDINATOR can be replaced too.** `Cluster.stream` takes a
`Checkpoint` — two methods over bytes, `save(epoch, bytes)` and
`latest` — and commits after every epoch: the fold, the per-partition
extents, the drop and merge counters, and the session ids. A second
`Cluster.stream` over the same journal picks the run up at the next
epoch. The workers need nothing new, because `Advance` has carried an
epoch INDEX since 6b: a resumed coordinator is a replacement worker
for everyone at once, and it inherits the session ids so its
predecessor's sessions are continued rather than stranded.

What makes this a `save` call rather than an achievement is that the
epoch loop is LOCK-STEP: every partition has contributed exactly
rounds 1..N before the coordinator folds, so a checkpoint taken after
absorbing round N is a consistent cut by construction — no alignment
protocol, no barrier in the stream, nothing to reconcile. The store is
the caller's: okay-cluster's compile graph stays at okay-codec, and
`TestPersisted` binds the seam to okay-persist's compacted log in
eight lines.

The window that remains, named rather than closed: a coordinator that
dies between WRITING a pane and COMMITTING its epoch re-offers that
epoch's panes. One epoch wide, and harmless to a keyed writer.

**A pane that LEAVES the engine.** `Sink.tumblingTo` / `slidingTo`
(and `Wire`'s twins, for a job at a distance) hand each retired pane
to a writer instead of folding it into a value the submitter reads at
the end, and answer how many they offered. What they hand over is the
pane's identity — `(window start, key)` — which is unique by
construction, so a writer keyed by it needs no protocol at all: no
transaction, no two-phase commit, no dedup table, nothing to tune.

The promise has two halves and both are true: every (window, key) a
run retires is offered AT LEAST ONCE, and every offer of one identity
carries the SAME value. A keyed writer therefore ends with each
identity present once, holding the batch answer — exactly-once
OUTCOME, not exactly-once execution. On 3,204 panes with one worker's
reply lost the writer is offered 3,598 (394 rewritten by the
recomputed partition) while the run's own answer stays 3,204; with two
losses, 3,981. A writer that COUNTS rather than keys — an append, a
`+= 1` — gets at-least-once and nothing more.

The writer runs WHERE THE PANE RETIRES, which is not one place: a pane
the completeness rule let a partition finish alone is written on that
WORKER, a boundary pane on the COORDINATOR when the watermark passes
it. So `write` is a closure over what the worker process can reach — a
table, a topic, a file — never over the submitting process's memory.
In a STREAM there is only one place, because a streaming partition
finishes nothing locally: offers equal panes exactly.

Across RUNS the promise is the same one, once the coordinator has a
journal (below): a successor re-offers only the epoch that was in
flight when its predecessor died, because a pane is written while its
epoch is absorbed and the epoch is committed after. Without a journal
it re-offers everything.

**What the distributed road costs**, measured on §20's Wrocław job
(docs/benchmarks.md, "The engine at a DISTANCE"): about 65 ms ONCE,
and then a marginal rate within a tenth of the in-JVM one — 79 ms
fixed and 15.9M ev/s over four OS processes against 14 ms and 18.1M
in one JVM. Four processes are not slower than four sockets in one
JVM, because in the one-JVM lanes the coordinator's decoding and the
partitions' encoding share a heap. What crosses is 5.44 bytes per
event in SIXTEEN requests for 1.26 million events, since a partition
is a recipe: a job name and one `Int` go out, and the worker builds
the plan and reads its own slice.

## Tutorial

A remote channel, indistinguishable from a local one:

```scala
import okay.cluster.Remote

// node A:
val ch: Channel[Chunk[Double]] = Remote.listen(ServerSocket(9000))
mergeChunks(localSource, /* drain ch into a source */ ...)

// node B:
val out = Remote.connect[Double]("nodeA", 9000)
chunksOf(source).foreach(out.send)
out.close()
```

The distributed fold that survives a death:

```scala
import okay.cluster.Cluster

val agg = Aggregator.variance[Double]
val acc = Cluster.distribute(source, Vector(wire1, wire2, local))(
  agg.init, agg.merge)
agg.present(acc)   // exact: every chunk counted once, deaths recomputed
```

The engine, on the job docs/benchmarks.md §20 measures — the
Wrocław timetable, five minutes tumbling per route:

```scala
import okay.cluster.{Flow, Flows}

val flow = Flow.slices(events, parts = 8)          // arrival order, 8 slices
  .filter(d => d.route >= 0 && d.route < tram.length)
  .map(d => Ride(d.ts, d.route, d.stop, d.vehicle, d.delay, tram(d.route)))
  .tumbling(5.minutes, lateness = 30.seconds)(_.route)(_.ts)(Job.stats)

val run = Flows.run(flow, intoTheReport).runWith
run.value      // the answer — equal to the single-threaded one, exactly
run.dropped    // the late elements, COUNTED: an engine that hides these
               // is hiding a wrong answer
```

A wire worker is a few lines — a socket, a line out, a line in:

```scala
val wire: Cluster.Worker[Double, Double] = c =>
  out.println(Json.write(c.toList))
  in.readLine() match
    case null => throw RuntimeException("connection lost")   // = dead
    case s => Json.read[Double](s).fold(m => throw RuntimeException(m), identity)
```

## API reference

| member | signature | meaning |
|---|---|---|
| `Remote.listen` | `(ServerSocket)(using Schema[List[A]], Scheduler) => Channel[Chunk[A]]` | accepted chunks land in a local channel |
| `Remote.connect` | `(host, port)(using Schema[List[A]]) => Sender[A]` | the sending end |
| `Remote.Sender` | `send(Chunk[A])`, `close()` | one JSON frame per line |
| `Cluster.Worker[A, Acc]` | `Chunk[A] => Acc` | the work seam; a dead worker throws |
| `Cluster.distribute` | `(source, workers)(zero, merge) => Acc` | round-robin over the living, per-chunk recompute |
| `Flow.slices` | `(IndexedSeq[A], parts, chunk) => Flow[A]` | contiguous slices of the input's own ORDER |
| `Flow.of` | `(Vector[() => Chunks[A]]) => Flow[A]` | partitions as recipes — a thunk, so a partition can be replayed |
| `Flow.map/filter` | `(A => B) / (A => Boolean) => Flow[…]` | per-partition, held as a `Chunks` transformer |
| `Flow.keyBy` | `(A => K, Finish)(Aggregator[A, Acc, O]) => Flow[(K, O)]` | a keyed aggregation |
| `Flow.tumbling/sliding` | `(size, slide, lateness, seeded, finish)(key)(at)(agg) => Flow[Pane[K, O]]` | event-time windows; `seeded` buys exactness for one pre-pass |
| `Finish` | `Merge` / `Shuffle(r)` / `Auto` | where the partials are combined; `Auto` decides during the run |
| `Flows.run` | `(Flow[A], Aggregator[A, Acc, O])(using Scheduler) => Run[O] ! Async` | the answer, the DROPPED count, the partitions and the reducers actually used |
| `Flows.fan` | `(Flow[A], Sink[A, R])(using Scheduler) => Run[R] ! Async` | several sinks, ONE pass; the completeness rule lives here |
| `Sink.fold / keyed / tumbling / sliding` | `… => Sink[A, R]` | one output: a stage plus its terminal |
| `Sink.writing / tumblingTo / slidingTo` | `(…)(write: Pane[K, O] => Unit) => Sink[A, Long]` | retired panes are WRITTEN, keyed by `(start, key)`; answers how many were OFFERED |
| `Wire.writing / tumblingTo / slidingTo` | the same, `(using Schema[K], Schema[Acc])` | the same for a `Job` — the writer is built on the worker, so no closure crosses the wire |
| `Sink.and` | `Sink[A, R1] => Sink[A, R2] => Sink[A, (R1, R2)]` | two sinks over one pass — `Aggregator.zip` one level up |
| `Flows.fold` / `Flows.collect` | as above / `Flow[A] => Vector[A] ! Async` | the answer alone; every element in partition order |
| `Job[P, R]` | `name / params / flow / sink` | what a worker can be asked for, by name |
| `Jobs.register / find / names` | | what a build knows how to run |
| `Cluster.run` | `(Job[P,R], P, parts, Vector[Serve]) => Run[R] ! Async` | the coordinator, for a bounded source |
| `Cluster.stream` | `(Job[P,R], P, parts, Vector[Serve], take, journal) => Run[R] ! Async` | the same, epoch by epoch, with the state kept on the workers; `journal` defaults to `Checkpoint.none` |
| `Checkpoint` | `save(epoch, bytes)` / `latest` | where a coordinator writes down what it has folded; `Checkpoint.none`, `Checkpoint.Memory` |
| `Wire.state` | `Schema[S]` | how the COORDINATOR's own state travels — what makes the journal possible |
| `Cluster.local` | `Req => Resp` | a worker made of the registry — in-process, and what a served process runs |
| `Served.serve / connect` | `(ServerSocket, Serve)` / `(host, port) => Serve` | the same worker on a socket |
| `WorkerMain` | `main(port, registrars…)` | a worker process |
| `Acceptance` | `agg / source / frames / expected` | the shared-source program of the acceptance run |
| `Client` (JS) | `main` | the Node client: connect, stream frames, verify via runAsync |

## Gotchas

- `distribute` demands a REPLAYABLE source by type (pure `Chunks`);
  a live effectful stream does not fit the signature — deliberately.
- Wire workers hold their connection lazily; a `PrintWriter` swallows
  write errors — death shows up as `readLine() == null`, throw there.
- Scala.js `main(args)` does NOT receive `process.argv` — the Node
  client reads it explicitly.
- `distribute`'s own contract is at-least-once + idempotent (combOp)
  merges. For a job whose panes are WRITTEN somewhere, the precise
  statement is `Sink.writing`'s: at-least-once offers with an
  identity, therefore exactly-once outcome for a keyed writer, within
  ONE run.

- A `Flow` partition is a THUNK (`() => Chunks[A]`), not a `Chunks`.
  A pure chunked source replays as a value; one built over an
  `Iterator` is consumed by its first run and answers nothing the
  second time — and the engine runs a partition twice whenever a
  windowed stage is seeded.
- Stage 1 has NO exchange, so one keyed stage per flow. Wrocław's
  three keyed stages are three flows and the feed is read three
  times; §20's hand-written lane reads it once. No number is quoted
  for the engine until stage 3 makes the pass single.
- `TestWroclawFlow` is `Live`-tagged (it needs the GTFS feed), so it
  is out of the default gate and runs under `integrationTest`. In a
  WORKTREE the feed is not there at all — it lives in the main
  checkout's `okay-flink/target/data/gtfs`, which `target/` keeps out
  of git; link it, or the suite silently reports zero tests.

- `Flows.autoBound` was measured on the CHEAPEST possible
  accumulator (a count) at eight partitions. It is a default for a
  plan that did not choose; a plan that knows its own shape should
  say `Merge` or `Shuffle` rather than consult a number measured on
  someone else's job.

- **`Flows.run` is now much the slower road for a windowed job.** The
  completeness rule is the fan's; the single-stage node still merges
  every pane at the coordinator, and on the Wrocław job the measured
  gap is 7.6x. Use `Flows.fan` with a `Sink` for anything windowed;
  `dataflow-run-complete-panes` is the backlog item that closes it.
- A fan finishes by MERGE — no exchange. Stage 3's own measurement
  found a stage that wants otherwise (~3x10^5 accumulators per
  partition, above the crossover), and the honest fix is to stop
  producing that merge rather than to parallelise it. See
  `dataflow-complete-panes`.

Every stage of specs/dataflow.md is landed. What is left is the
backlog: `dataflow-run-complete-panes`, `dataflow-fan-overhead` (about
a third of a fan's time is in none of its sinks), and
`dataflow-coordinator`, which is what a journalled coordinator would
need for exactly-once ACROSS runs — and a real CLUSTER, which §20's
distributed section declines to estimate.
