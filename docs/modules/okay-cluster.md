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
the `Chunk`, as everywhere; the wire format is a frame per chunk, ARROW
by default (a chunk is a batch of records, and measured, Arrow crossed a
socket 2.3–3.8x faster than JSON or CBOR, and smaller), CBOR or JSON by a
given, compressed by another (see [okay-arrow](okay-arrow.md)); the cross-node merge
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

**A failure is not yet a death.** A worker is buried after three
CONSECUTIVE failures and any answer clears its count, so a machine
that hiccups stays in the rotation and a run survives a blip on EVERY
worker — which it could not before `dataflow-reconnect`. Three is a
default for a wire that is not known to be lossy: measured on one
(`TestNetem`), it carries 20% loss with certainty and turns 50% loss
into "no workers left" for half the runs — not because the loss stops
progress, but because three lost packets are read as a dead machine.
With burial off the same wire finishes every run at 70% loss. So
`Cluster.run` and `Cluster.stream` take `tolerance` as a parameter,
and a deployment that knows its wire is lossy should say so. The partition
still moves to a survivor on every failure; the count changes who is
asked next time, not who answers now. `Run.retried` counts workers
BURIED and `Run.failed` counts attempts LOST, and they are different
questions now.

That is enough for a worker that hiccups and cannot be enough for a
SOCKET, whose failure is permanent by construction. So
`Served.reconnecting(host, port)` dials lazily and drops the socket on
any failure: the next request dials again, which is what makes a
RESTARTED worker process usable rather than merely tolerated. It does
not retry inside itself — the coordinator already has a policy for a
failed attempt, and a second one hidden in the transport would fight
it.

The other limit, the coordinator being a single point of failure,
held until stage 8: a STREAM can journal now, so a successor picks
the run up (see "And the COORDINATOR can be replaced too", below). A
BATCH `Cluster.run` still dies with its coordinator, and it is a
two-pass function with nothing to resume from — restart it.

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

**And who starts the successor.** `Cluster.leading(job, params, parts,
workers, take, journal, lease)` takes a lease, fences the journal by
the term it answers, runs the stream and gives the seat up; `None`
means somebody else holds it. `Lease` is three methods —
`take(): Option[Long]`, `held(term)`, `release(term)` — so a real
election can be handed in without okay-cluster depending on one:
okay-persist's `Election` answers all three as it stands, and
`Lease.solitary` is the no-election default.

It does not WAIT to be elected, on purpose — a retry loop needs a
clock and a backoff that belong to whatever supervises the process:

```scala
while running do
  Cluster.leading(job, params, parts, workers, take, journal, lease).runWith match
    case Some(run) => report(run)        // the stream ended
    case None      => sleep(a while)     // somebody else leads
```

The term is a FENCING TOKEN: the journal asks the lease before every
commit and throws `Checkpoint.Deposed` instead of writing, so a
predecessor that wakes up mid-run stops at its next epoch rather than
committing over its successor's state. It is a check before a write
and not a compare-and-set — a leader deposed between the two can land
one commit — so every record also carries its TERM, and
`Checkpoint.newest` takes the highest (term, epoch) out of a journal's
history: a stale commit is shadowed rather than read back. A log can
defend itself that way and a single cell cannot. The rows were right
either way, mind: a stale resume costs work, not correctness, as long
as the source replays and the writer is keyed.

**The commit window, and who closes it.** A coordinator that dies
between WRITING a pane and COMMITTING its epoch re-offers that epoch's
panes — one epoch wide, harmless to a keyed writer. The engine cannot
close it alone, because the write left the engine; what it does is
hand the writer two moments: `Sink.committed(epoch)`, called BEFORE
the journal records the epoch, and `Sink.recovered(epoch)`, called
once when a run resumes. Both default to nothing.

`Sink.staging` / `Wire.tumblingStaged` collect the panes an epoch
retires and hand the batch to `move(epoch, panes)` when it is final.
Because the sink is told before the journal is written, `move` may be
asked for the same epoch twice across a restart and never for two
epochs under one number — so a writer that records the epoch beside
its rows in one atomic write drops the repeat and is exactly-once.
That is a two-phase-commit sink, in twenty lines, for the same reason
the checkpoint is a `save` call.

A staging sink belongs to `Cluster.stream` and says so by throwing: a
batch run finishes panes inside each partition, on a WORKER, whose
stage no coordinator will ever commit. Use `Sink.writing` with a
writer keyed by `(start, key)` there.

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

## Running it

Four roads, smallest first. Every command below was run against real
processes before it was written down.

**1. One JVM, N fibres.** No processes, no protocol — a plan and a
sink, `Flows.fan` for several sinks over one pass or `Flows.run` for
a single-stage plan:

```scala
val got = Flows.fan(Flow.slices(events, parts = 8), sink).runWith
got.value      // the answer
got.dropped    // late elements, COUNTED
got.merged     // accumulators that reached the coordinator
```

**2. Across processes.** A worker builds the plan itself from a NAME
and a Schema'd parameter, so nothing ships a closure and every worker
must run the same artifact. Define the job once:

```scala
object WindowJob extends Job[Feed, Sum]:
  type A = Ev
  def name = "test.window"
  def params = summon[Schema[Feed]]
  def flow(f: Feed, parts: Int) = Flow.slices(events(f), parts)
  def sink(f: Feed) = Wire.tumbling(Size, Late, _.key, _.ts, value)(paneSum)

object MyJobs:                       // loading this class registers them
  Jobs.register(WindowJob)
```

Start the workers — the class name on the command line is what a build
declares it can run, and each prints the port it bound:

```
$ sbt "export okayClusterJVM/Test/fullClasspath"   # or your own build's
$ java -cp "$CP" okay.cluster.WorkerMain 7101 okay.cluster.TestJobs$
worker listening 7101 knowing test.window,test.fan
```

and run the coordinator against them:

```scala
val workers = Vector(Served.reconnecting("127.0.0.1", 7101),
                     Served.reconnecting("127.0.0.1", 7102))
val got = Cluster.run(WindowJob, Feed(20000, Late - 1), parts = 8, workers).runWith
```

```
answer   = Sum(3204,981508,-7189878780811552831)
dropped  = 0, partitions = 8
merged   = 179 accumulators reached the coordinator
failed   = 0 attempts lost, 0 workers buried
```

**`reconnecting`, not `connect`, unless you know every worker is up.**
`Served.connect` dials at construction, so one dead address throws
before the run starts; `reconnecting` dials lazily and on failure
drops the socket, which lets the COORDINATOR place the failure — move
the partition to a survivor, count it, bury that worker after three
consecutive ones. Kill a worker and re-run the lines above and the
answer is byte for byte the same:

```
answer   = Sum(3204,981508,-7189878780811552831)
failed   = 4 attempts lost, 1 workers buried
```

**3. As a stream, epoch by epoch**, with the coordinator's fold
written down so a successor can pick the run up:

```scala
val journal = Checkpoint.Memory()          // or your own store
val got = Cluster.stream(WindowJob, params, parts = 8, workers,
                         take = 1024, journal).runWith
```

**4. With an election**, when more than one process may try to be the
coordinator. `leading` takes the seat, fences the journal by the term
and gives the seat up; `None` means somebody else holds it. It does
not wait to be elected — that loop belongs to whatever supervises the
process:

```scala
while running do
  Cluster.leading(job, params, parts, workers, take, journal, lease).runWith match
    case Some(run) => report(run)        // the stream ended
    case None      => sleep(a while)     // somebody else leads
```

`Lease` is three methods over a term and `Checkpoint` is two over
bytes, so both bind to whatever you already run: `TestPersisted`
binds them to okay-persist's `Election` and its compacted log in
about a dozen lines each.

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

## The map in Python or R

A job's map step can run outside the JVM. `mapPy` and `mapR`
(okay-foreign-cluster, specs/foreign-map-reduce.md) put a stage on a
flow whose chunks cross to an interpreter as ONE frame each — an Arrow
IPC stream where the python has pyarrow or R has the `arrow` package,
the JSON frame otherwise — and come back as rows of the next type. The
reduce stays the JVM's `Wire`, so map-side combine, the exchange, resume
and the fault model are exactly what they are for a Scala map.

The Python code ships as an inline module inside the job, which is what
keeps "every worker runs the same artifact" true:

```scala
object Scaling:
  val mod = Foreign.module("scaling", """
    def double(frame):
        return {"key": frame["key"], "v": [x * 2 for x in frame["v"]]}

    def boom(frame):
        raise ValueError("no")
  """)
```

```scala
    def flow(p: Scale, parts: Int): Flow[Out] =
      Flow.slices(Rows.of(p.n), parts).mapPy[Out](Scaling.mod, "double", python)
    def sink(p: Scale): Wire[Out, Long] = Wire.fold(Aggregator.sum[Long].contramap[Out](_.v))
```

The function receives the frame as a dict of lists (or the
`pyarrow.Table` itself under `@okay.arrow`) and answers the columns of
`Out`; in R it receives and answers a `data.frame`:

```scala
    double <- function(frame) { frame$v <- frame$v * 2L; frame }
```

Over three in-process workers the answer is the fan's, to the row:

```scala
    val here = Flows.fan(PyJobs.Doubling.flow(p, 4), PyJobs.Doubling.sink(p)).runWith
    val there = Cluster.run(PyJobs.Doubling, p, 4, Vector.fill(3)(Cluster.local)).runWith
    assertEquals(there.value, here.value)
```

What decides the cost is the BATCH: `Flow.slices` chunks at 256, and a
round trip to Python per 256 rows would be the r-measure-harden number
again, so the stage rechunks to `batch` rows (4096 by default) before
the frame crosses. Interpreters are POOLED per worker JVM — `workers` of
them at most, opened on demand, shared by every partition and every job
that names the same module — because a `ForeignWorker` is one pipe and
partitions run on threads.

Two failures, two roads, both the cluster's own. The FUNCTION's failure
(its exception, a frame of the wrong shape) is a considered refusal:
the run fails naming the stage and the message, and the coordinator does
not carry it to the next worker as if this one had died. A WIRE failure
(the interpreter died, a deadline) is retried on a fresh interpreter,
three times, and only past that is the worker dead — the partition is
then recomputed on a survivor as any partition is. A row type that is
not a flat case class is refused when the stage is built, not on a
worker at the first chunk.

The REDUCE can run there too (foreign-reduce): an `Aggregator`'s two
functions, a `step` over a chunk and a `merge` of two partials, answered
by the interpreter. The accumulator is a flat case class; a partition
hands `step` its chunk as one frame with the running accumulator beside
it, and the coordinator folds partials through `merge`:

```scala
    def step(frame, acc):
        vs = frame["v"]
        n = len(vs) + (acc["n"] if acc else 0)
        s = sum(vs) + (acc["sum"] if acc else 0)
        m = max(vs + ([acc["max"]] if acc else []))
        return {"n": [n], "sum": [s], "max": [m]}

    def merge(a, b):
        return {"n": a["n"] + b["n"], "sum": a["sum"] + b["sum"], "max": max(a["max"], b["max"])}
```

```scala
    def sink(p: Scale): Wire[Rec, Option[Stat]] = Reduce.py[Rec, Stat](Stats.mod, "step", "merge", python)
```

`step` answers ONE ROW AS COLUMNS — a frame function answers a frame —
and `merge` two dicts of fields into one, since a call answers a value;
in R `step` answers a one-row `data.frame` and `merge` a named list. The
answer is `None` for a run that saw no rows. The map and the reduce of
one module share one pool of interpreters.

ONE API over all of it (`Engine`, a typeclass by the module's type): a
job names a module and its functions, and the implicit says what runs
them — Python for a `PyModule`, R for an `RModule`, the JVM for a
`JvmModule` of Scala (or Clojure, or Frege) functions by name. The job
is one text:

```scala
final class StatsJob[M](val name: String, mod: M)(using Engine[M], Reduces[M]) extends Job[Scale, Option[Stat]]:
  type A = Out
  def params: Schema[Scale] = summon[Schema[Scale]]
  def answer: Schema[Option[Stat]] = Schema.SOption(() => summon[Schema[Stat]])
  def flow(p: Scale, parts: Int): Flow[Out] = Flow.slices(Rows.of(p.n), parts).mapIn[Out](mod, "double")
  def sink(p: Scale): Wire[Out, Option[Stat]] = Reduce.in[Out, Stat](mod, "step", "merge")
```

```scala
  val mod: JvmModule = JvmModule("stats")
    .map[Rec, Out]("double")(rows => rows.map(r => Out(r.key, r.v * 2)))
```

`Engine` is the BASE (map) and `Reduces` an EXTENSION, each its own
instance and each optional: a language implements what it can, a job asks
for what it uses, and a reduce on a module type without `Reduces` does
not compile while a map on it still does. The interpreter is a given too
— `given Engine[PyModule] = Engine.py("/venv/bin/python3")` — so a job
moves between languages and interpreters with an import, never an edit.

Two more extensions, each its own typeclass, each optional. **A stateful
stage** (`Stateful[M]`): `open` makes a partition's state on the far
side, `step(frame, state)` folds each chunk through it, `finish` flushes
at the end — the state lives in one interpreter kept for the partition,
so a death loses it and the partition recomputes elsewhere:

```scala
    .stream[Rec, Array[Long], Run]("open", "step", "finish")(
      () => { opened.incrementAndGet(): Unit; Array(0L) },
      (s, rows) => rows.map { r => s(0) += r.v; Run(r.key, r.v, s(0)) },
      s => { finished.incrementAndGet(): Unit; Vector(Run(-1, 0, s(0))) })
```

```scala
    Flow.slices(Rows.of(p.n), parts).statefulIn[Run](mod, "open", "step", "finish")
```

**A model** (`Models[M]`): fit once from its parameters, then the second
argument of every chunk's map, `scale(frame, model)` — materialised once
per interpreter of the pool, since a model lives in one process:

```scala
    .model[Factor, Long]("fit")(f => f.by)
    .mapWith[Rec, Long, Out]("scale")((rows, by) => rows.map(r => Out(r.key, r.v * by)))
```

```scala
    Flow.slices(Rows.of(p.n), parts).mapModel[Out](Model.in(mod, "fit", Factor(3)), "scale")
```

In Python `open` returns a dict and `step` mutates it, `fit` returns the
model and `scale(frame, model)` uses it; in R an environment and a list.
These are not the facade's `Streams` (a flow through a stateless frame
function) nor its `Holds` (one handle on one worker, named and called) —
see [okay-foreign-cluster](okay-foreign-cluster.md).

Measured (`MeasureForeignMapReduce`, 1M rows, 4 partitions, medians of
three at box load 8–13; a run at load 21–27 read 2–3x slower on every
lane and was discarded):

| lane | fan, ms | 3 workers, ms |
|---|---|---|
| the map in Scala | 6–12 | 6 |
| the map in Python, JSON road | ~200 | ~205 |
| the map in Python, Arrow | ~95 | ~98 |
| `@okay.arrow` + `pyarrow.compute` | ~86 | ~90 |
| … and the reduce in Python | ~163 | ~170 |

Arrow halves the Python map against JSON; a Python map is ~10x a Scala
one on this shape, at 90 ms for a million rows; the cluster protocol
adds nothing visible; the reduce in Python costs ~70 ms that `Wire.fold`
does not — move it across only for a reduction the JVM has not got.

Rust, Haskell and Go do not take a stage yet: their shims serve calls,
not the `frame` op (`foreign-frame-op-rust-hs-go`). Clojure and Frege
need none — they run inside the JVM, so their map is `flow.map(f)`.

## API reference

| member | signature | meaning |
|---|---|---|
| `Remote.listen` | `(ServerSocket)(using Schema[A], Scheduler) => Channel[Chunk[A]]` | accepted chunks land in a local channel, whatever format the sender chose (each frame says) |
| `Remote.connect` | `(host, port)(using Schema[A], RemoteFormat, RemoteCompression) => Sender[A]` | the sending end |
| `Remote.Sender` | `send(Chunk[A])`, `close()` | one frame per chunk: a length, a format tag, a compression tag, the payload |
| `RemoteFormat` | `arrow` (the default), `Cbor.given`, `Json.given` | how a chunk is encoded |
| `RemoteCompression` | `none` (the default), `Lz4.given`, `Zstd.given` | Arrow compresses per buffer, CBOR and JSON the whole payload |
| `Cluster.Worker[A, Acc]` | `Chunk[A] => Acc` | the work seam; a dead worker throws |
| `Cluster.distribute` | `(source, workers)(zero, merge) => Acc` | round-robin over the living, per-chunk recompute |
| `Flow.slices` | `(IndexedSeq[A], parts, chunk) => Flow[A]` | contiguous slices of the input's own ORDER |
| `Flow.of` | `(Vector[() => Chunks[A]]) => Flow[A]` | partitions as recipes — a thunk, so a partition can be replayed |
| `Flow.map/filter` | `(A => B) / (A => Boolean) => Flow[…]` | per-partition, held as a `Chunks` transformer |
| `Flow.mapPy` | `(PyModule, fn, python, batch, workers)(using Schema[A], Schema[B]) => Flow[B]` | the map in Python, a chunk per frame; okay-foreign-cluster |
| `Flow.mapR` | `(RModule, fn, rscript, batch, workers)(using Schema[A], Schema[B]) => Flow[B]` | the same in R |
| `Engine[M]` / `Reduces[M]` | typeclasses by the module's type | the base (map) and the extension (reduce), each optional; `Engine.py(path)`, `Engine.r(path)`, `Engine.jvm` |
| `Flow.mapIn` / `Reduce.in` | `[B](module, fn, …)(using Engine[module.type])` / `[A, Acc](module, step, merge, …)(using Reduces[module.type])` | ONE API: the language is the module's type |
| `JvmModule` | `JvmModule(name).map[A, B](fn)(f).reduce[A, Acc](step, merge)(stepF, mergeF)` | Scala, Clojure, Frege functions by name, the shape a PyModule has |
| `Stateful[M]` / `Flow.statefulIn` | `[B](module, open, step, finish, …)(using Stateful[module.type])` | a stage with state per partition, kept in one interpreter for its life |
| `Models[M]` / `Model.in` / `Flow.mapModel` | `Model.in(module, fn, params)`; `[B](model, fn, …)` | a model fit once, materialised per interpreter, the map's second argument |
| `Reduce.py` / `Reduce.r` | `(module, step, merge, …)(using Schema[A], Schema[Acc]) => Wire[A, Option[Acc]]` | the reduce in Python or R: `step(frame, acc)` per chunk, `merge(a, b)` on the coordinator |
| `Reduce.through` | `(Reducer[A, Acc], batch, attempts) => Wire[A, Option[Acc]]` | any reducer |
| `Flow.through` | `(Batcher[A, B], batch, attempts) => Flow[B]` | any batcher — `PyStage`, `RStage`, or one of your own |
| `Batcher` | `name`, `apply(Vector[A]) => Either[Failed, Vector[B]]` | a batch of rows through something outside the JVM; `Batcher.transient` names the kinds that are retried |
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
| `Cluster.leading` | `(Job, P, parts, workers, take, journal, lease) => Option[Run[R]] ! Async` | run it if this process is the coordinator; `None` if not |
| `Lease` | `take(): Option[Long]` / `held(term)` / `release(term)` | who may be the coordinator; `Lease.solitary` is no election |
| `Checkpoint.fenced` | `(term, lease, under) => Checkpoint` | refuses a commit once the lease is gone (`Checkpoint.Deposed`) |
| `Sink.committed / recovered` | `(epoch: Int) => Unit` | the two moments a writer needs; no-ops by default |
| `Sink.staging` / `Wire.tumblingStaged` | `…(move: (Int, Vector[Pane[K, O]]) => Unit)` | an epoch's panes as one batch when the epoch is final — exactly-once for a writer that records the epoch |
| `Wire.state` | `Schema[S]` | how the COORDINATOR's own state travels — what makes the journal possible |
| `Cluster.local` | `Req => Resp` | a worker made of the registry — in-process, and what a served process runs |
| `Served.serve / connect` | `(ServerSocket, Serve)` / `(host, port) => Serve` | the same worker on a socket |
| `WorkerMain` | `main(port, registrars…)` | a worker process |
| `Acceptance` | `agg / source / frames / expected` | the shared-source program of the acceptance run |
| `Client` (JS) | `main` | the Node client: connect, stream frames, verify via runAsync |

## Gotchas

- A stream can change its partition count between epochs (`stage 13`,
  `Job.rescalable`) only over a STRIPED source (`Flow.striped`) into a
  KEYED or FOLD sink. A striped source leaves a clean global prefix to
  skip on resume; a keyed/fold sink keeps all its state in the
  coordinator's fold. A contiguous cut, or a WINDOWED sink (its open
  panes live in the worker, not the journal), is refused with a reason
  — a re-cut of either would silently lose data.
- A job that must NOT compute a partition — it is another party's
  (specs/federation.md) — throws `Cluster.Refused`, and the
  coordinator gets a `Resp.Failed` it does not retry elsewhere. Any
  OTHER throwable from a partition is a death in process (retried on
  the next worker) and a `Resp.Failed` over a socket; only `Refused`
  means the same on both roads. Refuse BEFORE reading: an empty
  answer for a foreign partition is a silent wrong share.
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

- **`Flows.autoBound` is measured, and the accumulator's weight does
  not move it.** 100 000 accumulators sits in the 80 000-to-160 000
  band where the two roads cross, and §20's own tuple-tree
  accumulator (`count zip sum zip max`, six objects per add) crosses
  in the same band as a Long count — what the weight changes is how
  fast the exchange pulls ahead afterwards, not where it starts to.
  It is still a default for a plan that did not choose; a plan that
  knows its own shape should say `Merge` or `Shuffle` rather than
  consult a number measured on someone else's job.

- **`Flows.run` has the completeness rule too, since
  `dataflow-run-complete-panes`.** It used to merge every pane at the
  coordinator where the fan merged the boundary handful — 8.2x apart
  on the Wrocław job. Both roads now merge the same accumulators, and
  `TestFlow` asserts the two counts are EQUAL rather than trusting a
  clock. What is left between them is what they are: a fan reads the
  source once and a plan per stage reads it once each.
- A fan finishes by MERGE — no exchange. Stage 3's own measurement
  found a stage that wants otherwise (~3x10^5 accumulators per
  partition, above the crossover), and the honest fix is to stop
  producing that merge rather than to parallelise it. See
  `dataflow-complete-panes`.

Every stage of specs/dataflow.md up to 10 is landed. Stages 11-13
(the log as the source, the network, rescale) and specs/federation.md
are the direction, with boxes that say which can be checked on one
machine and which wait for machines. What is left besides: `dataflow-run-complete-panes`, `dataflow-fan-overhead` (about
a third of a fan's time is in none of its sinks), and
`dataflow-coordinator`, which is what a journalled coordinator would
need for exactly-once ACROSS runs — and a real CLUSTER, which §20's
distributed section declines to estimate. The cluster is
specs/cluster-pool.md (2026-09-23): the four roads above become a POOL
— N copies of one process the manager (Kubernetes, Nomad, YARN, Slurm,
compose, a cloud) keeps alive, found by its DNS, any of which takes a
job by name over HTTP and coordinates it. Stage 1 is
`cluster-pool-process` in the sprint queue.

## Literature

- Jeffrey Dean, Sanjay Ghemawat. *[MapReduce: simplified data processing on large clusters.](https://doi.org/10.1145/1327452.1327492)* OSDI 2004 / CACM 51(1), 2008. The shape `Job` has: a map per partition, a merge the framework owns, recomputation as the fault model.
- Matei Zaharia et al. *[Resilient distributed datasets: a fault-tolerant abstraction for in-memory cluster computing.](https://www.usenix.org/conference/nsdi12/technical-sessions/presentation/zaharia)* NSDI 2012. A partition as a RECIPE that can be recomputed — `Flow.Src` holds thunks for the same reason.
- Mark Raasveldt, Hannes Mühleisen. *[Don't hold my data hostage: a case for client protocol redesign.](https://doi.org/10.14778/3115404.3115408)* PVLDB 10(10), 2017. Why a chunk crosses to Python as one columnar frame and not as rows: the cost is per message, not per byte.
