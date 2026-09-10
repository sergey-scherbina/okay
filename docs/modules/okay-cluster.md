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
| `Flows.fold` / `Flows.collect` | as above / `Flow[A] => Vector[A] ! Async` | the answer alone; every element in partition order |
| `Acceptance` | `agg / source / frames / expected` | the shared-source program of the acceptance run |
| `Client` (JS) | `main` | the Node client: connect, stream frames, verify via runAsync |

## Gotchas

- `distribute` demands a REPLAYABLE source by type (pure `Chunks`);
  a live effectful stream does not fit the signature — deliberately.
- Wire workers hold their connection lazily; a `PrintWriter` swallows
  write errors — death shows up as `readLine() == null`, throw there.
- Scala.js `main(args)` does NOT receive `process.argv` — the Node
  client reads it explicitly.
- Exactly-once is out of scope by design: at-least-once + idempotent
  (combOp) merges.

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

Next step per specs/dataflow.md: stage 3 — one pass, many sinks.
Until that lands the engine reads Wrocław's feed three times where
§20's hand-written lane reads it once, and no engine number is
comparable with that table.
