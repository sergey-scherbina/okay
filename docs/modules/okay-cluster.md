# okay-cluster

> The own small distributed runtime, assembled from existing parts:
> a remote channel that looks local, chunk work distributed with
> per-chunk recompute, and the cross-platform acceptance — a JS
> client driving a JVM server with one shared-source program.

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

Next step per specs/cluster.md: actors — a Stage with a mailbox
Channel, no new abstraction.
