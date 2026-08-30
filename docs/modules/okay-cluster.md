# okay-cluster

The remote channel: the Channel discipline with a socket underneath.

- `Remote.listen(serverSocket)` — accepted chunks land in an ordinary
  local `Channel[Chunk[A]]`; downstream code cannot tell it is remote.
- `Remote.connect(host, port)` — a `Sender`: one Schema-encoded JSON
  frame per line (CBOR when its dialect lands); closing the wire
  closes the channel after the drain.
- The cross-node contract is the Aggregator merge: a variance folded
  half locally and half across the wire merges to the local run's
  answer. A damaged frame is dropped as data; the stream lives.
- `Cluster.distribute(source, workers)(zero, merge)` — distribution
  of work with the fault model built in: a worker is ONE function
  `Chunk[A] => Acc` (in-process, or a wire away — send the chunk,
  await the partial; a dead worker THROWS, that is the whole
  protocol); chunks round-robin over the living, a failure removes
  the worker and hands its chunk — still in hand, the source is a
  value, that is the lineage — to a survivor; partials merge by the
  Aggregator's combOp, order-free.

```scala
val agg = Aggregator.variance[Double]
Cluster.distribute(chunkedSource, Vector(wireWorker, localWorker))(
  agg.init, agg.merge)   // exact even when a worker dies mid-stream
```

- The cross-platform policy's acceptance is IN THE TESTS: a JS client
  under Node (`Client.scala`, linked by scalaJS, driven by `runAsync`
  through the event loop) streams the shared `Acceptance` object's
  frames to a JVM fold-server and verifies the answer against the
  SAME shared-source computation — one program, two platforms, one
  codec.

Actors (a Stage with a mailbox Channel) are the stated next step
(specs/cluster.md).
