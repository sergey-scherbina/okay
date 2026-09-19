# okay-cluster — a small distributed runtime, assembled from parts

Nothing here is a new framework: a remote channel that looks local,
chunk work distributed with per-chunk recompute, a cross-platform
acceptance where a JS client drives a JVM server from ONE shared-source
program, and — since the dataflow spec's stage 1 — the engine itself.

A `Flow` is a distributed plan as a VALUE, and `Flows` runs it on a
fibre per partition. The plan being a value is what makes it
inspectable, testable without a cluster, and rewritable before it
runs.

## The pieces

| | |
|---|---|
| the remote channel | a `Channel` whose other end is elsewhere; the program above it does not know |
| chunk work | distributed with per-chunk recompute, so a lost worker costs its chunk rather than the run |
| `Flow` / `Flows` | the plan as a value, and the runtime that folds it per partition |
| the acceptance | one shared-source program, a JS client and a JVM server, proving the wire rather than asserting it |

## Further

| | |
|---|---|
| [`docs/modules/okay-cluster.md`](../docs/modules/okay-cluster.md) | the guide |
| [`specs/cluster.md`](../specs/cluster.md) | the design and its decisions |
| [`specs/dataflow.md`](../specs/dataflow.md) | the engine, stage by stage, with the numbers it was tuned against |
| [`okay-spark/`](../okay-spark), [`okay-flink/`](../okay-flink) | the same `Aggregator`, other people's engines |
