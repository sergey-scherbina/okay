# foreign-map-reduce: the MAP of a cluster job in Python or R

## Overview

The operator (2026-09-25): "ты можешь сделать map-reduce для вычислений
на R или Python, или Rust, Haskell, Frege, Clojure и тд?" — then
"Делай".

Two halves already exist and do not know each other:

| half | what it is | where it stops |
|---|---|---|
| okay-cluster's `Job[P, R]` + `Flow` (specs/dataflow.md) | map-reduce by another name: a partitioned source, a chunk-level `Flow.Local` transformer per partition, a `Wire[A, R]` as the reduce/merge, recomputed partitions as the fault model | every stage is Scala on the worker JVM |
| the foreign engines (specs/polyglot-one-wire.md, specs/r.md) | Python, R, TypeScript, Rust, Haskell, Go as handlers on one wire; `ForeignEval.Frame`/`REval.Frame` send a TABLE as one Arrow IPC stream (py-arrow, r-arrow) | one call at a time, no place in a `Flow` |

The one fact the design rests on: `Flow.Local(in, name, f: Chunks[A] =>
Chunks[B])` is ALREADY the seam. A chunk is a batch; a batch of typed
rows is a frame; a frame crosses to Python or R as one Arrow table
(the transport built the same day). So a foreign map stage is a `Local`
node whose `f` sends each chunk through a worker as one frame, and
nothing in the engine — partitions, epochs, resume, the exchange, the
fault model — has to learn anything. The reduce stays the JVM's `Wire`.

"Every worker runs the SAME ARTIFACT" (Job.scala's stated price) holds
unchanged: the Python or R code ships as an inline module
(`Foreign.module`, `R.module`) inside the job's own source, so a worker
that can run the job can start its interpreter.

## Interface

A new JVM module, `okay-foreign-cluster` (package `okay.cluster.foreign`),
depending on okay-cluster, okay-py and okay-r; okay-cluster stays free
of the engines, as okay-py stayed free of the workflow machinery in
okay-foreign-workflow.

```scala
/** a batch of typed rows through something outside the JVM, as one frame */
trait Batcher[A, B]:
  def name: String
  def apply(rows: Vector[A]): Either[Batcher.Failed, Vector[B]]

object Batcher:
  final case class Failed(kind: String, message: String)
  /** the kinds that are the WIRE's, not the function's — retried on a
   * fresh worker; anything else fails the chunk, and so the run */
  val transient: Set[String]

extension [A](flow: Flow[A])
  /** the chunk through `batcher`, `batch` rows at a time */
  def through[B](batcher: Batcher[A, B], batch: Int = Stage.Batch, attempts: Int = 3): Flow[B]
  /** Python: `fn` in `module` takes the frame (a dict of lists, or the
   * pyarrow.Table under `@okay.arrow`) and answers one of B's columns */
  def mapPy[B: Schema](module: PyModule, fn: String, python: String = "python3",
                       batch: Int = Stage.Batch, workers: Int = Stage.Workers)(using Schema[A]): Flow[B]
  /** R: `fn` in `module` takes a data.frame and answers one */
  def mapR[B: Schema](module: RModule, fn: String, rscript: String = "Rscript",
                      batch: Int = Stage.Batch, workers: Int = Stage.Workers)(using Schema[A]): Flow[B]
```

`PyStage(module, fn, python, workers)` and `RStage(module, fn, rscript,
workers)` are the two `Batcher`s; both hold a POOL of interpreters per
worker JVM (`workers` of them at most, opened on demand, shared by
every partition and every job that names the same module on this JVM),
because a `ForeignWorker` or an `RSubprocess` is one pipe and partitions
run on threads.

## Behavior

- [ ] A `Job` whose flow is `Flow.slices(rows, parts).mapPy[Out](mod,
      "scale")` with a JVM `Wire` reduce computes, through
      `Cluster.run` over several in-process workers, EXACTLY what the
      same job computes through `Flows.fan` in one process — the bar
      every level of dataflow is held to — with the map actually run
      by a real python3 (Live, skipped where there is none).
- [ ] The same over a real R (Live).
- [ ] The rows cross as ONE frame per chunk, `batch` rows at a time: the
      source's chunk size is not the batch size (a `Flow.slices` chunk
      of 256 would be a Python round trip per 256 rows); `through`
      rechunks to `batch` first. A fake batcher sees chunks of exactly
      `batch` rows, the last one shorter.
- [ ] The FUNCTION's failure (its own exception, a frame of the wrong
      shape) fails the chunk and so the run, by name — `Stage.Failed`
      carries the function's address and the far side's condition. A
      deterministic error is not retried into a wrong answer.
- [ ] A WIRE failure (the interpreter died, a deadline) is retried on a
      fresh interpreter, `attempts` times, and the answer is intact: a
      fake batcher that dies once mid-partition yields the same answer as
      one that never dies. After `attempts` the chunk fails as above.
- [ ] A row type that is not a flat case class is refused when the stage
      is BUILT, not at the first chunk: `mapPy[B]` on such a `B` throws
      naming it.
- [ ] Interpreters are pooled per worker JVM: four partitions on four
      threads through one `PyStage` open at most `workers` python
      processes, not one per chunk and not one per partition.

## Out of scope (v1)

- the REDUCE in the foreign language. A `Wire` is a Scala `Aggregator`
  whose merge is part of its type (that is what makes map-side combine
  free, Flow.scala's comment); a foreign monoid would have to declare a
  `merge` the coordinator can call, which is a second protocol. Filed:
  `foreign-reduce`.
- Rust, Haskell, Go: their shims do not serve the `frame` op (only
  Python's, R's and TypeScript's do), so a chunk cannot cross to them
  as a table yet. Filed: `foreign-frame-op-rust-hs-go`.
- Clojure and Frege: they run INSIDE the JVM (okay-clojure, okay-frege)
  — a map step there is `flow.map(f)` with a Clojure or Frege function,
  no wire, no frame, nothing this module has to add. Said here so nobody
  looks for a `mapClj`.
- TypeScript: serves `frame` over JSON, no Arrow; works through
  `ForeignWorker` the same way Python does and could take a `TsStage`
  for the cost of a name; not written until someone asks.

## Decisions

- **A `Local` node, not a new `Flow` case.** The engine's own seam for
  "anything per-partition" is a chunk transformer; a foreign map IS one.
  A new case would have made `Flows.shape`, resume, rescale and the
  exchange all learn a node that behaves exactly like `Local`.
  Rejected: `Flow.Foreign(...)`.
- **The batch is the frame is the Arrow table.** `Chunks.rechunk` to
  `batch`, `PyFrame.of`/`RFrame.of` by `Schema`, one `Frame` op per
  chunk — the py-arrow/r-arrow transport as it is. Rejected: a row at a
  time (`Call` per element — the 13.7 s road r-measure-harden measured),
  and a whole-partition frame (unbounded memory on the far side, and a
  resume that would have to replay it).
- **A pool per JVM, keyed by the module and the interpreter.** One
  interpreter per chunk is a process spawn per 4096 rows; one per
  partition is `parts` interpreters for a job with 64 partitions on a
  4-core node. `workers` bounds it; on-demand opening means a
  single-partition job costs one process.
- **Transport failures retry, function failures do not.** The same line
  okay-foreign-workflow's oracle draws (`ForeignActivity.transport`):
  a dead interpreter is not an answer, an exception is. A chunk retried
  after a death recomputes a pure map, so at-least-once is exact here.
- **The reduce stays on the JVM** — see Out of scope.

## Results

(after implementation)
