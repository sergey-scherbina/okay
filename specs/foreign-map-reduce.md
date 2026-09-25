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

- [x] A `Job` whose flow is `Flow.slices(rows, parts).mapPy[Out](mod,
      "scale")` with a JVM `Wire` reduce computes, through
      `Cluster.run` over several in-process workers, EXACTLY what the
      same job computes through `Flows.fan` in one process — the bar
      every level of dataflow is held to — with the map actually run
      by a real python3 (Live, skipped where there is none). RUN HERE:
      `TestPyMapReduce`, 2 passed over the box's python3 (no pyarrow, so
      the frames took the JSON road; Arrow where it is installed).
- [x] The same over a real R (Live) — `TestRMapReduce`, run by
      r-arrow-verify the same day in a container: the map in R over two
      in-process workers, the fan's answer.
- [x] The rows cross as ONE frame per chunk, `batch` rows at a time: the
      source's chunk size is not the batch size (a `Flow.slices` chunk
      of 256 would be a Python round trip per 256 rows); `through`
      rechunks to `batch` first. A fake batcher sees chunks of exactly
      `batch` rows, the last one shorter.
- [x] The FUNCTION's failure (its own exception, a frame of the wrong
      shape) fails the chunk and so the run, by name — thrown as
      `Cluster.Refused` with the stage's name and the far side's
      condition, the worker's considered answer. A deterministic error
      is not retried into a wrong answer.
- [x] A WIRE failure (the interpreter died, a deadline) is retried on a
      fresh interpreter, `attempts` times, and the answer is intact: a
      fake batcher that dies once mid-partition yields the same answer as
      one that never dies. After `attempts` the chunk fails as above.
- [x] A row type that is not a flat case class is refused when the stage
      is BUILT, not at the first chunk: `mapPy[B]` on such a `B` throws
      naming it.
- [x] Interpreters are pooled per worker JVM: four partitions on four
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

## Stage 2 — the REDUCE in Python or R (foreign-reduce)

The operator, after stage 1 landed: "Делай" to the two-function design.
v1 left the reduce on the JVM because a `Wire` is an `Aggregator` whose
merge is part of its type. The foreign reduce keeps that shape and moves
the two functions across the wire:

```scala
trait Reducer[A, Acc]:
  def name: String
  /** `acc` folded over one chunk: None for a partition's first chunk */
  def step(acc: Option[Acc], rows: Vector[A]): Either[Batcher.Failed, Acc]
  /** two partials into one — associative, as any Aggregator's merge */
  def merge(a: Acc, b: Acc): Either[Batcher.Failed, Acc]

object Reduce:
  def through[A, Acc: Schema](reducer: Reducer[A, Acc], batch: Int = Stage.Batch, attempts: Int = 3): Wire[A, Option[Acc]]
  def py[A: Schema, Acc: Schema](module: PyModule, step: String, merge: String, python: String = "python3", ...): Wire[A, Option[Acc]]
  def r[A: Schema, Acc: Schema](module: RModule, step: String, merge: String, rscript: String = "Rscript", ...): Wire[A, Option[Acc]]
```

`ForeignWire` is the `Wire[A, Option[Acc]]`: a partition buffers `batch`
rows and hands them to `step` as ONE frame with the running `Acc` beside
it; `finish`/`peek` flush the rest and hand over the partial, emptied;
the coordinator's `absorb` folds partials in partition order through
`merge`; the answer is `None` for a run that saw no rows. `W` and `S`
are both `Option[Acc]` with `Schema.SOption` — a job's `answer` is the
same. Nothing in the engine changes.

THE CONTRACT ON THE FAR SIDE, shaped by what each op already answers:
- Python `step(frame, acc)`: `frame` a dict of lists (or the
  `pyarrow.Table` under `@okay.arrow`), `acc` a dict of the fields
  (scalars) or `None` for the first chunk; answers ONE ROW AS COLUMNS,
  `{"n": [n], "sum": [s]}` — a frame function answers a frame.
  `merge(a, b)`: two dicts of fields, answers one — a call answers a
  value. R: `step(frame, acc)` takes a data.frame and a named list (or
  NULL), answers a one-row data.frame; `merge(a, b)` takes two named
  lists, answers one.
- the same failure roads as the map stage: a function's failure is a
  `Cluster.Refused` naming the reducer; a wire failure retries on a
  fresh interpreter; a partial that is not exactly one row of `Acc`
  is the function's failure ("ReduceShape").
- the interpreters are the SAME pool as the map stage's for the same
  module and interpreter (`PyPool.of`, `RPool.of`), so a job whose map
  and reduce are in one module costs one set of processes.

- [x] A job whose reduce is `Reduce.through(fake)` computes, through
      `Cluster.run` over 1 and 3 in-process workers, exactly what the
      fan computes and what the JVM computes directly (count, sum, max).
- [x] An empty input answers `None`; a partition shorter than `batch`
      still folds; the fake sees `batch`-row chunks and `None` as the
      first `acc` of every partition; `merge` runs `partitions - 1`
      times on the coordinator.
- [x] The function's failure is a `Cluster.Refused` naming the reducer;
      a wire failure that heals is invisible to the coordinator.
- [x] The same over a REAL python3: `step`/`merge` in Python (Live).
- [x] The same over a REAL R (Live, in the r-arrow-verify container).

## Stage 3 — one API over every language (foreign-engine-typeclass)

The operator, after stages 1 and 2: "можно тоже какую то удобную фасад
абстракцию поверх этого? Чтобы один и тот же код работал со всем этим
прозрачно через имплиситы и тайпклассы" — and, while it was being built:
"каждая модель сама опционально реализовывала набор базовых методов плюс
расширения в отдельных классах типов (тоже опционально) а фасад собирал
из этого работающую конструкцию".

The engine is a TYPECLASS BY THE MODULE'S TYPE, in two layers:

```scala
trait Engine[-M]:                       // the BASE: every engine maps
  def name: String
  def batcher[A: Schema, B: Schema](module: M, fn: String, workers: Int): Batcher[A, B]

trait Reduces[-M]:                      // an EXTENSION, its own instance
  def reducer[A: Schema, Acc: Schema](module: M, step: String, merge: String, workers: Int): Reducer[A, Acc]

extension [A](flow: Flow[A])
  def mapIn[B](module: Any, fn: String, …)(using Engine[module.type], Schema[A], Schema[B]): Flow[B]
object Reduce:
  def in[A: Schema, Acc: Schema](module: Any, step: String, merge: String, …)(using Reduces[module.type]): Wire[A, Option[Acc]]
```

- **A job names a module and functions, nothing else.** `StatsJob[M](mod:
  M)(using Engine[M], Reduces[M])` is ONE text; handed a `PyModule` it runs
  in Python, an `RModule` in R, a `JvmModule` in the JVM. The tests are
  that text three times (`TestEngine`, `TestPyEngine`, `TestREngine`).
- **Base and extensions are separate instances, each optional.** A
  language implements `Engine[M]` and whichever extensions it can;
  `mapIn` asks for the base, `Reduce.in` for `Reduces`, and a job that
  reduces on a module type without one does not COMPILE — while `mapIn` on
  it still does (`TestEngine`, by `compileErrors`). The facade assembles a
  working job out of the instances the module has; the next capability
  (a streaming stage, held objects) is the next typeclass, not a new
  method every engine must fake.
- **Contravariant, on `module.type`.** `mapIn[Out](mod, "double")` names
  no module type: the extension asks for `Engine[mod.type]`, and
  `Engine[PyModule]` is one since `mod.type <: PyModule`. A test's own
  `Engine[FakeModule]` is a given like any other: the typeclass is open.
- **The interpreter is a given**: `given Engine[PyModule] = Engine.py(path)`
  and `given Reduces[PyModule] = Reduces.py(path)` move a job to another
  python without an edit; the instances live in their own companions,
  where the implicit search looks.
- **`JvmModule`**: the JVM's own languages — Scala, Clojure (`Clj.fn` as
  the function it is), Frege — as functions by name, the shape a
  `PyModule` has, so a job reads the same. The one cast is its registry's
  (a heterogeneous map keyed by the name the job gives), isolated and
  said; a name never registered is refused when the flow is built,
  naming what the module has.
- `mapPy`/`mapR`/`Reduce.py`/`Reduce.r` stay as the named cases; the
  generic is the API.

- [x] The same job text over the JVM (`TestEngine`, default gate, 3 in-process
      workers, count/sum/max), a fake engine of the test's own, python3
      (`TestPyEngine`, Live, run here) and R (`TestREngine`, Live, run here
      in the r-arrow-verify container).
- [x] With the base alone a module maps and a reduce on it is a compile
      error naming `Reduces`.
- [x] A JVM function's exception is the function's failure (a
      `Cluster.Refused` naming `jvm:stats:boom`); a name the module lacks
      is refused at build, naming what it has.

## Results

foreign-map-reduce (2026-09-25). New JVM module okay-foreign-cluster
(`okay.cluster.foreign`): `Batcher`, `Stage.through` (a `Flow.Local`
node over `Chunks.rechunk` + `Chunks.mapWith`), `Pool`/`Pools`,
`PyStage`, `RStage`, and `flow.through / mapPy / mapR`. Nothing in
okay-cluster changed.

- `TestForeignStage` (7, default gate, no interpreter): the fan and
  `Cluster.run` over 1 and 3 in-process workers agree with the batcher
  in the JVM; a source chunked at 256 reaches the batcher as 1000, 1000,
  500 per 2500-row partition; a function failure is a `Cluster.Refused`
  naming the stage and the condition on both roads; a batcher that dies
  once is retried and the coordinator sees `retried == 0`; one that
  always dies exhausts `attempts` and the run names the stage and "2
  attempts"; `mapPy[Long]` is refused at build time; a `Pool` of 2 under
  4 threads opens 2, reuses them, replaces a dead one.
- `TestPyMapReduce` (2, Live): 20 000 rows, 4 partitions, 3 in-process
  workers, the map in python3 — `there.value == here.value ==
  Rows.doubled`, `retried == 0`, 0.5 s; a `ValueError` in the function
  fails the run naming `py:scaling:boom` and the message.
- `TestRMapReduce` (1, Live): passed in a container (r-arrow-verify).
- Decided while writing: `Cluster.Refused` is `final`, so a function
  failure is thrown AS one (with the stage's name in its message) rather
  than as a subclass; and `RSubprocess` has no `alive`, so `RStage`
  reports a death from the exception (`DEAD` in its message) and the pool
  replaces the session on that.

Stage 2 (foreign-reduce, 2026-09-25). `Reducer`, `ForeignWire`,
`Reduce.through/py/r`, `PyReducer`, `RReducer`; `PyPool`/`RPool` factored
out of the stages so a module's map and reduce share one pool of
interpreters; `Attempts.run` is the two failure roads, shared.
- `TestForeignReduce` (5, default gate): the fan, `Cluster.run` over 1
  and 3 workers and the JVM agree on count/sum/max; no rows answers
  `None` and 7 rows fold; the fake sees 1000, 1000, 500 per 2500-row
  partition with `None` first on each of the four, and `merge` runs 3
  times; a function failure is a `Cluster.Refused` naming the reducer; a
  reducer that dies once heals with `retried == 0`.
- `TestPyReduce` (2, Live, run here): 20 000 rows, 4 partitions, 3
  workers, `step`/`merge` in python3, the JVM's answer to the row; a
  `ValueError` in `step` names `py:stats:boom/merge`.
- `TestRReduce` (1, Live, run here in the r-arrow-verify container):
  the same in R, 5 000 rows over 2 workers.
- Decided while writing: `peek` IS `finish` for this wire — what is
  folded so far leaves and the partition restarts from `None`, which a
  merge that is associative makes exact; and a partial that is not
  exactly one row is the FUNCTION's failure (`ReduceShape`), since the
  contract says one row and a wrong count is the code, not the wire.

## Measured (foreign-map-reduce-measure, 2026-09-25)

`MeasureForeignMapReduce` (Live): 1M rows of `Rec(key: Int, v: Long)`, 4
partitions, doubled and summed — in one process (`Flows.fan`, the
stage's own cost) and over three in-process workers (`Cluster.run`).
Medians of three per lane; the JSON lane is the box's python3 (no
pyarrow), the Arrow lanes a venv of the SAME interpreter with pyarrow
25.0.1 (`OKAY_PYARROW_PYTHON`), so only the road differs. Two runs at
load 8 and 13 agree within ~10% and are the number; a first run at load
21–27 read 2–3x slower on every lane and is DISCARDED (it also showed a
batch-size effect, 4096 → 65536 halving the time, that the quiet runs do
not: an artefact of a busy box, not of the batch).

| lane (1M rows, 4 partitions) | fan, ms | 3 workers, ms |
|---|---|---|
| the map in Scala (`flow.map`) | 6–12 | 6 |
| the map in Python, JSON road | 198–204 | 197–215 |
| the map in Python, Arrow (batch 4096) | 89–108 | 94–103 |
| the map in Python, Arrow (batch 65536) | 101–102 | 101–107 |
| `@okay.arrow` + `pyarrow.compute` (no Python loop) | 83–90 | 88–92 |
| … and the REDUCE in Python (`step`/`merge`) | 158–168 | 167–172 |

What it says:
- Arrow halves the Python map against JSON (~200 → ~95 ms), the same
  ratio py-arrow measured on a single frame; the cluster protocol adds
  nothing visible (fan ≈ three workers on every lane).
- a Python map is ~10x a Scala map on this shape — but 90 ms for a
  million rows, all of it Python's own work and the pipe; a vectorised
  `@okay.arrow` function buys another ~10%, since `x * 2` per element
  was never the cost — crossing was.
- 4096 rows per frame is already past the knee on a quiet box; a bigger
  batch buys nothing here. Left at 4096.
- the reduce in Python costs ~+70 ms: `sum` over 1M ints in Python plus
  three merges; `Wire.fold` on the JVM is free by comparison. Move the
  reduce across only when the reduction is not one the JVM has.
