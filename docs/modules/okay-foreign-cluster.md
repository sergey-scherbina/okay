# okay-foreign-cluster

The MAP of a cluster job in Python or R (specs/foreign-map-reduce.md).
okay-cluster's `Job`/`Flow` is map-reduce — a partitioned source, a
chunk-level `Flow.Local` stage per partition, a `Wire` as the reduce,
recomputed partitions as the fault model — and every stage of it is
Scala on the worker JVM. This module puts a stage on a flow whose chunks
cross to an interpreter as ONE frame each (an Arrow IPC stream where the
python has pyarrow or R has the `arrow` package, the JSON frame
otherwise) and come back as rows of the next type. The reduce stays the
JVM's `Wire`; okay-cluster learns nothing.

| | |
|---|---|
| `flow.mapIn[B](module, fn, …)` / `Reduce.in[A, Acc](module, step, merge, …)` | ONE API: the language is the module's type, through `Engine[M]` (the base, map) and `Reduces[M]` (an extension) in scope — Python for a `PyModule`, R for an `RModule`, the JVM for a `JvmModule`; each instance optional, a job asks for what it uses |
| `JvmModule(name).map[A, B](fn)(f).reduce[A, Acc](step, merge)(…)` | Scala, Clojure, Frege functions by name, the shape a `PyModule` has |
| `flow.mapPy[B](module, fn, python, batch, workers)` | the map in Python: `fn` in an inline `PyModule` takes the frame (a dict of lists, or the `pyarrow.Table` under `@okay.arrow`) and answers `B`'s columns |
| `flow.mapR[B](module, fn, rscript, batch, workers)` | the same in R: a `data.frame` in, a `data.frame` out |
| `Reduce.py[A, Acc](module, step, merge, …)` / `Reduce.r` | the reduce in Python or R: `step(frame, acc)` folds one chunk (answers one row as columns), `merge(a, b)` folds two partials on the coordinator; a `Wire[A, Option[Acc]]`, `None` for no rows |
| `Reduce.through(reducer, batch, attempts)` | any `Reducer[A, Acc]` |
| `flow.through(batcher, batch, attempts)` | any `Batcher[A, B]` — `PyStage`, `RStage`, or one of your own (a test's fake) |
| `Batcher` | `name` and `apply(Vector[A]) => Either[Failed, Vector[B]]`; `Batcher.transient` names the failure kinds that are the wire's and are retried |
| `Pool` / `Pools` | interpreters per worker JVM: `workers` at most, opened on demand, shared by every partition and every job naming the same module; closed when the JVM exits |

## Using it

The foreign code ships as an inline module inside the job — "every
worker runs the same artifact" holds:

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

- The BATCH decides the cost: the stage rechunks to `batch` rows (4096)
  before a frame crosses, so a source chunked at 256 is not a Python
  round trip per 256 rows.
- The FUNCTION's failure is a considered refusal: the run fails naming
  the stage and the message, and it is not retried on the next worker.
- The WIRE's failure (a dead interpreter, a deadline) is retried on a
  fresh interpreter three times; past that the worker is dead and the
  partition is recomputed on a survivor, as any partition is.
- A row type that is not a flat case class is refused when the stage is
  built, not on a worker at the first chunk.

Not here: Rust, Haskell, Go, whose shims do not serve the `frame` op
yet (`foreign-frame-op-rust-hs-go`). Clojure and Frege need nothing — they
run inside the JVM, so their map is `flow.map(f)`.

The whole story: [okay-cluster, "The map in Python or R"](okay-cluster.md#the-map-in-python-or-r).
