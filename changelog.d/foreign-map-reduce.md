## foreign-map-reduce - the MAP of a cluster job in Python or R

specs/foreign-map-reduce.md, after the operator's "ты можешь сделать
map-reduce для вычислений на R или Python…" and "Делай". okay-cluster's
`Job`/`Flow` already was map-reduce (partitions, a chunk-level
`Flow.Local`, `Wire` as the reduce) and the foreign engines already sent
a table as one Arrow frame (py-arrow, r-arrow); nothing joined them.

- New JVM module okay-foreign-cluster (`okay.cluster.foreign`):
  `flow.mapPy[B](module, fn)` and `flow.mapR[B](module, fn)` put a
  `Flow.Local` stage on a flow whose chunks cross to an interpreter as
  ONE frame each (Arrow where pyarrow / R's `arrow` is installed, JSON
  otherwise) and come back as rows of `B` by `Schema`. `flow.through`
  takes any `Batcher`. The reduce stays the JVM `Wire`; okay-cluster is
  unchanged.
- The batch is the frame: `Chunks.rechunk` to `batch` (4096) rows, so a
  source chunked at 256 is not a round trip per 256 rows. Interpreters
  are POOLED per worker JVM (`workers` at most, on demand, shared by
  every partition and job naming the module).
- Two failures, two roads, both the cluster's own: the function's
  failure is a `Cluster.Refused` naming the stage (not carried to the
  next worker); a wire failure is retried on a fresh interpreter, three
  times, then the worker is dead and the partition recomputes elsewhere.
  A row type that is not a flat case class is refused at build time.
- Proven: `TestForeignStage` (7, default gate) and `TestPyMapReduce`
  (2, Live) — 20 000 rows over 3 in-process workers, the map in a REAL
  python3, the answer the fan's to the row, in 0.5 s. `TestRMapReduce`
  is written and skipped (no R here).
- Not here, filed: the reduce in the foreign language
  (`foreign-reduce`); Rust/Haskell/Go, whose shims do not serve `frame`
  (`foreign-frame-op-rust-hs-go`). Clojure and Frege need nothing: they
  run in the JVM, their map is `flow.map(f)`.
