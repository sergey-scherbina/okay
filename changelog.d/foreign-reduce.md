## foreign-reduce - the REDUCE of a cluster job in Python or R

specs/foreign-map-reduce.md stage 2, after "Делай". Stage 1 left the
reduce on the JVM; now an `Aggregator`'s two functions cross the wire:
`Reduce.py[A, Acc](module, "step", "merge")` and `Reduce.r(...)` are a
`Wire[A, Option[Acc]]` (okay-foreign-cluster).

- A partition buffers `batch` rows and hands `step` its chunk as ONE
  frame with the running accumulator beside it (a dict of fields, or
  `None` first); `step` answers one row as columns. The coordinator folds
  partials through `merge(a, b)` — two dicts in, one out (R: a named
  list). The accumulator is a flat case class by `Schema`; a run that saw
  no rows answers `None`. `peek` is `finish`: the merge is associative.
- The map and the reduce of one module share one pool of interpreters
  (`PyPool`/`RPool`, factored out of the stages); the two failure roads
  are `Attempts.run`, shared with the map stage.
- Proven: `TestForeignReduce` (5, default gate); `TestPyReduce` (2,
  Live, run here) — count/sum/max in python3 over 3 in-process workers,
  the JVM's answer to the row; `TestRReduce` (1, Live, run here in the
  r-arrow-verify container) — the same in R.
- With foreign-map-reduce this makes a job whose map AND reduce are in
  Python or R; Clojure and Frege are `flow.map`/`Aggregator` in the JVM;
  Rust, Haskell and Go wait on `foreign-frame-op-rust-hs-go`.
