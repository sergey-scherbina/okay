## streams-seam-1-bulk-flow - our engine as a Bulk instance: a Tables program gets its third backend

- `FlowBulk(parts, lines, bytes)` in okay-cluster (specs/streams-seam.md,
  lane 1): `Bulk[Flow]`, so `Tables.run(FlowBulk(4))(p)` runs a program that
  names no platform on the cluster engine, beside `localBulk` and
  `SparkBulk`. `of` slices into partitions, `map`/`filter`/`flatMap` are
  `Local` transformers, `read(path, format)` spreads a file's splits one
  partition per slice (the seam's default, on this engine one fibre per
  partition), `aggregate`/`cache`/`toChunks` run the engine under the
  scheduler given at construction — the seam answers plain values, so the
  instance is where the program's Async ends. `toChunks` is deferred (runs
  per consumption), `cache` eager, as the local instance.
- The join is the seam's hash join: the right side collected ONCE on first
  demand and shared by every partition of the left — Spark's broadcast road.
  A co-partitioned join of two large sides is a binary node `Flow` lacks;
  lane 2's `Flow.Join`, said in the spec so nobody reads this as distributed.
- The spec is the ARC's: the effect row (`Tables`, `Sort`, the coming
  `Streamed`) is the platform-free program and `Flow` is a target of it, as
  an RDD is — not a `Flow` interpreter per platform. Lanes 2-5 named:
  `Streamed` signatures with `viaTables` defaults and native answers,
  `Bulk[DataStream]` for Flink, the structural sub-language for DataFrames
  (Catalyst pays only for what it sees; GTFS 18 s vs 7 s is the measure),
  the one-job docs page.
- Tests: `TestFlowBulk` (5: TestPlan's job agrees at parallelism 1 and 4;
  every seam op agrees on random and empty input; splits read once per
  consumption; the right side collected once; csv pruning),
  `TestDocExamplesFlowBulk`; docs/modules/okay-cluster.md "A Bulk over the
  engine". Additive; gate `affected master Test/compile`.
- Commits: 5f4b25ded (spec), bc71f7214 (code).
