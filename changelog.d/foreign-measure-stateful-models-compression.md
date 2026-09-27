## foreign-measure-stateful-models-compression — the numbers the last lanes landed without: three instruments, the bytes, and two loaded runs on record

(operator, 2026-09-27: "1 и 2", item 2.) Three measurement suites, all
Live, all `--include-tags=Live`:

- `MeasureForeignMapReduce` (okay-foreign-cluster) gained a model lane
  (`Model.in` + `mapModel`, `measure.py.arrow.model`) and a stateful lane
  (`statefulIn`, the running sum per partition,
  `measure.py.arrow.stateful`) beside the plain Arrow map — 1M rows, four
  partitions, fan and three workers.
- `MeasureArrowCompression` (okay-arrow, JVM): a 500k-row Arrow body
  (float64, int64, utf8; 12.79 MB raw) compressed per buffer by
  `Compression.Okay` and by `Aircompressor`, written, read, and each
  reading the other's body (`Tables.same` asserted).
- `MeasureRemote` (okay-cluster) gained an implementation column: ZSTD on
  the wire, Arrow and CBOR chunks of 1000 and 10000, both ends on ours and
  on aircompressor (`io.airlift:aircompressor` a Test dependency of
  okayCluster's JVM side).

What landed as numbers: the BYTES — ours within 0.1% of aircompressor on
every body and chunk, smaller on CBOR chunks, every cross-read green
(specs/okay-compress.md Results, docs/modules/okay-compress.md "In
place"). What did not: the times. Two runs, at load 20–47 and 46–74 on 14
cores while siblings benchmarked and gated, swung 2–4x per column and are
on record as DISCARDED, with the two directions that held in both
(aircompressor faster on the Arrow body's ZSTD; the stateful lane at or
under the plain map) written down as leads, not prices
(specs/foreign-map-reduce.md "The model and the stateful lanes"). A
quiet-box re-run is `backlog.d/polyglot/foreign-measure-quiet-rerun.md`.
Found on the way: `gate-retry`'s quiet test reads the sbt count and free
RAM, not load — it started the second run at load 24 and the run ended
at 74.

specs/foreign-map-reduce.md also gained "Stage 5 — PROPOSED: a functional
stateful stage, for the compiled workers too", which the sibling lane
`pyvalue-table` is building.

Commits: b5021ea54 04cd01e2b (the two lane commits; the landing adds this line).
