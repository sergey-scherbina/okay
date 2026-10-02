## chunks-external-sort - Chunks.sortBy: a sort that holds one run, not the stream

- `Chunks.sortBy(p, budget)(key)` (okay-stream, `ExternalSort`): the input read
  in runs of `budget` elements, each sorted in memory and spilled, then a
  k-way merge by a heap over one cursor per run, read lazily as `Chunks`
  (Knuth vol. 3 §5.4). Stable; a stream that fits one run is never spilled;
  nothing is read before the first pull; no recursion.
- Two facades in okay-stream, because it cannot see okay-codec: `RunCodec[A]`
  (an element as bytes; givens for numbers, strings, booleans and pairs) and
  `Spill` (where runs go). `spillToTempFiles` is the default given on JVM and
  Native (`SpillFiles`, length-prefixed records, deleted when the output is
  read to its end and `deleteOnExit` otherwise); `Spill.memory` elsewhere —
  JS has no default, so a sort there without one does not compile.
- `okay.codec.RunCodecs.fromSchema`: every type with a `Schema` sorts through
  spilled runs via CBOR (`import okay.codec.RunCodecs.given`).
- Not yet: `Sort.viaTables` still sorts in memory — its `Sort.By` carries no
  codec; that is join-strategy-auto's to wire.
- Tests: `TestExternalSort` (JVM, JS, Native: stable sort at budgets 1/7/64/
  above the input, the run count, runs deleted, laziness, descending, empty),
  `TestExternalSortFiles` (200 000 rows in 20 files, none left),
  `TestRunCodecs` (JVM, JS). Additive: `affected master Test/compile`.
- Commits: bb787a381.
