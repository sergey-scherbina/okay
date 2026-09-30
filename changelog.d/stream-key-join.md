## stream-key-join - the join by key over streams: sort-merge on key-ordered Chunks and Source

- `Chunks.joinSorted` / `leftJoinSorted` / `fullJoinSorted` and the same
  three on `Source` (specs/stream-join.md): two sides non-decreasing in key,
  one cursor a side, the RIGHT run of equal keys held while the left side
  streams against it, nothing held beyond that run and one lookahead — so
  two unbounded streams join in bounded memory, which `Bulk.join`'s hash
  join (the right side whole) and `Tables.join` (a plan over it) cannot.
- `SortMerge` is the machine, once: `step` decides everything decidable and
  answers what it needs next (a left row, a right row, nothing); the chunk
  driver pulls on two cursors, one tree step and one output chunk per left
  chunk; the live driver is `Source.zip`'s shape line for line (a fiber per
  side, the scope entered in front and exited in the end branch, both sides
  closed at the end), repeated rather than shared so `zip` stays as its
  tests pin it.
- Sortedness is CHECKED, not assumed: a key smaller than the one before it
  on the same side fails the join naming the side and both keys, after
  everything decided before it. A row is matched only against a CLOSED run.
- Companion functions, not extensions (source-zip's split-package lesson:
  `join` is `Async`'s fiber join and every `Bulk.join` extension already).
- Additive: no existing body changed. Tests: `TestSortMerge` (9, JVM + JS +
  Native, in `src/test/scala-cross`), `TestSourceJoin` (6, JVM),
  `TestDocExamplesStreamJoin`; docs/guide.md §6. Gate: the suites,
  `TestDocSnippets`, `affected master Test/compile`.
- Stage 2 — the event-time WINDOWED join for unordered streams on the landed
  `Windows` watermark (Flink interval join, Kafka Streams KStream-KStream) —
  is specified in the same spec and filed as backlog `stream-join-windowed`,
  with the okay2 port of stage 1.
- Commits: a56f3ff6a (spec), 960d5e109 (code, tests, docs).
