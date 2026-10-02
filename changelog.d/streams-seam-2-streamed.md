## streams-seam-2-streamed - the stream operators as a signature in the row, and the engine's join

- `Streamed` (okay-stream; specs/streams-seam.md, lane 2): `joinSorted`,
  `joinWithin(within, lateness)(atL, atR)`, `windowed(size, slide,
  lateness)(key)(at)(agg)`, `zip` beside `Tables` and `Sort`, on a handle
  and on a program. `Streamed.viaTables` is the platform-free answer
  through `collect` and the local machines (`SortMerge`, `WindowJoin`,
  `Windows`, `Chunks.zip`). Bounded semantics stated once: the windowed
  join on a table is the interval join with each side fed in its time
  order (lateness changes nothing); a window keeps the table's order and
  drops as `Windows` drops; zip is positional.
- `Flow.Join(l, r, buckets, how)` (okay-cluster): the engine's one binary
  node, a `Wide` over both inputs — every partition of both sides buckets
  its rows by key hash, tagged; each reducer joins its buckets by
  `JoinHow.Hash`, `Sorted(ord)` (`Chunks.joinSorted`, fails by name on an
  unordered side) or `Within` (the interval join). `Flow` gets `join`,
  `joinSorted`, `joinWithin`. A keyed input is refused by name, as a second
  keyed stage is.
- `FlowBulk.join` is now `Flow.Join` (co-partitioned); lane 1's broadcast
  road is `broadcastJoin`. `FlowBulk.streamed` answers the signatures
  natively (joins as `Flow.Join`s, a window as `Flow.Windowed` seeded,
  `Finish.Auto`; zip through collect) and composes as `SparkBulk.sort`
  does; `FlowBulk.run(p)` runs a `Tables + Streamed` program on the engine.
- Tests: `TestStreamed` (4, JVM + JS + Native), `TestFlowJoin` (5),
  `TestFlowBulk` updated, docs example pinned. Not additive: gate
  `affected master staged`. Spark's native answers deferred to after lane 5.
- Commits: 8e7d2826e.
