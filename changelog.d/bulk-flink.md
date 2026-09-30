## bulk-flink - Flink as a Bulk: a Tables program runs on Flink unchanged

- `FlinkBulk` (okay-flink; specs/streams-seam.md, lane 3): `Bulk` over Flink's
  DataStream API in BATCH mode — `FlinkBulk.local(parallelism)` or over an
  environment of your own. `join` is a `coGroup` keyed on both sides in an
  end-of-stream global window; `aggregate` the okay `Aggregator` as Flink's
  `AggregateFunction`; elements as `AnyRef` under generic type information.
- `flink-streaming-java` and `flink-clients` are `optional` for okay-flink now;
  `FlinkBulk.missing` names them and `FlinkBulk.local` refuses by that name.
- Found: Flink 1.20's `fromData` over an empty collection still generates a
  record and fails; an empty table is a placeholder filtered at once.
- Tests: `TestFlinkBulk` (Live, local MiniCluster: the seam's operations and a
  Tables program agree with the local instance; an empty aggregate).
  docs/modules/okay-flink.md. Additive; gate the suite and
  `affected master Test/compile`.
- Commits: 8b1238d77.
