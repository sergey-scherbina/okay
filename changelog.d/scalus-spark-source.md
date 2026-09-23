## okay-scalus-spark: `format("cardano")`

The last lane of okay-scalus-spark (specs/scalus.md §6): a Spark
DataSource V2, batch and micro-batch, over okay-scalus's follower and
`CardanoTables`. It decides nothing — rows from `CardanoTables`, shape
from `Columns`, Spark types from `SparkSchema` — so a DataFrame and an
engine-free consumer (okay-watch) read the same tables.

- The driver follows the relay on a background thread and keeps
  confirmed blocks' bytes; partitions carry them and executors decode
  and explode. An offset is a confirmed block (`slot`, `hash`,
  `blockNo`), so a re-run batch is the same rows; admission control
  names the resume offset; a rollback past `confirmations` fails the
  query. Batch reads take `start` and `blocks`.
- Options: `table`, `network`, `relay` (`host:port` or a JVM-registered
  wire), `confirmations`, `start`, `blocks`, `blocksPerPartition`.
- Tested on the recorded preprod session: the outputs DataFrame equals
  `CardanoTables` row for row; SQL on `datum.kind` and `variant_get` on
  real inline datums; a streaming query with `confirmations = 2` reads
  exactly the first three blocks' transactions, then waits.
- build.sbt: okay-spark's test-JVM settings are now `sparkTestSettings`,
  shared by both Spark modules.

Docs: module page, the guide's §5 with the analysed snippet, typepedia,
docs index. Backlog: `scalus-executor-fetch` (the backfill optimisation).
