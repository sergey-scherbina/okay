## scalus-flink: the Cardano chain as a Flink source

specs/scalus.md stage 4, triggered by the Spark source running on
mainnet (scalus-mainnet-live). New module okay-scalus-flink:
`CardanoFlinkSource(CardanoConfig(...))`, a FLIP-27 source with ONE split
(a chain is one sequence; parallelism belongs after the source) whose
state is the last block emitted, so a job restored from a Flink
checkpoint resumes exactly after it; a rollback past `confirmations`
fails the job. `maxBlocks` makes it bounded.

- okay-flink: `FlinkSchema`, the twin of `SparkSchema` — okay-codec's
  `Columns` as a Flink `RowTypeInfo` and `Row`s (an enum is its name, a
  sum `kind` + branches, recursion `cbor` + json as TEXT, arrays of the
  element's class). okay-flink now depends on okay-codec.
- okay-scalus: the table registry (`CardanoTables.Table`, `all`,
  `named`) and `Relays` moved here from the Spark module, engine-free,
  so Spark and Flink serve the same tables by name.
- build.sbt: the Flink test JVM settings are `flinkTestSettings`,
  shared by okay-flink and okay-scalus-flink.
- Tested on a Flink MiniCluster over the recorded preprod session: a
  bounded job's outputs equal `CardanoTables`' row for row; the split
  serializer round-trips.

Docs: okay-scalus-flink page with the run snippet, okay-flink's
FlinkSchema section, the guide's §6, typepedia, index.
