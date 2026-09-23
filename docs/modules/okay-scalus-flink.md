# okay-scalus-flink

> The Cardano chain as a Flink source (FLIP-27): confirmed blocks from a
> relay, exploded into the same typed tables a Spark DataFrame and an
> engine-free consumer read.

Depends on: `okay-scalus` (the follower, `CardanoTables`, `Relays`),
`okay-flink` (`FlinkSchema`). JVM, Flink 1.20. Spec:
[specs/scalus.md](../../specs/scalus.md) §7; the whole road:
[Reading a blockchain](../cardano.md).

## Guide

```scala
val env = StreamExecutionEnvironment.getExecutionEnvironment
val source = CardanoFlinkSource(CardanoConfig(
  relay = relay, network = "preprod", table = "transactions",
  confirmations = 0, start = start, maxBlocks = Some(5)))   // no maxBlocks: follow forever
val txs = env.fromSource(source, WatermarkStrategy.noWatermarks(), "cardano")
  .setParallelism(1)                                           // the chain is one sequence
  .executeAndCollect().asScala.toList
```

**One split, because the chain is one sequence.** The enumerator hands
the single split to the first reader that asks; that reader runs
okay-scalus's follower and emits the chosen table's rows. Parallelism
belongs AFTER the source (`keyBy`, `rebalance`): there is no second
independent part of a chain to read.

**Checkpoints.** The split's state is the last block emitted, as a
`Checkpoint(slot, hash, blockNo)`. Blocks are emitted only once
confirmed (`confirmations`, default 15), so what a restored job has
already emitted cannot be taken back, and it resumes exactly after it.
A rollback deeper than `confirmations` fails the job instead of
emitting rows that were never final.

**Rows.** A row's type is okay-flink's `FlinkSchema` over okay-codec's
`Columns`: the same tabular decisions as Spark's (an enum is its case
name, a sum is `kind` plus a nullable row per case with fields, a
recursive value is `cbor` plus json). Flink has no VARIANT type, so
that json is TEXT — read it with Flink SQL's `JSON_VALUE`.

## Options (`CardanoConfig`)

| field | meaning | default |
|---|---|---|
| `relay` | `host:port`, or `registered:<name>` (`Relays.register`) | required |
| `network` | `mainnet`, `preprod`, `preview` | required |
| `table` | one of `CardanoTables.all`: blocks, transactions, inputs, outputs, assets, mints, certificates, withdrawals, redeemers | required |
| `confirmations` | blocks on top before a block is emitted | 15 |
| `start` | `tip`, or `slot:hash:blockNo` | `tip` |
| `maxBlocks` | stop after this many blocks (a BOUNDED source) | none: follow forever |

## Verification

`TestCardanoFlinkSource` runs a bounded job on a Flink MiniCluster over
the recorded preprod session: the outputs equal `CardanoTables`' row for
row (which okay-scalus checks against Koios). The split serializer — what
a Flink checkpoint stores — round-trips. The snippet above is run by
`TestDocExamplesCardanoFlink`.

## Gotchas

- `events` mode (rollbacks as rows) exists for Spark only so far; here a
  deep rollback fails the job.
- The Flink test JVM shares its settings with okay-flink
  (`flinkTestSettings` in build.sbt).
