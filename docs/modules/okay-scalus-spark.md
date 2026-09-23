# okay-scalus-spark

> `spark.read.format("cardano")` and `spark.readStream.format("cardano")`:
> the Cardano chain as Spark tables — confirmed blocks, read from a
> relay, exploded into the same typed rows an engine-free consumer
> reads.

Depends on: `okay-scalus` (the follower, `CardanoTables`), `okay-spark`
(`SparkSchema`). JVM. Spec: [specs/scalus.md](../../specs/scalus.md) §6;
the whole road from relay to table: [Reading a blockchain](../cardano.md).

## Guide

**Nothing is decided here.** The rows are `CardanoTables`', their
tabular shape okay-codec's `Columns`', their Spark types `SparkSchema`'s.
This module only connects Spark to them — which is why the same tables
exist without Spark, and why a DataFrame and okay-watch cannot disagree
about what a block contained.

```scala
val outputs = spark.readStream.format("cardano")
  .option("relay", "preprod-node.play.dev.cardano.org:3001")
  .option("network", "preprod")
  .option("table", "outputs")
  .option("confirmations", "15")
  .load()
outputs.createOrReplaceTempView("outputs")

val withDatums = spark.sql("""
  SELECT txHash, index, address, lovelace,
         variant_get(datum.Inline.data.json, '$.Constr.constr', 'string') AS constructor
  FROM outputs
  WHERE datum.kind = 'Inline'""")
// withDatums.writeStream.format("console").start() — one line per inline datum, as blocks confirm
```

A sum column is `kind` plus one nullable struct per case with fields
(`datum.kind = 'Inline'`, `datum.Inline.data`); a Plutus datum is
recursive, so it is `struct<cbor, json>` and the json is a Spark 4
VARIANT that `variant_get` reads into.

**How a micro-batch is read.** The driver follows the relay on a
background thread (headers and bodies, keep-alive while the chain is
quiet) and keeps each confirmed block's bytes. An offset is the last
confirmed block (`slot`, `hash`, `blockNo`); a partition carries a
range of blocks' bytes, and executors DECODE and explode them — the
expensive part, in parallel. Because an offset only ever names blocks
at depth ≥ `confirmations`, a batch re-run after a failure yields the
same rows (Spark's exactly-once contract with an idempotent sink). A
rollback deeper than `confirmations` FAILS the query: rows that were
never final are not produced.

**A bounded read.** `spark.read` (batch) takes `start` and `blocks`:
the first `blocks` confirmed blocks after the checkpoint.

## Options

| option | values | default |
|---|---|---|
| `table` | `blocks`, `transactions`, `inputs`, `outputs`, `assets`, `mints`, `certificates`, `withdrawals`, `redeemers` | `blocks` |
| `network` | `mainnet`, `preprod`, `preview` | `mainnet` |
| `relay` | `host:port`, or `registered:<name>` (a wire registered in this JVM with `Relays.register`) | required |
| `confirmations` | blocks on top of a block before it is read | `15` |
| `start` | `tip`, or `slot:hash:blockNo` | `tip` |
| `blocks` | batch only: how many confirmed blocks | required for batch |
| `blocksPerPartition` | blocks per input partition | `10` |

## API reference

| name | what |
|---|---|
| `CardanoSource` | the `TableProvider`, short name `cardano` (registered through `META-INF/services`) |
| `CardanoStream` | the `MicroBatchStream` (with admission control: Spark names the offset it resumes at) |
| `CardanoBatch` | the bounded read |
| `CardanoOffset` | `(slot, hash, blockNo)` as JSON |
| `Relays` | `register(name, () => Wire)` — a recorded session, a test double, an embedded transport |

## Gotchas

- The driver holds confirmed blocks until a batch commits them; a very
  long-stopped stream resuming far behind the tip catches up through
  the driver. Fetching bodies on executors for backfill is the next
  step (backlog `scalus-executor-fetch`).
- `events` mode — rollbacks as rows instead of a failure — is backlog
  `scalus-events-mode`.
- The Spark test JVMs of this module share okay-spark's settings
  (`sparkTestSettings` in build.sbt: the Scala 2.13 library first for
  Spark's reflection, a forked JDK 25, the `--add-opens` Spark needs).

## Verification

The tests read the RECORDED preprod session (okay-scalus's fixture):
the `outputs` DataFrame equals `CardanoTables`' outputs row for row
(which okay-scalus checks against Koios); SQL over `datum.kind` and
`variant_get` on real inline datums; a streaming query with
`confirmations = 2` reads exactly the first three blocks'
transactions and then WAITS on a quiet chain. The snippet above is
analysed by `TestDocExamplesCardanoSpark`.
