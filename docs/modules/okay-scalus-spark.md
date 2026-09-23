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
background thread (headers and bodies, a keep-alive every 20 s busy or
quiet) and keeps each confirmed block's bytes. An offset is the last
confirmed block (`slot`, `hash`, `blockNo`); a partition carries a
range of blocks' bytes, and executors DECODE and explode them — the
expensive part, in parallel. Because an offset only ever names blocks
at depth ≥ `confirmations`, a batch re-run after a failure yields the
same rows (Spark's exactly-once contract with an idempotent sink). A
rollback deeper than `confirmations` FAILS the query: rows that were
never final are not produced.

**Who fetches the bodies (`fetch`).** With `fetch = executor` the driver
follows HEADERS only and each partition carries its range's headers;
the executor opens its own session to the relay and block-fetches the
range — so the relay must be reachable from the executors
(`registered:` wires only exist in the same JVM, i.e. local mode). The
rows are the same: a confirmed range is the same bytes whoever fetches
it. It is NOT the default, because it did not measure faster: a
1 000-block preprod backfill read 86.6 / 164.3 s with the driver
fetching and 175.0 / 71.2 s with executors (alternated, same box), and
following the HEADERS alone took 43.7 s — about one relay round trip
per header, which both modes pay on the driver. That scan, not the body
transfer, is the lever (backlog `scalus-chainsync-pipelining`).

**Rollbacks as rows (`mode = events`).** A confirmed stream never
shows a block that is not final — and waits `confirmations` blocks for
it. When latency matters more, `events` mode shows every block as it
arrives and says so when the chain takes some back:

```scala
val events = spark.readStream.format("cardano")
  .option("relay", "preprod-node.play.dev.cardano.org:3001")
  .option("network", "preprod")
  .option("table", "outputs")
  .option("mode", "events")
  .option("confirmations", "0")          // every block as it arrives
  .option("journal", "/var/lib/cardano-journal")
  .load()
// event = 'applied'     → row is set: an output of a block just applied
// event = 'rolled_back' → rollbackTo is set: delete rows above rollbackTo.blockNo
val applied    = events.where("event = 'applied'").select("seq", "row.txHash", "row.index", "row.lovelace")
val rollbacks  = events.where("event = 'rolled_back'").select("seq", "rollbackTo.blockNo")
```

Every event is appended to a local journal (okay-persist, fsync'd)
BEFORE Spark sees it, and an offset is the journal's sequence number —
because a batch that held a block the chain later orphaned must still
read the same records when it is re-run, and a relay can no longer
serve that block. A restarted query resumes after the newest block
still standing in the journal (the rollbacks replayed). The consumer's
half is one rule: on `rolled_back`, delete every row whose `blockNo` is
above `rollbackTo.blockNo` (a Delta `MERGE`, or a `foreachBatch`).

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
| `fetch` | `driver` (the driver fetches the bodies), `executor` (each partition fetches its range) | `driver` |
| `blocksPerPartition` | blocks per input partition | `10`, or `100` with `fetch = executor` |
| `mode` | `confirmed` (a rollback past `confirmations` fails the query), `events` (rollbacks are rows) | `confirmed` |
| `journal` | `events` only: a local directory for the event journal | the checkpoint location, when local |

## API reference

| name | what |
|---|---|
| `CardanoSource` | the `TableProvider`, short name `cardano` (registered through `META-INF/services`) |
| `CardanoStream` | the `MicroBatchStream` (with admission control: Spark names the offset it resumes at) |
| `CardanoBatch` | the bounded read |
| `CardanoOffset` | `(slot, hash, blockNo)` as JSON |
| `Relays` | `register(name, () => Wire)` — a recorded session, a test double, an embedded transport |

## Gotchas

- Short forks at the TIP are routine on mainnet: a confirmed read at
  `confirmations = 0` met one within four minutes (2026-09-23) and
  failed, as it should. Use the default 15, or `mode = events` when
  latency matters more than finality.

- The driver holds confirmed blocks until a batch commits them; a very
  long-stopped stream resuming far behind the tip catches up through
  the driver. Fetching bodies on executors for backfill is the next
  step (backlog `scalus-executor-fetch`).
- `events` mode needs a LOCAL journal directory (the driver writes it);
  a checkpoint location on HDFS/S3 is refused with that reason — pass
  `journal`.
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
