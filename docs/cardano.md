# Reading a blockchain: Cardano, from a relay to tables

This page follows one road end to end: connect to a Cardano relay,
follow the chain to confirmed blocks, turn each block into typed
TABLES, and hand those tables to whatever reads them — your own code
(okay-watch reads them this way), an engine-free column layout for
DuckDB, Delta or Parquet, or Spark. No local node, no API key, and no
Spark unless you want Spark.

The design record is [specs/chain.md](../specs/chain.md) (the
chain-neutral abstractions) and [specs/scalus.md](../specs/scalus.md)
(Cardano, the tables, the column encodings), with every refuted
alternative and every finding.

## The layers

| layer | module | what it is |
|---|---|---|
| chains in general | `okay-chain` | CAIP identifiers, the follower (`Tracker`, `Poller`), the `Ledger` projection — no chain's code |
| Cardano | `okay-scalus` | the node-to-node client, `CardanoFollower`, `CardanoTables`, `CardanoLedger`, `Schema`s for scalus's ledger model |
| tables without an engine | `okay-codec` `Columns` | any `Schema` as column types and rows of plain values |
| Spark | `okay-spark` `SparkSchema`, `okay-scalus-spark` | `Columns` translated into a DataFrame; `format("cardano")` |

Only the last row depends on Spark. Everything above it runs in an
ordinary JVM process.

## 1. Follow the chain

```scala
CardanoFollower.open(Wire.tcp("preprod-node.play.dev.cardano.org", 3001), CardanoNetwork.preprod) match
  case Left(why) => println(s"could not follow: $why")
  case Right(follower) =>
    try
      while true do
        follower.step() match
          case Left(why) => throw IllegalStateException(why)
          case Right(events) => events.foreach {
            case Event.Confirmed(block) =>
              val t = CardanoTables.of(block)
              println(s"block ${block.header.blockNo}: ${t.transactions.size} txs, ${t.outputs.size} outputs")
            case Event.RolledBack(to, _) =>
              println(s"forget everything after block ${to.height}")
          }
    finally follower.close()
```

`CardanoFollower.open` connects, performs the node-to-node handshake,
and starts at the relay's current TIP — reading in real time is the
point, and a replay from genesis would begin with the Byron era, which
scalus does not model. To resume where you stopped, pass a
`Checkpoint(slot, hash, blockNo)`; the first block must extend it.

`step()` blocks until the relay has something to say. It answers
`Confirmed` for a block that is FINAL under the policy (by default
`Finality.Depth(15)`: fifteen blocks on top of it) and `RolledBack` when
blocks already confirmed are void. Cardano's security parameter is
k = 2160 blocks — final by protocol, about twelve hours; real-time
reading wants tens of blocks, and the choice is yours. A rollback
shallower than the policy is absorbed and never shown; one deeper is
said, never papered over; a block that does not extend the chain
followed is an error naming both, never a silent hole.

Depth is counted over blocks linked to the chain the follower holds,
not off the relay's reported head. The first version counted from the
head, and after a reorg the head sits on a fork the follower has not
seen yet — a dead block was confirmed and rolled back in one step. The
test that found it replays okay-watch's own follower cases.

## 2. A block as tables

```scala
val t = CardanoTables.of(block)

val withTokens = t.outputs.filter(_.assetCount > 0).map(o => s"${o.txHash.take(8)}#${o.index}")
val toEach     = t.outputs.groupMapReduce(_.address)(_.lovelace)(_ + _)
val minted     = t.mints.map(m => (m.policy, m.name, m.quantity))

// an input is a reference: resolve it against outputs you have seen
val seen     = t.outputs.map(o => (o.txHash, o.index) -> o).toMap
val resolved = t.inputs.flatMap(i => seen.get((i.spentTx, i.spentIndex)))
```

`CardanoTables.of(block)` is the whole explode of one block:

| table | one row per | notes |
|---|---|---|
| `blocks` | block | height, header hash, previous hash, time, era, counts |
| `transactions` | transaction | position, id, validity, fee, counts, `cbor` = the body's exact bytes |
| `inputs` | input | `role` (Spend, Collateral, Reference), the output it spends, and `spent` — the LEDGER's answer |
| `outputs` | created output | index, address (bech32), lovelace, datum, reference script |
| `assets` | native asset in an output | policy, name, quantity |
| `mints` | minted or burned asset | negative quantity is a burn |
| `certificates` | certificate | scalus's `Certificate`, a sum |
| `withdrawals` | reward withdrawal | reward account, lovelace |
| `redeemers` | redeemer | tag, index, `Data`, execution units |

Every row carries `slot`, `blockNo`, `blockHash` and `time`, so every
table joins back to its block and can be windowed by event time.

Two things a block cannot say, and the tables do not guess:

- an INPUT is a reference to an earlier output — its address and value
  live in that output. Resolving it is a join of `inputs` with the
  `outputs` you have seen, on `(spentTx, spentIndex)`, as the snippet
  does in memory; in a warehouse it is the same join.
- a transaction whose scripts failed spends its COLLATERAL instead of
  its inputs and creates only its collateral return. `spent` and
  `collateralReturn` say which happened; the lists alone would not.

## 3. Sum types and recursive values in a table

```scala
val (fields, toRow) = Columns.table[OutputRow]
val names = fields.map(_.name)
// Vector(slot, blockNo, blockHash, time, txHash, index, address, lovelace,
//        assetCount, datum, scriptRef, collateralReturn)
val rows = t.outputs.map(toRow)
```

A table has no sum type and no recursion; Cardano's ledger is mostly
sums (a credential is a key hash OR a script hash; a certificate is one
of eighteen kinds) and some of it is recursive (a Plutus datum, a
native script, transaction metadata). `Columns` decides their shape
once, for every engine:

- an enumeration whose cases carry nothing (`RedeemerTag`, `InputRole`)
  is a string of the case NAME — an ordinal would be renumbered under
  every old file by the next case a hard fork adds;
- a sum whose cases carry data is `kind` plus one nullable struct per
  case that has fields, exactly one of them set:
  `datum.kind = 'Inline'`, `datum.Inline.data`. A case with no fields
  has no struct (Parquet refuses an empty one). A new case is one more
  nullable column, which old files read as null;
- a recursive value is `struct<cbor, json>`: the value's CBOR, and the
  same value as JSON for engines that query into it (Spark's VARIANT,
  DuckDB's JSON). Recursion is found by the schema, not by a list of
  type names, so a new recursive type in a future scalus is handled
  without a change here;
- a big integer is `decimal(38,0)`.

This is how Spark's own Avro connector reads a union and its Protobuf
connector a `oneof`, with the discriminator those lack.

## 4. The chain-neutral view

```scala
val ledger = CardanoLedger(Network.cardanoPreprod)
val moved = block.transactions.flatMap(ledger.movements)
val ada   = moved.filter(_.asset == ledger.ada).map(_.amount).sum
// `from` is None on Cardano: who paid needs the UTXO set (inputs ⋈ outputs)
```

`okay-chain`'s `Ledger` is the view every chain can give: an id, a
fee, and MOVEMENTS of value — declared lossy where it is lossy. On a
UTXO chain there is no single sender, and on Cardano a block does not
even name the paying address (inputs are references), so `from` is
`None` here rather than a guess; the exact picture is `utxo(tx)`. The
same abstraction is what an EVM or Bitcoin source implements; okay-watch
can adopt it without moving its sources.

## 5. Into Spark

With `okay-scalus-spark` on the classpath the same tables are a Spark
DataSource, batch or streaming:

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

Nothing new is decided on the way: the rows are `CardanoTables`', the
shape `Columns`', and a `Json` column is a Spark VARIANT. An offset is a
confirmed block, so a re-run micro-batch yields the same rows; a
rollback deeper than `confirmations` fails the query instead of
producing rows that were never final. When latency matters more than
finality, `mode = events` shows every block as it arrives and a
rollback as a row (`event = 'rolled_back'`, `rollbackTo.blockNo`), backed
by a local journal so a re-run batch still reads what it read. Details and options:
[okay-scalus-spark](modules/okay-scalus-spark.md).

## How this is verified

- A preprod session was recorded by an independent Python probe (cbor2
  and hashlib, in the test resources). The client replays it byte for
  byte and sends exactly the probe's requests.
- Block hashes, transaction ids and fees, every output's address,
  lovelace, assets and datum presence, and every input reference are
  checked against what Koios reports for the same transactions.
- `TestLive` follows a real preprod relay to its next block.
- The code on this page is compiled and run by `TestCardanoGuide`; the
  Spark snippet is analysed by `TestDocExamplesCardanoSpark`, and the
  DataSource is checked row for row against `CardanoTables` on the
  recorded session, streaming included.

## Limits, stated

- Byron-era blocks are not modelled: start at a Shelley-or-later point.
- scalus holds native-asset quantities as `Long`; the protocol allows
  uint64, and a quantity past 2⁶³ fails in scalus's decoder.
- The exact on-chain bytes are kept for transaction bodies (their hash
  is the transaction id). A datum's exact bytes are not yet cut out of
  the body, so a datum hash cannot be recomputed from the table.
- Chain-sync and block-fetch number eras differently (Conway is 6 in a
  header, 7 in a block); both are read where they belong.
- There is no mempool: the tables hold what is in blocks.

## References

- D. Coutts, N. Davies, M. Fontaine, K. Knutsson, A. Santos,
  M. Szamotulski, A. Vieth, *Ouroboros Network Specification*
  (IntersectMBO/ouroboros-network, `docs/network-spec`), and the
  mini-protocols' CDDL in the same repository.
- B. David, P. Gaži, A. Kiayias, A. Russell, *Ouroboros Praos: An
  adaptively-secure, semi-synchronous proof-of-stake blockchain*,
  EUROCRYPT 2018, doi:10.1007/978-3-319-78375-8_3 — why depth is
  finality on Cardano.
- The Cardano ledger's Conway CDDL (IntersectMBO/cardano-ledger) — the
  transaction, output and certificate shapes scalus models.
- CAIP-2 / CAIP-10 / CAIP-19, ChainAgnostic/CAIPs — the identifiers.
- C. Bormann, P. Hoffman, *CBOR*, RFC 8949 (2020), doi:10.17487/RFC8949.
- Apache Avro Specification, "Unions"; Protocol Buffers Language
  Guide, "Oneof" — the tabular reading of a sum this page follows.
