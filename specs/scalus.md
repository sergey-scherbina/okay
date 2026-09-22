# okay-scalus + okay-scalus-spark: the Cardano chain as a live DataFrame

## Overview

The operator's ask (2026-09-22): read the Cardano blockchain in real
time into Spark — as a DATAFRAME, not an RDD of opaque objects — and
later into Flink or anything else. The hard part, named up front: how
a DataFrame represents the ledger's ENUMS (sum types), because Spark
has no sum type and the ledger is mostly sums.

Two modules, operator's split:

- **okay-scalus** — the chain as a stream of typed, rollback-aware
  events. Engine-agnostic: no Spark, no Flink. Owns the transport
  (Ouroboros node-to-node), the follower (confirmation depth, parent
  continuity, rollbacks) and the ledger model's `Schema` instances.
- **okay-scalus-spark** — a Spark DataSource V2 (`format("cardano")`),
  batch and micro-batch streaming, whose rows are the ledger model
  encoded by one generic ADT→Catalyst derivation.

What we take from scalus and what we do not (operator: "take the CBOR
data model; anything else only where it makes sense"):

| from scalus 1.2.0 | used? | why |
|---|---|---|
| `scalus.cardano.ledger.*` — Block, Transaction, TransactionOutput, Certificate, GovAction, Value, Data, … | **yes** | the Conway-era ledger model, complete and maintained |
| their borer CBOR codecs, `KeepRaw[T]` (decoded value + the original bytes) | **yes** | decoding a block is the part not worth re-deriving; `KeepRaw` gives the lossless column for free |
| Bech32, Base58, hash types, `CardanoInfo`/`SlotConfig` | yes, where needed | small, correct, already there |
| `BlockchainStreamProvider`, `BlockfrostChainFollower`, `SubscriptionHub` | **no** | in 1.2.0 the ONLY implementation is `StreamingEmulator`, which does not even declare `SubscriptionKind.Block`; the Blockfrost follower is `private[stream]` and address-scoped. Nothing there streams whole blocks from a real chain |
| `StreamEvents.BlockEvent` (`Applied`/`RolledBack`) | shape only | our event type mirrors it so an adapter is one line if scalus ever ships a chain-sync provider |

Scalus is published for Scala 3.3.8 (`scalus_3`,
`scalus-cardano-ledger_3`); okay is on 3.9 and reads 3.3 TASTy. Spark
is the 2.13 artifact already mixed in by okay-spark (`for3Use2_13`).
Scalus pulls cats, upickle, jsoniter, borer, bouncycastle and
cardano-client-lib 0.7.2; eviction against Spark's classpath is
checked in stage 0, not assumed.

## Design

### 1. Transport: Ouroboros node-to-node, written here

A public relay speaks node-to-node (N2N) over TCP: no local node, no
API key, no rate limit, whole blocks as CBOR. Four mini-protocols
over one multiplexer:

- **mux** — 8-byte segment header (timestamp, mode bit + protocol id,
  payload length), segments ≤ 12 288 bytes, one TCP socket;
- **handshake** (0) — version proposal with network magic
  (mainnet 764824073, preprod 1, preview 2);
- **chain-sync** (2) — `FindIntersect(points)`, then `RequestNext` →
  `RollForward(header, tip)` | `RollBackward(point, tip)` |
  `AwaitReply` (we are at the tip: the live part);
- **block-fetch** (3) — `RequestRange(from, to)` → `StartBatch`,
  `Block(bytes)*`, `BatchDone`. Deterministic for any range that has
  not rolled back — this is what makes Spark batches replayable
  (below);
- **keep-alive** (8) — or the relay drops an idle subscriber.

Blocking sockets on virtual threads (the JVM default scheduler is
Loom already). Message CBOR is small and fixed; the block body is
handed to scalus's decoder with its bytes kept.

**Block identity is the HEADER hash**, Blake2b-256 of the header's
CBOR, computed by us from `KeepRaw` bytes. Scalus's `Block.hash`
returns `header.headerBody.blockBodyHash` (Block.scala:50, still so on
master 796a1ef) — the hash of the BODY, which is not what chain-sync
points, explorers or `ChainPoint` mean. Reported upstream; never used
here.

Byron-era blocks (before slot 4 492 800 on mainnet) are out of scope:
scalus models Shelley onwards. A backfill starts at a Shelley+ point.

### 2. The follower: confirmation depth + parent continuity

The okay-watch `Follower` (d485b22 there) already settled the shape,
re-derived here on N2N:

```scala
enum ChainEvent:
  case Applied(block: Block, raw: Array[Byte], point: Point, height: Long)
  case RolledBack(to: Point)

final case class Point(slot: Long, hash: Bytes32)   // header hash
```

- `Follower.live(relay, from: Point, depth: Int)`: emits `Applied`
  for a block only once `depth` blocks sit on top of it. A rollback
  shallower than `depth` is absorbed inside the follower and never
  seen downstream; a rollback DEEPER than `depth` (at or below an
  emitted block) is emitted as `RolledBack` in `events` mode and is a
  hard FAILURE in `confirmed` mode — never silently wrong.
- **parent continuity** is checked on every emitted block (its
  `prevHash` is the previous emitted point's hash). A break is a
  failure with both points in the message. This is the check that
  turns "the relay switched forks under us" into an error instead of a
  table with a hole.
- `depth` has no universal right value: 2160 (the security parameter
  k) is final by protocol, ~12 hours; real-time wants tens of blocks.
  Default 15, stated in the option's doc with what it risks.

okay-scalus exposes this as an okay stream source (okay-stream) and a
plain pull iterator; Spark and Flink consume the iterator.

### 3. The ledger model as `Schema` — the one derivation

`okay.codec.Schema[A]` is already "the reified shape every derivation
— JSON, CBOR, a validator, **a Spark encoder** — is a catamorphism
over" (Schema.scala header). The Spark encoder is that algebra, and
the scalus types get `Schema` instances:

- case classes and enums: `Schema.derived` via `Mirror` — Certificate,
  GovAction, DRep, Credential, DatumOption, Relay, Timelock, …;
- the non-Mirror types, by hand, each an `SIso`: `ByteString` and the
  opaque `Hash[_, _]` family → bytes; `IndexedSeq`/`TaggedSortedSet`/
  `TaggedOrderedStrictSet` → `Vector`; `Map[K, V]`/`MultiAsset` →
  `Vector[(K, V)]` (see §4.5); `KeepRaw[T]` → `T` (the raw bytes go to
  their own column where wanted, not on every node); `Sized[T]` → `T`.

The instances live in okay-scalus, not in okay-scalus-spark: a Flink
or Parquet or JSON consumer derives from the same `Schema`, which is
the point of `Schema`.

**Gap found: `Schema` has no big integer.** Multi-asset quantities are
`uint64` (up to 2⁶⁴−1, beyond `Long`), minted quantities are `int64`
with sign, and `Data.I` is an unbounded `BigInt`. An `SIso` over
`String` would make every consumer see text. The honest fix is an
`SBigInt` case in okay-codec — which touches every algebra (eight of
them) and is its own lane (`schema-bigint`), stage 0 of this work.

### 4. Enums in a DataFrame — the decision

Spark's type system: primitives, `struct`, `array`, `map`, and since
4.0 `variant` (VariantType is in spark-sql-api 4.2.0). No sum, no
recursion. Four encodings, chosen per shape by the derivation, not by
hand per type:

**4.1 Pure enum (no case has fields)** — `RedeemerTag`, `Language`,
`Network`, `Vote`: a `string` column holding the CASE NAME.

```
tag: string        -- 'Spend' | 'Mint' | 'Cert' | 'Reward' | 'Voting' | 'Proposing'
```

Not the ordinal: a hard fork that inserts a case renumbers every
ordinal after it and silently rewrites the meaning of old Parquet
files; a name only ever gains values. Parquet dictionary-encodes a
low-cardinality string, so it costs what an int would.

**4.2 Sum with payloads** — `Credential`, `DatumOption`, `DRep`,
`Certificate`, `GovAction`, `Script`, `TransactionOutput`: a
**tagged sparse struct**, one discriminator plus one nullable struct
per case that HAS fields:

```
credential: struct<
  kind:       string,                        -- 'KeyHash' | 'ScriptHash'
  KeyHash:    struct<hash: binary>,          -- non-null iff kind = 'KeyHash'
  ScriptHash: struct<hash: binary>>

drep: struct<
  kind: string,                              -- 'KeyHash' | 'ScriptHash' | 'AlwaysAbstain' | 'AlwaysNoConfidence'
  KeyHash:    struct<hash: binary>,
  ScriptHash: struct<hash: binary>>          -- the two singleton cases have NO branch field
```

```sql
SELECT cert.StakeDelegation.poolKeyHash, count(*)
FROM certificates WHERE cert.kind = 'StakeDelegation' GROUP BY 1
```

Invariants, each a test: exactly one branch is non-null and it is
the one `kind` names; a case without fields has no branch (Parquet
refuses to write an empty `struct<>`, so a field-less branch would
make the table unwritable, not merely wasteful); `kind` is never null
for a non-null value.

Why this and not the alternatives:

- It is what Spark's OWN converters do with foreign sums: spark-avro
  maps a union to `struct<member0, member1, …>`, spark-protobuf a
  `oneof` to sibling nullable fields. A Spark user has met it; we add
  only the `kind` column those two lack (with Avro's positional
  `member0` one cannot tell which case one is looking at without
  knowing the schema).
- Columnar storage makes it cheap: a null branch is a definition level
  in Parquet, not bytes; `kind` is dictionary-encoded; a query
  touching one case reads only that branch's columns (projection
  pushdown), and `kind = 'X'` is a pushed-down predicate.
- **Schema evolution is additive.** The next era adds a certificate
  case → one more nullable field; Delta/Parquet `mergeSchema` accepts
  it and every old file reads the new column as null. That is the
  property the ordinal lacks and the one this table needs most: it
  outlives hard forks.
- Refuted: ONE flat struct with the union of every case's fields
  (`Certificate` has `credential` in nine cases but `coin` as `Coin`
  in some and `Option[Coin]` in others — names collide on types);
  top-level columns per case (a nested sum inside an `array` has no
  "top level"); JSON string or `map<string,string>` (no types, no
  pushdown, every query parses); a Spark UDT (`UserDefinedType` is a
  private API since 2.0 and opaque to SQL).

**4.3 Recursive types** — `Data` (Plutus datums and redeemers:
`Constr(tag, List[Data]) | Map | List | I | B`), `Timelock`
(`AllOf(scripts: IndexedSeq[Timelock])`, …): a StructType is finite,
so a recursive type cannot be a struct. Encoded as TWO columns:

```
datum: struct<
  cbor: binary,      -- the exact on-chain bytes (KeepRaw), lossless, hashable
  json: variant>     -- the same value as Spark 4 VARIANT, queryable
```

```sql
SELECT variant_get(datum.json, '$.fields[0].int', 'long') AS amount
FROM outputs WHERE datum.json IS NOT NULL
```

- the bytes are the TRUTH: the datum hash is Blake2b-256 of exactly
  these bytes, and a re-encoding (any CBOR library, any `Data`
  round-trip) may choose a different but valid encoding (definite vs
  indefinite lists) and change the hash;
- the variant is the QUERY surface, using the Cardano CIP-0116 /
  cardano-cli "detailed schema" JSON shape
  (`{"constructor": n, "fields": [...]}`, `{"int": n}`,
  `{"bytes": hex}`, `{"list": [...]}`, `{"map": [{"k","v"}...]}`) so a
  path someone copies from an explorer works.

**Where the bytes are kept, and where not** (operator, 2026-09-23:
"keep the bytes where they are needed for exactness"). The rule is
whether something on chain is a hash OVER these exact bytes, or
whether the value cannot be reconstructed from its decoded form:

| value | `cbor` kept | why |
|---|---|---|
| datum (inline and witness) | yes | datum hash = Blake2b-256 of these bytes; `DatumOption.Hash` outputs are joined to witness datums by it |
| redeemer `Data` | yes | covered by `scriptDataHash` together with datums and cost models |
| script (Plutus, native `Timelock`) | yes | script hash = hash of tag + these bytes; it is the policy id and the script address |
| auxiliary data / tx metadata | yes | `auxiliaryDataHash` in the body is over these bytes |
| transaction body | yes | tx hash = Blake2b-256 of the body bytes |
| block header | no column, hashed at ingest | the block hash (§1); `hash` is the column |
| the whole block | only with `raw=true` | everything in it is already one of the above |
| every other derived struct | no | nothing hashes it; the struct is the value |

Beside every kept `cbor`, a `hash: binary` column computed AT INGEST
from those bytes, so a join or a lookup never recomputes it in Spark.
And the variant is not always lossless: Spark's variant decimal holds
38 digits, `Data.I` is unbounded — an integer beyond that is written
into the variant as a string, and the bytes stay the exact form.

Detection is not a list of type names: `Schema.fold` already calls
`Algebra.ref(name)` exactly when a strict algebra meets the back edge
of a recursive type (Schema.scala, `inProgress`). The Spark algebra
answers `ref` with a marker, and a product/sum node whose own name was
referred to from below collapses itself to the binary+variant pair.
So a recursive type nobody listed — a future scalus model change —
still produces a valid DataFrame schema instead of a stack overflow.

Refuted: bounded unrolling (spark-protobuf's
`recursive.fields.max.depth`): a datum nested one level deeper than
the bound is silently truncated, and on-chain datums are
sender-chosen depth.

**4.4 Options, products, lists** — `Option` → nullable; product →
struct; `Vector` → `array`. Nothing to decide.

**4.5 Maps with non-string keys** — `MultiAsset`
(`Map[PolicyId, Map[AssetName, Long]]`), `Withdrawals`, redeemer maps:
`array<struct<key, value>>`, not Spark `map`. A Spark map cannot be
grouped, compared or used as a join key, and the first thing anyone
does with multi-assets is `explode` and group by policy:

```
value: struct<
  lovelace: long,
  assets: array<struct<policy: binary, name: binary, quantity: decimal(20,0)>>>
```

`MultiAsset`'s two nesting levels flatten to one array of triples
(its `Schema` `SIso` does that, so every consumer gets it).

**4.6 Leaves** — hashes and key hashes: `binary` (28/32 bytes, half
of hex, joins byte-exact; `hex()` is built in). Addresses: `binary`
AND a `bech32` string, because people search by the bech32 they
copied. Lovelace (`Coin`): `long` (max supply 4.5×10¹⁶ < 2⁶³).
Asset quantities: `decimal(20,0)` (uint64). Slots, heights, epochs:
`long`. Block time: a `timestamp` computed from the slot by the
network's `SlotConfig`.

### 5. Tables: one derived tree, the rest are SQL

One row per block, derived mechanically from the model (§3–4):

```
blocks: struct<
  slot, height, hash, prev_hash, time, era,
  header: struct<...>,                 -- derived
  txs: array<struct<                   -- body + witnesses + aux joined by index
    index, hash, valid: boolean,       -- invalidTransactions folded in
    body: struct<...>,                 -- derived: inputs, outputs, certificates, mint, ...
    witnesses: struct<...>,
    metadata: struct<cbor, json: variant>>>,
  cbor: binary>                        -- whole block, only when option raw=true
```

The analytic tables are NOT a second encoder: they are views over
`blocks` (`explode`), shipped as SQL in okay-scalus-spark and exposed
as `option("table", ...)`:

| table | grain | key |
|---|---|---|
| `blocks` | block | `hash` |
| `transactions` | tx | `tx_hash` |
| `inputs` | spent input (+ collateral/reference, `role` column) | `(tx_hash, role, index)` |
| `outputs` | created output | `(tx_hash, index)` |
| `mints` | (policy, asset) per tx | `(tx_hash, policy, name)` |
| `certificates` | cert | `(tx_hash, index)` — `cert` is the §4.2 struct |
| `withdrawals`, `redeemers`, `datums`, `votes`, `proposals`, `metadata` | … | … |

Every row carries `slot`, `block_hash`, `block_height`, `time`, so any
table joins back and any table can be windowed by event time.

Inputs are references `(tx_hash, index)`; resolving the spent
output's address/value needs the UTXO set, which a block does not
carry. `outputs` joined to `inputs` IS that resolution, in Spark, and
the spec does not pretend the source does it.

### 6. Spark DataSource V2

```scala
spark.readStream.format("cardano")
  .option("network", "mainnet")               // mainnet | preprod | preview | magic number
  .option("relays", "backbone.cardano.iog.io:3001,...")
  .option("start", "tip" | "slot:hash" | "shelley")
  .option("confirmations", "15")
  .option("mode", "confirmed" | "events")
  .option("table", "outputs")
  .load()
```

- `TableProvider` → `Table` with `SupportsRead`, schema from §4;
  `MicroBatchStream` + `SupportsAdmissionControl`
  (`maxBlocksPerTrigger`, so a backfill does not become one batch of
  six million blocks).
- **Offset** = the last confirmed `(slot, hash, height)` as JSON.
  `latestOffset` asks the driver's follower (§2) for the confirmed
  tip; `planInputPartitions(start, end)` cuts the range into block
  ranges; each partition on an EXECUTOR opens its own N2N connection
  and `RequestRange`s its blocks, decodes with scalus, emits rows.
  Driver: headers only. Executors: bodies, CBOR decode, encoding —
  the expensive part runs in parallel, which is what makes a backfill
  fast.
- **Why a re-run batch returns the same rows**: in `confirmed` mode an
  offset only ever names blocks at depth ≥ `confirmations`, and a
  `RequestRange` over them is deterministic. That is Spark's
  exactly-once contract (replayable source + idempotent sink), met
  without a log.
- `events` mode (rollbacks as rows, `kind: 'applied' | 'rolled_back'`
  plus the rolled-back point): a range that included a now-orphaned
  block cannot be re-fetched, so this mode needs the events written
  durably BEFORE they are offered — the driver appends to a journal
  (okay-persist) in the checkpoint directory and offsets are journal
  sequence numbers. Stage 3, after `confirmed` works.
- Batch: `spark.read.format("cardano").option("from", ...).option("to", ...)`
  — the same partitions, a bounded range.

### 7. Flink, later

The same follower (§2) as a FLIP-27 `Source` (split = block range,
enumerator = the follower), the same `Schema` derivation folded into
Flink's `RowType` instead of Catalyst's. Flink has no variant type;
the recursive fallback there is `cbor` + a JSON string. Not staged
until Spark stage 2 has run on mainnet.

## Stages

- **Stage 0 — groundwork**
  - [x] `schema-bigint`: `SBigInt` in okay-codec, every algebra
        (Json, Cbor, Yaml, Xml, JsonSchema, Form, ToolSpec, Typed,
        Compat) — its own lane, its own spec entry in codecs.md
  - [ ] okay-scalus module in build.sbt (JVM), scalus-cardano-ledger
        1.2.0; `evicted` report read against okay-spark's classpath
  - [ ] a real Conway block's CBOR as a fixture (preprod), decoded by
        scalus in a test; header hash computed from the raw header
        bytes equals the hash the chain names (the `Block.hash` trap
        pinned by a test that would pass on the body hash only by
        accident — it cannot)
- **Stage 1 — okay-scalus: the chain as events**
  - [ ] mux + handshake + chain-sync + block-fetch + keep-alive,
        tested against RECORDED relay sessions (fixture bytes), a
        `Live`-tagged test against a public preprod relay
  - [ ] `Follower`: depth, parent continuity, shallow rollback
        absorbed, deep rollback → `RolledBack`/failure, each a test
        over a scripted chain-sync conversation (fork at depth d−1, d,
        d+1)
  - [ ] `Schema` instances for the ledger model; a round-trip test
        (value → Json → value) over every fixture block's types
- **Stage 2 — okay-scalus-spark: batch + confirmed streaming**
  - [ ] generic `SparkSchema`: `Schema.fold` algebra → `(DataType,
        A => InternalRow value)`, §4.1–4.6, including the `ref`
        collapse (§4.3); lives in okay-spark (generic, not Cardano:
        SparkInterop's own comment reserved the slot), used by
        okay-scalus-spark
  - [ ] invariants of §4.2 as tests; a Parquet write/read of every
        table (the empty-struct trap); schema evolution: a type with
        a case added reads an old file
  - [ ] DataSource V2 batch + `MicroBatchStream`; replay test: the
        same offsets planned twice give identical rows
  - [ ] the §5 views; a docs page with the SQL examples above, each
        run in a gated test
- **Stage 3 — events mode** (journal-backed offsets, rollback rows)
- **Stage 4 — Flink** (§7)

## Decisions

- 2026-09-22 — **transport is ours (N2N), model is scalus's.**
  Operator: take scalus's CBOR data model; do everything else
  ourselves where that is better. Measured before deciding: scalus
  1.2.0 has no whole-block stream on a real chain (only
  `StreamingEmulator`, without `SubscriptionKind.Block`); Blockfrost
  and Koios would cost one request per transaction for a whole block
  and a key/quota — okay-watch's Koios source (d485b22 there) is
  fine for watching addresses, wrong for a full-chain table.
- 2026-09-22 — **enums are tagged sparse structs, pure enums are
  names, recursive types are cbor+variant** (§4), rather than one
  scheme for all: each alternative fails on a named case (§4.2
  "Refuted", §4.3 "Refuted").
- 2026-09-22 — **the Spark encoder is a `Schema.fold` algebra in
  okay-spark**, not a Cardano-specific encoder, and not Spark's
  `ExpressionEncoder` reflection (Scala 2 `TypeTag`-based, sees no
  Scala 3 enum).

- 2026-09-23 — **`cbor` is kept exactly where a hash is defined over
  the bytes** (datums, redeemers, scripts, metadata, tx bodies), with
  the hash precomputed beside it; nowhere else. Operator asked why the
  bytes at all; the answer is the hash, and the operator chose
  exactness over the storage it costs (§4.3 table).

## Results

(none yet)
