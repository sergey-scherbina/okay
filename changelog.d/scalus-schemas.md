## scalus-schemas: scalus's ledger model as okay `Schema`s

`okay.scalus.CardanoSchemas` (import `.given`): a `Schema` for every
scalus ledger type reachable from `Block` and `Transaction`, so they
fold into JSON, CBOR, a validator and engine-free `Columns` — the
tables work without Spark (operator: okay-watch must not need it).

Mapped by hand, the rest derives: `ByteString`/`Hash` → bytes;
`KeepRaw`/`Sized` → the value; `IndexedSeq`, `Set`, `SortedSet`,
scalus's prelude `List`, `Map`, `SortedMap` → vectors; the tagged maps →
their VALUES (scalus derives each key from its value with `KeyOf`, so the
wire cannot disagree with the keys; the witness set's `KeyOf` instances
live in its companion and have to be imported); `TaggedOrderedStrictSet`
→ a vector whose empty case is `empty` and whose duplicates are a decode
error (scalus's `from` throws on both in Conway); `Coin`/`Slot`/`Word64`
→ `Long`; `NonNegativeInterval` (a plain class, no Mirror) → a ratio;
addresses → their bech32 text; `MultiAsset` → flat
`(policy, name, quantity)` rows. `Data`, `Timelock`, `Metadatum` are
recursive and `Columns` collapses them to `cbor` + json by itself.
Tested on the recorded preprod blocks: every transaction round-trips
JSON and CBOR to the same bytes; a block is one `Columns` row.
