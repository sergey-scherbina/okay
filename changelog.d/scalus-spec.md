## scalus-spec: the Cardano chain as a live DataFrame — spec, before code

specs/scalus.md: two new modules, okay-scalus (the chain as rollback-aware
events over our own Ouroboros node-to-node transport, scalus-cardano-ledger
1.2.0 taken for its ledger MODEL and CBOR codecs only) and okay-scalus-spark
(a DataSource V2, batch + micro-batch). The enum question answered per
shape: pure enums as case-name strings, sums as tagged sparse structs
(`kind` + one nullable struct per case with fields), recursive `Data` and
`Timelock` as `cbor: binary` + `json: variant`, found by `Schema.fold`'s
own `ref` back edge rather than a list of names. Found while reading scalus:
1.2.0 streams no whole blocks from a real chain, and `Block.hash` is the
body hash. Queued: schema-bigint, okay-scalus-chain, okay-scalus-spark;
backlog: scalus-events-mode, scalus-flink.

Follow-up (scalus-spec-bytes, 2026-09-23): §4.3 now says WHERE the
exact bytes are kept — only where a chain hash is defined over them
(datums, redeemers, scripts, metadata, tx bodies), each with its hash
precomputed at ingest — and that the variant loses integers beyond 38
digits, which the bytes keep.
