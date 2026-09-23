## cardano-tables: a Cardano block as typed rows, and the guide

`okay.scalus.CardanoTables.of(block)`: the whole explode of one block into
typed rows — `BlockRow`, `TransactionRow` (with the body's exact `cbor`,
whose hash is the id), `InputRow` (`role` Spend/Collateral/Reference and
the ledger's `spent`), `OutputRow` (address, lovelace, datum, reference
script; an invalid transaction's collateral return), `AssetRow`,
`MintRow`, `CertificateRow`, `WithdrawalRow`, `RedeemerRow` — every row
carrying slot, block number, block hash and time. Engine-free by
decision (operator: okay-watch reads these without Spark); `Columns`
gives every row type its tabular shape, Spark will derive from the same
types. Checked against KOIOS on the recorded preprod blocks: transaction
ids, positions and fees, input references (as sets — the body keeps them
sorted), every output's address, lovelace, asset count and datum
presence, and every asset row (fixture `n2n/koios-txs.json`).

Docs: the guide [Reading a blockchain](docs/cardano.md) — the layers,
following the chain, the tables, sum types and recursive values as
columns, the chain-neutral view, verification and stated limits — with
its code run by `TestCardanoGuide` (snippets copied from the test into
the page by script). specs/scalus.md §5 marked superseded by typed rows.
Operator: "продолжай. И напиши документацию".
