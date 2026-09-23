## scalus-mainnet-live: okay-scalus and `format("cardano")` on mainnet

Two `Live`-tagged tests against a mainnet relay (backbone.cardano.iog.io),
out of the default gate: `TestLiveMainnet` follows three blocks from the
tip through `CardanoTables` (13977824..26: 24 transactions, 318 assets,
mints, certificates, withdrawals, redeemers — the mix preprod rarely
has), every table folded into `Columns`, every transaction's `cbor`
hashing to its id; `TestLiveMainnetSpark` reads two confirmed blocks'
outputs through the DataSource with SQL over `datum.kind`.

Found on the way: at `confirmations = 0` the confirmed read met a real
mainnet tip fork within four minutes (block 13977847 taken back) and
failed the query as designed; the test now reads at depth 2, and the
okay-scalus-spark page says why the default is 15. This is also the
trigger scalus-flink was waiting for (the Spark source run on mainnet).
