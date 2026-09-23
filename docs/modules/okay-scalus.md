# okay-scalus

> The Cardano chain as okay-chain events, read straight from a relay
> over Ouroboros node-to-node — no local node, no API key — with
> scalus's ledger model for the blocks it brings.

Depends on: `okay-chain`, `okay-codec`, and `org.scalus:scalus-cardano-ledger`
1.2.0 for the ledger MODEL and its CBOR codecs only. JVM. Specs:
[specs/scalus.md](../../specs/scalus.md), [specs/chain.md](../../specs/chain.md).

## Guide

**What comes from scalus and what does not.** scalus models the
Conway ledger — `Block`, `Transaction`, outputs, certificates, `Data` —
and decodes it from CBOR while keeping the original bytes (`KeepRaw`),
which is what makes a transaction's id (Blake2b-256 of its body bytes)
exact. It does not, in 1.2.0, stream whole blocks from a real chain,
so the transport is written here: the node-to-node mux, the handshake,
chain-sync (headers, rollbacks), block-fetch (bodies) and keep-alive,
from the protocol's own CDDL.

**A follower in three lines.** `CardanoFollower` is a session, a
chain-sync source and okay-chain's `Tracker`; `step()` blocks until
the relay has something and answers what a consumer may act on:

```scala
CardanoFollower.open(Wire.tcp("preprod-node.play.dev.cardano.org", 3001),
                     CardanoNetwork.preprod, from = None, finality = Finality.Depth(15)) match
  case Left(why) => println(why)
  case Right(f) =>
    try
      for _ <- 1 to 3 do
        f.step().foreach(_.foreach {
          case Event.Confirmed(b) =>
            println(s"${b.header.blockNo} ${b.header.hash} ${b.transactions.size} txs")
          case Event.RolledBack(to, _) =>
            println(s"everything after block ${to.height} is void")
        })
    finally f.close()
```

With no checkpoint it starts at the relay's TIP — reading in real
time is the point, and a replay from genesis would begin with Byron,
which scalus does not model. With a `Checkpoint(slot, hash, blockNo)`
it resumes there; the first block must extend it.

**How a block arrives.** Chain-sync announces headers; the source
reads them until the relay says "wait" (or a batch fills) and fetches
all their bodies with ONE range request — so catching up is one round
trip per batch, and at the tip one per block. A block's identity is
Blake2b-256 of its header bytes; scalus's `Block.hash` returns the body
hash and is never used for identity. `CardanoBlock` keeps the body's
bytes and decodes them only when `transactions` is asked for.

**The ledger view.** `CardanoLedger(network)` is okay-chain's `Ledger`
for scalus's `Transaction`: id, fee, the lossless `utxo` view (inputs
by reference, outputs with owner and value), and one `Movement` per
asset per output. `from` is `None`: an input is a reference to an
earlier output, so who paid is known only with the UTXO set — in
Spark, `outputs` joined to `inputs`. ADA is `slip44:1815`; a native
token is `token:<policy>.<name>` in hex, okay's convention, because
CAIP-19 registers no Cardano asset namespace.

**A block as tables.** `CardanoTables.of(block)` explodes one block
into typed rows — blocks, transactions, inputs, outputs, assets,
mints, certificates, withdrawals, redeemers — each carrying `slot`,
`blockNo`, `blockHash` and `time`. The explode is written once and
every consumer reads the same rows: your own code, `Columns` for any
engine, Spark. Inputs are references (resolve them by joining
`outputs`); `spent` and `collateralReturn` say what the LEDGER did with
a transaction whose scripts failed. Walkthrough with every table:
[Reading a blockchain](../cardano.md).

**The model as a `Schema`.** `import okay.scalus.CardanoSchemas.given`
gives every scalus ledger type an okay `Schema`, so a `Transaction` or a
`Block` folds into JSON, CBOR, a validator and engine-free `Columns` —
the same tables with Spark or without it. What scalus made opaque is
mapped by hand: hashes and `ByteString` to bytes, `KeepRaw`/`Sized` to
their value, the tagged sets and maps to vectors (a tagged map carries
its VALUES: scalus derives each key from its value, so nothing can
disagree), `Coin` to a `Long`, an address to its bech32 text, and a
`MultiAsset` to flat `(policy, name, quantity)` rows. `Data`, `Timelock`
and `Metadatum` are recursive; `Columns` finds that itself and reads
them as `cbor` + json. Every real preprod transaction in the fixture
round-trips through JSON and through CBOR to the same bytes.

**Tested against the real thing.** A preprod session recorded by an
independent Python probe (cbor2 + hashlib, in the test resources) is
replayed byte for byte: the client sends exactly the probe's requests,
and the header hashes, transaction ids and fees are the ones Koios
reports for the same blocks. `TestLive` (tag `Live`) follows a real
relay to its next block.

References: D. Coutts, N. Davies, M. Fontaine, K. Knutsson, A. Santos,
M. Szamotulski, A. Vieth, *Ouroboros Network Specification*
(IntersectMBO/ouroboros-network, `docs/network-spec`: the mux segment,
the 12 288-byte node-to-node SDU); the mini-protocols' CDDL
(`cardano-diffusion/protocols/cddl/specs`, read at a3d8017); B. David, P. Gaži, A. Kiayias, A. Russell,
*Ouroboros Praos*, EUROCRYPT 2018, doi:10.1007/978-3-319-78375-8_3;
RFC 8949 (CBOR), tag 24 "encoded CBOR data item".

## API reference

| name | shape | note |
|---|---|---|
| `CardanoFollower` | `open(wire, net, from, finality)`, `headers(...)`, `step()`, `close()` | session + source + `Tracker`; `headers` confirms headers only (`ChainFollower[Header]`) |
| `ChainSyncSource` | `open(): Either[String, N2N.Tip]`, `next(): Either[String, Vector[Observed[CardanoBlock]]]` | the push source alone; `HeaderSync[A]` is the same source completing a header batch into `A`; pipelined, up to 100 requests in flight behind the tip |
| `BlockFetch.range` | `(session, net, headers) => Either[String, Vector[CardanoBlock]]` | one range request, bodies paired with their headers |
| `Session`, `Wire` | `Session.open(wire, magic)`; `Wire.tcp(host, port)` | single-threaded; a keep-alive every 20 s, busy or idle, one in flight |
| `CardanoNetwork` | `mainnet` / `preprod` / `preview` | CAIP-2 id, magic, slot config |
| `Checkpoint` | `(slot, hash, blockNo)` | where to resume |
| `Header` | `era, blockNo, slot, hash, prev` | read from the header bytes |
| `CardanoBlock` | `header`, `bytes`, `time`, `block`, `transactions` | `BlockOf` instance |
| `CardanoLedger` | `Ledger[scalus.cardano.ledger.Transaction]` | |
| `CardanoTables` | `of(block): Tables`; `BlockRow`, `TransactionRow`, `InputRow`, `OutputRow`, `AssetRow`, `MintRow`, `CertificateRow`, `WithdrawalRow`, `RedeemerRow` | a block as typed rows |
| `CardanoSchemas` | `import CardanoSchemas.given` | a `Schema` for every scalus ledger type |
| `N2N` | segments, `Demux`, the four protocols' messages | pure |

## Gotchas

- Two era numberings: chain-sync's header says Conway is 6 (the
  hard-fork combinator's index), block-fetch's `[era, block]` says 7
  (Byron's boundary blocks count separately). Each is read where it
  belongs.
- Byron headers are refused, not skipped: start at a Shelley-or-later
  point.
- scalus's `MultiAsset` holds quantities as `Long`; the protocol allows
  uint64. A quantity past 2⁶³ fails in scalus's decoder.
- The relay opens every chain-sync with a rollback to the
  intersection; with a checkpoint it is a `Backward` to the tracker's
  own frontier, which the tracker absorbs.
