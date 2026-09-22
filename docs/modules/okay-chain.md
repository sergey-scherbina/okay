# okay-chain

> Blockchains and ledgers read uniformly: CAIP identifiers, a follower
> that turns what a source saw into what a consumer may act on, and a
> ledger view that projects any chain's transactions into movements of
> value — beside the native transaction, never instead of it.

Depends on: `okay` (core), `okay-codec` (every type has a `Schema`).
Pure Scala — cross-built for JVM, JS and Native. Abstractions only: no
chain's source lives here (Cardano is `okay-scalus`); spec:
[specs/chain.md](../../specs/chain.md).

## Guide

**Two levels, because chains agree on one and not the other.** Every
chain is a sequence of blocks that name their parent and can be
replaced near the tip — that part is genuinely uniform, and the
follower is written once for all of them. What a transaction DOES is
not uniform: a UTXO transaction (Bitcoin, Cardano) spends outputs and
makes new ones and has no single sender; an account transaction (EVM,
Tron) moves a coin and emits token events; Solana changes balances.
So the uniform `Ledger` view is a projection — `movements(tx)` — and
declares where it loses: `from` on a UTXO chain is an attribution (the
lossless truth is `utxo(tx)`), and `Movement.complete = false` is how a
source says it saw only part of what moved (EVM internal transfers are
invisible without a trace API).

**Identifiers are the CAIP family.** A network is CAIP-2
(`eip155:8453`), an account CAIP-10, an asset CAIP-19 — the format
x402 already speaks, so a payment requirement's `network` IS a
`Network`. An `Amount` is a `BigInt` in atomic units (EVM amounts are
uint256), and on a JSON wire a digit string. Cardano has no registered
CAIP-2 namespace (checked against the ChainAgnostic registry,
2026-09-23); `Network.cardano` is CIP-34's `cip34:1-764824073`.

```scala
val base = Network.parse("eip155:8453")                 // Right(Network("eip155", "8453"))
val ada  = Asset.native(Network.cardano, 1815).toString // "cip34:1-764824073/slip44:1815"
```

**The follower is sans-I/O.** `Tracker` holds the blocks not yet final
and consumes what a source observed — `Forward(block)`,
`Backward(to)`, `AtTip(tip)` — emitting `Confirmed(block)` and
`RolledBack(to, from)`. It does no I/O and has no clock, so one machine
serves a blocking driver, a Spark driver and a stream, and every
rollback case is a deterministic test:

```scala
val events = List(
    Observed.Forward(Blk(0, "a0", "g")),
    Observed.Forward(Blk(1, "a1", "a0")),        // a0 has one block on top: Confirmed
    Observed.Backward(Point(0, BlockId("a0"))),  // a1 was never said: absorbed
    Observed.Forward(Blk(1, "b1", "a0")),
    Observed.Forward(Blk(2, "b2", "b1")))        // b1 has one on top: Confirmed
  .foldLeft[Either[Broken, (Tracker[Blk], Vector[Event[Blk]])]](
    Right((Tracker[Blk](Finality.Depth(1)), Vector.empty))) { (acc, o) =>
      acc.flatMap((t, es) => t.feed(o).map((t2, more) => (t2, es ++ more))) }
  .map(_._2)
// Right(Vector(Confirmed(Blk(0, "a0", "g")), Confirmed(Blk(1, "b1", "a0"))))
```

(`Blk` is any block type with a `BlockOf` instance: where it is, what
it extends.) A rollback to a block not yet confirmed is ABSORBED —
nothing it voids was ever said; one below the frontier is
`RolledBack`; a block that does not extend the chain followed is
`Broken`, never a silent hole.

**Two kinds of source.** A push source (Cardano chain-sync, a
websocket) produces `Observed` itself. A poll source — `head` and
`block(n)`, which is how most RPC APIs look — goes through `Poller`,
also pure: it says what it wants (`Request.Head`, `Request.Block(n)`),
is told the answer (`Found`, `Missing`, `Gap` for a skipped slot), and
recognises a replaced chain by a parent that does not match, walking
back the blocks it kept until one still stands. `Follow` is the
blocking driver over both machines for a `PollSource`.

**Finality is a policy, not a number.** `Depth(n)` is probabilistic
finality — Bitcoin's and Ouroboros's: a block is safe once `n` blocks
sit on it (Cardano's protocol bound is k = 2160). `Finalized` is for
chains that SAY which block is final — Ethereum's Gasper checkpoints,
Solana's commitment, Tron's solidified blocks — and it confirms only
that very block, by id.

**The finding that shaped `Depth`.** The first version counted depth
off the source's head. After a reorg the head is on the NEW fork
while the follower's pending blocks are still the old one's, so a dead
block was confirmed and rolled back in the same step. Depth now counts
blocks linked to the chain followed, and a finalized tip carries the
final block's id, not just its height (`TestWatchShape`, which replays
okay-watch's follower cases, caught it).

References: CAIP-2 *Blockchain ID Specification*, CAIP-10 *Account ID
Specification*, CAIP-19 *Asset Type and Asset ID Specification*,
ChainAgnostic/CAIPs; x402 Protocol Specification v2 (§11.1 network
identifiers); S. Nakamoto, *Bitcoin: A Peer-to-Peer Electronic Cash
System* (2008), §11; B. David, P. Gaži, A. Kiayias, A. Russell,
*Ouroboros Praos*, EUROCRYPT 2018, doi:10.1007/978-3-319-78375-8_3;
V. Buterin et al., *Combining GHOST and Casper* (Gasper, 2020),
arXiv:2003.03052; B. Cannon et al., *Network protocols, sans I/O*
(sans-io.readthedocs.io).

## API reference

| name | shape | note |
|---|---|---|
| `Network` / `Account` / `Asset` | CAIP-2 / 10 / 19, `parse` + `toString` | a `Schema` as the CAIP string |
| `BlockId`, `TxId` | opaque over the chain's canonical string | hex, base58 — as the chain renders it |
| `Amount` | `BigInt` | atomic units; a digit string in JSON |
| `Point`, `BlockRef`, `Tip` | height + id; + parent, time; + finalized point | |
| `Finality` | `Depth(n)` \| `Finalized` | |
| `BlockOf[B]` | `ref(b)`, `txs(b)`, `type Tx` | what the follower needs of a native block |
| `Observed[B]` | `Forward` \| `Backward(to)` \| `AtTip(tip)` | what a source saw |
| `Event[B]` | `Confirmed(b)` \| `RolledBack(to, from)` | what a consumer may act on |
| `Tracker[B]` | `feed(o): Either[Broken, (Tracker[B], Vector[Event[B]])]` | pure; `from` resumes a checkpoint |
| `Poller[B]` | `want`, `onHead`, `onBlock` | pure; `keep` bounds a recognisable reorg |
| `PollSource[B]`, `Follow[B]` | `head`, `block(n)`; `step()` | the blocking driver |
| `Ledger[Tx]` | `id`, `fee`, `movements`, `utxo` | the projection |
| `Movement`, `UtxoView`, `OutRef`, `Output` | | |

## Gotchas

- A gap (a skipped slot) is not a block: it is never `Confirmed`.
- A reorg deeper than the `keep` blocks a `Poller` holds is `Broken`,
  not a restart from the beginning.
- `Finalized` needs the source to report the final block's id
  (`Tip.finalized: Option[Point]`); a height alone is not enough.
