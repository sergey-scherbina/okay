# okay-chain: blockchains and ledgers, read uniformly

## Overview

The operator's ask (2026-09-23): abstractions over Cardano and other
chains, so they are worked with uniformly. The task now is READING
transactions; building and submitting them may follow, and x402 may be
needed. Two constraints from the operator:

- **abstractions only.** okay-watch's chain sources (EVM, Cardano/Koios,
  Bitcoin, Tron, Solana) are NOT moved here. Whether that code becomes
  public is undecided.
- **okay-watch must be able to use them.** Its `Chain`/`Follower` must
  be expressible over this module with no loss, so adopting it is a
  swap and not a rewrite.

What okay-watch taught (its specs/chain-source.md, 5 chains, real
fixtures), and what this spec keeps or changes:

- `Follower` (depth, parent continuity, `Rewound`) is right and general.
  It moves here in spirit, re-derived rather than copied.
- The chain is POLLED by height there (`head`, `block(n)`). A Cardano
  node PUSHES (chain-sync `RollForward`/`RollBackward`). Both shapes are
  first class here.
- Everything there is normalised to one `Transfer(from, to, asset,
  amount)`. That is right for AML and too lossy for a library: a UTXO
  transaction has several senders (okay-watch elects the largest input
  and keeps the rest in meta). Here the normalised view is a PROJECTION
  beside the native transaction, never a replacement.
- One `depth` for every chain is wrong for chains with explicit
  finality (Ethereum's `finalized` tag, Solana's commitment, Tron's
  solidified blocks). Here finality is a per-chain policy.

## Design

### 1. Identifiers: the CAIP family

x402 names networks in CAIP-2, and so does every multi-chain wallet
standard; adopting it means an x402 `network` field IS a `Network`.

- `Network(namespace, reference)` — CAIP-2: `eip155:8453` (Base),
  `solana:5eykt4UsFv8P8NJdTREpY1vzqKqZKvdp`, `bip122:000000000019d6689c085ae165831e93`
  (Bitcoin: the genesis hash prefix). Cardano: CIP-34's
  `{networkId}-{networkMagic}` under the `cip34` namespace (`cip34:1-764824073`);
  the CAIP namespaces registry has NOT adopted it (checked 2026-09-23,
  ChainAgnostic/namespaces at 463bae5: no `cardano`, no `cip34`); the
  form is CIP-34's own, which CAIP-2's syntax admits.
- `Account(network, address)` — CAIP-10. `address` is the chain's
  canonical rendering (bech32, EIP-55 hex, base58).
- `Asset(network, namespace, reference)` — CAIP-19: `eip155:8453/erc20:0x8335…2913`,
  the native coin as `slip44:<coin type>`.
- `BlockId`, `TxId` — opaque over the chain's canonical string
  rendering (hex, base58). Bytes are a per-chain view, not the
  identity: okay-watch's hashes are strings today, and Solana's are
  base58, so a byte identity would force a conversion at every
  adoption edge for nothing.
- `Amount` — `BigInt` in ATOMIC units (lovelace, wei, satoshi). EVM
  amounts are uint256; `schema-bigint` exists for exactly this, and it
  puts an amount in JSON as a digit string — which is x402's own
  `"amount": "10000"`.

### 2. The chain as a sequence: `Point`, `BlockRef`, `Finality`

```scala
final case class Point(height: Long, id: BlockId)
final case class BlockRef(point: Point, parent: BlockId, time: Option[Long])  // epoch millis

enum Finality:
  case Depth(blocks: Int)       // probabilistic: Bitcoin, Cardano
  case Finalized                // the chain says so: Ethereum `finalized`, Solana, Tron
```

A tip is `Tip(point, finalized: Option[Point])`: the block the chain
itself declares final, when it declares one — a point, not a height
(Decisions, 2026-09-23).

### 3. Two source shapes, one follower

The follower is a PURE state machine (the sans-I/O shape: it performs
no I/O, it consumes observations and emits events and requests), so it
is the same code under okay-watch's blocking calls, under a Spark
driver, and under an okay stream — and every rollback case is a
deterministic test with no network.

```scala
enum Observed[B]:
  case Forward(block: B)        // the next block on the chain the source follows
  case Backward(to: Point)      // the source's chain switched; everything after `to` is void
  case AtTip(tip: Tip)          // progress: nothing new, here is where the chain is

enum Event[B]:
  case Confirmed(block: B)      // final under the Finality policy
  case RolledBack(to: Point, from: Point)  // blocks after `to`, up to `from`, were Confirmed and are void
```

- `Tracker[B]` consumes `Observed[B]`, holds unconfirmed blocks, and
  emits `Confirmed` once a block is final under the policy. A
  `Backward` above the confirmed frontier is absorbed silently; one AT
  or below it is `RolledBack` — and a caller that cannot express
  rollback turns that into a failure (the `confirmed` mode of
  specs/scalus.md). Parent continuity is checked on every `Forward`: a
  block whose parent is not the previous block is a failure naming
  both, never a silent hole.
- A PUSH source (Cardano chain-sync, a websocket) produces `Observed`
  directly.
- A POLL source (okay-watch: `head`, `block(n)`) goes through a
  `Poller`, also pure: it asks for `Head`/`Block(n)` (requests), turns
  the answers into `Forward`, and detects a switched chain the way
  okay-watch's `rewind` does — walk back the kept ring until a block's
  id still matches — which it emits as `Backward`. okay-watch's
  `Follower` is `Poller` + `Tracker`.
- A gap (a slot the chain skipped: okay-watch's empty-hash Solana
  block) is not a block; `Poller` steps over it without linking.

The generic parameter `B` is the NATIVE block; what the machine needs
from it is a typeclass:

```scala
trait BlockOf[B]:
  type Tx
  def ref(b: B): BlockRef
  def txs(b: B): Vector[Tx]
```

### 4. The ledger view: a projection, lossy by declaration

```scala
trait Ledger[Tx]:
  def id(tx: Tx): TxId
  def fee(tx: Tx): Option[Amount]
  def movements(tx: Tx): Vector[Movement]
  def utxo(tx: Tx): Option[UtxoView] = None          // UTXO chains only: lossless

final case class Movement(asset: Asset, amount: Amount,
                          from: Option[Account], to: Option[Account],
                          complete: Boolean)
final case class OutRef(tx: TxId, index: Int)
final case class Output(ref: OutRef, owner: Account, holds: Vector[(Asset, Amount)])
final case class UtxoView(spent: Vector[OutRef], created: Vector[Output])
```

- `from`/`to` are `Option`: a mint has no `from`, a burn no `to`.
- `complete = false` is how a source SAYS its projection is partial —
  an EVM source reading logs cannot see a contract's internal ETH
  transfers without a trace API. A caller learns it from the value,
  not from a wrong total.
- On a UTXO chain `from` is an attribution (which input paid); the
  truth is `utxo`. The spec names this rather than hiding it.

### 5. What okay-watch's adoption looks like (not done here)

- its `Block(number, hash, parent, at, transfers)` gets a `BlockOf`
  instance with `Tx = Transfer`; its `Transfer` gets a `Ledger` that is
  one `Movement` each (`complete` true: it only ever claims what it
  reads);
- its `Chain` (`head`, `block(n)`) drives the `Poller`; its `Follower`
  becomes `Poller` + `Tracker`, with `Progress.Rewound(from, to)` read
  off `Event.RolledBack`;
- its `Rail.Chain(network: String)` can carry a CAIP-2 `Network`
  string unchanged.

The test that proves it is here, not there: `TestWatchShape` builds a
fake poll source with okay-watch's exact shapes (number, hash, parent
strings; an empty hash for a gap; a switched chain after a reorg) and
checks the events are the ones okay-watch's `TestFollower` expects.

### 6. Writing, later — the places reserved

Not built; named so the read side does not close the door:

- `Submit`: signed bytes in, `TxId` out; status tracked by the SAME
  `Tracker` over one transaction (pending → included → confirmed, or
  rolled back out). Uniform across chains.
- Building is chain-specific except for a plain payment ("pay amount
  of asset to account"): UTXO coin selection and account nonces/gas
  have no common shape worth pretending to. Cardano's builder is
  scalus's `TxBuilder`.
- Keys and signing stay outside this module.

### 7. x402

specs/x402.md. It depends on this module for `Network`, `Account`,
`Asset`, `Amount`, and on `Tracker` to confirm a settlement.

## Stages

- **Stage 0 — the module and the identifiers**
  - [x] okay-chain, cross-built (JVM/JS/Native), on okay-codec so every
        type has a `Schema`
  - [x] `Network`/`Account`/`Asset` parse and render CAIP-2/10/19;
        round-trip laws; the x402 examples parse
  - [x] the Cardano CAIP-2 form checked against the namespaces registry
        and recorded in Decisions
- **Stage 1 — the follower**
  - [x] `Tracker`: Depth and Finalized policies; shallow `Backward`
        absorbed, deep `Backward` → `RolledBack`; parent break fails;
        each a scripted test
  - [x] `Poller`: requests/answers, rewind by the kept ring, gaps
  - [x] `TestWatchShape`: okay-watch's shapes and expectations
- **Stage 2 — the ledger view**
  - [x] `Ledger`, `Movement`, `UtxoView`; a toy UTXO chain and a toy
        account chain in tests, both projecting
- **Stage 3 — adopters** (other lanes): okay-scalus implements
  `BlockOf`/`Ledger` for scalus's `Block`/`Transaction`; okay-watch
  adopts when the operator decides.

## Decisions

- 2026-09-23 — **abstractions only; okay-watch's sources stay there.**
  Operator: whether that code goes public is a separate, undecided
  question. The module is shaped so okay-watch can adopt it.
- 2026-09-23 — **CAIP identifiers**, because x402 already speaks
  CAIP-2 and amounts as digit strings — both line up with this module
  and with `schema-bigint` without a translation layer.
- 2026-09-23 — **the follower is sans-I/O.** One machine for poll and
  push sources, blocking and async drivers, and deterministic tests of
  every rollback case.
- 2026-09-23 — **ids are canonical strings, not bytes** (§1).
- 2026-09-23 — **Depth counts the chain followed, not the source's
  head; `Finalized` confirms a POINT.** Found by `TestWatchShape`
  replaying okay-watch's "depth hides a shallow reorg": the first cut
  confirmed up to `head - depth`, and after a reorg the head is on the
  new fork while the pending blocks are the old one's — the dead `a8`
  was Confirmed and RolledBack in one step, where okay-watch (which
  fetches only what it is about to emit) said nothing. The same hole
  exists for a finalized HEIGHT: the block held there may be on the
  dead fork. So `Tip.finalized` is a `Point` and only that block, by
  id, is confirmed.
- 2026-09-23 — **two stated differences from okay-watch's follower**:
  a gap is not a block (okay-watch confirms an empty one), and a reorg
  below every kept block is `Broken` (okay-watch restarted from its
  first height). An adopter that wants either keeps it in its adapter.

## Results

- okay-chain landed with stages 0–2: 24 tests on JVM, JS and Native —
  `TestIds` (CAIP-2/10/19 from the registry's and x402's examples, the
  JSON wire shape), `TestTracker` (Depth, Finalized, a head or a
  finalized point on an unseen fork confirming nothing, shallow vs deep
  rollback, parent break, checkpoint resume), `TestWatchShape` (all six
  okay-watch `TestFollower` cases through `Follow`, plus the ring
  bound), `TestLedger` (a toy UTXO and a toy account ledger),
  `TestDocExamplesChain` (the docs' snippets verbatim).
- follow-keeps-progress (2026-09-25, found from okay-watch): a
  `PollSource` that throws in the middle of `Follow.step` — a 429 after
  the retries, a timeout — used to take the step's events with it:
  blocks the Tracker had already CONFIRMED (its frontier moved), which
  no caller then received. A step now ends with what it confirmed when
  the source fails after progress; the poller asks the same height again
  next step, and a failure that gains nothing is thrown. `TestWatchShape`
  "a failure after progress keeps what was confirmed".
