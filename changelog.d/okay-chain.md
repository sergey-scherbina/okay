## okay-chain: blockchains and ledgers, read uniformly

A new cross-built module (specs/chain.md), abstractions only — operator
2026-09-23: okay-watch's chain sources stay in okay-watch (whether they
go public is undecided), but this is shaped so it can adopt it.

- CAIP identifiers: `Network` (CAIP-2), `Account` (CAIP-10), `Asset`
  (CAIP-19), each a `Schema` as its string — x402's own network format;
  `Amount` is a `BigInt`, a digit string in JSON. Cardano has no
  registered CAIP namespace (registry checked); CIP-34's
  `cip34:1-764824073` is used and the gap recorded.
- A sans-I/O follower: `Tracker` (push sources: `Forward`/`Backward`/
  `AtTip` in, `Confirmed`/`RolledBack` out; `Depth` or `Finalized`),
  `Poller` (poll sources: `head`/`block(n)`, rewind by the kept ring,
  gaps) and `Follow`, the blocking driver okay-watch's `step()` maps to.
- `Ledger[Tx]`: `movements` as a declared-lossy projection
  (`complete = false` when a source saw part), `utxo` as the lossless
  UTXO view.
- FOUND by `TestWatchShape`, which replays okay-watch's follower cases:
  depth counted off the source's head confirmed a block on a dead fork
  after a reorg. Depth now counts the chain followed, and a finalized
  tip is a `Point`, not a height.

Also: specs/x402.md (x402 v2 on okay-chain + okay-http, facilitator
first), backlog `okay-x402`; `okay-scalus-chain` re-scoped to implement
okay-chain rather than a follower of its own. Docs: module page with
the gated snippets and references (CAIP, x402, Nakamoto, Ouroboros
Praos, Gasper, sans-I/O), typepedia, docs index.
