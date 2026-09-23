## okay-scalus-chain: the Cardano chain from a relay, as okay-chain events

New JVM module okay-scalus (specs/scalus.md stages 0–1, on specs/chain.md):
scalus-cardano-ledger 1.2.0 for the ledger model and its CBOR codecs,
and the Ouroboros node-to-node transport written here from the
protocol's CDDL — mux segments and a demux that reassembles messages
across segments, handshake (v14–16), chain-sync, block-fetch in batches
(headers until "wait", then one range request), keep-alive on the
socket's idle timeout. Single-threaded, so the whole client replays
from a recording.

- `CardanoFollower` = session + `ChainSyncSource` + okay-chain `Tracker`;
  starts at the relay's tip or resumes at a `Checkpoint`.
- `CardanoBlock` has a `BlockOf` instance (height = block number, id =
  Blake2b-256 of the HEADER bytes — never scalus's `Block.hash`, the
  body hash); `CardanoLedger` is okay-chain's `Ledger` for scalus's
  `Transaction` (id, fee, utxo view, movements with `from = None`).
- Verified against the real thing: a preprod session recorded by an
  independent Python probe replays byte for byte; header hashes, tx
  ids and fees match Koios; `TestLive` follows a real relay to its next
  block (28.6 s).
- Findings recorded: two era numberings (chain-sync 6, block-fetch 7
  for Conway); scalus `MultiAsset` quantities are `Long`; slf4j-api 2
  over 1.7 is the one dependency conflict. `Schema` instances for the
  ledger model moved to okay-scalus-spark, their only consumer.

Docs: module page with a compiled snippet and references (Ouroboros
Network Specification, the CDDL, Ouroboros Praos, RFC 8949), typepedia,
docs index. Operator: "Потом продолжай okay-scalus-chain".
