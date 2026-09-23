## x402-exact-local: x402's `exact` scheme verified offline

specs/x402.md stage 2a (operator: "Продолжай 402"). New JVM satellite
okay-x402-evm (BouncyCastle), so okay-x402 stays dependency-free:

- `Evm`: keccak-256, secp256k1 public-key RECOVERY (low-s and v 27/28
  only — USDC's FiatToken rejects high-s, so accepting one would verify a
  payment that cannot settle), addresses, deterministic signing for tests;
- `Eip712`: the domain separator and the `TransferWithAuthorization`
  digest EIP-3009 signs;
- `ExactEvm.verify`: signature → `from`, recipient, window (with the
  reference's 6 s margin), amount, domain present, eip155 network — with
  the reference implementation's `invalidReason` codes;
- `LocalFacilitator(remote)`: local first (a refusal never reaches the
  remote), then the remote for balance and simulation; settles remotely.

Verified against oracles this code did not produce: the x402 spec's own
example payment recovers to its payer; published keccak vectors; private
key 1's address; EIP-712's `Ether Mail` domain separator. Stage 2b
(following the settlement on chain) is deferred: it needs an EVM chain
source, which okay does not have while okay-watch's stays unpublished.
Docs: module page, okay-x402 cross-link, typepedia, index.
