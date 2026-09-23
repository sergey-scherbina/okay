# okay-x402-evm

> Verifying x402's `exact` scheme on EVM OFFLINE: the EIP-3009 payment's
> EIP-712 signature recovered to its payer with keccak-256 and
> secp256k1, and its parameters checked against the requirements — so
> a server does not take a facilitator's word for a signature.

Depends on: `okay-x402`, BouncyCastle (keccak, secp256k1). JVM — a
satellite so okay-x402 itself stays dependency-free and cross-built, as
okay-security-argon2 is for okay-security. Spec:
[specs/x402.md](../../specs/x402.md) stage 2a.

## Guide

**Paying.** `EvmPayer(signer)` builds x402's `exact` payment as the
reference client does — `validAfter = now − 600`, `validBefore = now +
maxTimeoutSeconds`, a random 32-byte nonce, `to = payTo` — and has it
signed by an `AuthorizationSigner`: `address` plus a signature over the
authorization in its EIP-712 domain. A `Signer` — `address` plus
`sign(digest)`, nothing else — is one; a service that signs the typed
data itself (`CdpSigner`, [okay-x402-cdp](okay-x402-cdp.md)) is the other. That
is the whole custody seam: a KMS, an HSM or a wallet service implements
`Signer` and the key never enters the process. `Signer.local(key)` holds
a key in memory and is for development only. The payer declines what it
cannot sign honestly (another scheme, a non-EVM network, no EIP-712
domain); WHETHER to pay is decided before it is asked, by `Policy` and
`Consent` ([okay-x402](okay-x402.md)).

```scala
val conf = X402Conf.load(path).fold(e => sys.error(e), identity)
val journal = PaymentJournal.on(store.topic("x402-payments", 1, okay.persist.Policy.default))
val (policy, consent) = X402Conf.client(conf.client.get, journal)
val client = Paying(http, policy, EvmPayer(signer), consent)   // signer: your KMS-backed Signer
```

**Verifying.**

```scala
val facilitator = LocalFacilitator(remoteFacilitator)   // local checks first, then the remote's balance and simulation
val paid = Gate(_ => Some(PaymentRequired(ResourceInfo("/report"), Vector(price))), facilitator)(routes)
```

`LocalFacilitator` verifies locally FIRST — a refusal never reaches the
remote facilitator — and then, by default, asks the remote one too, for
what cannot be checked offline; it settles through the remote one,
because broadcasting a transaction needs a node.

**What is checked** (`ExactEvm.verify`), with the reference
implementation's `invalidReason` codes so a refusal reads the same
everywhere:

| check | reason when it fails |
|---|---|
| the scheme is `exact`, the network `eip155:<chainId>` | `unsupported_scheme`, `network_mismatch` |
| `extra.name` / `extra.version` give the EIP-712 domain | `missing_eip712_domain` |
| the signature RECOVERS to `authorization.from` (low-s, v 27/28) | `invalid_exact_evm_payload_signature` |
| `authorization.to` is `payTo` | `invalid_exact_evm_payload_recipient_mismatch` |
| `validBefore >= now + 6`, `validAfter <= now` | `…_authorization_valid_before` / `…_valid_after` |
| `value >= amount` | `invalid_exact_evm_payload_authorization_value` |

**What is not**, because it needs the chain: the payer's balance, a
simulated `transferWithAuthorization`, and smart-wallet signatures
(EIP-1271 / EIP-6492). Those stay the remote facilitator's.

**Why low-s only.** USDC's FiatToken — the EIP-3009 contract x402 settles
through — rejects a signature whose `s` is in the upper half of the
curve order. Accepting one here would verify a payment that cannot
settle.

## The primitives

| name | what |
|---|---|
| `Evm.keccak` | keccak-256 (Ethereum's, not SHA3-256) |
| `Evm.recover(digest, signature)` | the signer's address from `r ‖ s ‖ v` |
| `Evm.addressOf(privateKey)`, `Evm.sign` | for tests and tools |
| `Eip712.domainSeparator`, `.digest` | EIP-712 for `TransferWithAuthorization` |
| `ExactEvm.verify(payload, requirements, now)` | the offline checks above |
| `LocalFacilitator(remote, alsoRemote, clock)` | local first, remote for the rest |

## Verification

Against oracles this code did not produce: the published keccak vectors;
private key 1 → `0x7E5F4552091A69125d5DfCb7b8C2659029395Bdf`; EIP-712's
own `Ether Mail` domain separator; and the x402 specification's example
payment, whose real signature recovers to its `from`
(`0x857b06519E91e3A54538791bDbb0E22373e36b66`). A tampered amount no
longer recovers; a high-s twin of a valid signature is refused.

References: EIP-3009 *Transfer With Authorization*; EIP-712 *Typed
structured data hashing and signing*; EIP-2 (low-s signatures); SEC 2
(secp256k1); x402 `exact` scheme on EVM (coinbase/x402,
`specs/schemes/exact/scheme_exact_evm.md`).
