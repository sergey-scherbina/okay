# okay-x402-cdp

> x402 payments signed by a Coinbase CDP Server Wallet: the payment's
> EIP-712 typed data goes to CDP, the key stays in CDP's enclave, and
> the signature that comes back is checked before it is used.

Depends on: `okay-x402-evm`, `okay-conf`. JVM. Spec:
[specs/x402.md](../../specs/x402.md) stage 4b.

## Guide

```scala
val conf = CdpConf(account, apiKeyId, Secret("env:CDP_API_KEY_SECRET"), Secret("env:CDP_WALLET_SECRET"))
val signer = CdpSigner.fromConf(conf, http, Secrets.env).fold(e => sys.error(e), identity)
val client = Paying(http, policy, EvmPayer(signer), consent)   // pays 402s from the CDP wallet
```

**What CDP gets to see.** `EvmPayer` asks an `AuthorizationSigner` to
sign an EIP-3009 authorization in its EIP-712 domain. A `Signer`
(okay-x402-evm) would reduce that to a 32-byte digest; `CdpSigner` sends
the TYPED DATA itself to `POST /platform/v2/evm/accounts/{address}/
sign/typed-data`. CDP therefore sees the token, the recipient and the
amount it signs for, and a policy set on the wallet in the CDP portal —
a spend limit, an allowed token contract — refuses there, before any
signature exists. It is the second fence behind `Policy` and `Consent`,
and the one this process cannot talk its way past.

**Authentication**, as Coinbase's own SDK does it (coinbase/cdp-sdk,
java `com.coinbase.cdp.auth`): a Bearer JWT from the API key — EdDSA for
an Ed25519 key (the 64-byte base64 secret), ES256 for an EC PEM key —
naming the request as `POST api.cdp.coinbase.com/…` and expiring in 120
s; and an `X-Wallet-Auth` JWT from the Wallet Secret (ES256) whose
`reqHash` is the SHA-256 of the request body with its keys sorted. Both
are made per request.

**Trust, but recover.** The returned signature is recovered against the
digest this process computes, and must be the account's; a signature
from any other key fails the payment naming both addresses. CDP refusing
— a policy, an expired key — fails it with CDP's status and body.

**Settings.** `CdpConf(account, apiKeyId, apiKeySecret, walletSecret)`
is an okay-conf config: the two secrets are REFERENCES
(`env:…`, `file:…`), resolved once by `CdpSigner.fromConf`, so a missing
one fails at startup naming itself.

## API reference

| name | what |
|---|---|
| `CdpSigner(http, address, credentials, base, clock)` | an `AuthorizationSigner` over CDP's typed-data endpoint |
| `CdpSigner.fromConf(conf, http, secrets)` | from the settings, secrets resolved now |
| `CdpSigner.typedData(domain, authorization)` | the request body |
| `CdpConf`, `CdpCredentials` | settings (references) and resolved keys |
| `CdpAuth.apiKey`, `walletKey`, `bearer`, `wallet`, `sorted` | the tokens, as the SDK builds them |

## Verification

`TestCdpSigner` runs against a fake CDP that checks what CDP must check —
both tokens verified under the registered public keys (Ed25519 and EC
API keys), their claims and `uris`, the `reqHash` against the body it
received — and signs the typed data it was sent; the payment
`EvmPayer` builds through `CdpSigner` then verifies under
`ExactEvm.verify`. A signature from another key and a 403 are refused.
`TestLiveCdp` does the same against the real CDP when `CDP_ACCOUNT`,
`CDP_API_KEY_ID`, `CDP_API_KEY_SECRET` and `CDP_WALLET_SECRET` are set
(`Live`; a signature moves no money until a facilitator settles it).
The snippet above is run by `TestDocExamplesCdp`.

## Gotchas

- The Server Wallet's account must hold the token on the network the
  server asks for; CDP signs regardless, and settlement is where an
  empty wallet fails.
- The CDP prose docs name the bearer claim `uri`; the SDK sends `uris`
  (a list). This module follows the SDK.

References: Coinbase Developer Platform, Server Wallet v2 API — "Sign
EIP-712 typed data" and "API Authentication"; coinbase/cdp-sdk (java);
EIP-712; EIP-3009; RFC 7515 (JWS), RFC 8037 (EdDSA in JOSE).
