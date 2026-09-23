# okay-x402-signers

> Three more places for the key that pays x402: a Circle
> developer-controlled wallet, Turnkey, or a self-hosted Web3Signer (or
> geth's Clef). Each signs the payment's EIP-712 typed data where the key
> lives; each answer is checked before it is used.

Depends on: `okay-x402-evm`, `okay-conf`. JVM. No provider SDKs: okay-http
and the JDK's RSA and P-256. Spec: [specs/x402.md](../../specs/x402.md)
stage 4c. Coinbase CDP is its own module, [okay-x402-cdp](okay-x402-cdp.md).

## Guide

```scala
val circle = Signers.circle(CircleConf(address, walletId, Secret("env:CIRCLE_API_KEY"), Secret("env:CIRCLE_ENTITY_SECRET")), http, Secrets.env)
val turnkey = Signers.turnkey(TurnkeyConf(address, organizationId, apiPublicKey, Secret("env:TURNKEY_API_PRIVATE_KEY")), http, Secrets.env)
val local = Signers.web3signer(Web3SignerConf(address, "http://web3signer.internal:9000"), http)
val client = circle.map(signer => Paying(http, policy, EvmPayer(signer), consent))
```

**Which one.**

| | where the key is | what it takes | when |
|---|---|---|---|
| `CircleSigner` | Circle's developer-controlled wallet | an API key, the wallet id, the 32-byte entity secret | the money is USDC — Circle issues it |
| `TurnkeySigner` | Turnkey's enclave | the organization id, a P-256 API key pair | agent wallets with Turnkey's policy engine |
| `Web3Signer` | your own signer (a keystore, Vault, a cloud KMS behind it) | its URL | no third party at all |

**Circle** signs at `POST /v1/w3s/developer/sign/typedData`. Every request
carries an `entitySecretCiphertext` made for it alone — Circle refuses a
reused one — which is the entity secret under RSA-OAEP (SHA-256, with
MGF1 SHA-256 too; the JDK's shorthand name would use SHA-1 for MGF1, so
the parameters are spelled out) with Circle's entity public key, fetched
once from `/v1/w3s/config/entity/publicKey`.

**Turnkey** signs a raw payload with `PAYLOAD_ENCODING_EIP712`: the typed
data itself, so Turnkey's policies see what they approve. The request is
STAMPED — `X-Stamp`, a P-256 signature over the exact body with the API
key — as Turnkey's own `api-key-stamper` does it. An activity that did
not complete (a policy asking for more approvals) fails the payment
naming its status.

**Web3Signer** is `eth_signTypedData` over JSON-RPC, which geth's Clef
speaks too. It has no authentication of its own: keep it on a private
network or behind mTLS, which is the `Http` you hand it.

**Every answer is checked**: the signature is recovered against the
digest this process computes and must be the configured address's, or
the payment fails naming both (`Eip712.checked`, shared with
`CdpSigner`). A refusal carries the provider's status and body.

## API reference

| name | what |
|---|---|
| `CircleSigner(http, address, walletId, apiKey, entitySecret, base)` | Circle developer-controlled wallets |
| `TurnkeySigner(http, address, organizationId, apiPublicKey, apiPrivateKey, base, clock)` | Turnkey |
| `Web3Signer(http, address, url)` | Web3Signer, Clef — any `eth_signTypedData` |
| `CircleConf`, `TurnkeyConf`, `Web3SignerConf`; `Signers.circle` / `turnkey` / `web3signer` | settings, secrets as references, resolved at once |
| `Eip712.typedData`, `Eip712.checked` (okay-x402-evm) | the typed data sent, the answer checked |

## Verification

`TestSigners` runs each against a fake that checks what the provider
must: Circle's ciphertext DECRYPTS to the entity secret under the private
half and is never reused (and the public key is fetched once); Turnkey's
stamp verifies under the registered API key over the body it received,
the activity type, encoding and hash function are Turnkey's; Web3Signer's
call is `eth_signTypedData` with the typed data object. Each fake signs
what it was sent with a local key, and the payment verifies under
`ExactEvm.verify`; another key's signature, an incomplete activity and a
JSON-RPC error are refused. `TestLiveSigners` does it against the real
providers when their credentials are in the environment (`Live`). The
snippet above is run by `TestDocExamplesSigners`.

References: Circle Wallets API — "Sign typed data", "Entity secret
management" and circlefin/w3s-entity-secret-sample-code; Turnkey API —
`sign_raw_payload` and tkhq/sdk (`api-key-stamper`, `viem`); Consensys
Web3Signer JSON-RPC API; EIP-712; EIP-3009; RFC 8017 (RSA-OAEP).
