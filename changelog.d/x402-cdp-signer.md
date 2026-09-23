## x402-cdp-signer — x402 payments signed by a Coinbase CDP Server Wallet

- okay-x402-evm: `AuthorizationSigner` (an address and a signature over
  the EIP-3009 authorization in its EIP-712 domain) is what `EvmPayer`
  takes; `Signer` is one, through the digest; `Eip712.Domain`.
- new okay-x402-cdp (JVM): `CdpSigner` sends the typed data to CDP's
  `sign/typed-data`, so CDP's wallet policies see what they sign; the
  Bearer and `X-Wallet-Auth` JWTs as coinbase/cdp-sdk builds them
  (Ed25519 or EC API keys, `reqHash` over the sorted body); the answer is
  recovered and must be the account's. `CdpConf` with secrets as
  references (okay-conf).
- Tested against a fake CDP that verifies both tokens; a `Live` test runs
  against the real one when credentials are in the environment.
  Docs: docs/modules/okay-x402-cdp.md.
