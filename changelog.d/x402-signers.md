## x402-signers — Circle, Turnkey and Web3Signer signers for x402

- new okay-x402-signers (JVM, no provider SDKs): `CircleSigner` (Circle
  developer-controlled wallets; a fresh RSA-OAEP entity-secret ciphertext
  per request, the entity public key fetched once), `TurnkeySigner`
  (`sign_raw_payload` with `PAYLOAD_ENCODING_EIP712`, the body stamped
  with a P-256 API key as Turnkey's `api-key-stamper` does; an activity
  not completed is a refusal), `Web3Signer` (`eth_signTypedData` over
  JSON-RPC — Web3Signer or geth's Clef). Settings through okay-conf.
- okay-x402-evm: `Eip712.typedData` and `Eip712.checked` (v normalised,
  the signature recovered to the account), shared with `CdpSigner`.
- Tested against fakes that verify what each provider verifies; `Live`
  tests for the real providers with credentials from the environment.
  Docs: docs/modules/okay-x402-signers.md.
