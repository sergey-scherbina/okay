## x402-hardening — keys, limits, memory, settings (specs/x402.md stage 4)

- okay-x402-evm: `Signer` (`address`, `sign(digest)`) — the one seam a
  KMS, HSM or wallet implements; `Signer.local` for development only.
  `EvmPayer(signer)` builds the `exact` payment as the reference client
  does (window now−600 .. now+maxTimeoutSeconds, random 32-byte nonce)
  and declines what it cannot sign honestly; its payload verifies under
  `ExactEvm.verify`.
- okay-x402: `Policy.payTo` and `Policy.and`; `Consent.resources`,
  `Consent.audit`, and `Consent.paid` (the settlement of every payment
  taken). `PaymentJournal` (`inMemory`, `on(topic)` over okay-persist):
  every decision as a record. `Budget` is the fold of its own records —
  a restart does not refill it — with an optional window (daily cap).
  `Settled.journaled` for the server. `HttpFacilitator` takes headers
  per request.
- okay-x402: `X402Conf` — policy, recipients, resources, budget and the
  facilitator (URL + `Secret` reference) as a settings file (okay-conf);
  a missing key fails at startup naming its reference.
- Docs: "Security and settings" in docs/modules/okay-x402.md, paying in
  docs/modules/okay-x402-evm.md (snippet run by TestEvmPayer).
