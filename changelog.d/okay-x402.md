## okay-x402: paying for an HTTP resource with a chain payment

specs/x402.md stages 0–1 (operator chose x402 next). New cross-built
module (JVM, JS) on okay-http and okay-chain:

- the x402 v2 objects (`PaymentRequirements`, `PaymentRequired`,
  `PaymentPayload`, `SettlementResponse`, `VerifyResponse`, …) with a
  codec over okay's `Json` — hand-written because x402 carries open JSON
  and omits absent optionals — and the base64 headers; networks are
  okay-chain CAIP-2 `Network`s, amounts `BigInt` digit strings;
- `Gate(price, facilitator)(route)`: 402 with `PAYMENT-REQUIRED`; then
  match, an atomic claim against replay, verify, run the route, and
  settle only a 2xx answer (a failed settlement withholds it);
- `HttpFacilitator` (`/verify`, `/settle`, `/supported`; an unreadable
  answer is a refusal with its reason);
- `Paying(http, policy, payer)`: a client that pays what `Policy.upTo`
  allows, keys behind `Payer`.

Tests on JVM and JS: the spec's four printed headers decode and re-encode
to the same JSON; the whole flow in memory, seven ways; the facilitator
client against a service built from the protocol's own shapes. Docs:
module page with the run snippet, typepedia, index; spec decisions and
results.
