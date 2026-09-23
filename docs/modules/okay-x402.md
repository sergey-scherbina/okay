# okay-x402

> x402: paying for an HTTP resource with a chain payment. A priced
> okay-http route answers `402 Payment Required` with what it accepts; a
> client that can pay repeats the request with a signed authorization;
> the server verifies and settles it through a facilitator and serves
> the resource. Networks, assets and amounts are okay-chain's.

Depends on: `okay-http`, `okay-chain`. JVM and JS. Spec:
[specs/x402.md](../../specs/x402.md); the protocol: x402 v2
(coinbase/x402, `specs/x402-specification-v2.md` and
`specs/transports-v2/http.md`).

## Guide

```scala
// the SERVER: what a route costs, verified and settled by a facilitator
val price = PaymentRequirements("exact", Network.base, BigInt(10000), usdcOnBase, merchant, maxTimeoutSeconds = 60)
val paid = Gate(
  req => Option.when(req.url.endsWith("/report"))(PaymentRequired(ResourceInfo(req.url), Vector(price))),
  facilitator)(routes)
// Server.serve(8080)(paid)                    — JVM: the route, now priced

// the CLIENT: pays what the policy allows, with keys behind `payer`
val http: Http = new Http { def send(r: Request) = paid(r) }   // in production: a real transport
val client = Paying(http, Policy.upTo(BigInt(50000), Set(Network.base), Set(usdcOnBase)), payer)
val report = client.send(Request.get("https://api.example.com/report"))
// Response(200, ..., PAYMENT-RESPONSE: {"success":true,"transaction":"0xtx",...})
```

**The flow.** A request to a priced route without `PAYMENT-SIGNATURE`
gets `402` and a `PAYMENT-REQUIRED` header (base64 JSON: the resource
and a list of accepted requirements — scheme, CAIP-2 network, amount in
atomic units, asset, recipient). The client picks one, has it SIGNED
(for `exact` on EVM: an EIP-3009 `transferWithAuthorization`, signed
with EIP-712) and sends it back as `PAYMENT-SIGNATURE`. The gate then:

1. requires the payment to MATCH an accepted requirement (scheme,
   network, asset, recipient, amount);
2. refuses a payment already used (an atomic claim, so two concurrent
   requests with one signature cannot both pass);
3. asks the facilitator to VERIFY it;
4. runs the route, and SETTLES only a successful (2xx) answer — the
   resource is not delivered unpaid, and a payment is not taken for a
   failure. A failed settlement withholds the answer and returns `402`
   with the failed `PAYMENT-RESPONSE`.

**The client.** `Paying(http, policy, payer)` is an `Http`: on a `402`
it chooses the first accepted requirement the `Policy` allows —
`Policy.upTo(max, networks, assets)`, never "pay whatever is asked" —
asks the `Payer` to sign, and repeats the request once. Keys never
enter okay-x402: the `Payer` is yours (a wallet, a KMS, a hardware
signer). When the policy allows nothing, or the payer declines, the
`402` is the answer.

**The facilitator.** `HttpFacilitator(http, base)` speaks x402's
facilitator API — `POST /verify` and `POST /settle` with
`{paymentPayload, paymentRequirements}`, `GET /supported`. A transport
failure or an unreadable answer is a REFUSAL carrying its reason, never
a payment taken as good. Verifying `exact` locally needs keccak-256,
secp256k1 recovery and EIP-712, which okay does not have yet.

**The wire.** Every object has a reader and a writer over okay's `Json`
(`X402.paymentRequired`, `.paymentPayload`, `.settlement`, …, and
`X402.toJson`); `X402.header`/`unheader` are the base64 headers. An
amount is a digit string, a network a CAIP-2 id; an absent optional
field is OMITTED, as the protocol does.

## API reference

| name | what |
|---|---|
| `PaymentRequirements`, `PaymentRequired`, `PaymentPayload`, `SettlementResponse`, `VerifyResponse`, `ResourceInfo`, `SupportedKind` | the protocol's objects |
| `X402` | readers, `toJson`, `header` / `unheader`, the header names |
| `Gate(price, facilitator, settled)(route)` | a priced okay-http route |
| `Facilitator`, `HttpFacilitator(http, base)` | verify, settle, supported |
| `Settled` | used payments; `Settled.inMemory()` |
| `Paying(http, policy, payer)`, `Policy`, `Payer` | the paying client |

## Verification

`TestX402Wire` decodes the four headers printed in x402's
`transports-v2/http.md` into these types and encodes them back to the
same JSON. `TestGate` runs the whole flow in memory — unpaid, paid,
replayed, over the policy, invalid signature, failing route, failed
settlement — and `HttpFacilitator` against a facilitator service built
from the protocol's own request and response shapes. The snippet above
is run by `TestDocExamplesX402`. All of it on JVM and JS.

## Gotchas

- `Settled.inMemory()` forgets on restart: a multi-instance or
  restartable server should back `Settled` with a shared store.
- The MCP and A2A transports of x402 are not here yet (spec stage 3).

References: x402 Protocol Specification v2 and its HTTP transport
(coinbase/x402); EIP-3009 *Transfer With Authorization*; EIP-712
*Typed structured data hashing and signing*; RFC 9110 §15.5.3
(402 Payment Required); CAIP-2 / CAIP-19.
