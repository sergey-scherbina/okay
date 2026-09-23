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

**Consent: the decision with the price in hand.** `Policy` says which
requirements are acceptable at all; a `Consent` is asked about the ONE
the client chose, with the resource, before anything is signed — the
place for a person, a model or a budget. `Consent.budget(total,
network, asset)` is a running total over every payment: an approval
reserves its amount atomically, and a payment that was not taken
(refused, declined by the payer, or answered without a successful
`PAYMENT-RESPONSE`) gives it back. `Consent.ask(f)` asks someone;
`budget and ask(f)` needs both, and the second's no returns the first's
reservation. `Paying(http, policy, payer, consent)` takes it; the
default is `Consent.always`.

**Security and settings** (spec stage 4). What stands between a server's
402 and your money, in order:

1. `Policy` — which requirements are acceptable at all: `Policy.upTo(max,
   networks, assets)`, and `Policy.payTo(recipients)`, joined with `and`.
   Without a recipient list a hostile server can direct a payment to any
   address inside the limit.
2. `Consent` — the one chosen requirement, with its resource:
   `Consent.resources(url => …)`, `Consent.ask(…)`, a `Budget`.
3. The `Payer` — and behind it the key: okay-x402 never holds one.
   [okay-x402-evm](okay-x402-evm.md)'s `EvmPayer` signs through a
   `Signer`, which a KMS, an HSM or a wallet service implements.

What the client decided is kept in a `PaymentJournal` —
`PaymentJournal.on(topic)` over an okay-persist topic, durable before the
decision is acted on. `Consent.audit(journal)` writes every price asked,
every payment made (with its transaction) and every one not taken; a
`Budget` is the fold of its own records there, so a restart does not
refill it, and `window` makes it a daily (or hourly) cap. On the server,
`Settled.journaled(journal)` keeps the replay record across restarts.

The settings are a file, with secrets as references (okay-conf):

```json
{ "client": { "maxAmount": "20000", "networks": ["eip155:8453"],
              "assets": ["0x833589fCD6eDb6E08f4c7C32D4f71b54bdA02913"],
              "payTo": ["0x209693Bc6afc0C5328bA36FaF03C514EF312287C"],
              "resources": ["https://api.example.com/"],
              "budget": { "total": "50000", "network": "eip155:8453",
                          "asset": "0x833589fCD6eDb6E08f4c7C32D4f71b54bdA02913",
                          "windowSeconds": 86400 } },
  "facilitator": { "url": "https://facilitator.example", "apiKey": "env:X402_FACILITATOR_KEY" } }
```

`X402Conf.client(conf, journal)` builds the `Policy` and the `Consent`
(audit, resources, budget); `X402Conf.facilitator(conf, http, secrets)`
the facilitator, its key resolved at once — a missing secret is an error
naming the reference, at startup. `HttpFacilitator` asks for its headers
on every request, so a rotating token is read when used.

**The facilitator.** `HttpFacilitator(http, base)` speaks x402's
facilitator API — `POST /verify` and `POST /settle` with
`{paymentPayload, paymentRequirements}`, `GET /supported`. A transport
failure or an unreadable answer is a REFUSAL carrying its reason, never
a payment taken as good. Verifying `exact` locally needs keccak-256,
secp256k1 recovery and EIP-712: [okay-x402-evm](okay-x402-evm.md) does it,
as `LocalFacilitator` in front of the remote one.

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
| `Paying(http, policy, payer, consent)`, `Policy`, `Payer` | the paying client |
| `Consent`, `Consent.always` / `ask` / `budget` / `resources` / `audit`, `and` | the decision before paying |
| `Budget(id, total, network, asset, window, journal, clock)`, `remaining` | a running total folded from its journal |
| `PaymentJournal`, `PaymentJournal.inMemory` / `on(topic)`, `PaymentEvent` | every payment decision, as records |
| `Policy.payTo`, `Policy.and` | recipients the client will pay |
| `Settled.journaled(journal)` | the server's replay record, durable |
| `X402Conf`, `ClientConf`, `BudgetConf`, `FacilitatorConf` | the settings file |
| `Charge.admit`, `settle`, `release` | the payment rules without a transport — `Gate` and okay-x402-mcp both run them |

## Verification

`TestX402Wire` decodes the four headers printed in x402's
`transports-v2/http.md` into these types and encodes them back to the
same JSON. `TestGate` runs the whole flow in memory — unpaid, paid,
replayed, over the policy, invalid signature, failing route, failed
settlement — and `HttpFacilitator` against a facilitator service built
from the protocol's own request and response shapes. The snippet above
is run by `TestDocExamplesX402`. All of it on JVM and JS.

## Gotchas

- `Settled.inMemory()` and `PaymentJournal.inMemory()` forget on
  restart — use `Settled.journaled` and `PaymentJournal.on(topic)` over
  a durable store. Several processes sharing one budget need one journal
  they all write and a lock around the reservation; one process per
  budget is what is tested.
- A budget reservation is released when the answer carries no
  SUCCESSFUL settlement. A server that took the money and then lost the
  answer makes the budget think nothing was paid; the facilitator's
  record, and settlement tracking (spec stage 2b), are the check.
- The MCP transport is [okay-x402-mcp](okay-x402-mcp.md); A2A is not here yet.

References: x402 Protocol Specification v2 and its HTTP transport
(coinbase/x402); EIP-3009 *Transfer With Authorization*; EIP-712
*Typed structured data hashing and signing*; RFC 9110 §15.5.3
(402 Payment Required); CAIP-2 / CAIP-19.
