# okay-x402-mcp

> x402 over MCP: a tool that costs money answers the JSON-RPC error 402
> with its price, a client pays what its policy allows and asks again,
> and the answer carries the settlement — the HTTP transport's objects,
> in the places JSON-RPC already has.

Depends on: `okay-x402`, `okay-mcp`. JVM and JS. A satellite, so
okay-x402 does not pull the agent runtime okay-mcp brings and okay-mcp
knows nothing of payments. Spec: [specs/x402.md](../../specs/x402.md)
stage 3.

## Guide

```scala
// the SERVER: which tools cost what, and a facilitator to verify and settle
val price = X402Mcp.byTool(name => if name == "report" then Vector(requirements) else Vector.empty)
Server.run(serverLink, serving, X402Mcp.gate(price, facilitator))

// the CLIENT: a Session that pays what the policy allows, keys behind `payer`
val paying = X402Mcp.Paying(session, Policy.upTo(BigInt(50000), Set(Network.base), Set(usdcOnBase)), payer)
val report = paying.call(ToolCall("1", "report", Rpc.obj()))
// "the report" — and requestRpc's answer has result._meta["x402/payment-response"]
```

**Where everything travels** (x402 `specs/transports-v2/mcp.md`). HTTP
has headers; JSON-RPC has an error object with a `data` member and MCP
has `_meta` on every request and result, so:

| | HTTP | MCP |
|---|---|---|
| the price | `402` + `PAYMENT-REQUIRED` | error `code: 402`, `PaymentRequired` in `error.data` |
| the payment | `PAYMENT-SIGNATURE` | `params._meta["x402/payment"]` |
| the receipt | `PAYMENT-RESPONSE` | `result._meta["x402/payment-response"]` |
| settlement failed | `402` with the failed `PAYMENT-RESPONSE` | error 402, the failed settlement in `data` beside `accepts` |
| a malformed payment | `402` | `-32602` invalid params |

**The server.** `X402Mcp.gate(price, facilitator, settled)` is a
`Server.Around[Async]` — okay-mcp's hook around every request, which
either answers at once or passes the request on and then sees the
reply. The payment rules are okay-x402's `Charge`, the same code the
HTTP `Gate` runs: the payment must match an accepted requirement, is
CLAIMED before it is verified (so one signature cannot pay for two
calls racing), and is settled only when the tool DELIVERED — a tool
that answers with `isError` is not charged, and its payment is released
for the retry. `price` sees the whole request, so `initialize` or
`resources/read` can be priced too; `X402Mcp.byTool` is the common
case, `tools/call` by tool name, with the resource
`mcp://tool/<name>` the transport's examples use.

**The client.** `X402Mcp.Paying(session, policy, payer)` wraps a
`Session`: `requestRpc` sees a 402 refusal, reads the `PaymentRequired`
from its data, picks the first requirement the `Policy` allows, asks the
`Payer` to sign, and repeats the request ONCE with the payment in
`_meta`. A refused payment is not paid again, and when the policy
allows nothing or the payer declines, the 402 is the answer. `call` is
`Session.call` with payment: the tool's text, or `error: <code> <why>`.

**For an agent.** `paying.handler` (or `paying.interpret` where
nothing may block) is the agent's `Tool` handler, so an agent program
does not change by one character when its tools cost money: the price
is met outside the program, by the policy, the consent and the payer.
A payment refused — over budget, a person's no — reaches the model as
the tool's answer, `error: 402 …`, which it can read and work around.

```scala
// the AGENT: at most 0.05 USDC over the whole conversation, asked of nobody
val budget = Consent.budget(BigInt(50000), Network.base, usdcOnBase)
val tools: Handler[Tool] = X402Mcp.Paying(session, Policy.upTo(BigInt(10000), Set(Network.base), Set(usdcOnBase)), payer, budget).handler
// ... Agent.converse(task, specs) with `tools` in scope; budget.remaining is what is left
```

**The hook, for other uses.** `Server.Around` is plain okay-mcp and
knows nothing of money: `apply(request)` returns `Left(reply)` to answer
now, or `Right(Pass(request, leave))` to go on, where `leave` turns the
protocol's reply into what is sent. It is one closure rather than a
before/after pair because what the first half learns — here, the
admitted payment — is exactly what the second half needs.

## API reference

| name | what |
|---|---|
| `X402Mcp.gate(price, facilitator, settled)` | the server side, a `Server.Around[Async]` |
| `X402Mcp.byTool(accepts, description)` | price `tools/call` by name |
| `X402Mcp.Paying(session, policy, payer, consent)` | `requestRpc`, `call`, and `handler` / `interpret` — the paying client, and the agent's `Tool` handler |
| `X402Mcp.paymentRequired(id, required, settlement)` | the 402 refusal as the transport prints it |
| `X402Mcp.receipt(result)`, `meta`, `withMeta` | reading and writing `_meta` |
| `Server.Around`, `Around.Pass`, `Around.none` (okay-mcp) | the hook around every request |
| `Charge.admit`, `settle`, `release` (okay-x402) | the transport-neutral payment rules |

## Verification

`TestX402McpWire` reads the transport spec's three examples — the paid
`tools/call`, the settled answer, the 402 — and writes the 402 back
byte for byte. `TestX402McpGate` runs every road of the gate as a pure
stage (unpaid, paid, replayed, forged, malformed, a failing tool, a
failed settlement). `TestX402McpSession` puts a paying client and a
gated server on an in-memory wire (JVM), and `TestX402McpAgent` runs
the same agent program against a free tool table and a paid server.
The snippets above are run by `TestDocExamplesX402Mcp`.

## Gotchas

- `Paying` pays ONCE per request: a server that refuses the payment
  gets no second signature, and the 402 is returned.
- The receipt is in `result._meta`, which `Session.call` and `Paying.call`
  do not surface — use `requestRpc` and `X402Mcp.receipt` to see it.
- `Settled.inMemory()` forgets on restart, as for HTTP.

References: x402 Protocol Specification v2 and its MCP transport
(coinbase/x402, `specs/transports-v2/mcp.md`); Model Context Protocol
specification, 2025-06-18, *Basic — `_meta`*; JSON-RPC 2.0
Specification §5.1 (the error object); RFC 9110 §15.5.3 (402 Payment
Required).
