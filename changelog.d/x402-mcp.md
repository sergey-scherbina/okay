## x402-mcp — x402 over MCP (specs/x402.md stage 3)

- okay-mcp: `Server.Around[G]`, a hook around every request in
  `serveIn` — answer at once, or pass on with a `leave` that sees the
  reply; `Server.run(link, serving, around)`. `Around.none` is the old
  server exactly.
- okay-x402: `Charge` (admit = match → claim → verify; settle; release),
  the transport-neutral half of `Gate`, which now runs it.
- new okay-x402-mcp (JVM+JS): `X402Mcp.gate` — JSON-RPC 402 with the
  `PaymentRequired` in `error.data`, the payment from
  `params._meta["x402/payment"]`, -32602 for a malformed one, the
  receipt in `result._meta["x402/payment-response"]`, no charge for an
  `isError` answer, a failed settlement as a 402 beside the
  requirements; `X402Mcp.byTool`; `X402Mcp.Paying`, a Session that pays
  once by policy and repeats with `_meta`.
- Golden tests read the transport spec's three examples and write its
  402 back byte for byte. Docs: docs/modules/okay-x402-mcp.md.
