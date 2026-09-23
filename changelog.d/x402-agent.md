## x402-agent — x402 for an agent: consent before paying (specs/x402.md stage 3b)

- okay-x402: `Consent` — asked about the one chosen requirement, with
  its resource, after `Policy` and before the `Payer`, and told
  (`returned`) when an approved payment was not taken. `Consent.ask`,
  `Consent.budget` (a running total, reserved atomically, given back on
  a refusal or an unsettled payment), `a and b`. The HTTP `Paying`
  takes it.
- okay-x402-mcp: `X402Mcp.Paying` takes a consent too, and is the
  agent's `Tool` handler (`handler`, `interpret`): the same agent
  program against a free tool table and a paid MCP server gives the
  same answer, the price met outside the program.
- Docs: consent in docs/modules/okay-x402.md, the agent in
  docs/modules/okay-x402-mcp.md (snippet run by TestDocExamplesX402Mcp).
