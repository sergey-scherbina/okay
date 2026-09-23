## mcp-error-data: JSON-RPC `error.data` in okay-mcp

`Rpc.Failed(id, code, message, data = None)`: JSON-RPC 2.0's optional
structured error detail (§5.1), decoded when present and written only
when set. Found building x402's MCP transport, which carries its
`PaymentRequired` in `error.data` under code 402. The client folded every
refusal into a `JErr("code message")` string; `Session.requestRpc` now
answers an `Outcome` — `Answered`, `Refused(failed)` with code and data
whole, `Ended` — and `request` is that folded exactly as before: all 55
existing okay-mcp tests pass unchanged. Tests: data round-trips, an
absent member stays absent, x402's own MCP 402 example decodes intact.
Also recorded: gate.sh did not recognise an okayAsyncNative lost process
(signal 9 + accept timeout, no Failed/Errors line) — native-runner-error.
