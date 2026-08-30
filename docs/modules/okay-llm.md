# okay-llm

A completion is a stream.

- `Transport` — the seam: post a body, stream back response lines
  (`java.net.http` for real, a canned mock in tests).
- SSE framing joins `data:` fields into event payloads (a Stage, and
  an inlined variant over the effectful row); Anthropic's events
  decode through okay-codec — a damaged or unknown event is simply
  not a token.
- `Anthropic.stream` — text tokens as `Writer % String + Async`:
  lazy, cancellable through Fiber, retried with the P2 policies.
- Structured output is the flagship of parse totality: a JSON answer
  cut off mid-string still decodes to the value it carried.

The agentic layer (tool calls as effect operations, agents as Stages)
is designed for, not yet built — see specs/llm.md.
