# okay-llm — language models as streams

## Overview
Two layers, both in scope (the user's decision: "и тонкий и большее"):
a thin streaming client now, an agentic layer later — designed so the
thin layer is exactly what the agentic one stands on. The larger shape
exists next door in ../rozum (meeting rooms, multi-agent coordination);
consult it WHEN the agentic layer starts — not urgent, but the module
boundaries here must not preclude it.

## The thin client
- A completion is a stream: `Chunks[Token] ! Async` (SSE/chunked HTTP
  arrives batched — chunks by nature, like Kafka polls).
- Providers (Anthropic, OpenAI, local) behind one small interface;
  requests/responses through okay-codec (JSON dialect).
- Structured output rides okay-parse's TOTALITY: streaming JSON from a
  model is truncated JSON at every instant — the error-tolerant parser
  yields a partial tree with holes that each arriving chunk refines.
  This is the flagship consumer of streaming-parse.md.
- Resilience from P2: retry policies (backoff streams) on transport,
  timeouts, rate-limit handling as data.
- Tokenization (BPE/SentencePiece) implements okay-lex's `Scan` —
  dictionary-driven, boundary-buffered, same Stage interface as any
  lexer (for counting, budgeting, and local models).

## The agentic layer (later, designed-for now)
- A conversation is a stream of messages; a tool call is an EFFECT
  (an operation the handler interprets — the agent loop is literally
  an effect handler); an agent is a Stage (awaits observations, tells
  actions). Multi-agent coordination = Channels + merge.
- Nothing in the thin client may assume a single request/response —
  everything is already a stream.

## Behavior
- [x] a mocked SSE stream yields tokens, in order, non-token events
      dropped (cancellation rides Fiber, as everywhere)
- [x] structured-output parsing: a mid-answer cut-off JSON decodes to
      the value it carried (the total pipeline end to end); a payload
      cut mid-json is simply not a token — no fault at any prefix
- [ ] retry-with-backoff on a flaky transport (the P2 combinators
      apply as-is; a dedicated test is the follow-up)
- [ ] a BPE Scan tokenizes a corpus identically to the reference
      (waits for a dictionary; the Scan interface is ready)

## Out of scope (for the thin layer)
- prompt frameworks; vector stores; the rozum-shaped multi-agent
  runtime (later phase, over okay-cluster/Channels)
