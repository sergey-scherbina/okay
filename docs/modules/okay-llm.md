# okay-llm

A completion is a stream.

- `Transport` — the seam: post a body, stream back response lines
  (`java.net.http` for real, a canned mock in tests).
- SSE framing joins `data:` fields into event payloads (a Stage, and
  an inlined variant over the effectful row); Anthropic's events
  decode through okay-codec — a damaged or unknown event is simply
  not a token.
- `Anthropic.stream` — text tokens as `Writer % String + Async`:
  lazy, cancellable through Fiber, retried with the P2 policies
  as-is — `retry(Retry.immediate(2))(async(...))` survives a
  transport that dies mid-first-attempt (tested with a mock that
  throws once).
- Structured output is the flagship of parse totality: a JSON answer
  cut off mid-string still decodes to the value it carried.
- `Bpe` — byte-pair encoding on okay-lex's `Scan` interface: per-word
  merges by rank (lowest first), exact spans, whitespace on the
  Trivia channel (lossless), agreeing with a whole-string reference
  over the same merges table. A tokenizer is a Scan; the dictionary
  is the only difference between BPE and a JSON lexer.

```scala
val bpe = Bpe(List(("h","e"), ("l","l"), ("he","ll"), ("hell","o")))
Scan.all(bpe)("hello her").tokens.map(_.lexeme)  // hello, " ", he, r
```

The agentic layer (tool calls as effect operations, agents as Stages)
is designed for, not yet built — see specs/llm.md.
