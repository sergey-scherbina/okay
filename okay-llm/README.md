# okay-llm — a completion is a stream

Nothing in this client assumes one request and one response. The
transport answers response LINES; SSE framing joins `data:` fields
into payloads; each payload decodes through okay-codec. A truncated
payload is DATA, not a fault — an unknown or damaged event is simply
not a token, at any prefix.

Laziness is the resilience story: the program is a value, nothing is
sent until the stream is pulled, the pull can stop at any token, and
the retry combinators apply unchanged.

## The pieces

| | |
|---|---|
| `trait Transport` | the seam: `post(url, headers, body)` answering lines. One method, so a test transport is three |
| `Sse.events` | `Stage[String, String, Unit]` — lines in, event payloads out |
| `Anthropic` | the request and the block vocabulary: `textBlock`, `toolUseBlock`, `toolResultBlock`, `toolDecl`, and `stream` |
| `Cut` | the flagship of totality: a model's JSON answer cut off mid-generation still decodes to the value it carried |
| `Bpe` | tokenization as a `Scan`, like every lexer here |

## What it looks like

A completion is pulled, not awaited:

```scala
import okay.llm.*

val tokens: Unit ! Writer % String + Async =
  Anthropic.stream(transport, apiKey, request)
```

Stop at any token and the rest is never fetched; wrap the whole
exchange in `retry(policy)` and it reruns correctly, because the
program is a value rather than an in-flight call.

The layer above is [`okay-agent`](../okay-agent), where a model is an
effect rather than a client — that is where tools, context and
compaction live.

## Further

| | |
|---|---|
| [`docs/modules/okay-llm.md`](../docs/modules/okay-llm.md) | the guide: streaming, laziness, structured output |
| [`specs/llm.md`](../specs/llm.md) | the design and its decisions |
| [`okay-agent/`](../okay-agent) | agents as programs, on top of this |
