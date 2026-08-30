# okay-llm

> A completion is a stream: transport lines through SSE framing
> through the total parser — a truncated payload is data, not a
> fault. BPE tokenization is a `Scan`, like every lexer here.

Depends on: `okay-codec` (and through it the whole total text stack).

## Guide

**Everything is already a stream.** Nothing in the thin client
assumes one request/one response. The transport answers response
LINES as `Unit ! (Writer % String + Async)` — telling is streaming,
the Async ops are where the wire blocks (a virtual thread parks in
them). SSE framing joins `data:` fields into event payloads; each
payload decodes through okay-codec — an unknown or damaged event is
simply NOT a token, no fault at any prefix.

**Laziness is the resilience story.** The program is a value:
nothing is sent until the stream is pulled, the pull can stop at any
token (cancellation rides `Fiber`, as everywhere), and the P2 retry
combinators apply UNCHANGED — `retry(policy)(async(...))` reruns the
whole exchange, which is correct for an idempotent completion call.

**Structured output is the totality flagship.** A model's JSON
answer cut off mid-generation still decodes to the value it carried:
the total parser yields a tree with holes, the projection reads the
fields that are there. This is why the parse stack is total — not
error recovery bolted on, but the LLM streaming case designed in.

**BPE is a Scan.** The LLM world's tokenizer implements the same
interface as the JSON lexer: per-word merges by rank (lowest first),
exact spans, whitespace on the Trivia channel (lossless), and every
`Scan` capability — chunked runs, snapshots — comes along for free.

## Tutorial

Stream a completion (the transport seam makes it a mock away from a
test — no network needed to develop against it):

```scala
import okay.llm.{Anthropic, Transport}

val tokens: Unit ! (Writer % String + Async) =
  Anthropic.stream(Transport.http(), apiKey,
    Anthropic.Request("claude-sonnet-5", 1024,
      List(Anthropic.Message("user", "hi")), stream = true))

tokens.toLazyList.take(10).toList   // pull ten tokens, lazily
```

Survive a flaky wire with the P2 combinators as-is:

```scala
retry(Retry.exponential(100).take(5))(
  async(collect(tokens)))           // at-least-once, replayable call
```

Decode a cut-off structured answer:

```scala
case class Answer(city: String, country: String)
given Schema[Answer] = Schema.derived
Json.read[Answer]("""{"city": "Kyiv", "country": "Ukraine"""")
// Right(Answer("Kyiv", "Ukraine")) — cut off, still a value
```

Tokenize with BPE:

```scala
import okay.llm.Bpe
val bpe = Bpe(List(("h","e"), ("l","l"), ("he","ll"), ("hell","o")))
Scan.all(bpe)("hello her").tokens.map(_.lexeme)
// "hello", " ", "he", "r" — spans exact, whitespace on Trivia
```

## API reference

| member | signature | meaning |
|---|---|---|
| `Transport` | `post(url, headers, body): Unit ! (Writer % String + Async)` | the seam: lines stream back |
| `Transport.http` | `(client = default) => Transport` | java.net.http, streaming lines |
| `Sse.events` | `Stage[String, String, Unit]` | SSE framing: lines in, event payloads out, trailing event flushed |
| `Anthropic.Request/Message` | case classes with derived Schemas | the request body |
| `Anthropic.Event/Delta` | derived Schemas | the streaming events that matter |
| `Anthropic.token` | `String => Option[String]` | payload to text token; total |
| `Anthropic.stream` | `(transport, apiKey, request, url?) => Unit ! (Writer % String + Async)` | the completion as a token stream |
| `Anthropic.tokensOf` | reusable tail: SSE lines to tokens | build other providers on it |
| `Bpe` | `Bpe(ranks: Map[(String, String), Int])` / `Bpe(merges: Seq[(String, String)])` | byte-pair encoding as a `Scan[String, Bpe.S]` |
| `Bpe.encode` | `String => Vector[String]` | merge one word by rank |

## Gotchas

- The transport's Async ops run when the stream is PULLED — sending
  happens at first pull, not at `Anthropic.stream`.
- `retry` reruns from the beginning (at-least-once): fine for
  completions, mind it for tool-effect streams later.
- BPE word boundaries are whitespace runs; a GPT-2-style regex
  pre-tokenizer would slot into `step` unchanged.

The agentic layer (tool calls as effect operations, agents as
Stages over Channels) is designed for, not yet built — specs/llm.md.
