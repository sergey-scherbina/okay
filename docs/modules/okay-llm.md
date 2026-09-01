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
  Anthropic.stream(Transports.http(), apiKey,
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
import okay.lex.Bpe
val bpe = Bpe(List(("h","e"), ("l","l"), ("he","ll"), ("hell","o")))
Scan.all(bpe)("hello her").tokens.map(_.lexeme)
// "hello", " ", "he", "r" — spans exact, whitespace on Trivia
```

**Two protocols, one seam.** `Anthropic` and `OpenAi` are both built
on the same `Transport`, and the OpenAI-compatible one reaches most
of the market — OpenAI, Groq, Together, OpenRouter, Fireworks and the
local runtimes (Ollama, vLLM, llama.cpp) all serve it. Requests are
built as `Json` values and printed (a tool's `parameters` is an
arbitrary JSON Schema, not something a derived codec should
describe); responses decode by derived Schemas through the total
pipeline, so a body cut mid-string still yields the text it carried.

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
| `OpenAi.request/message/tool` | build the wire body as Json | derived tool schemas pass through untouched |
| `OpenAi.complete` | `(transport, key, body, url) => Response ! Async` | one completion, whole (what an agent loop needs) |
| `OpenAi.stream` / `token` | SSE deltas to text tokens | total: a cut-off payload is simply not a token |
| `Bpe` | `Bpe(ranks: Map[(String, String), Int])` / `Bpe(merges: Seq[(String, String)])` | byte-pair encoding as a `Scan[String, Bpe.S]` |
| `Bpe.encode` | `String => Vector[String]` | merge one word by rank |

## Gotchas

- The transport's Async ops run when the stream is PULLED — sending
  happens at first pull, not at `Anthropic.stream`.
- `retry` reruns from the beginning (at-least-once): fine for
  completions, mind it for tool-effect streams later.
- BPE word boundaries are whitespace runs; a GPT-2-style regex
  pre-tokenizer would slot into `step` unchanged.

The agentic layer this module is designed for — tool calls as effect
operations, context as an effect, the loop as a program — is built:
see [okay-agent](okay-agent.md), and [okay-rag](okay-rag.md) for
retrieval under the same budget.

## The streaming cut (llm-streaming-cut)

`Cut` closes P9's open item: a validator stands IN the token stream
and, on a violation, ABORTS to a prompt installed over the
generation — the non-local exit no specialised effect can spell
(Delim as the doctrine's PRIMARY case), behind an ADDITIVE wrapper:
`guarded`/`checked` explicit, `guard`/`watched`/`violation` with the
prompt ambient (nearest guard by nesting). The poisoned token never
flows; the source records NO further pulls after the cut; a passing
stream is identical to the unguarded run and never captures.
