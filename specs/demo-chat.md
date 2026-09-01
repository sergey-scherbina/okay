# demo-chat — the chat with an LLM, as a web page

## Overview
The user-requested showcase: a demo web application for chatting
with an LLM, built out of what the repository already ships and
nothing else. One JVM main serves a page and streams completions;
the browser needs no build step (a small inline script — the demo is
of the SERVER stack, not of a frontend toolchain).

The pieces, each doing its one job:
- **okay-jetty** serves the page and STREAMS the response body live
  (`Response.body` is a `Source[Chunk[Byte]]`; jetty writes chunk by
  chunk on a virtual thread when the content type is
  `text/event-stream`).
- **okay-llm** turns the provider's SSE into a token stream
  (`Llm.stream`), over the real `TransportJvm` when
  `ANTHROPIC_API_KEY` is set.
- **Cut** (llm-streaming-cut) guards the stream: a demo rule cuts
  generation mid-sentence and the page SHOWS the cut — the P9
  feature made visible.
- **The offline mode is not a mock of the demo — it IS the demo**:
  with no key, a scripted model streams a deterministic reply, so
  the application always runs, tests prove it end to end on a real
  socket, and the key only swaps the model handler (the seam
  doctrine, once more).

## Interface
- `ChatDemo.main` — serves on `OKAY_CHAT_PORT` (default 8090).
- `GET /` — the page: message list, input, send; a fetch-reader
  appends tokens as they arrive; a cut is rendered as a marked line.
- `POST /chat` — body: `{"messages":[{"role":..,"content":..}]}`
  (the client keeps the history; the server stays stateless).
  Answer: `text/event-stream` of `data: <json string>` token events,
  then one `event: done` or `event: cut` with the violation.
- The model seam: `ChatDemo.model` is `Seq[Message] => tokens`;
  `scripted` (offline) and `live` (key) both fit it.
- The guard: a token budget (`OKAY_CHAT_MAX`, default 512) enforced
  by `Cut.checked` — over budget, the stream is cut, named.

## Behavior
- [x] offline, on a real socket: POST /chat streams the scripted
      reply token by token (the test reads the stream incrementally,
      not as one blob), ending with `event: done`
- [x] the page serves and carries the client script
- [x] the cut shows: a scripted reply exceeding the budget streams
      its prefix and ends with `event: cut` naming the rule; no
      tokens follow the cut
- [ ] with a key in the environment the same route speaks to the
      real API (the TestLive pattern: skipped when absent) — OPEN
      pending a key on the box; the seam is one function swap

## Out of scope
- Auth, persistence of conversations, multi-user rooms (okay-match
  and persist own those stories).
- A Scala.js frontend — deliberate: the server stack is the exhibit;
  the ui-wire browser leg is its own demo when taken.

## Decisions
- **SSE over WebSocket** — the reply is a one-direction stream per
  request; SSE is the smaller honest tool, and jetty's streaming
  path already speaks it.
- **History client-side** — a stateless server demos the stack
  without inventing session storage the spec would then owe.
- **The stub streams like the wire does** — token by token with the
  same framing, so the offline test proves the same path the key
  exercises.
