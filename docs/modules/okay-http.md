# okay-http

REST and WebSocket, as programs (specs/http.md).

Three specs already deferred to a "transport module" that had never
been written — `cross-platform-async.md`, `codecs.md` and `cluster.md`
all name it as an ingredient, and no ROADMAP phase owned it. This is
that module, and it is small because the vocabulary decided most of it:

- **a response body is a stream** — `Source[Chunk[Byte]]`, which is
  `Unit ! (Writer % Chunk[Byte] + Async)`. Nothing is read when the
  head arrives; the body goes `through` a decoding `Stage` exactly as
  SSE lines already did. `Pipe.scala` names that case in its own doc
  comment as "the generalization the LLM client walks by hand" — this
  is the caller that stopped walking it.
- **a WebSocket session is a `Stage[Frame, Frame, A]`** — it awaits
  incoming frames and tells outgoing ones. Not an analogy: okay-mcp is
  already `Stage[Rpc, Rpc, Unit]` for the same reason.
- **the transport is not an effect** — a trait speaking `Async`, like
  `llm.Transport`, `mcp.Link` and `cluster.Remote`. The house rule
  mints an effect signature for domain logic above the wire, never for
  the wire.

## The pieces

| | |
|---|---|
| `Request` / `Response` / `Method` / `Body` | the wire, as data — a 4xx is a `Response`, and no `Throws` appears anywhere |
| `trait Http { def send(r: Request): Response ! Async }` | the seam. One method, like `llm.Transport`, but carrying the verb, the status and the headers back |
| `Http.bytes / text / lines / json / sse` | reading a body — `lines` streams, `json` is total, `sse` IS `llm.Sse.events` |
| `Frame` / `trait Socket` / `trait Sockets` | the WebSocket side |
| `Ws.over(socket)(session)` | run a `Stage[Frame, Frame, A]` over a socket |
| `Ws.link(socket)` | a socket AS an `mcp.Link` |
| `Transports.http` / `.sockets` (JVM), `.fetch` / `.sockets` (JS) | the two platform seams |
| `Server.serve(port)(route)` | a REST server, JVM only |

## What it buys okay-mcp

MCP has two standard transports: stdio, which okay-mcp had, and
HTTP+SSE, which it did not. A `Link` is `send(line)` plus
`lines: Source[String]`, and a WebSocket is exactly that with frames
around it — so `Mcp.run(Ws.link(socket), serving)` is the same server
over a different wire, with no protocol code changed. `TestWs` carries
an `Rpc` over frames and decodes it back to the identical message.

## Two honesty constraints, in the interface rather than papered over

**Backpressure is asymmetric.** The JDK's `WebSocket.Listener` is
genuinely pull-based — demand starts at zero, `request(n)` raises it,
each call lowers it, and at zero the socket stops calling, which is
flow control down to TCP. Browser and Node `WebSocket` have no
receive-side lever at all. So `request(n)` appears **nowhere** in
`Socket`: the JVM transport spends its own demand, one `request(1)` per
frame handed on, and the JS transport buffers into a bounded `Channel`
with the bound stated. A shared method one platform silently fakes
would be worse than the asymmetry.

**Serving WebSocket is out of scope.** The JDK has no server-side
WebSocket API, and `HttpServer` does not surrender its socket, so it
would mean hand-rolling RFC 6455. The tests do exactly that, in test
scope, to exercise the client against a real socket — `WsEcho` is 120
lines and is not a library feature.

## Platforms

`crossProject(JVM, JS)`, not Native — the same call okay-llm and
okay-cluster made. Scala Native has `java.net.Socket` but no
`HttpClient`, no `HttpServer`, no WebSocket and no complete
`javax.net.ssl`, so an implementation there means hand-rolling HTTP/1.1
in plaintext or binding libcurl.

"JS" here means Node, as everywhere in this repository — `fetch` and
the global `WebSocket`, over raw `js.Dynamic` rather than
scala-js-dom, matching `llm.TransportJs` and the dependency rule. The
JS body reader is incremental (`ReadableStream.getReader`), which is
the step `TransportJs`'s own comment had left as "the stated next".

## Not done

A JVM server driven by a JS client in one shared-source program — the
acceptance shape okay-cluster established with a linked Scala.js
subprocess. It is a build fixture rather than a module concern, and it
is the honest next step here.
