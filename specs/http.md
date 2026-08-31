# okay-http — REST and WebSocket, as programs

## Overview
Three specs already defer to a module that was never written.
`cross-platform-async.md`: *"Cross-platform interop between running
programs = codecs + a transport module."* `codecs.md`: *"a transport
module (its own, later)."* `cluster.md` names it as an ingredient. No
ROADMAP phase owns it. This is that module.

It is not an HTTP library that happens to be written in this codebase;
it is HTTP said in the vocabulary that is already here, and the
vocabulary decides almost every question in it:

- a response body is a **stream** — `Source[Chunk[Byte]]`, which is
  `Unit ! (Writer % Chunk[Byte] + Async)` — so it goes `through` a
  decoding `Stage` exactly as SSE lines already do, and `Pipe.scala`
  names that case in its own doc comment.
- a WebSocket session is a **`Stage[Frame, Frame, A]`**: it awaits
  incoming frames and tells outgoing ones. That is not an analogy —
  okay-mcp is already `Stage[Rpc, Rpc, Unit]` for the same reason, and
  its `over(link)(stage)` is the connector this module supplies the
  other end of.
- the transport is **not an effect**. Per the house rule visible in
  `llm.md`, `external-systems.md` and `cluster.md`, a new signature is
  minted for domain logic above the wire, never for the wire; the wire
  is a value speaking `Async`, like `llm.Transport` and `mcp.Link`.

The payoff beyond REST: MCP has two standard transports, stdio (done)
and HTTP+SSE (not). `mcp.Link` is `send(line)` plus `lines: Source[String]`,
so a WebSocket or an SSE response IS a `Link`, and okay-mcp gets its
second transport for free.

## Interface

```scala
// ---- REST, as data

enum Method:
  case Get, Head, Post, Put, Patch, Delete, Options

enum Body:
  case Empty
  case Text(s: String)                 // UTF-8
  case Bytes(b: Chunk[Byte])

final case class Request(method: Method, url: String,
                         headers: Seq[(String, String)] = Nil,
                         body: Body = Body.Empty)

/** The response head arrives first; the body is a stream that has not
  * been read yet. Nothing is buffered until someone consumes it. */
final case class Response(status: Int,
                          headers: Seq[(String, String)],
                          body: Source[Chunk[Byte]])

/** The seam. One method, like `llm.Transport` — but with the verb, the
  * status and the headers that Transport could not carry. */
trait Http:
  def send(r: Request): Response ! Async

// ---- reading a body, through the stack that is already total

def bytes(r: Response): Chunk[Byte] ! Async            // drain
def text(r: Response): String ! Async                  // drain, UTF-8
def lines(r: Response): Source[String]                 // streamed
def json[A](r: Response)(using Schema[A]): Either[String, A] ! Async
def sse(r: Response): Source[String]                   // lines through llm.Sse.events

// ---- WebSocket

enum Frame:
  case Text(s: String)
  case Binary(b: Chunk[Byte])
  case Ping(b: Chunk[Byte])
  case Pong(b: Chunk[Byte])
  case Close(code: Int, reason: String)

/** An open socket, in the shape okay-mcp already consumes. */
trait Socket:
  def send(f: Frame): Unit ! Async
  def frames: Source[Frame]
  def close(code: Int, reason: String): Unit ! Async

trait Sockets:
  def connect(url: String, headers: Seq[(String, String)] = Nil,
              subprotocols: Seq[String] = Nil): Socket ! Async

/** A session is a Stage — the same shape as `Mcp.serve`. */
def over[A](s: Socket)(session: Stage[Frame, Frame, A]): A ! Async

/** and the bridge that gives okay-mcp its second transport */
def link(s: Socket): okay.mcp.Link

// ---- the platform seams (never in shared code)

// scala-jvm
object Transports:
  def http(client: java.net.http.HttpClient = ...): Http
  def sockets(client: java.net.http.HttpClient = ...): Sockets

// scala-js  (Node: global fetch, global WebSocket)
object Transports:
  def fetch: Http
  def sockets: Sockets

// scala-jvm only — there is no server in the browser and none in Node's
// standard globals; and no WebSocket server in the JDK at all
object Server:
  def serve(port: Int)(route: Request => Response ! Async)
           (using Scheduler): Resource[Unit]
```

## Design

**Module**: `crossProject(JVMPlatform, JSPlatform)`, `CrossType.Pure`,
`.dependsOn(okayMcp)` — following okay-mcp's own declaration. **Not
cross-built to Native**, for the same reason okay-llm and okay-cluster
are not: Scala Native's javalib has `java.net.Socket` but no
`HttpClient`, no `HttpServer`, no WebSocket and no complete
`javax.net.ssl`, so a Native implementation means hand-rolling HTTP/1.1
in plaintext or binding libcurl — which is what "no dependency unless it
earns it" rules out. The traits compile there; nothing implements them.

**Framing is a `Stage`, everywhere.** Chunked-transfer decoding, WS
frame decoding and SSE all have the same shape and all use
`Stage.transduce`, the skeleton `llm.Sse.events` is already written
with. Nothing in this module parses by hand.

**Bodies ride `through`.** `Pipe.scala:337` documents
`throughProducerG` as *"the generalization the LLM client walks by
hand (SSE lines ! Async through the event stage)"* — this module is
the caller that stops walking it by hand.

**Backpressure is asymmetric and the interface says so.** The JDK's
`WebSocket.Listener` is genuinely pull-based: the demand counter starts
at zero, `request(n)` raises it, each listener call lowers it, and at
zero the socket stops calling — real flow control down to TCP. Browser
and Node `WebSocket` have no receive-side lever at all; only
`bufferedAmount` on send. So `request(n)` does **not** appear in
`Socket`: the JVM transport spends its demand to satisfy `frames`, and
the JS transport buffers into a bounded `Channel` — the same
socket-to-`Channel` adaptation `cluster.Remote.listen` already uses.
The bound is stated, not hidden, and overflow fails the channel rather
than growing without limit.

## Behavior
- [ ] a GET returns status, headers and an unread body; nothing is
      fetched from the body until it is consumed
- [ ] a 4xx/5xx is a `Response`, not a failure — status is data, and no
      `Throws` appears anywhere in this module
- [ ] a body streams: a response larger than memory is folded chunk by
      chunk at constant memory, on both platforms
- [ ] `json` on a TRUNCATED body decodes to the value it carried, with
      the damage visible — the contract `codecs.md` and
      `streaming-parse.md` state, inherited rather than re-invented; a
      damaged element does not fail the whole decode
      (`llm-agentic.md:328` records that exact regression)
- [ ] `sse` over a mocked event stream yields the same payloads
      `llm.Sse.events` yields, because it IS that stage
- [ ] retry and timeout come from P2 unchanged — `retry(Retry.immediate(2))`
      around a send recovers a transport that fails its first attempt
- [ ] a WebSocket echo session written as `Stage[Frame, Frame, Unit]`
      round-trips text and binary frames
- [ ] a fragmented text message arrives as one `Frame.Text` — the JDK's
      `last` boolean is joined by the transport, not exposed
- [ ] ping is answered without the session seeing it; an explicit
      `Pong` sent by the session is still delivered
- [ ] `close` is half-duplex: after sending Close, frames already in
      flight still arrive, and the stream ends at the peer's Close
- [ ] a JVM server serves a route and a JS client drives it with the
      same shared-source program — the acceptance shape `cluster.md`
      established (JS client, JVM server, one program)
- [ ] okay-mcp runs over `link(socket)` unchanged: the same
      `Stage[Rpc, Rpc, Unit]` that runs over stdio runs over a WebSocket
- [ ] an abandoned body is cancelled, not leaked — the JDK documents
      that an unconsumed `ofLines`/`ofPublisher` stream blocks orderly
      `HttpClient` shutdown, so `Resource` release cancels it

## Decisions
- **A trait, not an effect signature** — chosen because the house rule
  is explicit and consistent: `llm.Transport`, `mcp.Link` and
  `cluster.Remote` are all values speaking `Async`, and a new signature
  is minted only for domain logic above the wire. Rejected: an `Http`
  effect with a `Handler` (tempting, since handlers are what this
  library is for) — it would put a transport in the row that every
  caller must then forward, and buys nothing a trait does not already
  give, since the seam is already substitutable for tests.
- **The response body is a `Source`, not an `Array[Byte]`** — chosen
  because the wire is chunked by nature (`llm.md:12`) and because a
  streamed body is what makes SSE, MCP-over-HTTP and large downloads
  one mechanism instead of three. Rejected: returning the whole body,
  which is what `llm.TransportJs` does today and its own comment calls
  a temporary simplification.
- **A WebSocket session is a `Stage`, not a pair of callbacks** —
  chosen because `Stage[In, Out, A]` already means "awaits In, tells
  Out", which is exactly a session, and because okay-mcp proves the
  shape composes. Rejected: an `onMessage` listener interface, which
  would need its own combinators for everything `through`, `transduce`
  and `pipe` already do.
- **No `request(n)` in the shared interface** — chosen because JS
  cannot honour it, and a shared type that one platform silently fakes
  is worse than an honest asymmetry. The JVM side keeps real
  backpressure; the JS side states its buffer bound.
- **Raw `js.Dynamic`, no `scala-js-dom`** — chosen to match
  `TransportJs` and the dependency rule. Reconsider only if the untyped
  surface (Headers, ReadableStream reader, WebSocket events) proves to
  cost more in bugs than the dependency costs in weight; this is a real
  fork and is recorded so it is not re-litigated by accident.
- **`com.sun.net.httpserver` for the server** — chosen because it is
  genuinely supported (JEP 403 kept `jdk.httpserver` exported) and adds
  no dependency. Its limits are accepted and stated: HTTP/1.1 only, and
  no WebSocket upgrade, so serving WebSocket is out of scope rather
  than half-built.

## Out of scope
- **Serving WebSocket.** The JDK has no server-side WebSocket API and
  `HttpServer` does not surrender its socket, so this would mean
  hand-rolling RFC 6455 framing and the handshake. Named here so the
  gap is a decision, not an omission.
- **Streaming request bodies.** JVM can (`BodyPublishers.fromPublisher`);
  fetch needs duplex mode that is not reliably available. Requests stay
  `Empty | Text | Bytes` until both sides can do it honestly.
- **HTTP/2 specifics, proxies, cookie jars, redirects policy,
  connection-pool tuning, multipart, compression** — `HttpClient`
  carries them and the seam can expose them later; none is needed to
  make the vocabulary work.
- **The browser as a target.** This repo's JS side is Node
  (`okay-cluster` uses `require("net")` and `process`). A browser build
  would additionally lose custom headers (the forbidden-header list),
  raw sockets and any server; that is a narrowing to face when someone
  wants it, not now.
- **Native.** See Design.
