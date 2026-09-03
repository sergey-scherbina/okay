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
                          body: Source[Chunk[Byte]],
                          release: Unit ! Async = pure(()))

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
def discard(r: Response): Unit ! Async                 // let a body go unread
def framing: Stage[Chunk[Byte], String, Unit]          // the framer itself, reusable
def one(bs: Array[Byte]): Source[Chunk[Byte]]          // a body already in hand

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
def texts: Stage[Frame, String, Unit]                  // text frames out, rest dropped

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
           (using CanBlock): com.sun.net.httpserver.HttpServer ! Resource
  def port(s: HttpServer): Int          // the bound port, when 0 asked for any
  def text(status: Int, s: String, headers: Seq[(String, String)] = Nil): Response ! Async
  def json[A: Schema](status: Int, a: A, headers: ...): Response ! Async
  def notFound: Response ! Async
  def path(r: Request): String          // the url without its query
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
- [x] a GET returns status, headers and an unread body; nothing is
      fetched from the body until it is consumed (`TestHttp`)
- [x] a 4xx/5xx is a `Response`, not a failure — status is data, and no
      `Throws` appears anywhere in this module
- [x] a body streams: 400KB folded chunk by chunk at constant memory
- [x] `json` on a TRUNCATED body decodes to the value it carried, with
      the damage visible — inherited from `Json.read`, not re-invented
- [x] `sse` over an event body yields the same payloads
      `llm.Sse.events` yields, because it IS that stage
- [x] framing survives every chunk boundary, including one that splits a
      multi-byte character — which is why it happens on bytes, before
      decoding, and `TestFraming` splits a `—` in half to prove it
- [x] retry from P2 applies unchanged: `retry(Retry.immediate(2))`
      around a send recovers a transport that fails its first attempt
- [x] a WebSocket echo session written as `Stage[Frame, Frame, Unit]`
      round-trips text and binary, against a real socket (`WsEcho`, a
      test-scope RFC 6455 server — the library does not serve WS)
- [x] a fragmented message arrives as ONE frame: the server splits every
      4 bytes and the session still sees one `Text`. Both directions are
      exercised — the JDK fragments a 200KB send of its own accord, and
      the echo server had to learn to reassemble continuations
- [x] ping is answered by the transport and shown to the session as
      information; a session that tells a `Pong` still has it sent
- [x] okay-mcp runs over `Ws.link(socket)`: an `Rpc` encoded, carried as
      frames and decoded back to the identical message
- [x] a route that throws is a 500 carrying its message — damage as data
      on the wire too
- [x] `Resource` stops the server: a request after the scope fails
- [x] closing twice is a no-op, not an `Output closed` — a session may
      close itself and the caller may close after it
- [x] close half-duplex in full: frames in flight after a Close still
      arrive. `WsEcho(partingWords = 3)` answers a Close with three
      more texts BEFORE echoing it, and the session sees all three, in
      order, then the Close — a transport that tore the read half down
      with the write half would drop them
- [x] a JVM server driven by a JS client, one shared-source program.
      `Acceptance.check` is that program — the schema, the routes, the
      session and the expectations all shared — and both ends run it:
      the JVM against its own transports as a control, and a linked
      Scala.js client over `fetch` and the global `WebSocket` as the
      acceptance. It lives in okay-jetty because that is the module
      which can serve both halves. Verified to be able to FAIL:
      mistyping one field name in the JS transport turns the run red
      with `undefined cannot be cast to java.lang.Integer`, which is
      exactly the failure `js.Dynamic` is worst at and exactly why this
      run had to exist
- [x] a body can be let go UNREAD. `Response` carries a `release`, and
      `Http.discard` is it — draining a large body only to throw it
      away is the wrong fix, and the JDK documents that leaving one
      open can block an orderly `HttpClient` shutdown. Twenty abandoned
      200KB bodies followed by an ordinary request, which answers

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

## Results
30 tests on the JVM (both transports, the server, the framer, the MCP
bridge) and 11 shared ones that run on JS too (the framer, the session
stages, the projections — everything pure, which is most of the
module). No warnings.

Three divergences from this spec, all where the implementation was
right and the spec has been corrected above: `Resource` in this library
is an EFFECT, so `serve` answers `HttpServer ! Resource` rather than a
`Resource[Unit]`; `Http.one` and `Http.framing` had to be public for a
server and a caller to build bodies and reuse the framer; and `Server`
grew the four small helpers a route cannot do without.

One bug worth recording because it cost the most time and was mine, not
the library's: `WsEcho`'s handshake used the wrong RFC 6455 GUID —
`258EAFA5-E914-47DA-95CA-5AB0DC85B11F` instead of
`258EAFA5-E914-47DA-95CA-C5AB0DC85B11`, a dropped `C` and an invented
trailing `F`. The JDK answered `Bad Sec-WebSocket-Accept`, which says
nothing about which half is wrong. Checking the RFC's own example
vector (`dGhlIHNhbXBsZSBub25jZQ==` -> `s3pPLMBiTxaQ9kYGzzhZRbK+xOo=`)
found it in one step, and that is the move to reach for first with any
constant one is confident about.

## http-message-phases — REFUTED at implementation (2026-09-01)

Filed on a false premise and corrected here rather than silently
dropped: the section claimed the Nio parser "holds the phases as an
enum today" — but this codebase has NO hand-written HTTP/1.1
message parser, BY DECISION (Nio.scala's own doc: writing HTTP/1.1
by hand where the JDK, Netty and Jetty already own the codec is
work without a payoff; Server.scala rides com.sun.net.httpserver).
Building a parser in order to refactor it onto phased3 would be
machinery for a need nobody named, twice over. `Stage.phased3`
stands on its own (landed, additive; its test exhibits the http
message SHAPE as scripted data, which is all the shape was ever
needed for). If a hand parser ever becomes real — a Native http
leg without a platform codec is the one imaginable door — phased3
is ready for it, and this note is where that conversation resumes.

## The peer address (http-peer-address)

`Request` carried the method, the url, the headers and the body — and
nothing about WHO sent it. That is fine for a client, which builds the
request, and wrong for a server, which is handed one and has to decide
whether to answer it.

The consumer that named it: a downstream service rate-limiting its
endpoints could only key on identities the caller supplies, all of them
forgeable, or on `X-Forwarded-For`, which is a claim in a header. A
flood with a fresh handle per request met nothing but a global ceiling
that throttles everyone at once.

    peer: Option[String] = None

Additive, defaulted, and therefore inert for every existing
construction — including `Request.get`/`post`/`json`, which build what
a CLIENT sends and leave it `None`. It is the servers that fill it, and
only where the transport actually knows: okay-jetty, okay-netty and
okay-http's own JVM server.

**The HOST, without the port.** A port changes per connection, so
keying a rate limiter on `host:port` would give every connection a
fresh budget — which is the bug the field exists to fix. What is
recorded is what identifies the sender across connections.

**`None` means unknown, never "trusted zero".** A transport that cannot
say (a client-built request, a test, a future backend) says nothing,
and a caller deciding on it must treat absence as absence rather than
as a default address everyone shares.

**It is not a header and never becomes one.** `X-Forwarded-For` remains
the proxy's claim and a consumer's own decision to trust; this is what
the socket says. A consumer that has both should prefer the proxy's
value only when it has decided the proxy is real.

- [ ] a served request carries the peer's host on jetty
- [ ] the same on netty
- [ ] the same on okay-http's JVM server
- [ ] a client-built request carries `None`
- [ ] the field is additive: every existing construction compiles and
      behaves unchanged

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

## Typed facades (2026-09-02, typed-js-facades)
The "raw `js.Dynamic`, no scala-js-dom" decision above was written to
be revisited out loud, and the cast audit (AGENTS.md: no cast without
a real necessity) was the occasion. The dependency half stands — still
no scala-js-dom — but the untyped half is gone: `okay.Web` in the
core's scala-js sources states the web globals this stack uses as
`js.native` facades — `fetch` with `RequestInit`, `Response`,
`Headers`, the body `Reader` and its `ReadResult`, `WebSocket` with
`MessageEvent`/`CloseEvent` — and both transports (okay-http's fetch
and sockets, okay-llm's fetch) are written on them: seventeen casts to
none. A message's `data` is declared `Any` so that text versus binary
is a type TEST (`case s: String`, `case buf: ArrayBuffer`), not a
cast. What a facade cannot make safe it makes explicit: the shape of
the API is now one file's claim about Node 18+/22+ and browsers,
instead of a claim at every call site.

