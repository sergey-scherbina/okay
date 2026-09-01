# okay-http backends — NIO, Jetty, Netty

## Overview
`okay-http` defines two seams — `Http` and `Sockets` — and implements
them once per platform over what the platform already has
(`java.net.http` on the JVM, `fetch` on JS). This spec adds three more
implementations on the JVM, and the reason to have three is that they
answer three different questions, not one question three ways:

- **NIO** — no dependency, and the byte level rather than HTTP.
  `AsynchronousSocketChannel`'s completion handlers map onto
  `Async.Await` exactly: register, get called back, cancel. Nothing
  parks. This is the primitive `cluster.Remote`'s blocking sockets
  could stand on, and the one place where the callback shape the JS
  side is forced into is also the natural shape on the JVM.
- **Jetty** — the SERVER side, and specifically the one thing
  `okay-http` declared out of scope: **a WebSocket server**. The JDK
  has no server-side WebSocket API at all and `HttpServer` will not
  surrender its socket, so `specs/http.md` named that a gap rather than
  half-build it. Jetty closes it, and brings HTTP/2 and a real thread
  pool with it.
- **Netty** — NIO with the codecs already written, for a stack that
  already has it. This is interop in the sense `okay-fs2` and
  `okay-zio` are: not a better implementation, an implementation in
  the runtime someone is already running.

Nothing in `okay-http` changes. That is the test of whether the seam
was drawn in the right place: three backends, three dependencies, one
`trait Http` and one `trait Sockets`, and a program written against
them does not know which is underneath.

## Interface

```scala
// okay-http, scala-jvm — no dependency, so it stays in the base module
object Nio:
  /** a byte channel as our two halves: bytes out, bytes in */
  final class Conn:
    def send(b: Chunk[Byte]): Unit ! Async
    def bytes: Source[Chunk[Byte]]
    def close(): Unit ! Async

  def connect(host: String, port: Int): Conn ! Async
  def listen(port: Int)(serve: Conn => Unit ! Async)
            (using Scheduler): AsynchronousServerSocketChannel ! Resource

  /** a Conn as an MCP link — lines over a raw socket, no HTTP */
  def link(c: Conn): okay.mcp.Link

// okay-jetty — JVM only
object Jetty:
  def http(): Http ! Resource                    // the client
  def sockets(): Sockets ! Resource              // the WS client

  /** the gap okay-http could not fill: a WebSocket SERVER.
    * A session is the same Stage[Frame, Frame, A] a client writes. */
  def serve(port: Int)(routes: PartialFunction[Request, Response ! Async])
           (ws: PartialFunction[Request, Stage[Frame, Frame, Unit]] = PartialFunction.empty)
           (using CanBlock): org.eclipse.jetty.server.Server ! Resource

// okay-netty — JVM only
object Netty:
  def http(): Http ! Resource
  def sockets(): Sockets ! Resource
  def serve(port: Int)(routes: PartialFunction[Request, Response ! Async])
           (ws: PartialFunction[Request, Stage[Frame, Frame, Unit]] = PartialFunction.empty)
           (using CanBlock): Channel ! Resource
```

## Design

**Modules.** `okay-jetty` and `okay-netty` are plain JVM projects
depending on `okayHttp.jvm`, like `okay-kafka` and `okay-spark` — a
dependency each, and nothing in the core or in `okay-http` gains one.
NIO needs no dependency, so it lives in `okay-http`'s own `scala-jvm`
tree rather than earning a module of its own.

**Every backend is a `Resource`.** A Jetty client, a Netty event loop
group and an NIO channel group all own threads, and a module that hands
back a bare value hands back a leak. `Resource.run` is the scope, and
release is the scope's obligation — the discipline
`external-systems.md` already states for consumers and statements.

**Servers take routes as a `PartialFunction`, not a DSL.** A route is
`Request => Response ! Async` in `okay-http`, and a partial function
over the same type is the smallest thing that dispatches without
inventing a router. A WebSocket route is
`Request => Stage[Frame, Frame, Unit]`, which is the same idea with
the same session type a client already writes — so an echo session
written for a test runs on the server unchanged.

**Backpressure, again, honestly.** Netty and Jetty both have their own
demand mechanisms (`Content.Source.demand`, channel auto-read). They
are used to feed the `Source`, and they stay inside — `Socket` still
has no `request(n)`, for the reason `specs/http.md` gives.

## Behavior
- [ ] the same program runs on every backend: a test written once
      against `Http` passes with `java.net.http`, Jetty and Netty
      underneath, which is what having a seam is for
- [ ] a body still streams on each: a large response is folded chunk by
      chunk without being materialized
- [ ] a Jetty WebSocket SERVER runs a `Stage[Frame, Frame, Unit]`, and
      the JDK client from `okay-http` talks to it — the gap closed, and
      closed with the session type that already existed
- [ ] a Netty server does the same, for both REST and WebSocket
- [ ] NIO: two ends exchange bytes with nothing parked, the completion
      handler feeding `Async.Await` directly
- [ ] NIO carries an MCP session — `Mcp.run(Nio.link(conn), serving)`
      over a raw socket, no HTTP involved
- [ ] every backend releases what it owns: after the `Resource` scope
      the port is free and no thread survives
- [ ] a route that throws is a 500 on every server, as on the built-in
      one — damage as data does not depend on the backend

## Decisions
- **Three backends, three reasons** — chosen because symmetry is not a
  reason. NIO earns its place by having no dependency and by being the
  byte level; Jetty by serving WebSocket, which the JDK cannot; Netty
  by being the runtime a caller may already have. Rejected: adding
  backends for completeness, which is how a library grows a matrix it
  cannot test.
- **NIO stays in okay-http, not its own module** — chosen because it
  adds no dependency, and the rule is that a module carries one.
- **No hand-rolled HTTP over raw NIO** — chosen for the reason
  `specs/http.md` gives for not cross-building to Native: writing
  HTTP/1.1 by hand where a good client exists is work without a
  payoff. Raw NIO is offered as what it is, a byte transport. Netty is
  the answer to "NIO with HTTP", and it is a dependency because that
  codec is worth one.
- **Servers are `Resource`, clients too** — chosen because all three
  own threads. Rejected: returning a bare client and documenting a
  `close()`, which is a leak with instructions.

## Out of scope
- **Serving from JS.** Unchanged: no server in a browser, and Node's
  own server is not the web-standard surface `okay-http`'s JS side
  speaks.
- **HTTP/3, ALPN tuning, TLS configuration beyond defaults, proxies,
  connection-pool sizing.** Every backend exposes them on its own
  builder; the seam does not, and a caller who needs them constructs
  the backend's own object and passes it in.
- **A routing DSL, middlewares, content negotiation.** A route is a
  partial function over `Request`; anything beyond that is a framework,
  and this is a seam.
- **Replacing okay-cluster's transport.** NIO makes it possible and
  that is a separate change with its own measurements.
