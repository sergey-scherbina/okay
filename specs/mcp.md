# okay-mcp — the Model Context Protocol, as programs

## Overview
MCP is JSON-RPC 2.0 over a byte stream: a client asks a server for its
tools and calls them, a server answers. This module makes both ends
available to okay, and the design was already decided next door — from
specs/llm-agentic.md: *"an MCP server is another `Tool` handler, and
its JSON-RPC framing is our total parser plus `Schema`."*

That sentence is the whole architecture. An agent program does not
change by one character when its tools come from an MCP server rather
than from a local table, because a tool call is an EFFECT and where it
is executed is a handler's business. In the other direction our tools
are already `ToolSpec` + `ToolCall => String`, which is exactly what an
MCP server serves, so serving them is a Stage over the message stream
— no I/O in the protocol layer at all.

## Interface

```scala
// JSON-RPC 2.0 as data, total in both directions
enum Rpc:
  case Request(id: Json, method: String, params: Json)
  case Notify(method: String, params: Json)
  case Answer(id: Json, result: Json)
  case Failed(id: Json, code: Int, message: String)

object Rpc:
  def decode(line: String): Rpc          // a damaged line is a Failed, never a throw
  def encode(m: Rpc): String
  def messages: Stage[String, Rpc, Unit] // lines in, messages out

// the transport seam — a subprocess, a socket, a test mock
trait Link:
  def send(line: String): Unit ! Async
  def lines: Source[String]

// the client: an MCP server, as our own vocabulary
object Client:
  def connect(link: Link, name: String, version: String): Session ! Async
final class Session:
  def tools: Seq[ToolSpec] ! Async               // tools/list, cursor followed
  def call(c: ToolCall): String ! Async          // tools/call
  def interpret: Tool ==> ([X] =>> X ! Async)    // the cross-platform handler
  def handler(using CanBlock): Handler[Tool]     // the blocking one, JVM/Native

// the server: our tools, as an MCP server. A pure Stage — no I/O
object Server:
  def serve(info: Info, tools: Seq[ToolSpec],
            table: Map[String, ToolCall => String]): Stage[Rpc, Rpc, Unit]
  def over(link: Link)(stage: Stage[Rpc, Rpc, Unit]): Unit ! Async

// the transports (JVM)
object Stdio:
  def of(process: Process): Link     // a spawned server, over its pipes
  def std: Link                      // this process's own stdin/stdout
```

## Behavior
- [x] a JSON-RPC line decodes to its message; a damaged, truncated or
      non-JSON line decodes to `Failed` and the stream continues —
      totality, as everywhere in this library
- [x] `initialize` negotiates: the client sends its protocol version
      and info, the server answers with capabilities that declare
      `tools`, the client follows with `notifications/initialized`
- [x] a server whose `tools/list` answers over two pages is listed
      whole (the `nextCursor` is followed)
- [x] `tools/call` answers the joined text of the content blocks; an
      `isError` result answers `error: ...` rather than throwing — an
      unknown tool is an ANSWER the model can recover from, which is
      the convention `Handlers.tools` already set
- [x] an agent program runs UNCHANGED against MCP tools: the same
      program, one handler swapped, no mention of MCP in it
- [x] our tools served over MCP round-trip: `Server.serve` answers
      `initialize`, `tools/list` and `tools/call` for a `ToolSpec`
      table, and the schemas it publishes are the derived ones
      (`ToolSpec.jsonSchema`, no hand-written JSON Schema)
- [x] client and server compose: one `Server.serve` stage feeding one
      `Session` through an in-memory Link executes a real tool call,
      with no process and no socket in the test
- [x] the loop survives a server that dies mid-call (the Link ends):
      the pending call answers `error: ...`, the agent continues

## Design
Four files, in the layering the library already uses:

- `Rpc.scala` — the wire as DATA plus a `Stage[String, Rpc, Unit]`.
  Pure, cross-platform, no notion of MCP at all.
- `Mcp.scala` — the protocol's own vocabulary: method names, the
  initialize handshake, the `tools/list` and `tools/call` shapes,
  `ToolSpec` <-> `inputSchema`. Pure.
- `Client.scala` — `Session`, built on a `Link`. Correlation of
  request to answer is the one stateful thing here, and it is the only
  one.
- `Server.scala` — `serve` as a `Stage[Rpc, Rpc, Unit]`: awaits
  requests, tells answers, and holds the session state (initialized or
  not) the way every stage in this library holds state — as the
  transducer's parameter.
- `Stdio.scala` (JVM) — the process transport: lines out of a pipe are
  a `Source[String]`, which is what the rest already consumes.

The blocking `Handler[Tool]` and the cross-platform `interpret` are
the same door as everywhere else: a comonadic handler must ANSWER, so
it needs `CanBlock`; `translate` forwards into `Async` instead and
works where nothing may park.

## Out of scope
- resumability (`Last-Event-ID` and per-event ids on the SSE stream):
  a reconnecting client replays from the server's own record, which a
  server has to KEEP — a durability question, not a transport one
- elicitation (`elicitation/create`): the server asking the human, not
  the program — it needs a UI contract this library has no opinion on
- completion (`completion/complete`): argument autocompletion for
  prompts and resource templates
- resource TEMPLATES (RFC 6570 uri patterns)
- the HTTP/SSE transport (okay-llm already has the SSE half; the
  streamable-HTTP session layer is its own task)
- OAuth and any authorization
- JSON-RPC BATCHES (a top-level array of messages): the 2025-06-18
  revision removed them, and this implementation answers one message
  per line
- serving over anything but a byte-line link

## Decisions
- **An MCP server is a `Handler[Tool]`, not a new effect** — chosen
  because the agent program must not know. A new `Mcp` effect would
  make every agent that wants MCP tools mention MCP.
- **The server is a pure `Stage`, the transport is separate** — the
  protocol is then testable with no process, no socket and no clock,
  and the same stage serves stdio, a socket or a test.
- **JSON-RPC decoding is total** — a damaged line is a `Failed` value,
  because a stream that throws on one bad line loses every good line
  after it. This is the streaming-parse doctrine, applied to the wire.
- **The two error codes are read off the TREE, not off a throw** —
  found while implementing, and it is the total parser paying for
  itself: `Json.parse` never fails, it returns a tree with `JErr`
  NODES where the damage is. So `-32700 Parse error` is "the tree
  contains damage" and `-32600 Invalid Request` is "it parsed clean
  and is still not a message". A throwing parser can only tell the
  first, and only by throwing.
- **A prompt message can only be `user` or `assistant`** — MCP has no
  system role in prompts, so `Turn.System` goes on the wire as a user
  message and comes back as one. The loss is real and it is the
  protocol's, not ours; the alternative (dropping system turns) would
  lose more.
- **A response that owes an answer and does not give one becomes a
  `Failed`, not a wait** — found by writing the transport: over stdio
  a peer that never answers is a peer that is gone, but HTTP has
  statuses that mean exactly "no answer is coming" (202 to a request,
  404 for a dead session, any 4xx/5xx), and turning those into a
  message for the waiting id is the difference between an error and a
  hang. The link decodes the line it is SENDING to know which id is
  owed one.
- **The send is part of the program, never spawned** — found by a
  hang. Putting each request's write on its own fiber makes the
  writing thread ephemeral, and a `PipedOutputStream` remembers which
  thread wrote last: once that fiber has died and the reader finds the
  buffer empty, the pipe declares the write end dead and the session
  stops. Sending in the caller's own thread of control also keeps
  lines in the order the calls were made, which racing writers would
  not.
- **The transport suite runs over a SOCKET, not a pipe** — for the
  same reason, from the other side: a duplex session writes from
  whichever fiber is answering, so a transport with thread affinity is
  not one this protocol has. Testing on a pipe was testing the wrong
  thing.
- **A capability a server does not declare answers `MethodNotFound`**
  — found by a v1 test that used `resources/list` as its example of an
  unknown method and started passing for the wrong reason once v2
  landed. A polite empty list would have hidden the difference between
  "no resources" and "does not do resources"; the handshake already
  says which, and now the methods agree with it.
- **An unknown resource uri is a protocol ERROR, an unknown tool is an
  ANSWER** — chosen because of who is asking. A model picks a tool
  name and must be able to read its own mistake and retry; a program
  asks for a uri it got from `resources/list`, and a wrong one there
  is a bug, not a conversation.
- **The demo serves over stdio as a plain class, not through sbt** —
  `sbt -batch` keeps stdin for itself, so the documented invocation is
  `java -cp <classpath> okay.demo.RepoMcp <repo>`. Measured, not
  assumed: that command answers `initialize` and `tools/list` on the
  wire.

## v2 — resources and prompts (2026-09-01)

The other two capabilities, and each lands on a type this library
already has. That is the same test v1 passed: if MCP's concept has no
home here, the mapping is doing the work; if it has one, the protocol
is a transport detail.

- a **resource** is `okay.rag.Source(id, text)` — a document with an
  identity, which is exactly what the retriever indexes. So an MCP
  server's resources become a `Corpus`, and an agent RETRIEVES over a
  remote server's documents with nothing new in the agent.
- a **prompt** is `Seq[Turn]` — a conversation opening, which is
  exactly what okay-agent's context is made of. So a server's prompt
  is something the agent can be started from.

```scala
final case class Resource(uri: String, name: String,
                          description: String = "", mimeType: Option[String] = None)
final case class Prompt(name: String, description: String = "",
                        arguments: Seq[Prompt.Arg] = Nil)

// serving: what a server has, in one value
final case class Serving(info: Mcp.Info,
                         tools: Seq[ToolSpec] = Nil,
                         call: Map[String, ToolCall => String] = Map.empty,
                         resources: Seq[Resource] = Nil,
                         read: String => Option[String] = _ => None,
                         prompts: Seq[Prompt] = Nil,
                         prompt: (String, Map[String, String]) => Option[Seq[Turn]] = ...)
object Server:
  def serve(s: Serving): Stage[Rpc, Rpc, Unit]

// using: the client side, in our own vocabulary
final class Session:
  def resources: Seq[Resource] ! Async          // resources/list, cursor followed
  def read(uri: String): Option[String] ! Async // resources/read
  def corpus: Corpus ! Async                    // every resource, as okay-rag documents
  def prompts: Seq[Prompt] ! Async              // prompts/list
  def prompt(name: String, args: Map[String, String]): Seq[Turn] ! Async
```

### Behavior
- [x] `capabilities` declares only what the server actually has: a
      server with no prompts does not advertise prompts, and a client
      reading the handshake can tell
- [x] `resources/list` pages like `tools/list`, and `resources/read`
      answers the text of a uri; an unknown uri is a protocol error
      (unlike an unknown TOOL, which is an answer — the difference is
      that a model chooses tools and a program chooses uris)
- [x] a server's resources become a `Corpus`, and the retriever
      indexes it exactly as it indexes local files
- [x] `prompts/list` and `prompts/get` round-trip a `Seq[Turn]`,
      arguments substituted by the server
- [x] a v1 client talking to a v2 server, and the reverse, both work:
      the capabilities say what is there and nothing else breaks
- [x] what a server DECLARES is exactly what it answers: a method of a
      capability it does not have is `MethodNotFound`, not a polite
      empty list

## v3 — duplex: the server talks first (2026-09-01)

Everything left in MCP is one thing: the server initiating. Sampling,
roots, subscriptions, progress and cancellation are all "a message
arrives that nobody asked for", and the session's strict
request-then-answer loop could not carry them — it read lines until it
saw the answer it wanted and dropped the rest.

So the session becomes duplex, and the three shapes it now has to
serve are three things this library already owns:

- an incoming REQUEST is answered by the same kind of handler the
  server side uses — the client is a server too, and MCP is symmetric
- an incoming NOTIFICATION is a `Channel[Rpc.Notify]`: something
  arrives when it arrives, which is what a channel is FOR
- an outgoing push (a server telling a client a resource changed) is
  the stage's answers `merge` a channel of pushes — the readiness
  merge, used for the thing it was built for

And the one that is more than plumbing:

- **`sampling/createMessage` IS the `Model` effect.** A server asking
  the client for a completion is `Model.Complete(context, tools)`,
  answered by whatever `Handler[Model]` the client already has. An MCP
  server can use YOUR model, and in our vocabulary that is not new
  machinery — it is the handler that was already in scope.

```scala
// what a client answers when a server asks
final case class Peer(roots: Seq[Root] = Nil,
                      sample: Option[Handler[Model]] = None)
final case class Root(uri: String, name: String = "")

final class Session:
  def notifications: Channel[Rpc.Notify]        // everything unasked-for
  def subscribe(uri: String): Boolean ! Async
  def unsubscribe(uri: String): Boolean ! Async
  def rootsChanged(rs: Seq[Root]): Unit ! Async // notifications/roots/list_changed

object Client:
  def connect(link: Link, client: Mcp.Info, peer: Peer = Peer()): Session ! Async

// what a server pushes
final class Pushes:
  def resourceUpdated(uri: String): Unit        // to every subscriber
  def sample(context: Seq[Turn]): Reply ! Async // ask the client's model
object Server:
  def run(link: Link, serving: Serving): (Unit ! Async, Pushes)
```

### Behavior
- [x] a notification arriving while the client is IDLE is delivered:
      the reader is a fiber, not a side effect of asking something
- [x] a server that asks `roots/list` gets the client's roots, and a
      client whose roots change notifies the server
- [x] `sampling/createMessage` is answered by the client's
      `Handler[Model]` — the SAME handler an agent uses, with the
      conversation carried as `Seq[Turn]`
- [x] a client that declares no sampling handler refuses the request
      rather than hanging, and the server reads the refusal
- [x] subscribe, then a change on the server, delivers
      `notifications/resources/updated` for that uri and no other
- [x] unsubscribe stops it
- [x] answers, requests and notifications interleave on one wire
      without confusion: a sampling request arriving while a
      `tools/call` is in flight is answered, and the call still gets
      its answer
- [x] the capabilities of a CLIENT are declared too (roots, sampling),
      so a server knows what it may ask for

## v4 — the streamable HTTP transport (2026-09-01)

MCP's other standard transport, and the point is how little of it is
new: a `Link` is `send(line)` plus `lines: Source[String]`, so the
whole session and server machinery is untouched — what changes is
where the lines come from.

It lives in `okay-http`, beside the WebSocket link that module already
has, because the layering there is right: transports depend on the
protocol, not the other way round.

Streamable HTTP is one endpoint that answers three ways, and each maps
onto something okay-http already returns:

- a POST whose answer is a single JSON-RPC message — `application/json`
- a POST whose answer is a STREAM of them — `text/event-stream`,
  which is `Http.sse(response): Source[String]` verbatim
- a GET that opens a stream for what the server says unasked — the
  same `Http.sse`, drained onto the link's inbound channel

Plus one piece of state: the server may issue an `Mcp-Session-Id` on
the initialize response, and every later request must carry it.

```scala
// the client side: an endpoint AS a Link
object McpHttp:
  def link(http: Http, url: String)(using Scheduler): McpLink
final class McpLink extends okay.mcp.Link:
  def sessionId: Option[String]     // what the server issued, if it did
  def open(): Fiber[Unit]           // the GET stream for server-initiated messages

// the server side: a Serving AS a route
  def route(serving: Server.Serving)(using Scheduler, CanBlock)
  : Request => Response ! Async
```

### Behavior
- [x] a client over HTTP does the whole handshake and calls a tool,
      against a real `okay-http` server on a real port
- [x] the answer may arrive as `application/json` or as
      `text/event-stream`, and the client cannot tell the difference
      from the outside
- [x] `Mcp-Session-Id` is issued on initialize, carried on every later
      request, and an unknown one answers 404 — the protocol's own
      "reinitialize" signal
- [x] a POSTed NOTIFICATION answers 202 with no body (there is
      nothing to answer)
- [x] a GET stream delivers server-initiated messages onto the same
      session, so `session.notifications` works over HTTP exactly as
      it does over stdio
- [x] one MCP program, three wires: the SAME `Serving` and the same
      agent test pass over stdio, over a socket and over HTTP

- [x] a server that answers 202 to a REQUEST does not hang the caller
      — it is told no answer is coming

### Out of scope, and whose it is
The HTTP server cannot hold a stream open — `okay-http`'s JVM server
drains the response body before sending headers (`Http.bytes(res)`),
which is that module's decision and a reasonable one for REST. So the
route serves POST-with-a-JSON-answer, and answers 405 to GET. A server
that needs to PUSH over HTTP needs a streaming response in okay-http
first; the same server pushes fine over stdio and WebSocket today.

## v5 — the acceptance run (2026-09-01)

Everything so far tested this implementation against itself. The one
thing no amount of that answers is whether our reading of the protocol
matches the ecosystem's, so: a live run against the reference server
(`@modelcontextprotocol/server-everything`, over stdio, spawned by
npx), skipped when node is not there — the same bargain okay-agent's
TestLive makes with a model.

What is under test is not their server's behaviour but OUR assumptions:
that the handshake we send is accepted, that a real `tools/list`
decodes into `ToolSpec`s, that a real tool call comes back as text our
`Tool` handler can answer with, that real resources become documents
and real prompts become turns.

### Behavior
- [x] the handshake is accepted by a server nobody here wrote, and its
      capabilities decode
- [x] its tools decode into `ToolSpec`s, schemas and all, and calling
      one answers what it says
- [x] its resources become a `Corpus` and its prompts become `Seq[Turn]`
- [x] a notification the server sends BEFORE the initialize answer is
      not mistaken for one (the reference server does exactly this)
- [x] the run is skipped, not failed, where node is absent

## v6 — server push over HTTP (2026-09-01)

The gap v4 named and left: an HTTP server that cannot hold a stream
open cannot push, so `route` answered 405 to GET and a subscription
over HTTP delivered nothing. This closes it — and note where the fix
had to go, because it says what the gap really was.

MCP has TWO standard transports, stdio and streamable HTTP. WebSocket
is not one of them (okay-http's `Ws.link` is ours, for okay-to-okay),
so the push that matters is the GET event-stream, not a WebSocket
session.

Three pieces:

- **a streaming response** — okay-jetty's REST handler drained the
  body (`Http.bytes`) before sending the head, which is right for
  REST and fatal for SSE. It now writes chunk by chunk when the
  response says `text/event-stream`, which is exactly when a caller
  meant a stream. The other backends keep buffering until someone
  needs otherwise.
- **the GET stream in the route** — a session's pushes as an SSE body:
  a `Channel[Rpc]` becomes a `Source[Chunk[Byte]]`, which is what a
  `Response` body already is.
- **the pushes themselves** — `route` now takes the `Serving` and
  hands back the `Pushes` handle, so the owner tells subscribers a
  resource changed exactly as it does over stdio.

### Behavior
- [x] a POST body actually reaches the route (see below — it did not)
- [x] a GET with `accept: text/event-stream` opens a stream instead of
      405, and the session id ties it to that client's session
- [x] a resource update pushed after a subscription arrives on the
      client's `notifications` channel — over HTTP, with nothing in
      the client changed
- [x] a client that never opens the GET stream still works; the
      pushes simply have nowhere to go
- [x] a streaming response is written incrementally: the client sees
      the first event before the source has ended (asserted by a
      source that does not end until asked)

### What it found
okay-jetty's REST handler never read the request body: `requestOf`
built a `Request` from the method, path and headers and stopped. Every
POST route on that backend therefore saw `Body.Empty`, and an MCP
route answered every message as though it were damaged. The body is
read on the REST path only — the same function also builds the request
a WebSocket UPGRADE is dispatched on, where reading would consume it.

## Results
Shipped 2026-09-01. Five files, 22 tests in okay-mcp (wire 5, server
as a pure stage 8, session over channels 5, the transport over real
byte pipes 1, agent-over-MCP 3), all eight behavior items covered.

- the server needs no I/O to be exercised: `TestServer` drives the
  whole protocol through `Writer.of(List[Rpc])`
- `TestAgentOverMcp` runs the SAME `Agent.converse` program against a
  local tool table and against an MCP server and asserts the answers
  are equal — the module's thesis, as an assertion
- `okay.demo.RepoMcp` serves this repository over stdio; verified by
  piping real JSON-RPC lines into it

v2 (resources and prompts) shipped the same day: 11 more tests (server
side 6, session and the two bridges 5), 33 in the module. The bridges
are the point and both are asserted — a server's resources go through
`Corpus` into the SYMBOL INDEX (a remote file's `add` is found by name
at its uri), and a server's prompt comes back as `Seq[Turn]`.
`RepoMcp` now serves all three capabilities: the agent's two tools,
every indexed file as a resource, and an `explain` prompt that finds a
definition and opens a conversation about it.
