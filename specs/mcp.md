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

## Out of scope (v1)
- resources, prompts, sampling, roots, completion, progress and
  cancellation notifications — the shapes are list+fetch, the same as
  tools, and they are follow-ups rather than design questions
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
- **The demo serves over stdio as a plain class, not through sbt** —
  `sbt -batch` keeps stdin for itself, so the documented invocation is
  `java -cp <classpath> okay.demo.RepoMcp <repo>`. Measured, not
  assumed: that command answers `initialize` and `tools/list` on the
  wire.

## Results
Shipped 2026-09-01. Five files, 22 tests in okay-mcp (wire 5, server
as a pure stage 8, session over channels 5, the transport over real
byte pipes 1, agent-over-MCP 3), all eight behavior items covered.

- the server needs no I/O to be exercised: `TestServer` drives the
  whole protocol through `Writer.of(List[Rpc])`
- `TestAgentOverMcp` runs the SAME `Agent.converse` program against a
  local tool table and against an MCP server and asserts the answers
  are equal — the module's thesis, as an assertion
- `okay.demo.RepoMcp` serves this repository's own two tools
  (definition, read_file) over stdio; verified by piping real
  JSON-RPC lines into it
