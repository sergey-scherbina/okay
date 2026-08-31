# okay-mcp

The Model Context Protocol, both ends (specs/mcp.md).

MCP is JSON-RPC 2.0 over a byte stream, and this module is small
because the two ends were already in the library's vocabulary:

- **an MCP server is a `Handler[Tool]`** — so an agent program does
  not change by one character when its tools come from a server
  instead of a local table. `TestAgentOverMcp` runs the same program
  both ways and asserts the answers are equal.
- **our tools ARE an MCP server** — `ToolSpec` + a
  `Map[String, ToolCall => String]` is exactly what a server serves,
  and the schemas on the wire are the DERIVED ones (`Schema[A]`), so a
  published tool cannot drift from its parser.

## The pieces

| | |
|---|---|
| `Rpc` | JSON-RPC 2.0 as data, plus `Stage[String, Rpc, Unit]` framing. Decoding is total: a damaged line is a `Failed`, never a throw |
| `Mcp` | the protocol vocabulary: methods, the handshake, `inputSchema` <-> `ToolSpec` |
| `Client` / `Session` | a server as `tools`, `call`, and a `Handler[Tool]` (or `interpret`, where nothing may park) |
| `Server` | `Serving` is everything a server has (tools, resources, prompts); `serve` is a PURE `Stage[Rpc, Rpc, Unit]` — the whole protocol is testable with no process, socket, clock or thread; `over` is the only part that touches a wire |
| `Stdio` (JVM) | the transport: a spawned server's pipes, or this process's own stdin/stdout |

## The three capabilities, and where each lands

| MCP | here | so that |
|---|---|---|
| tool | `ToolSpec` + `Map[String, ToolCall => String]` | an agent program is unchanged when its tools are remote |
| resource | `okay.rag.Source` (a `Corpus`) | the retriever indexes a server's documents like local files |
| prompt | `Seq[Turn]` | a server's prompt is something an agent can be started from |

Capabilities are computed from what a server actually has, so a
tools-only server does not advertise resources, and a client reads the
handshake (`session.has("prompts")`) rather than guessing.

## Using an MCP server from an agent

```scala
val link = Stdio.of(Stdio.spawn(Seq("npx", "-y", "@modelcontextprotocol/server-everything")))
val session = Client.connect(link, Mcp.Info("okay", "1")).runWith
given Handler[Tool] = session.handler          // the only line that changes
Agent.converse("...", session.tools.runWith)   // its tools, discovered
```

## Serving ours

`okay.demo.RepoMcp` is this repository as an MCP server, all three
capabilities: the demo agent's two tools (a definition by name, a file
by path), every indexed file as a resource, and an `explain` prompt
that finds a definition and opens a conversation about it.
Launch it as a plain class, which is what an MCP client does (`sbt
-batch` keeps stdin for itself):

```bash
CP=$(sbt -batch --error "export okayDemo/Runtime/fullClasspath" | tail -1)
java -cp "$CP" okay.demo.RepoMcp /path/to/repo
```

```scala
// and the other two, in our own vocabulary
val corpus = session.corpus.runWith                    // resources as documents
val opening = session.prompt("explain", Map("name" -> "transduce")).runWith
```

## Duplex: when the server talks first

MCP is symmetric — a server asks the client for its roots, asks it to
sample from its model, and tells it when a resource changed. So a
session is a reader FIBER with three destinations: an answer completes
the request waiting for it, a notification lands on a `Channel`, and
an incoming request is answered by the `Peer` (on its own fiber, so a
slow completion cannot stop the reader).

```scala
val session = Client.connect(link, Mcp.Info("okay", "1"), Duplex.Peer(
  roots = Seq(Mcp.Root("file:///work")),
  sample = Some(modelHandler)))          // the SAME Handler[Model] an agent uses
session.subscribe("okay://a").runWith
session.notifications.toLazyList.foreach(n => ...)   // updates, progress, list-changed

// serving, with what a server says unasked
val (program, pushes) = Server.duplex(link, serving)
pushes.resourceUpdated("okay://a")       // to every subscriber, and only them
```

`sampling/createMessage` is the `Model` effect: a server asking for a
completion is `Model.Complete(context, tools)`, answered by whatever
handler the client already had. An MCP server borrows YOUR model, and
nothing new interprets it. A client with no model refuses rather than
hanging, and the server reads the refusal.

The outbound side of a server is the stage's answers `merge` a channel
of pushes — the readiness merge, one fiber each.

## Not here
elicitation (the server asking the human — it needs a UI contract
this library has no opinion on), completion (argument autocompletion),
resource templates, the streamable-HTTP transport, OAuth, and
JSON-RPC batches (removed in the 2025-06-18 revision). Progress and
cancellation arrive as ordinary notifications on the channel; nothing
interprets them for you.
