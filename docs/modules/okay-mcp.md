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
| `Server` | `serve` is a PURE `Stage[Rpc, Rpc, Unit]` — the whole protocol is testable with no process, socket, clock or thread; `over` is the only part that touches a wire |
| `Stdio` (JVM) | the transport: a spawned server's pipes, or this process's own stdin/stdout |

## Using an MCP server from an agent

```scala
val link = Stdio.of(Stdio.spawn(Seq("npx", "-y", "@modelcontextprotocol/server-everything")))
val session = Client.connect(link, Mcp.Info("okay", "1")).runWith
given Handler[Tool] = session.handler          // the only line that changes
Agent.converse("...", session.tools.runWith)   // its tools, discovered
```

## Serving ours

`okay.demo.RepoMcp` is this repository as an MCP server — the demo
agent's two tools (a definition by name, a file by path) over stdio.
Launch it as a plain class, which is what an MCP client does (`sbt
-batch` keeps stdin for itself):

```bash
CP=$(sbt -batch --error "export okayDemo/Runtime/fullClasspath" | tail -1)
java -cp "$CP" okay.demo.RepoMcp /path/to/repo
```

## Not here (v1)
resources, prompts, sampling, roots, progress/cancellation, the
streamable-HTTP transport, OAuth, and JSON-RPC batches (removed in the
2025-06-18 revision). Capabilities are declared honestly, so a client
knows to ask only for tools.
