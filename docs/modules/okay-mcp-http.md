# okay-mcp-http

MCP over okay-http's wires (http-mcp-agent-edge, 2026-09-25).

| | |
|---|---|
| `okay.mcp.WsLink(socket)` | a WebSocket AS an MCP `Link`: frames around lines |
| `okay.mcp.NioLink(conn)` | a bare TCP connection as a `Link`: a line per message, no HTTP (JVM) |
| `okay.http.McpHttp` | streamable HTTP: `link(http, url)` to reach a server, `route(serving)` to serve, `routed(serving)` when the server also pushes (JVM; the push half needs okay-jetty) |
| `okay.security.McpAuth` | the door in front of it: RFC 9728 metadata, the 401 that teaches, discovery, `connect`, per-tool authorization (JVM) |

**Depends on:** okay-mcp, okay-http, and okay-security on the JVM.

## Why a module of its own

Transports depend on the protocol, and on the wire they run over —
and neither of those should depend on the other. These four lived in
okay-http (and `McpAuth` in okay-security), so okay-http depended on
okay-mcp, which depends on okay-agent, okay-llm, okay-rag and
okay-frame: every HTTP server and client built on okay carried an
agent runtime, an LLM client and RAG, and an auditor reading a
product's classpath (okay-watch: "no language model on the alert
path") had to be told none of it runs. okay-http also depended on
okay-persist for `McpHttp`'s journal alone.

The packages did not move — `okay.http.McpHttp` and
`okay.security.McpAuth` read the same in an import — so a program that
used them changes one thing: its build names `okay-mcp-http`. The two
socket links were methods (`Ws.link`, `Nio.link`) and are values here
(`WsLink`, `NioLink`), because a method cannot be added to another
module's object.

The SSE event framing the HTTP client reads with moved too, from
okay-llm to okay-stream (`okay.Sse.events`, beside `Lines`);
`okay.llm.Sse.events` is that.
