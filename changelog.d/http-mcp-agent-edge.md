## http-mcp-agent-edge - okay-http no longer carries an agent, an LLM client and RAG

okay-http depended on okay-mcp (for the MCP transports it hosted), and
okay-mcp depends on okay-agent → okay-llm, okay-rag, okay-frame: every
HTTP server and client built on okay carried an agent runtime, an LLM
client and RAG. Found from okay-watch, whose rule is "no language model
on the alert path". okay-http also depended on okay-persist for
McpHttp's journal alone, and on okay-llm for the SSE framing.

- New module **okay-mcp-http** (JVM/JS): `okay.mcp.WsLink` (was
  `Ws.link`), `okay.mcp.NioLink` (was `Nio.link`), `okay.http.McpHttp`
  and `okay.security.McpAuth` — packages unchanged, so imports read the
  same; a user's build names okay-mcp-http. Transports depend on the
  protocol and on the wire; neither depends on the other.
- okay-http depends on the core and okay-codec only. `okay.Sse.events`
  (okay-stream, beside `Lines`) is the SSE framing; `okay.llm.Sse.events`
  delegates to it.
- Tests moved with the code (TestMcpHttp, TestMcpPushServer, TestMcpAuth,
  TestMcpReadme, TestMcpTools; TestMcpLinks and TestMcpDoor split out of
  TestWs, TestNio and TestDoors). okay-jetty and okay-netty take
  okay-mcp-http in Test, okay-demo in Compile; okay-security takes
  okay-agent in Test (TestReadmes).
- docs/modules/okay-mcp-http.md, the index row, READMEs, okay-http.md,
  okay-mcp.md, okay-security.md, specs/http.md and specs/mcp.md.
- Gate: `affected master` 7271 test results GREEN, no warnings (a first
  run red on clojure-go-block-timeout-under-load, second sighting
  recorded; a second on an E198 master already fixed by fda2f1314).

Landed as 4269b9156.
