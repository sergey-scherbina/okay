- [ ] okay2-http — okay-http's HTTP half on okay2: `Route` (typed routes
      over Schema — Scala 3 derives them by Mirror, here a macro), `Http`
      (request/response, Acceptance, Urls), `Ws`, the NIO `Server` and
      the client `Transports` on the JVM (JS client after). NOT McpHttp
      and TsClient: okay-http depends on okay-mcp for those, and the MCP
      stack is not ported. Its Schema is okay2-codec (landed, stage 41); its SQL neighbour okay2-sql landed at stage 42. (2026-09-25)
