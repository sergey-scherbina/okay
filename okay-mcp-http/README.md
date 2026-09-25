# okay-mcp-http — MCP over okay-http's wires

`WsLink` (a WebSocket as an MCP `Link`), `NioLink` (a bare TCP socket),
`McpHttp` (streamable HTTP, both ends) and `McpAuth` (the OAuth door in
front of it). Packages unchanged from where they were before
http-mcp-agent-edge: `okay.http.McpHttp`, `okay.security.McpAuth`.

| | |
|---|---|
| [`docs/modules/okay-mcp-http.md`](../docs/modules/okay-mcp-http.md) | the pieces, and why this is a module |
| [`okay-mcp/`](../okay-mcp) | the protocol these carry |
| [`okay-http/`](../okay-http) | the wires |
