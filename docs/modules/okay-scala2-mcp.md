# okay-scala2-mcp

okay-mcp for **Scala 2.13**. Arguments and schemas cross as JSON text, and a
server's tools are okay-scala2-agent's `Tools`, so one declaration serves a
local agent and an MCP server.

| | |
|---|---|
| `McpClient.connect(link, name, version)`, `McpClient.spawn(command, ...)` | a session: tools, call, resources, read, prompts |
| `McpServer.run(link, name, version, tools, resources)` | serve until the link closes |
| `McpLink.pair()`, `McpLink.of(in, out)` | an in-process link, or one over a process's stdio |

The walkthrough is section 8n of
[okay from Scala 2.13](../scala2.md#8n-models-retrieval-mcp-okay-llm-okay-rag-okay-mcp), and the signatures are in
[okay-scala2](okay-scala2.md#api-reference).
