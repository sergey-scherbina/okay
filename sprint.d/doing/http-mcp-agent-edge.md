- [ ] http-mcp-agent-edge — `okay-http` depends on `okay-mcp` (for
      `Ws.link`, `Nio.link` and `McpHttp`, the MCP-over-HTTP transport),
      and okay-mcp depends on `okay-agent`, which depends on okay-llm,
      okay-rag and okay-frame. So every HTTP user — a server, a client,
      okay-jetty, okay-resilience, okay-ops — carries an agent, an LLM
      client and RAG. Found 2026-09-25 from okay-watch, whose invariant
      is "no language model on the alert path": its jar holds
      okay/agent, okay/llm, okay/rag and okay/mcp (1.2 MB) that nothing
      calls, and an auditor reading the classpath has to be told so.
      Fix: invert — the MCP transports move to okay-mcp (or an
      okay-mcp-http) that depends on okay-http; okay-http keeps the
      socket shapes they are built over. Settled when
      `okayHttpJVM/dependencyClasspath` has no okay-agent.
