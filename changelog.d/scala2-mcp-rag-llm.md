## scala2-mcp-rag-llm - okay-llm, okay-rag, okay-mcp from Scala 2.13

Stage 15.5 of specs/scala2-facade.md (operator: "делай всё что возможно
чтобы работало в скале 2").

- okay-scala2-llm: `Llm.anthropic` / `Llm.openAi` stream a completion as a
  `Source[String]`, `Llm.first[A]` reads only until the text decodes as
  an `A`, and `Llm.transport` builds a transport from a function (Scala 2
  cannot implement okay-llm's `Transport`, whose method answers a
  program).
- okay-scala2-rag: splitting and keyword search are used directly. The
  vector side is `VectorIndex` (`Rag.memory(embed)`, `add`, `search`,
  `hybrid`), with the embedding model as a plain function.
  `VectorStore[okay.Pure]` cannot appear in a Scala 2-facing signature
  at all: `Pure` is a top-level alias, invisible even inside a type
  argument.
- okay-scala2-mcp: `McpClient` / `McpServer` / `McpLink`, JSON as text
  (`ToolCall` and `ToolSpec` carry okay-codec's `Json`, which Scala 2
  cannot read), a server's tools from okay-scala2-agent's `Tools`.
- Not wrapped: `PgVector`. Its wrapper was written and removed before
  landing, because a Scala 2 test of it needs a live Postgres and the
  default gate runs none.
- 7 tests across `TestLlmFromScala2`, `TestRagFromScala2` and
  `TestMcpFromScala2`. The first run hung on the test's own bug (an
  unescaped `"` in a scripted SSE payload), found with a thread dump;
  the scripted stream is now bounded so that failure fails instead of
  hanging.
- Docs: section 8n of docs/scala2.md (copied from the probes; RRF cited
  as Cormack, Clarke and Büttcher, SIGIR 2009), three module pages, the
  API reference, a new row in section 10, and the spec's stage 15.5.
