## scala2-pgvector - okay-rag's PgVector from Scala 2.13

Operator: "Доделай" (2026-09-23): the one store scala2-mcp-rag-llm left
out, because a Scala 2 test of it needed a live Postgres.

- `Rag.pgvector(db, table, dim, embed)` over okay-scala2-sql's `Db` answers
  a `PgIndex` (`add`, `search`, `hybrid`, `size`, each `Eff[Async, _]`);
  the extension and table are created if absent. okay-scala2-rag now
  depends on okay-scala2-sql.
- `Embed` has no TypeableK, so it cannot be translated into Async
  programs; the handler is `Handler.union[okay.Async, Embed]` inside
  `Async.delay`. The embedding handler is its own `Embedder` now, shared
  by the memory and the Postgres index.
- `TestRagLiveFromScala2` (Live, `sbt integrationTest`), run here against
  `pgvector/pgvector:pg16` in a throwaway container: GREEN, and its
  nearest segment is the memory index's.
- Docs: section 8n of docs/scala2.md (the "not wrapped" note replaced),
  the module page, the API reference, the spec's stage 15.5b.
