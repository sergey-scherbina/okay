# okay-scala2-rag

okay-rag for **Scala 2.13**. Splitting and keyword search are okay-rag's own
plain functions, used directly (`Ingest.segment`, `Keyword.index`,
`Keyword.search`). The vector side is a `VectorIndex`: the store and the
embedding model, where the model is a plain function from a batch of
texts to one vector per text.

| | |
|---|---|
| `Rag.memory(embed)` | an index in memory |
| `index.add(sources)` | split, embed in batches, store |
| `index.search(query, k)` | the nearest segments |
| `index.hybrid(keywords, query, k)` | vector and keyword hits fused by reciprocal rank |
| `Rag.pgvector(db, table, dim, embed)` | the same index in Postgres with pgvector; its operations are programs |

The walkthrough is section 8n of
[okay from Scala 2.13](../scala2.md#8n-models-retrieval-mcp-okay-llm-okay-rag-okay-mcp), and the signatures are in
[okay-scala2](okay-scala2.md#api-reference).
