# okay-langchain4j-embed

> Their `EmbeddingModel` as our `Embedding` seam (specs/llm-agentic.md,
> rag-langchain4j) — the interop sentence's OTHER half, narrower than
> the module name suggests: this one is about VECTORS, not chat.

Depends on: `okay-rag`, langchain4j's local ONNX MiniLM model (a real
~90MB download — this module is deliberately kept OUT of the root
`.aggregate(...)` list in build.sbt; build/test it explicitly with
`sbt okayLangchain4jEmbed/test`).

## Guide

**As a plain function.** `Langchain4jEmbed.embed(model): String =>
Embedding` is the exact shape `okay-match`'s `MemoryMatch`/`SqlMatch`
already take as their `embed` constructor parameter — no new seam, a
constructor argument.

**As okay-rag's effect.** `Langchain4jEmbed.handler(model):
Handler[Embed]` wraps the same call as an `Embed` handler, for a
program built against `Retrieve.vector` — one call per text, matching
the model's own API (no batch endpoint to prefer).

**Where this is used.** `okay-demo-embed` (docs/modules/okay-demo.md)
wires `Langchain4jEmbed.embed(model)` into `ChatDemo.marketOf`'s
`embed` parameter, proving the chat demo's attribute registry catches
near-synonym duplicates ("разработчик"/"программист") that
`Vectors.hashing()`, the demo's zero-dependency default, cannot.
