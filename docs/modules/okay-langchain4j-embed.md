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
Embedding` is the shape a caller takes as a constructor parameter —
no new seam, a constructor argument. okay-intent's tiers name exactly
this function as their dependency (`String => Embedding`, not "a
server"), so where the encoder is in process the probe is the
cheapest tier and not the dearest.

**As okay-rag's effect.** `Langchain4jEmbed.handler(model):
Handler[Embed]` wraps the same call as an `Embed` handler, for a
program built against `Retrieve.vector` — one call per text, matching
the model's own API (no batch endpoint to prefer).

**Where this is used.** okay-intent's `Fit`/`Probe` take it directly:
a classifier fitted over real embeddings rather than over
`Vectors.hashing()`, which is character-trigram counting and scores
surface overlap rather than meaning. The measured difference is in
specs/intent-classify.md.