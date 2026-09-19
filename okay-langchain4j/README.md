# okay-langchain4j

The interop sentence's Model half (specs/llm-agentic.md, "Interop, not reimplementation"): their `ChatModel` becomes a `Handler[Model]` — every program written against the effect (compaction, search, grounding, the durable journal) runs over langchain4j's provider breadth unchanged. Depends on their CORE only; the caller constructs any of their provider models (OpenAI, Anthropic, Gemini, Bedrock, Ollama, Azure, Mistral…) and hands it in.

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-langchain4j.md`](../docs/modules/okay-langchain4j.md) | what it is, and the reasoning |
| [`specs/llm-agentic.md`](../specs/llm-agentic.md) | the design and its decisions |
