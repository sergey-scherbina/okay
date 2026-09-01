# okay-langchain4j

The interop sentence's Model half (specs/llm-agentic.md, "Interop,
not reimplementation"): their `ChatModel` becomes a `Handler[Model]`
— every program written against the effect (compaction, search,
grounding, the durable journal) runs over langchain4j's provider
breadth unchanged. Depends on their CORE only; the caller constructs
any of their provider models (OpenAI, Anthropic, Gemini, Bedrock,
Ollama, Azure, Mistral…) and hands it in.

| | |
|---|---|
| `Langchain4j.message` | Turn ↔ their ChatMessage, choice for choice with Provider.message (a Summary rides as a system message) |
| `Langchain4j.declaration` | the fourth algebra's JSON schema walked into their JsonSchemaElement tree — `required` intact, so a DEFAULTED field stays omittable across the interop (codec-defaults holds) |
| `Langchain4j.reply` | text + tool requests, arguments parsed by our total Json |
| `Langchain4j.model` | the comonadic handler (a virtual thread parks in their blocking client); `count` stays LOCAL — the compaction budget never costs a round trip |

Proven against a scripted ChatModel recording what it saw — no
network anywhere. The EmbeddingStore half is filed as
rag-langchain4j, gated on a consumer naming a store.

`Langchain4j.wired` is the handler-awaiting-environment form:
`ChatModel ?=> Handler[Model]` — store it, ship it,
`provide(chatModel) { ... }` at the edge.
