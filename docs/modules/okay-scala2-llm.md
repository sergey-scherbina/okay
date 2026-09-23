# okay-scala2-llm

okay-llm for **Scala 2.13**: the layer under okay-scala2-agent's `Model` and
`Chat`. One completion, token by token, as a `Source[String]`:

| | |
|---|---|
| `Llm.anthropic(transport, apiKey, model, messages)` | Anthropic's Messages API, streamed |
| `Llm.openAi(transport, apiKey, model, messages)` | OpenAI's chat completions, or any server speaking them, streamed |
| `Llm.first[A](tokens)` | read until the text decodes as an `A`, then stop reading |
| `Llm.http`, `Llm.transport(post)` | the JVM transport, or one from a function answering the response's lines |

The walkthrough is section 8n of
[okay from Scala 2.13](../scala2.md#8n-models-retrieval-mcp-okay-llm-okay-rag-okay-mcp), and the signatures are in
[okay-scala2](okay-scala2.md#api-reference).
