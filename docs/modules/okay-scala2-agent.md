# okay-scala2-agent

okay-agent for **Scala 2.13**. okay-agent's data (`Turn`, `Reply`) is
readable from Scala 2. The agent program and the assembly of its
handlers are not, and this module provides them:

| | |
|---|---|
| `Chat` | okay-agent's loop (ask, run the tools, repeat), with a conversation that persists across `say`; `approve` gates each tool call |
| `Model` | scripted (`scripted`, `scriptedCalls`) or a provider (`anthropic`, `openAi`) |
| `Tools` | declared and implemented at once; arguments decoded by the `Schema` that declares them |
| `Policy` | `all`, or `window(budget)` with pinned system turns and a reported elision |
| `Call` | a tool call as Scala 2 sees it: the arguments as JSON text |

The walkthrough is section 8d of
[okay from Scala 2.13](../scala2.md#8d-agents-a-model-tools-a-conversation),
and the signatures are in [okay-scala2](okay-scala2.md#api-reference).
