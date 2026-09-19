# okay-agent — agents as programs

A tool call is an effect operation, the conversation is a fold, and
every policy question is answered by a HANDLER. The agent program
never holds a message list: it performs `Context` operations, and the
handler owns what remembering means. That inversion is the module —
compaction stops being an emergency branch, tool execution becomes
swappable, and a test needs no mocking framework, because a different
handler IS the mock.

## The pieces

| | |
|---|---|
| `Model` | complete, count — what a language model does, as an effect |
| `Tool` | call — where it runs is the handler's business, which is why an MCP server drops in unchanged |
| `Context` | remember / recall / mark / restore — the conversation, as operations rather than a list |
| `ToolSpec` | a tool declared by its `Schema`: the published JSON Schema and the argument parser are the same derivation, so they cannot drift |
| `Handlers` | the policies: `tools`, `scripted`, `recording`, `context(policy)`, gates |
| `Compact` | a context policy as an `Aggregator[Turn, S, Seq[Turn]]` — folding one turn at a time, so the compacted view is ready on every recall |

## A conversation with a tool in it

```scala
import okay.agent.*

final case class SearchArgs(query: String, limit: Option[Int])
given Schema[SearchArgs] = Schema.derived
val searchSpec = ToolSpec[SearchArgs]("search", "search the corpus")

val tools = Handlers.tools(Map("search" -> { c =>
  // the SAME Schema that declared the tool decodes its arguments
  ToolSpec.args[SearchArgs](c).fold(e => s"bad args: $e",
    a => s"${a.limit.getOrElse(10)} hits for '${a.query}'")
}))
val (state, ctx) = Handlers.context(Compact.all)

// `run` here is one handler per effect, unioned along the row —
// twelve lines, and TestAgent carries them verbatim
run(Agent.converse("find okay", Seq(searchSpec)))(model, tools, ctx)
```

Three handlers, three independent decisions: which model answers, what
a tool call actually does, and what the conversation remembers. Swap
any one and the program above does not change by a character — that is
how the same agent runs against a scripted model in a test, a real one
in production, and a remote MCP server's tools either way.

An unknown tool is an ANSWER rather than a fault: the model asked for
something impossible, reads that, and tries again.

## Further

| | |
|---|---|
| [`docs/modules/okay-agent.md`](../docs/modules/okay-agent.md) | the guide: the three effects, context as an algebra, the laws |
| [`specs/llm-agentic.md`](../specs/llm-agentic.md) | the design and its decisions |
| [`okay-mcp/`](../okay-mcp) | the same `Tool` effect, answered by somebody else's server |
| [`okay-llm/`](../okay-llm) | the model seam underneath |
