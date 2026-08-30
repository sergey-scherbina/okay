# okay-agent

> Agents as programs: a tool call is an effect operation, the
> conversation is a fold, search over completions is `Logic`, and
> every policy question is answered by a handler.

Depends on: `okay-llm` (and through it the whole total text stack).

## Guide

**Three effects, no message list.** `Model` (complete, count),
`Tool` (call), `Context` (remember / recall / mark / restore). The
agent program never holds a conversation — it performs Context
operations, and the HANDLER owns the policy. That inversion is the
module: compaction becomes automatic, tool execution becomes
swappable, and testing needs no mocking framework, because a
different handler IS the mock.

**Context management is an algebra, not a container.** A policy is an
`Aggregator[Turn, S, Seq[Turn]]` — P1's type, unchanged:

- `add` folds ONE turn in O(new tokens), so the compacted view is
  ready on every `recall`. Compaction is the default path, not an
  emergency branch: being over budget only changes what `present`
  returns.
- `merge` makes summarization hierarchical and parallel — halves of a
  history combine into the whole (tested split-point-agnostic), so a
  long history compacts on fibers or across a cluster with the same
  value.
- `zip` runs several policies in one pass (a window, a running
  summary, a fact extractor).

`Compact.window` pins system turns, evicts by SUBTRACTION (a `Group`,
so no re-counting) and REPORTS the elision instead of pretending the
conversation began mid-sentence — and the marker counts against the
budget, because it goes on the wire. Token counts come from
okay-llm's `Bpe` Scan: local, incremental, no provider call.

**State is threaded, not held.** `Memory.handle` carries the
accumulator through the walk (mirroring `State.handle`), so a
continuation invoked twice — a multi-shot search branch, a backtrack
— sees the context as it was AT THAT POINT. Handler ORDER then says
what you mean: `Memory` inside the search gives each branch its own
conversation; outside, one shared transcript. No flag, just where you
run it.

**Search over completions.** Sampling is nondeterminism, so
`Search.bestOf` (choose among N, `once` to commit), `Search.validated`
(the soft cut: use every answer that validates, re-prompt only when
none did), `Search.all` + `majority` (self-consistency) are
one-liners over `Choose` and `Logic`.

**Tool declarations derive.** `ToolSpec.jsonSchema` is the FOURTH
algebra over `Schema` (after JSON, CBOR and YAML), and
`ToolSpec.args` decodes with the SAME schema — a tool's declaration
cannot drift from its parser.

## Tutorial

```scala
import okay.agent.*
import okay.codec.Schema

case class SearchArgs(query: String, limit: Option[Int])
given Schema[SearchArgs] = Schema.derived
val spec = ToolSpec[SearchArgs]("search", "look something up")  // schema derived

// the agent: no message list, no truncation, no "if context too big"
val conversation: String ! Agent = Agent.converse("find okay", Seq(spec))

// policy lives here — and only here:
val (state, ctx) = Handlers.context(Compact.window(4000)(Compact.chars))
val tools = Handlers.gated(Map("search" -> { c =>
  ToolSpec.args[SearchArgs](c).fold(e => s"bad args: $e", a => search(a.query))
}))(approve = c => askTheHuman(c))          // or: execute, sandbox, record
```

Swap the model handler for a scripted one and the same program is a
unit test; swap the tool handler for a recorder and it is a fixture.

Search strategies:

```scala
Search.bestOf(4)(complete)(_.isValidJson)         // sample until valid, commit
Search.validated(attempt)(use)(reprompt)          // soft cut: fallback only if none
Search.majority(Search.all(5)(complete)(ok))      // self-consistency
```

## API reference

| member | signature | meaning |
|---|---|---|
| `Turn` | `System / User / Assistant / Result / Summary` | one turn; Summary is a compaction artifact |
| `ToolCall` / `Reply` | `(id, name, args)` / `(text, calls)` | what the model asks for and answers |
| `Model` | `Complete(context, tools)` / `Count(text)` | the model effect (counting is local) |
| `Tool` | `Call(call)` | one invocation; the handler decides what that means |
| `Context` | `Remember / Recall / Mark / Restore` | the conversation as effects |
| `Agent` | `Model + (Tool + (Context + Async))` | the row |
| `Agent.converse` | `(message, tools, maxSteps) => String ! Agent` | the loop: ask, run tools, repeat |
| `Compact.window` | `(budget)(size) => Aggregator[Turn, Window, Seq[Turn]]` | pinned system turns, subtract-on-evict, reported elision |
| `Compact.all` | keep everything | the baseline |
| `Memory.handle` | `(policy)(init)(prog) => (S, A) ! F` | state-threading context handler |
| `Memory.run` / `runWithState` | the common cases | answer, or answer + transcript |
| `Handlers.context/tools/gated/recording/scripted/observing/counter` | the policies | execute, approve, record, script, count |
| `ToolSpec` / `.jsonSchema` / `.args` | derive a declaration, decode a call | one Schema for both |
| `Search.bestOf / validated / all / majority` | strategies over Choose + Logic | best-of-N, soft cut, self-consistency |

## Gotchas

- `Handler.union` (core) assembles one handler per effect into a row
  handler; it is an explicit combinator, not a given, because a given
  over a union type lambda crashes the 3.7.1 type comparer.
- Effect-row ORDER matters for the shapes handlers expect; unions are
  ACI, so an ascription with explicit type arguments re-associates.
- With the v1 `Handlers.context` (a mutable cell) multi-shot search
  would leak state between branches — use `Memory` for anything that
  backtracks.

## Grounded recall (okay-rag)

`Grounded.context(policy, retriever, budget, share)` replaces the
plain context handler with one that also retrieves: the last question
drives a search, passages become turns, and conversation and code
share ONE budget. This is where the design's third claim cashes out —
retrieval and memory were never two subsystems here, so the trade-off
between history and passages is a policy you can test rather than an
accident you discover in production. Costs no tool call per turn.

## Not yet (specs/llm-agentic.md)

Lineage-backed tool results (the model sees a projection, a follow-up
re-observes the full stream), streaming validation that cuts
generation the moment the value is structurally complete, durability
by replay (specs/llm-agentic.md), and provider handlers — an
OpenAI-compatible one covers most of the market, including the local
runtimes.
