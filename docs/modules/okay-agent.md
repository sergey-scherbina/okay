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
- `merge` makes compaction hierarchical and parallel — halves of a
  history combine, so a long one compacts on fibers or across a
  cluster. Note the honest limit: a LOSSY policy's merge cannot equal
  the sequential fold (evicting inside the right half discards what
  the whole fold would have kept), so what it guarantees is a valid
  window over the join — within budget, in order, a true suffix. The
  exact-merge law holds for `Compact.all` and for the statistics
  aggregators.
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

## Two providers, and the portable form

`Provider.openAi` and `Provider.anthropic` are both `Handler[Model]`,
speaking protocols that differ in every way that could have leaked
into the effect (system as a field or a message, content as blocks or
a string, `input_schema` or `parameters`, arguments as an object or a
string of JSON, tool results merged or separate). None of it did —
which is the seam's test, not a convenience.

`Provider.relay` / `openAiRelay` are the PORTABLE form. A comonadic
handler must answer with a value, so it runs the request inside
itself, which needs a thread that can park; where nothing may park
(JS), the model is peeled into Async instead and the program is
driven by `Async.runAsync`. Tools peel by `Handlers.relayTools`,
context by `Memory.run`, and the agent program is unchanged — the
cross suite runs the same `Agent.converse` under Node that the JVM
suites run.

## A live model

`Provider.openAi(transport, key, model)` is a `Handler[Model]`
speaking the OpenAI-compatible protocol, so the same agent programs
run against OpenAI, Groq, Together, OpenRouter or a local runtime
with no change above the effect. Swap it for `Handlers.scripted` and
the identical program is a unit test — which is the whole argument
for handlers owning policy, now demonstrated on a real protocol.
`Provider.counting(bpe)` makes the token budget local. Both providers
are proven live against a local gateway serving both shapes; the
suite is `assume`-gated (OKAY_LLM_URL / OKAY_LLM_MODEL /
OKAY_LLM_KEY), so CI needs no model.

## Large results

`Large.projecting` wraps any tool handler: a result over the limit is
stored whole and reaches the context as its head plus a handle and a
size, and the `expand` tool reads any window of it later — lossy in
the view, lossless in the lineage, as in retrieval.

## Durability (Durable.scala)

The journal is written intent-first and the recovery decision is per
operation: `Redo`, `WithKey` (retry carrying the FIRST attempt's key,
so the far end deduplicates — the answer for payments), `Reconcile`
(never repeat: ask the far end), `Escalate`, `Fail`. One handler
serves the first run and the recovery; `Durable.replaying` runs an
incident again offline with the world untouched. Exactly-once
EXECUTION is impossible and the module says so — what it provides is
the decision, and tests for the ugly cases rather than the happy one.

## Not yet (specs/llm-agentic.md)

Lineage-backed tool results (the model sees a projection, a follow-up
re-observes the full stream), streaming validation that cuts
generation the moment the value is structurally complete, durability
by replay (specs/llm-agentic.md), and provider handlers — an
OpenAI-compatible one covers most of the market, including the local
runtimes.
