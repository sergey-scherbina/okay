# P9 — okay-agent: agents as programs, context as a fold

## Overview
The agentic layer specs/llm.md deferred. Nothing new is invented: a
tool call is an EFFECT OPERATION, an agent is a STAGE, the
conversation is a FOLD, search over plans is `Logic`, and every
policy question (who executes a tool, what stays in context, what to
retry) is answered by a HANDLER — swappable per environment, testable
without mocks, inspectable as a value.

The organizing claim: an agent framework is mostly a context manager
and a control-flow engine wearing a trench coat. We already have the
control-flow engine (effects + coroutines + backtracking) and the
right algebra for context (Aggregator + incremental token counting),
so this module is small.

## The effects

```scala
enum Model[+A]:                       // the model itself
  case Complete(req: Request) extends Model[Reply]      // streams
  case Count(text: String) extends Model[Int]           // local, BPE

enum Tool[+A]:                        // one typed tool invocation
  case Call[A](name: String, args: Json) extends Tool[Json]

enum Context[+A]:                     // the conversation, as effects
  case Remember(turn: Turn) extends Context[Unit]
  case Recall() extends Context[Seq[Turn]]     // ALREADY within budget
  case Mark() extends Context[Snapshot]        // for backtracking
```

An agent is `A ! (Model + Tool + Context + Async)`. It NEVER holds a
message list: it performs `remember`/`recall`, and the handler owns
the policy. That inversion is what makes context management automatic
rather than exceptional.

## Context management (the design)

**Compaction is an Aggregator, so it is incremental and mergeable.**
A policy is `Aggregator[Turn, S, Seq[Turn]]` — our P1 type, unchanged:

- `add` (seqOp) folds ONE new turn into the state: O(new tokens), not
  O(history). Called on every `remember`, so the view is always ready.
- `present` is the view that goes on the wire — the compacted context.
- `merge` (combOp) is what makes compaction HIERARCHICAL: summaries of
  two halves merge into a summary of the whole, so a long history
  compacts in parallel (a fiber per chunk, `parMap`) and a
  cluster-sized history compacts on a cluster (`Cluster.distribute`),
  with the same value.
- `zip` runs several compactors in ONE pass: a token-window view, a
  running summary, and a fact/entity extractor, all fed once.

**Token counting is exact and free.** `Bpe` is a `Scan`, and a Scan's
state is a value that crosses chunks — so tokens are counted AS THE
REPLY STREAMS, incrementally, with no extra pass and no provider
call. A token-window is then a `Group` (our `sliding` already demands
one): adding a turn adds its count, evicting subtracts it. No
re-counting, ever.

**Compaction may itself use the model.** A compactor is a program, so
it can perform effects: `summarizeOldest(k)` is a `Model.Complete`
inside the compaction fold. Then compaction is literally a
`Stage[Turn, Turn, S]` — a stream transformer over the conversation,
demand-driven like every other stage; `through` composes several
(evict trivia → summarize old → pin system+goals).

**Per-iteration by default, not as an emergency.** The loop is

```scala
def agent: Unit ! Row =
  Stage.await[Msg, Msg].flatMap {
    case None => pure(())
    case Some(msg) =>
      remember(User(msg)) *> recall >>= { ctx =>       // <- compaction here
        complete(ctx) >>= { reply =>
          remember(reply) *> runTools(reply.calls) >>= { results =>
            traverse(results)(remember) *> agent } } }
  }
```

`recall` applies the fold every single turn — the default path, cheap
because the state is kept. "Over budget" does not switch on an
exceptional code path; it only changes which view `present` returns.
The decision is `Selective`: `overBudget.ifS(compactHard)(pure(()))`
declares both branches statically and runs one — inspectable, and
stageable.

**Lossy in the view, LOSSLESS in the lineage.** The context hogs are
tool results and documents. Here a tool result is a `Chunks[A]` — a
VALUE that recomputes (P2's lineage). So the context keeps a handle
plus a head/tail projection; when the model needs more, the handler
re-observes the source. Compaction never destroys information, it
only chooses what to show — the opposite of drop-oldest eviction.

**Backtracking over context is free.** `Mark` returns a snapshot;
because the state is persistent, restoring it is a pointer, not an
undo log. Combined with `Logic`: try a plan, `guard` its result, and
on failure backtrack to the mark and take the next alternative —
`ifte` gives "if this plan produced anything usable, continue with
all its outcomes; otherwise re-prompt", which is the soft cut neither
exceptions nor plain flatMap can express.

## Search over plans (why backtracking belongs here)

- best-of-N sampling = `Choose` over N completions, `once` to commit;
- validate-and-retry = `ifte(parseOk)(use)(reprompt)`;
- Tree-of-Thoughts = `msplit` search: a node expands by a completion,
  `guard` prunes, `observe(n)` takes n leaves, `interleave` splits
  the budget fairly between branches instead of diving into the first;
- self-consistency = `runChoice` + an `Aggregator` (majority vote is a
  fold; confidence quantiles are a t-digest).

## Streaming, validated as it arrives

The total parser + O(damage) reparse make a partial structured answer
usable BEFORE it is complete: each arriving token reparses the tail,
the tree-with-holes projects, and the decode either yields a partial
value or says what is still missing. Two consequences: downstream
work can start early, and generation can be CUT the moment the value
is structurally complete (fewer tokens billed).

## Interop, not reimplementation

The P3 doctrine applies verbatim: `okay-langchain4j` makes their
`ChatModel`/`StreamingChatModel` a HANDLER for our `Model` effect and
their `EmbeddingStore` a handler for a `Retrieve` effect. We inherit
their provider and store breadth in one small module; they get a
composable runtime. Same for MCP: an MCP server is another `Tool`
handler, and its JSON-RPC framing is our total parser plus `Schema`.

## Behavior
- [x] a tool call round-trips: model asks, handler executes, result
      goes back, conversation continues (scripted model, no network)
- [x] tool schemas derive from `Schema[Args]` — no hand-written JSON
      schema anywhere (ToolSpec.jsonSchema is the FOURTH algebra over
      Schema; `ToolSpec.args` decodes with the same one, so a tool's
      declaration cannot drift from its parser)
- [x] context stays within a token budget across N turns WITHOUT the
      agent program mentioning compaction (asserted on every context
      the model actually saw). The elision MARKER counts against the
      budget too — it goes on the wire (caught by the test)
- [x] the compactor's `merge` makes a summarized history
      split-point-agnostic (the P1 property, tested at every split)
- [x] system turns are pinned: they survive any amount of pressure
- [x] an unknown or denied tool is an ANSWER, not a fault, so the
      model can recover from its own mistake
- [ ] token counts from the BPE Scan match the provider's usage
      report within a stated tolerance (needs a real dictionary; the
      local counter is wired and tested against itself)
- [ ] a tool result too large for the context is kept as lineage: the
      model sees a projection, a follow-up re-observes the full value
- [x] mark/restore: a snapshot restores the conversation exactly (a
      pointer, not an undo log) — the backtracking-with-Logic
      combination is v2
- [ ] best-of-N and ifte-reprompt are library one-liners, tested
- [ ] a partial structured answer decodes mid-stream and generation
      stops when the value is complete
- [ ] the same agent program runs on the JVM and under Node (the
      cross-platform policy, as in okay-cluster)

## Decisions
- **Handlers own policy, programs own logic.** Execute a tool, ask a
  human, record-and-replay, or refuse — all are handlers for the same
  program. Testing an agent needs no mocking framework: it needs a
  different handler.
- **Context is an effect, not an object.** langchain4j's `ChatMemory`
  is a mutable container the caller configures; ours is an operation
  the handler answers, which is what lets compaction be automatic,
  mergeable and backtrackable.
- **No prompt DSL, no chain abstraction.** flatMap is the chain; a
  Stage is the pipeline; `through` is composition.

## Out of scope (v1)
- embeddings/vector search of our own (a `Retrieve` effect with
  handlers pointing at existing stores instead), document loaders,
  provider breadth — that is what the interop module is for.
