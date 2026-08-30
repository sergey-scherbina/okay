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

## Durability: replay, not serialization

The problem LangGraph solves with a checkpointer and a graph of
nodes. Our answer starts from what we already have: every
nondeterministic thing in a program is an OPERATION (a model call, a
tool, a clock read, a random draw, a Choose), so a journal of
`(operation fingerprint, answer)` pairs is a complete recording of
everything the outside world contributed. Restart = run the program
again with a handler that answers from the journal until it runs out,
then hands over to the live handlers.

**What is skipped and what is not.** The journal-fed handler does NOT
re-execute effects: no second model call, no repeated tool
side-effect, no network. What DOES re-run is the pure glue between
operations — the flatMap continuations. That is the honest cost, and
it is small: our own bind-chain lane measures a 10 000-step chain at
~95us, so re-running the pure part of a several-hundred-step agent
costs less than a single generated token. The expensive steps are
exactly the ones replay skips.

**When the glue is NOT cheap, three dials, in order of preference:**

1. *Make it an operation.* Anything you do not want recomputed —
   a large parse, an expensive fold over a tool's output — becomes an
   effect, and is then journaled and skipped like any other. The
   effect row is the dial between "recomputed" and "recorded", chosen
   per computation by where the boundary is drawn.
2. *Snapshot the state and resume from it.* An agent written as a
   FOLD over turns (which `Agent.converse` already is: the Context
   accumulator is the state) has its resume point determined by the
   state, not by a continuation — so a serialized accumulator (via
   `Schema`, like everything else here) lets the replay start at the
   last checkpoint instead of the beginning, and the journal before
   it can be truncated. This is the LangGraph model, available as a
   DISCIPLINE (`Durable.step`) rather than imposed as the only shape.
3. *Accept the replay.* For most agents, the glue is microseconds.

**"Then why not always skip everything?"** Because naming the resume
point costs something, and the cost lands on the program's SHAPE:

- To skip everything you must say WHERE to resume, as data. In a
  free-form program that place is a continuation — a closure — and
  closures do not serialize (and would not survive a redeploy if they
  did). So snapshot-resume is available exactly when the agent is a
  FOLD over an explicit state, which is the trade LangGraph makes:
  durable resume in exchange for writing every agent as a graph of
  nodes over a state schema. Replay asks nothing of the shape.
- The state must then be DATA ONLY — and that collides with the best
  thing in this design. Our context is allowed to hold program
  values: a tool result kept as lineage is a `Chunks` program, a
  passage is a handle that recomputes. Those are exactly what cannot
  be serialized, and exactly what replay re-derives for free by
  re-running the pure glue.
- Versioning cuts the same way. A snapshot binds you to the shape of
  the state: change the compactor, add a field, and old snapshots
  need migration, while nothing tells you the code that produced them
  has moved. A journal binds you only to the operations' wire types —
  a model call and its reply, a tool call and its result — which are
  far more stable, and a fingerprint mismatch says so out loud.
- What a journal gives besides recovery: the whole history, so
  auditing and time travel are available at ANY point, not only at
  checkpoints. Fine-grained time travel over snapshots means
  snapshotting every step, which is the expensive version of this
  argument.
- The honest point for the other side: snapshot-resume does not care
  whether the pure glue is deterministic, and replay does. Glue that
  depends on iteration order or identity hashes is a real hazard, and
  the same fingerprints that catch code drift are what catch it.

**So they compose, and that is the answer.** This is a write-ahead
log with checkpoints, the shape databases settled on decades ago: the
journal is the foundation (correctness — an effect never runs twice;
any program shape; the full history), and a periodic snapshot BOUNDS
how much of it is replayed. You do skip everything — up to the last
checkpoint — and the journal covers the tail after it. Making the
journal the foundation rather than the snapshot keeps snapshotting an
OPTIMIZATION you add where the glue is expensive, instead of a
CONSTRAINT on how agents may be written.

Scale, so the choice is not made on feeling: the glue is bind-chain
work, measured at ~95us for ten thousand binds, so a
several-hundred-step agent replays in microseconds — against seconds
of model latency. Reach for checkpoints when the glue stops being
glue (a re-parse of a large corpus between turns), and prefer the
first dial — promote that computation to an operation — before
reaching at all.

**Is it worth building at all?** Scoped, yes; unscoped, no.

- It EARNS its place for long runs (an agent editing five hundred
  files, a corpus job), for human gates where the process cannot stay
  alive between the proposal and the approval, and for server agents
  that meet a deploy mid-conversation. The concrete argument is
  money: an agent that dies at step ninety re-pays for ninety model
  calls.
- It is OVERHEAD for a short interactive turn, which is most turns.
  So the journal must be something you switch on for a run, not
  something the loop always pays for.
- The underrated half is not recovery but DETERMINISTIC REPLAY: run a
  production incident again, exactly, offline, with no model calls.
  In a world where the interesting failures are nondeterministic,
  that is worth as much as the crash story and comes free with the
  same journal.
- The comparison worth being honest about: Temporal and Restate do
  durable execution properly for general workflows, and their core
  rule is ours — every nondeterministic thing must go through the
  runtime or replay diverges. The difference is that they ask for the
  discipline in documentation and catch violations with a linter,
  while here the effect row states it in the type. Not enforcement
  (nothing stops a stray currentTimeMillis in pure code), but the
  idiom points the right way and the fingerprints catch the drift at
  runtime.
- The risk that decides the scope: a durability feature that silently
  runs a payment twice is worse than none. So the crash window and
  the drift check are not extras to add later — they are the reason
  the thing can be trusted, and the tests must be about them.

Priority note: this ranks BELOW a real provider handler. Until the
agent and retrieval layers have met an actual model, a durability
feature is insurance on a machine nobody has driven.

**Two hazards, both stated rather than hidden:**

- *Code drift.* If the program changed between runs, the operations
  it requests may not match the journal. Each entry therefore carries
  a fingerprint (the operation's class plus a hash of its arguments);
  a mismatch stops the replay loudly at that point instead of feeding
  an answer to the wrong question — the caller decides whether to
  continue live from there or refuse.
- *The crash window.* A process that dies after a tool ran but before
  its answer was journaled cannot know the outcome. Write the INTENT
  ahead and the answer after; on replay an intent without an answer
  is an explicit "unknown", and the handler's policy decides
  (re-execute when idempotent, ask a human, or fail) — the same
  at-least-once honesty as the Kafka source.

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
- [x] best-of-N and ifte-reprompt are library one-liners, tested —
      Search.bestOf / validated / all / majority over Choose+Logic;
      a row CONTAINING Choose is now a MonadPlus, so `guard` prunes
      inside an effectful search
- [x] a multi-shot branch does not see a sibling's turns — Memory
      threads the compaction state (mirroring State.handle) instead
      of holding it, so a resumed continuation sees the context as it
      was AT THAT POINT. Handler ORDER then says what you mean:
      Memory inside the search = a private conversation per branch,
      outside = one shared transcript. Both tested.
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
