# The durable dialogue, hardened — and what a workflow engine still owes

## Overview

`Delim.resumable` stops a program in the middle and hands the rest of
it back; `Delim.replay` re-derives that place from the answers;
`okay.persist.Dialogue` puts the answers in a topic (durable-dialogue,
dialogue-snapshots, both 2026-09-17). The operator's question the same
day: is that ready for an ordinary engineer, and what is still missing
against a real workflow engine?

Four failure modes were found by probing, all confirmed by running
(the probes are kept, as the pinned suite `TestDialogueHardening`). None of them is exotic; every one of them
destroys or corrupts a production dialogue, and three of them do it
SILENTLY:

| # | what happens | measured |
|---|---|---|
| A | an answer the program cannot digest is journalled BEFORE it is tried, so every later process replays it and throws — the dialogue is dead and the answer can never be corrected | `answer("abc")` on a program that does `.toInt`: the second process, and every one after it, fails with the same `NumberFormatException` |
| B | the program changed since the journal was written, and replay maps old answers onto new questions with no complaint | a v2 program with a new FIRST question reads the v1 journal's `"Kyiv"` as the promo code and carries on |
| C | `run(oracle)` performs the outside call and journals the answer AFTER — a crash in that window re-asks on restart, so the side effect happens twice | crashed on `nights?`, restarted, asked `nights?` again |
| D | two processes answering one dialogue both append; the fold takes both | `a` answered "Kyiv", `b` answered "Lviv", the journal holds both and `b` reads a finished dialogue with data nobody chose |

A, B and D are correctness; C is a contract (at-least-once) that every
workflow engine has and that ours does not state or help with.

The three of them converge on one change — the journal record stops
being a bare answer and becomes an ENVELOPE — so they are one stage,
not three.

## Stage 0 — the journal record grows two fields

```scala
enum Entry[A] derives Schema:
  /** an answer, and the two facts that make it safe to accept */
  case Answered[A](program: String, expect: Int, a: A) extends Entry[A]
  /** a patch decision, taken once per run and remembered (stage 2) */
  case Patched[A](program: String, expect: Int, id: String, on: Boolean) extends Entry[A]
```

- **`program`** — the identity of the code that wrote this record.
  A record whose program is not the reader's is NOT folded: the
  journal stops there and `recovered` names it, exactly as a record
  that does not decode already does. Silent mis-mapping (B) becomes a
  loud stop. It is stamped per record rather than in a header so that
  it survives compaction and needs no write on a read-only start.
- **`expect`** — how many answers the writer had accepted when it
  wrote this one. The fold accepts a record only when `expect`
  equals the number of answers accepted so far; a record that does
  not match is a LOSER of a race, is reported in `recovered.rejected`
  and changes nothing. This is optimistic concurrency implemented in
  the PROJECTION, not in the log — `Topic.append` has no conditional
  form and giving it one would change every store, the wire protocol
  and Kafka interop. Damage is data; so is a lost race (D).

### The order of operations changes (A)

Today: append durably, then advance. The comment defending that order
is right about one half — the other order loses an answer the outside
world already acted on — and wrong about the whole, because an answer
the program REFUSES is committed forever.

The new order uses the property the design already has: **a `Paused`
is a value, so advancing it does not consume it.**

1. advance a copy of the program in hand with the answer;
2. if it throws, nothing is journalled: the throw reaches the caller
   where they ran the program, and the dialogue is still answerable;
3. if it succeeds, append durably;
4. keep the already-advanced program — the advance is not repeated.

The crash window that remains is between 2 and 3, and it replays to
the same place and re-asks, which is the safe direction.

### `at` stops throwing

A dialogue whose journal makes the program throw (a bad deploy, a
half-migrated answer type) must be VISIBLE, not an exception storm in
whatever asked. `at` answers `Either[Dialogue.Stopped, Paused]`, and
`Stopped` is either `Damage` (a record that did not decode) or
`Mismatch` (a record from another program), each naming its offset.
An attempt to answer such a dialogue is `Answered.Broken`, so the
four outcomes of answering — `Advanced`, `Lost`, `NotAsking`,
`Broken` — are one type with no nesting.

### The oracle gets an idempotency key (C)

`run(oracle)` calls `oracle(q, Attempt(id, index))`, where `index` is
the position this answer will occupy. The pair is stable across
restarts and is exactly what an idempotent external call needs
(`Idempotency-Key`, a natural key on the payment, a conditional
insert). The at-least-once contract is then stated where it belongs —
in the type and in the docs — rather than discovered.

### Behavior — stage 0

- [x] an answer the program refuses is NOT journalled, and the
      dialogue is still answerable afterwards
- [x] a second, valid answer lands after a refused one
- [x] a record written by a different `program` stops the fold and is
      named in `recovered`; the dialogue does not advance on it
- [x] two writers from the same standing start: the first append is
      accepted, the second is told `Lost` and shown where the dialogue
      actually stands, and the journal holds one answer
- [x] `at` answers `Left(Stopped)` rather than throwing when the
      journal cannot be folded into a place
- [x] `run(oracle)` hands the oracle a stable `(id, index)` per
      question, equal across a restart
- [x] `Schema` evolution of the ANSWER type works: `Dialogue` takes
      the `version` and `upcasts` `Typed` already supports, instead of
      hard-coding version 1 and no upcasts
- [x] the warm path stays O(n): the won-the-race check costs NOTHING
      when the append landed where this instance had read to

## Stage 1 — the discipline becomes a type

The exactness of replay rests on one sentence: *everything the outside
world tells the program enters through `pause`*. Today that is a
sentence in a doc. A program that reads a clock, calls a service or
rolls a die between pauses replays that call — measured in
`TestDelimPersist`, and nothing stops it being written.

- **`Replayable[F]`** — evidence that a row contains only effects
  whose re-execution is unobservable (`State`, `Reader`, pure
  computation). `Async`, `Writer`, `Uid`, anything I/O, is not
  replayable, and `resumable`/`replay`/`Dialogue` require the
  evidence. Breaking the discipline becomes a compile error at the
  place that breaks it.
- **The standard non-determinism, as questions.** A workflow needs a
  clock, ids and randomness; refusing them is not an answer. They
  become questions the runtime answers and the journal remembers:
  `Wf.now()`, `Wf.uuid()`, `Wf.random()`. Temporal's `SideEffect` and
  `workflow.now()` are the same move.
- **`perform(cmd)`** — the general form: a command whose result is
  journalled, so it is executed once per journal position and read
  from the log on every replay. This is what an "activity" is, and it
  is `pause` with a command for a question.

### Behavior — stage 1

- [x] a body that performs an `Async` effect outside a `pause` does
      not compile as a dialogue
- [x] the row's other unsafe members are refused too: `Writer`
      (replay tells the log again — the measured case), `Resource`
      (replay acquires again)
- [x] a deliberate breach is WRITTEN DOWN: `Replayable.unchecked`, a
      method rather than a given, so it cannot be summoned by
      accident and a reviewer sees the name
- [x] an abstract row propagates the obligation rather than crashing
      the compiler
- [ ] `now`/`uuid`/`random` answer from the journal on replay, and the
      same run twice gives the same values
- [ ] `perform` executes once per position across a restart

The last two are a SEPARATE lane (`dialogue-nondeterminism`), and the
reason is a design question this one should not answer in passing:
a dialogue's question type `Q` is the author's own, so "give me the
clock" has nowhere to live in it. Either `Q` becomes a sum the library
owns (`Ask[Q] = Mine(Q) | Now | Uuid | Random`, which changes every
signature and the journal's records), or the clock is a second channel
beside the questions. Deciding that badly in a hurry would cost more
than the feature is worth.

### Results — stage 1 (2026-09-17)

**The encoding took a spike, and the obvious form is refuted.** The
natural spelling of "every effect in this row is safe to re-run" is an
inductive instance over the row:

```scala
given union[F[+_], G[+_]](using Replayable[F], Replayable[G]): Replayable[F + G]
```

It does not resolve. `F + G` is `[A] =>> F[A] | G[A]`, and matching a
CONCRETE row against it asks the compiler to invert a union into
halves; it leaves both unsolved and then reports every instance as
ambiguous for both ("both given instance reader and given instance
delim match type `Replayable[F]`"). Worth knowing that it FAILS
rather than crashes — the same territory killed dotty outright in
delim-safety, which is why the spike came first.

**What works is subtyping with the concrete row on the left** — the
same trick as `Delim.OneMachine`:

```scala
type Safe = Delim[Any] | State[?, Any] | Reader[?, Any] | Throws[?, Any]
given replayable[F[+_]](using F[Any] <:< Safe): Replayable[F]
```

`A | B <: C | D` decomposes the LEFT side, which the compiler does
happily, and an abstract row is neither proved nor refuted but
PROPAGATES to the caller, where the row is usually concrete.

**The constraint's only casualty was the test that breaks the
discipline on purpose.** `TestDelimPersist`'s "the limit: replay
re-runs what did not come through pause" watches a `Writer` log say
the same thing twice across two runs — it is the reason anybody
believes the rule — and it now says `Replayable.unchecked` to be
allowed to. Nothing else in the tree had to change: no production
code was quietly breaking the discipline, which is itself worth
recording.

## Stage 1b — the library's own questions (`dialogue-asks`)

Stage 1 made the discipline a type: a durable program may not reach
outside except through `pause`. That leaves the obvious hole — every
real workflow needs a CLOCK, an id and sometimes a die, and refusing
them is not an answer. The same hole blocks stage 2: `patch(id)` is a
question the LIBRARY answers from the journal, not one the author's
oracle answers, so it has nowhere to live either. One design decides
all of them, which is why they are one lane.

### The problem, stated exactly

A dialogue's question type `Q` belongs to its author. `Wf.now()` has
nothing to send through a `Q` the author defined as, say, `String`,
and the ANSWER has nowhere to live in the author's `A` either.

### The decision: the channel is a sum the library owns

The dialogue's question becomes `Either[Sys, Q]` and its answer
`Either[SysA, A]`, with `Sys`/`SysA` closed enums belonging to the
library. The author never writes the `Either`: `Wf.pause(q)` wraps in
`Right` and unwraps the `Right` answer, `Wf.now()` sends `Left(Now)`
and unwraps `Left(Millis)`.

Two candidates were weighed and this one wins for a reason that is
not taste:

- **a second channel beside the questions** (a separate prompt, a
  separate journal) would need its two journals interleaved to
  replay, and the interleaving is exactly the information the single
  tagged journal carries for free;
- **an open sum the author extends** puts the library's questions in
  the author's type, so every consumer pattern-matches cases it does
  not own.

### What the tag buys, and it is the whole of stage 2

A journal entry now SAYS whether it answers a library question
(`Left`) or an author question (`Right`). That single bit is what
makes `patch(id)` correct:

- replaying, the pending question is `Sys.Patch(id)` and the next
  journal entry is a `Left(Flag(b))` for it — use `b`;
- replaying, the pending question is `Sys.Patch(id)` and the next
  entry is a `Right` — the run that wrote this journal did NOT have
  this patch, so the answer is `false` and the entry is NOT consumed;
- live (the journal is exhausted) — the answer is `true`, recorded.

That is Temporal's `getVersion` semantics, and here it falls out of
the tagging rather than being a rule the driver has to remember. The
mis-alignment the tag prevents is the same class of failure as B in
this spec's table, which is why the bit is worth the `Either`.

### Behavior — stage 1b

- [x] `now`/`uuid`/`random` answer from the journal on replay: the
      same run replayed twice gives the same values
- [x] the author's `pause` is unchanged in spelling and type
- [x] `perform(cmd)` — and it needed no machinery: an "activity" is a
      command performed outside and a result remembered, which is a
      question and its answer. `Wf.perform` is `Wf.pause` under the
      name the literature uses, and saying so is the feature
- [x] `patch(id)`: a journal written before the patch existed takes
      the OLD branch and does not lose its next answer; a fresh run
      takes the new branch; a third process agrees with both
- [x] the driver answers `Left` itself and `Right` through the
      author's oracle, and a test counts both
- [x] `Wf.replay` TAKES NO RUNTIME — its signature is the proof that
      a replay cannot read a clock

### Results — stage 1b (2026-09-17)

**Landed in the core as `okay.Wf`** (`TestWf`, 6 tests), and it
carried stage 2's `patch` with it, because they are one mechanism.

- The tag pays for itself exactly where the design said it would. The
  decisive test is a v1 journal (`Right("Kyiv"), Right("3")`) read by
  a v2 program that gained a `patch` BETWEEN the two questions: the
  patch answers `false` and does not consume, so `"3"` still answers
  `nights?` and the old run finishes the way it began. Without the
  tag the patch would have eaten `"3"`.
- A half-finished old journal replays into the new program and then
  goes LIVE at the patch: the driver decides `true` and appends the
  decision, so the run finishes on the new branch with its history
  intact. That is the migration case, and it works without a
  migration.
- `perform` was listed as work and turned out to be a name.

**Wired through to the log** by `wf-durable-journal` the same day:
`Dialogue.workflow` is the door, and the only thing that had to change
in `Dialogue` was HOW A JOURNAL BECOMES A PLACE. That is now a
parameter (`place`), because it is the one part of the class the
program's shape decides: an ordinary dialogue folds with
`Delim.replay`, a workflow with `Wf.replay`, and the envelope, the
races and the advance-then-append order are written once for both.
All fourteen of `Dialogue`'s own tests passed unchanged through the
refactor, which is what says the seam was in the right place.

**The rough edge is gone** (`wf-direct-door`, the same day). `Wf`'s
doors took four type arguments at every call site; they are now
methods ON THE EVIDENCE, so a body names its four types once in its
own signature and no call site repeats them:

```scala
def booking(using w: Wf.Asks[String, String, String, Pure]) = direct:
  val city = !w.pause("city?")
  val id   = !w.uuid
  if !w.patch("promo") then … else …
```

No macro was needed, and that is the other half of the point: the
inline `Delim.pause` exists because a mark gives its argument no
expected type, and it pays one cast for that. Here the types are on
the object, so there is nothing to infer and nothing to cast.

## Stage 2 — the program is allowed to change

Stage 0 makes a changed program a loud stop. That is right and not
enough: long-running dialogues outlive deploys, and a stop is an
outage.

- **`patch(id)`** (Temporal's `patched`/`getVersion`): the first time
  a run reaches it, the decision is journalled; a run whose journal
  has no decision for `id` and whose answers were written before the
  patch existed gets `false`, a fresh run gets `true`. Old dialogues
  keep the old path, new ones take the new one, and the branch is
  deletable once no old run is left.
- **A program identity that is a VERSION, not a hash.** The author
  states it (`Dialogue(..., program = "booking/3")`); a hash of the
  code would change on a comment and is therefore a worse lie than an
  honest hand-maintained number.
- **Retirement**: a tool that says which programs are still present in
  a topic, so a branch can be deleted with evidence.

### Behavior — stage 2

- [x] a dialogue started under `booking/2` and resumed under
      `booking/3` takes the OLD branch at `patch("promo")` and
      finishes correctly — `TestWf`, at the `Delim` level
- [x] a dialogue started under `booking/3` takes the new branch
- [x] the decision is in the journal, so a third process agrees
- [x] the same, through `okay.persist.Dialogue.workflow` — the clock
      read once across a restart, an old journal keeping the old
      branch, and a half-finished one going LIVE at the patch and
      finishing on the new branch with its history intact
      (wf-durable-journal, TestWorkflow)
- [ ] retirement: a tool that says which programs are still present
      in a topic, so a branch can be deleted with evidence

## Stage 3 — bounded history

Replay re-runs the program over its answers. Chapters
(dialogue-snapshots) cut the READING; nothing cuts the RUNNING, and a
dialogue with ten thousand answers runs the program over ten thousand
answers on every cold start.

- **`continueAs(seed)`** — Temporal's `continueAsNew`: the program
  ends this run with a serializable seed, a new run begins with that
  seed as its first answer, and the old journal is closed. History is
  bounded by the author's choice of where a stage ends.
- **A resume cache** — a process that holds many dialogues should
  replay each one once. An LRU of `id -> Paused` turns every step
  after the first into the warm path that already exists.

### Behavior — stage 3

- [ ] a dialogue that `continueAs`es twice is read from the last seed,
      and its cold start does not depend on the length of the whole
      history
- [ ] with a cache, n answers to one dialogue replay once, not n times

## Stage 4 — what a workflow ENGINE owes beyond the model

Stages 0–3 make the model sound. An engine is more than the model, and
the honest list of what is NOT there, with what each one actually
needs here:

| Temporal has | we have | what it would take |
|---|---|---|
| durable timers (`sleep`) | nothing | a question whose answer is delivered by a scheduler that persists deadlines — one topic of `(due, dialogueId, question)`, one poller |
| activity retries, timeouts, heartbeats | `okay-resilience` (Retry, Hedge, CircuitBreaker, deadlines) — not wired | `perform` takes a policy; the retry is the driver's, not the program's, so the journal sees one answer |
| signals (an external event into a running run) | `answer` IS a signal | a named channel per dialogue, so a signal is not confused with an answer to the current question |
| queries (read-only inspection) | `at`, `asking` | a read path that does not append and does not need the writer's lease |
| cancellation and compensation | `okay-persist`'s `Saga` | a cancellation question the program can observe, and compensations as journalled commands |
| visibility (list, filter, find the stuck ones) | nothing | a projection of the dialogues topic into a status index: id, program, standing question, last movement |
| workers and task queues | `run(oracle)` in one process | a worker over a partition with a lease per id; stage 0's `expect` is what makes two workers safe, a lease is what makes them rare |
| child workflows | nesting works in memory (`pausing`) | a child whose journal is its own topic key, and a parent question answered by its result |

**The operator asked for the engine, 2026-09-17**, so this stopped
being a list of what is missing and became a plan. What follows is the
architecture, and it turns on one observation.

### The keystone: a driver that can SUSPEND

Three of the eight rows above — durable timers, signals, child
workflows — look like three features and are one. Each is a question
the driver CANNOT ANSWER WHEN IT IS ASKED: a sleep is answered by the
passage of time, a signal by somebody else's action, a child by
another run finishing. Today `run(oracle)` has no way to say that: it
answers every question or it throws.

So the driver's result grows a case, and that single change is what
makes the other three possible:

```scala
enum Step[Q, R]:
  case Done(value: R)                  // finished
  case Asking(q: Q)                    // the AUTHOR's question; an oracle answers
  case Waiting(on: Wait, since: Long)  // nobody here can answer it yet
```

with

```scala
enum Wait:
  case Until(millis: Long)             // a timer
  case Signal(name: String)            // an external event
  case Child(id: String)               // another dialogue's result
```

A driver that returns `Waiting` has done its job and stops. Something
else — a scheduler for `Until`, an API call for `Signal`, the child's
own completion for `Child` — appends the answer to the journal later,
and a worker picks the dialogue up again. The journal is still the
only state, and the tag on each entry still says what it answers.

**Why this is not a second mechanism.** A `Waiting` question is an
ordinary `Sys` question that the runtime declines to answer in place.
Everything downstream — the journal, replay, `patch`, the `expect`
race check — is untouched, which is what makes the engine an addition
rather than a rewrite.

### The order, and what each lane owes

| lane | what it adds | depends on |
|---|---|---|
| `workflow-suspended-driver` | **LANDED 2026-09-17**: `Step`, `Wait`, `Wf.sleep`, `awaitSignal`, `awaitChild`, `Wf.advance`, and `Dialogue.runUntil`/`runWorkflow` | — |
| `workflow-timers` | **LANDED 2026-09-17**: `Timers`, a compacted keyed topic of deadlines — `arm`, `disarm`, `armed`, `due(now)`. It hands back IDS and appends nothing: turning an id into an `Elapsed` needs the dialogue's schema, program and body, which is the worker's business and not a clock's | the driver |
| `workflow-worker` | **LANDED 2026-09-17**: `Worker` — `start`, `advance`, `wake`, `tick(now)`, each ending by telling the timers what it learned. NO LEASE, on purpose: `expect` is what makes two workers safe, a lease only makes collisions rare, and the tests show one journal coming out of two workers without one | the driver, timers |
| `workflow-visibility` | **LANDED 2026-09-17**: `Statuses`, written BY THE WORKER on every advance. The model can answer "what is every run doing" only by running every program over every journal — right, and the wrong thing to pay on a dashboard refresh | the driver, the worker |
| `workflow-signals` | **LANDED 2026-09-17**: `Signals` — a mailbox topic keyed by dialogue id and a compacted cursor PER NAME, because a signal may arrive long before the run waits for it and a journal has nowhere to hold an answer to a question nobody asked. The worker drains what it can before it reports a wait | the driver, the worker |
| `workflow-retries` | **LANDED 2026-09-17**: `Worker.retrying(policy)(oracle)` — three lines, because the two halves already existed (`Retry`'s policies are streams of delays, and the activity row is where an attempt is allowed to fail). An exhausted policy gives up FOR NOW: nothing is journalled, the run still stands at its question, and a later worker asks again | the activity row |
| `workflow-cancel` | **LANDED 2026-09-17**: `Cancels`, a compacted keyed topic of stop requests, `Wf.cancelled` as an ordinary `Sys` question, and `Runtime.cancellable` wrapping the ambient runtime per dialogue. COOPERATIVE, and not by taste — see the Result below. Compensation is ordinary code on the cancelled branch | the driver, the worker |
| `workflow-children` | a child dialogue keyed under its parent, and a parent question answered by its result | the driver, the worker |
| `dialogue-continue-as` | bounded history: a `Continued(seed)` record that supersedes everything before it | — |

### Result — cancellation is a QUESTION, not a throw (2026-09-17)

`TestCancel` (6 tests) and one more in `TestWorkflowGuide`.

**The alternative that was refuted, and the reason it had to be.** The
obvious shape is pre-emptive: the driver notices a cancel request and
throws into the program, which catches it and compensates. That cannot
work here, and the fact that kills it is already pinned by
`TestDelimLimits`: a `direct` block's `try/catch` guards the BUILDING
of a program, not its running. A `!`-bound step inside a `try` does
not run inside that `try` — it is a node in a tree that runs later —
so a thrown cancellation could not be caught by the program being
cancelled. An `if` can. Cancellation is therefore a question the
author asks where they decide it is safe to stop.

**What that costs, said out loud** because a team has to plan for it:
a program with no check is not cancellable; a run asleep for a year
learns it was cancelled when it wakes; a run waiting on a signal that
never comes never learns at all.

**What it buys, and it is the reason to prefer it even without the
constraint above: the decision is replayable.** The request lives in
an operational topic, but the ANSWER — cancelled or not, and why — is
journalled like every other answer. A run told "no" at 10:00 is told
"no" by every replay of that position even after the request arrives
at 10:01, so a rebuild never takes a branch the original run did not
take. The fifth test withdraws the request from a finished run and
asserts the run still finishes the way it finished; a design that read
the topic at replay time would fail it.

**Where the request is read.** `Runtime.cancellable` takes `why` BY
NAME and the worker builds it per drive, not per worker: a request
arriving mid-drive is seen by the next check and not by the checks
already behind it. A worker built without a `Cancels` returns `false`
from `cancel` — a refusal rather than a silently dropped request.

### Results — the keystone (2026-09-17)

`TestWfSuspend` (8 tests) and two more in `TestWorkflow`. Three things
worth keeping:

- **The three features really are one.** `sleep`, `awaitSignal` and
  `awaitChild` differ only in which `Sys` case they ask and which
  `Wait` the runtime answers with; the driver's loop does not know
  them apart, and neither does the journal.
- **The deadline is journalled, not computed on the fly.**
  `sleep(d)` is `now` then `Timer(now + d)`, so the `now` reading is
  an ordinary journal entry and every later process computes the SAME
  instant. Measured: a second process whose clock reads 9 999 999
  still waits until the deadline the first one chose. Had the sleep
  been relative to the reading process's clock, a restart would have
  slid the deadline forward for ever — the durable-timer bug every
  engine has had once.
- **The durable driver needed a generalisation, not a sibling.**
  `run(oracle)` became `runUntil[S](oracle)` where an oracle may
  answer `Left(s)` — "nobody here can answer this" — and the drive
  ends with `s`, everything it DID answer already in the log. The
  warm path, the `expect` race check and the append-after-advance
  order are the same moves either way, which is why there is one
  driver and not two.

### A fourth rule, learned the hard way: TWO ROWS

`workflow-activity-row`, 2026-09-17, found while wiring retries and
fixed before them because everything downstream depends on it.

A durable program's row must be `Replayable` — no `Async`, nothing a
replay would perform again. The ORACLE is the half that DOES reach
outside: it calls the service, charges the card, writes the file.
Sharing one row made the oracle as constrained as the program, so an
activity could only do I/O by side-effecting in Scala, PAST the effect
system — which is what this library exists not to do.

So the driver runs in `F + E`: the program's replayable row, plus
whatever the activities need. The programs the driver moves are built
in `F` and widened at the seam; the journal, the replay and the race
check never see `E`.

**Addition, not membership, and that was measured.** The natural
spelling is a single driver row `G` containing `F` — `RowLift.at`
exists for exactly that, one licensed cast — but `In[F, G]` over TWO
ABSTRACT rows crashes dotty 3.9 in `orDominator` ("Failure to join
alternatives F and G"), the same crash delim-safety recorded. Second
time in one day; the complement form is the one the compiler handles.
The cost, stated: `F + E` with both `Pure` is `[X] =>> Nothing |
Nothing`, which is not `Nothing`, so a driver with no effects at all
cannot be `!.run`. In practice `E` is what the activities need and is
never `Pure` — a driver that can do nothing has no activities to
drive.

### The three rules this architecture keeps

1. **The journal is the only state.** Nothing above adds a second
   place where a run's position lives; a timer's deadline and a
   lease are OPERATIONAL data about a run, not part of it.
2. **Every new question is a `Sys` question.** The author's `Q` never
   grows, so no consumer's `match` gains a case it does not own.
3. **A worker may always be killed.** Everything a worker does is
   either idempotent or guarded by `expect`, so the engine's failure
   mode is a repeated attempt, never a lost or doubled answer.

## Decisions

- **The envelope, not a conditional append.** `Topic.append` has no
  expected-offset form; adding one would change MemoryStore,
  Replicated, the wire protocol and the Kafka interop for one
  consumer. Putting `expect` in the record makes the concurrency
  check a property of the FOLD, which every store already has, and
  keeps "damage is data" — a lost race is reported, not thrown.
- **Trial before commit, not commit before trial.** The landed order
  (append, then advance) was chosen against losing an answer the
  world acted on; it loses more — a refused answer is permanent. The
  `Paused` being a value is what makes the trial free.
- **The program's identity is stated by its author.** A fingerprint
  taken from the code changes when a comment does, which trains
  people to ignore it.
- **Refusing non-determinism outright is not an option.** Every real
  workflow needs a clock and an id; stage 1 gives them as questions
  rather than pretending a program can do without them.

## Out of scope

- distributed transactions across dialogues (that is `Saga`);
- a scheduler, a worker pool or an operator UI — stage 4 names them
  and does not promise them;
- making a continuation itself serializable. It is a JVM closure. The
  journal is the answer, and stage 3 is how its cost is bounded.

## Results

**Stage 0 landed 2026-09-17** (`TestDialogueHardening`, 6 tests; the
7 tests of `TestDialogue` unchanged in intent).

- The four probes that failed against the first cut now pin the fix.
  A refused answer leaves the journal alone; a journal from another
  program stops the fold and names both program ids; the oracle's
  `Attempt` is equal across a restart; the second writer is told
  `Lost` and shown where the dialogue actually stands.
- **Two numbers moved, both for a stated reason.** The cold loop's
  reads went from `N(N+1)/2` to `N(N-1)/2`, because the fold now
  happens BEFORE the append — that is what lets a refused answer
  leave the journal alone. The warm path stayed at ZERO reads only
  because of the `seen` optimisation: the naive won-the-race check
  re-folded the journal per answer and put `run` back at O(n²), which
  is the regression dialogue-snapshots had already paid to remove. An
  append that lands exactly where this instance had read to cannot
  have been overtaken, so the common case reads nothing.
- **One landed decision was reversed, deliberately.** `at` used to
  answer from the intact prefix when a record did not decode; it now
  refuses. A record we cannot read might be an answer, and carrying
  on past it re-asks a question the outside world has already
  answered — a duplicate side effect is worse than an outage. The
  prefix is still readable through `recovered` for an operator tool
  that wants it.
- **The race is only visible on the warm path**, and the test says
  so: `answer` re-reads the log first, so its window is narrow;
  `step` has a wide one, and `expect` covers both identically.
