# Durable workflows

A workflow here is **a program that waits** — for a person, another
service, a date in the calendar — written as straight-line code rather
than as a state machine, and able to outlive the process running it.

This page is the practical half. The theory is
[continuations in practice](continuations-in-practice.md); the design
record, with every refuted alternative, is
[specs/durable-workflow.md](../specs/durable-workflow.md).

The same workflow can also be written as a **term** rather than a
monadic program — see [static workflows](static-workflows.md). It
shares this engine, this journal and this worker; what it adds is that
the shape is a value, so the questions, the position and the
deploy-fits-the-journal check are available without running anything.

## The one idea

A paused program is a continuation, and a continuation is a closure:
it cannot be written to disk. So nothing tries to.

What is written down is **the answers** — and where the program
stands is re-derived by running it again over them. That is event
sourcing with one difference worth the whole page:

| Event sourcing | Here |
|---|---|
| events | the answers |
| the aggregate's state | where the program stands |
| `apply(state, event)`, written and maintained by hand | **the program itself** |
| rebuild = fold the events | rebuild = run the program on its journal |

The fold you would otherwise write, keep in step with the code, and
get subtly wrong **is the straight-line program you already wrote**.

## What it looks like

```scala
def booking(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure = direct:
  val city  = !w.pause("which city?")          // the WORLD answers
  val start = !w.now                           // the RUNTIME answers, once
  !w.sleep(24 * 3600 * 1000L)                  // the run ENDS here and resumes tomorrow
  val ok    = !w.awaitSignal("payment")        // somebody sends this, whenever
  if !w.patch("promo") then s"$city/promo" else city
```

Five lines, and each of the five is something a workflow engine
exists to provide:

- **`pause`** — a question for the outside world. Its answer is
  journalled, so it is asked once in the life of the run, however many
  times the program is replayed.
- **`now`** (and `uuid`, `random`) — non-determinism as a QUESTION.
  Read once, remembered, replayed. A restart does not move the clock.
- **`sleep`** — the run *stops*: no thread parked, nothing in memory,
  a deadline in a topic. A worker picks it up when the instant passes.
- **`awaitSignal`** — the same stop, waiting on somebody's action
  instead of on time. A signal that arrives EARLY waits in a mailbox.
- **`patch`** — a branch that new runs take and old runs do not, so a
  deploy does not have to wait for the old runs to drain.

## The pieces, and which of them is the truth

```
    Dialogue ── the journal. THE TRUTH.
    Worker   ── moves runs forward: start, advance, wake, tick
    Timers   ── deadlines          }
    Signals  ── the mailbox        } operational: lose any of them and
    Statuses ── the dashboard      } no run is WRONG — only slower,
    Cancels  ── stop requests      } or blind, or not stopping
    Children ── results to wait on }
    Leases   ── who is working      } or doing it twice
```

**Only the journal is state.** Everything else is operational data
*about* runs. There is a test for each: delete the timer topic and
every run is still exactly where its journal says it is; run a worker
with no status index and it finishes a run the indexed one started.
That is not an accident of the implementation, it is the first rule of
the design, and it is what makes the engine's failures boring.

## The four rules

1. **The journal is the only state.** A deadline, a lease, a status
   line are things to look at, never things to decide from.
2. **Every question the library adds is a `Sys` question.** The
   author's own question type never grows, so no consumer's `match`
   gains a case it does not own.
3. **A worker may always be killed.** Every move is idempotent or
   guarded by `expect`: an answer carries the position it expected, so
   a second worker's answer for a filled position is REJECTED by the
   fold. Two workers produce one journal. The failure mode is a
   repeated attempt, never a doubled answer.
4. **The driver's row is not the program's.** The program's row must
   be `Replayable` — no `Async`, nothing a replay would perform again.
   The oracle is the half that reaches outside, and it lives in the
   wider row. A workflow whose own row is `Async` does not compile.

## The discipline, and why it is a type

> Everything the outside world tells the program enters through
> `pause` (or `now`/`uuid`/`random`/`awaitSignal`).

Break it — read a clock, call a service, roll a die anywhere else —
and replay re-runs that, because replay is just running the program
again. So it is not a convention: `Delim.replay` and
`okay.persist.Dialogue` ask for `Replayable[F]`, and a row holding
`Async`, `Writer` or `Resource` does not have it.

Breaking it on purpose is possible and has to be written down:
`Replayable.unchecked`, a method rather than a given, so a deliberate
breach appears in the diff.

## Running one

```scala
val store  = MemoryStore()                       // or any Store
val timers = Timers.over(store)
val sigs   = Signals.over(store)
val index  = Statuses.over(store)

val worker = Worker[String, String, String, Pure, Async](
  store.topic("bookings"), program = "booking/1", timers,
  oracle = Worker.retrying(Retry.exponential(100))(askTheUser),
  signals = Some(sigs), statuses = Some(index))(booking)

worker.start("booking-42")        // drives until it waits, then returns
worker.tick(System.currentTimeMillis())   // everything whose deadline passed
sigs.send("booking-42", "payment", "ok")  // whenever, from anywhere
index.waitingOn("payment")                // who is blocked, as one read
```

**Everything past `timers` is optional, and each one buys one thing.**
A worker with none of them is correct; the journal is the only state
and nothing below changes that. The full set, with the section that
explains each:

```scala
// `seedOf` only means anything for a program that RETURNS a
// `Wf.Next`, so the full set is shown on `stage` from the bounded-
// history section rather than on `booking`
Worker[String, String, Wf.Next[String, String], Pure, Async](
  topic, program = "stage/1", timers, oracle,
  snapshots     = Some(snaps),            // cold starts read a chapter + tail
  snapshotEvery = 64,
  signals       = Some(sigs),             // awaitSignal can be answered
  statuses      = Some(index),            // the dashboard has something to read
  cancels       = Some(cancels),          // `w.cancelled` can say yes
  children      = Some(kids),             // awaitChild can be answered
  seedOf        = Wf.Next.seed,           // the program may bound its history
  continuations = 64,                     // chapters per drive before handing back
  leases        = Some(leases),           // collisions become rare
  owner         = "box-3",
  resume        = Some(Resume()))(stage)   // replay once, not once per call
```

`start` and `advance` say where the run stopped, and nothing blocks:

| | |
|---|---|
| `Finished(value)` | it ended |
| `Sleeping(until)` | a deadline is armed; `tick` will come back |
| `Waiting(on)` | a signal or a child; somebody else's move |
| `Continued(n)` | it opened a new chapter, `n` in this drive — call again |
| `Busy(owner)` | another worker holds the lease; nothing was driven |
| `Failed(why)` | the drive threw and `isolate` caught it; nothing journalled, the run stands |
| `Incompatible(why)` | this code cannot replay history it accepted — a bad deploy |
| `Broken(why)` | the journal cannot be folded — and `why` names a LINE |

## When a program changes under a running journal

Two things happen, and both are deliberate.

- **The program's NAME is stamped on every record.** A journal written
  by `booking/1` and read by `booking/2` STOPS the fold and names both
  — an outage rather than a silent mis-mapping, which is the failure
  that corrupts instead of stopping. And it points at code:
  `Progress.Broken` carries a diagnosis that reads

  ```
  Mismatch(0,booking/2,booking/1) after 1 answer(s);
  this program is at Booking.scala:31 asking nights?
  ```

  A deploy that keeps the NAME and changes the code has the other
  failure: the fold accepts every record and the BODY refuses one.
  That is `Progress.Incompatible`, and it is what `patch` exists to
  avoid — see below.

  Nothing travels in the journal to make that possible — the reader
  replays the part it DID accept and reports where its own program is
  standing, which is the line that matters, because the deploy that
  cannot fold is the reader's.
- **`patch(id)` is how a program changes without a new name.** A run
  whose journal has no decision for that id, and which is still
  replaying, answers `false` and does NOT consume the next answer; a
  fresh run answers `true` and records it. Old runs finish the way
  they began, new ones take the new branch, and a half-finished run
  goes live at the patch and finishes on the new branch with its
  history intact.

## Asking a run to stop

```scala
worker.cancel("booking-42", "customer withdrew")     // an operator, a service, a test
```

and in the program, wherever the author decides it is safe to stop:

```scala
def booking(using w: Wf.Asks[String, String, String, Pure]) = direct:
  val city = !w.pause("which city?")
  !w.sleep(24 * 3600 * 1000L)
  !w.cancelled match                      // the author decides WHERE
    case Some(why) => s"released $city: $why"
    case None      => s"confirmed $city"
```

**Cooperative, on purpose.** A cancellation cannot be delivered here
as a thrown exception: a `direct` block's `try/catch` guards the
*building* of the program, not its running, so a throw could not be
caught by the program being cancelled. An `if` can. What follows from
that is worth saying plainly to anyone adopting this:

- a program with no check is not cancellable;
- a run asleep for a year learns it was cancelled when it wakes;
- a run waiting on a signal that never arrives never learns at all.

What it buys is the property the rest of the engine is built on: **the
decision is replayable**. The request lives in an operational topic
(`Cancels`), but the *answer* — cancelled or not, and why — is
journalled like every other answer. A run told "no" at 10:00 is told
"no" by every replay of that position, even after the request arrives
at 10:01, so a rebuild never takes a branch the original run did not
take. Withdraw a request after the fact and the finished run still
finished the way it finished. There is a test named for exactly that.

Lose the whole cancel topic and no journal is wrong; the runs that
would have stopped simply carry on.

## A run that waits for another run

```scala
def booking(using w: Wf.Asks[String, String, String, Pure]) = direct:
  val id  = !w.pause("start the payment run")   // the ACTIVITY spawns it
  val got = !w.awaitChild(id)                   // the run ENDS here
  s"paid: $got"
```

`awaitChild` stops the run exactly as `sleep` and `awaitSignal` do:
nothing in memory, nothing on the clock. When the child finishes, its
worker writes the result into `Children`, and the next time anybody
advances the parent that result becomes an answer in the parent's
journal — once, guarded by `expect`, however many workers notice it.
A child that finishes BEFORE the parent gets there is found waiting,
the same way an early signal is.

**The parent does not spawn the child, and that is deliberate.** A
worker is built for ONE program — one topic, one body, one set of
types — so a parent's worker has no way to run a different program's
code, and giving it one would mean a registry of erased bodies and a
cast at every spawn. So **starting a child is an activity**: the
parent asks its own question, the oracle calls the child's worker, and
the child's id comes back as an ordinary journalled answer. That costs
no new machinery.

An activity that must not happen twice — a spawn, a charge, a send —
can ask where it is:

```scala
oracle = q => okay.async:
  val at = summon[Dialogue.Attempt]     // (id, index)
  charge(idempotencyKey = s"${at.id}/${at.index}")
```

`Attempt(id, index)` is **the journal's own position**, so it is the
same on every restart and the same across every retry of one
question — which is exactly what makes it usable as an idempotency
key. There are tests for both.

It arrives as context, so an oracle that does not want it is written
exactly as before. **One sharp edge:** an oracle written inline adapts
automatically, but one held in a `val` or `def` does not — pass it as
`q => myOracle(q)`. The compiler says so clearly ("Required:
`String => (Attempt) ?=> ...`"), and it was four call sites in this
repository out of forty.

```scala
oracle = q => okay.async:
  val id = idFor(q)
  drive(childWorker.start(id))      // an ordinary worker, for the child's program
  kids.link(id, parentId, "child/1")
  id
```

`kids.of(parentId)` is the tree view — who started whom, and which of
them are done — which is the question an operator actually asks.

## When a run goes on too long to replay

Replay re-runs the program over its answers. A dialogue with ten
thousand answers runs the program over ten thousand answers on every
cold start — and a `Snapshots` chapter does not help, because a
chapter cuts the READING and this is the RUNNING.

What cuts it is the program ending a stage itself:

```scala
def stage(using w: Wf.Asks[String, String, Wf.Next[String, String], Pure]) =
  direct:
    val input = !w.pause("input")               // the SEED, on a continued run
    if input.length >= 4 then Wf.Next.Done(s"done:$input")
    else Wf.Next.Continue(input + "x")          // close this chapter, open the next
```

```scala
Worker(..., seedOf = Wf.Next.seed)(stage)
```

The worker writes a `Continued(seed)` record, which **supersedes
everything before it**: the journal becomes the seed alone and the
next replay costs one answer however long the history was. A run that
has been through four chapters has a journal of one answer, and there
is a test that asserts exactly that.

Three things to know before using it:

- **The first pause is the input.** A fresh run has it answered by the
  oracle, a continued run by the seed, and the program cannot tell.
  That is the same contract Temporal's `continueAsNew` has.
- **The seed is one of your own answers.** It goes in the journal, and
  the journal holds answers — so its type is the program's answer
  type, not a new one.
- **It is a RESULT, not a call.** Temporal's version is a call that
  never returns. Here the seed has to reach the driver, and the only
  typed channel that carries the author's own types is the result. The
  spec has the refutation in full.

## The cost of replaying, and how to stop paying it twice

Where a run stands is re-derived by running it over its answers. That
is the design, and it has a bill: a process that touches one dialogue
ten times replays it ten times, because nothing held the program in
between. A worker loop spends most of its time exactly there —
touching runs that are still waiting.

A paused program is a closure. It cannot be written down, but it can
be **kept**:

```scala
Worker(..., resume = Some(Resume()))     // default bound: 256 ids
```

Measured, in `TestResume`: five touches of a waiting run replay the
journal **once** with the cache and **five times** without.

The only hard part is knowing when the held program is stale, and the
requirement is sharp — finding out must not cost a fold, or the cache
has paid what it exists to save. The check is one offset read against
the partition's end, and it is deliberately conservative: another
dialogue sharing the partition makes it say "disturbed" when this one
was not. A false "disturbed" costs one replay, which is life without a
cache; a false "undisturbed" would be a program that has missed an
answer. Only one of those is affordable.

It is a per-process optimisation over a journal that stays the only
state. Drop it, restart, run two — nothing changes but how often a
replay happens.

## One run's failure, and the batch

`advance` drives one run, so when its oracle gives up — an exhausted
retry policy throws, on purpose, with nothing journalled and the run
still standing — that throw belongs to the caller who asked.

`tick` drives ALL the due ones, and there the same throw ends the
pass, skips every run after the failing one, and discards what the
earlier ones already did. So a worker loop hands `tick` the ability to
catch:

```scala
Worker(..., isolate = Some(Worker.isolating))
```

and a failing run comes back as `Progress.Failed(why)` beside the
others instead of taking them with it. Nothing was journalled, so the
run is where it was and the next pass asks again — `Failed` is "not
now".

**Three verdicts, and the difference between them is what to do:**

| | what happened | what helps |
|---|---|---|
| `Failed` | an activity threw | come back later; the run is untouched |
| `Incompatible` | the code threw while replaying history it ACCEPTED | a person: fix the code, or retire the run |
| `Broken` | the journal itself cannot be folded | a person: damage, or a foreign program |

The index keeps them apart, so the question an operator actually asks
is a query rather than a text match:

```scala
index.needsAttention        // Broken and Incompatible — never Failed
```

`Failed` is left out on purpose. The worker retries it by itself, so
listing it would fill the page with rows that fix themselves, and a
page like that is one nobody reads.

`Incompatible` is separated from `Failed` on purpose, and the reason
is the discipline the whole design rests on: replay is deterministic,
so a program that throws on its own accepted history throws again on
every pass, for ever. Reporting that as "we will retry" is worse than
saying nothing. The two are told apart without guessing — this one
throws while the place is being REBUILT, before any question is
asked.

It is a parameter and not a default because catching belongs to a
concrete row and a worker's `G` is abstract. That is the same rule the
rest of the library follows: an obligation over a row is carried, not
searched for.

## Two workers on one run

Nothing bad happens, and that is a designed property rather than
luck. Both append, the fold accepts the one whose `expect` matches
the position, and the loser's record changes nothing. The worst case
is a repeated attempt, never a doubled answer.

`Leases` makes the repeat rare:

```scala
Worker(..., leases = Some(Leases.over(store)), owner = "box-3")
```

A worker that finds the lease held reports `Progress.Busy(owner)` and
drives nothing. **It is advisory, and there are two reasons it can
never be more than that.**

1. `acquire` reads, decides and writes — `Topic.append` has no
   conditional form — so two workers whose reads both land before
   either write will both hold it.
2. Even an atomic acquire would not fence. Expiry is decided by a
   clock, and a clock cannot stop a thread: the holder whose lease
   just expired may be inside a slow call and about to append, while
   the next worker takes the lease entirely legitimately.

Closing (2) needs a fencing token checked **at the write** — which is
exactly what `expect` already is. So the lease saves work and `expect`
saves correctness, and a test forces the expired-lease collision and
asserts the journal is still one, so nobody starts relying on the
wrong half.

## Deleting code with evidence

A workflow outlives deploys, so before removing `booking/1` — or the
`else` half of a `patch` — somebody has to answer "is anything still
there". Guessing has one failure mode and it is the expensive one: a
journal folds onto a program that no longer exists, the fold STOPS,
and the run is stuck until the code goes back.

`Retire` answers three questions that cost three different things:

```scala
val c = Retire.census[Wf.Ans[String]](topic)   // envelopes only: exact
c.gone("booking/1")                     // nothing in this topic wrote it

Retire.states(c.programs("booking/1").ids.toList)(worker.dialogue)
                                        // one replay each: who is still ASKING

Retire.patches(journals)(booking)       // a replay AND the body
  .apply("promo").oldHalfDead           // ...nobody is on the else any more
```

**Why the branch census needs the body**, which is the one surprising
part: a journal holds ANSWERS, and a patch's id lives in the
QUESTION. Nothing that reads records can say which branch a
`Flag(true)` belongs to — only running the program pairs them up
again. That is the same fact the whole design rests on, seen from the
other side: the fold is the program, so questions about questions are
questions for the program.

An unreadable record is NAMED rather than skipped: a census that hid
one would be evidence for a deletion it never checked.

This is an operator's tool — it scans partitions and replays runs. A
dashboard should read `Statuses`.

## What this is NOT

Say this half too. The MODEL is smaller and better than a workflow
engine's — the fold that rebuilds the state is the program you already
wrote. The OPERATIONS are younger:

- there is no scheduler process: `tick(now)` is a call you make from
  your own loop;
- retirement is a tool you RUN, not a policy that runs: `Retire` tells
  you what is still there and deletes nothing;
- the lease is ADVISORY (see below): two workers collide harmlessly
  either way, and `expect` is what makes that safe;
- cancellation is **cooperative** (see below), not pre-emptive;
- a parent does not SPAWN its child; starting one is an activity
  (see below), which is a shape to know rather than a gap;
- `Timers.due` and `Signals.next` scan a topic, which is honest for
  thousands of runs and wrong for millions.

A team adopting this gets Temporal's core idea with none of its
operations, and should plan for the difference rather than discover
it.

## Reading order

1. [continuations in practice](continuations-in-practice.md) — the
   four shapes, and what a capture does to everything else.
2. this page.
3. [specs/durable-workflow.md](../specs/durable-workflow.md) — the
   architecture, the stages, and every alternative that was refuted
   with the reason.

---

← [Continuations in practice](continuations-in-practice.md) ·
[Docs](README.md)
