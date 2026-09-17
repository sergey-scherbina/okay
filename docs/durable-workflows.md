# Durable workflows

A workflow here is **a program that waits** — for a person, another
service, a date in the calendar — written as straight-line code rather
than as a state machine, and able to outlive the process running it.

This page is the practical half. The theory is
[continuations in practice](continuations-in-practice.md); the design
record, with every refuted alternative, is
[specs/durable-workflow.md](../specs/durable-workflow.md).

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
def booking(using w: Wf.Asks[String, String, String, Pure]): String ! (Delim + Pure) = direct:
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

`start` and `advance` return where the run stopped: `Finished`,
`Sleeping(until)`, `Waiting(on)` or `Broken(why)`. Nothing blocks.

## When a program changes under a running journal

Two things happen, and both are deliberate.

- **The program's NAME is stamped on every record.** A journal written
  by `booking/1` and read by `booking/2` STOPS the fold and names both
  — an outage rather than a silent mis-mapping, which is the failure
  that corrupts instead of stopping.
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

## What this is NOT

Say this half too. The MODEL is smaller and better than a workflow
engine's — the fold that rebuilds the state is the program you already
wrote. The OPERATIONS are younger:

- there is no scheduler process: `tick(now)` is a call you make from
  your own loop;
- there is no lease, so two workers collide harmlessly rather than
  rarely (`expect` is what makes that safe);
- cancellation is **cooperative** (see below), not pre-emptive;
- child workflows are named in the spec and not built yet;
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
