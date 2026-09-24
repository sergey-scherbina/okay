# 23 · Durable workflows

> **Part VI is production.** The operator's guide is
> [docs/durable-workflows.md](../durable-workflows.md) and the
> architecture with every refuted alternative is
> [specs/durable-workflow.md](../../specs/durable-workflow.md). This
> chapter is the book's version: the problem, the shape, what it
> replaced, what it cost, and what went wrong.

---

## The problem

A booking takes three days. It asks a customer a question, waits for a
payment that arrives whenever it arrives, sleeps until tomorrow, and
then decides something. The process running it will be restarted twice
in that window, because deploys happen.

The industry answer is a workflow engine: a state machine, a database
row per run, a `switch` on a status column, and a handler per
transition. The program you actually wanted — five straight lines — is
nowhere in it, and the state machine is a translation of those five
lines that somebody must keep in step by hand.

## The shape

```scala
def booking(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure = direct:
  val city  = !w.pause("which city?")      // the WORLD answers
  val start = !w.now                       // the RUNTIME answers, once
  !w.sleep(24 * 3600 * 1000L)              // the run ENDS here, resumes tomorrow
  val ok    = !w.awaitSignal("payment")    // somebody sends this, whenever
  if !w.patch("promo") then s"$city/promo" else city
```

Five lines, and each is a thing a workflow engine exists to provide:

- **`pause`** — a question for the outside world, asked **once in the
  life of the run** however many times the program is replayed.
- **`now`**, `uuid`, `random` — non-determinism as a *question*. Read
  once, remembered, replayed. A restart does not move the clock.
- **`sleep`** — the run *stops*. No thread parked, nothing in memory: a
  deadline in a topic, and a worker picks it up when the instant
  passes.
- **`awaitSignal`** — the same stop, waiting on an action instead of on
  time. A signal that arrives early waits in a mailbox.
- **`patch`** — a branch new runs take and old runs do not, so a deploy
  need not wait for old runs to drain.

`sleep` is worth staring at. The line looks like it blocks for a day.
It does not block anything: the process exits, the machine is turned
off, and the program resumes tomorrow in a different process on a
different host — at that line, with `city` and `start` in hand.

## The one idea

A paused program is a continuation, and a continuation is a closure:
it cannot be written to disk. **So nothing tries to.** What is written
down is the answers, and where the program stands is re-derived by
running it again over them.

That is event sourcing with one difference that is worth the whole
chapter:

| Event sourcing | Here |
|---|---|
| events | the answers |
| the aggregate's state | where the program stands |
| `apply(state, event)`, written and maintained by hand | **the program itself** |
| rebuild = fold the events | rebuild = run the program on its journal |

The fold you would otherwise write, keep in step with the code, and
get subtly wrong **is the straight-line program you already wrote**.
That is the entire value proposition, and it is why this is a chapter
in a book about continuations rather than a chapter about databases.

## What it replaced

Concretely, for the five lines above:

- a status column and the `match` on it — replaced by the program
  counter, which the replay recovers;
- a handler per transition — replaced by the lines between the pauses;
- a "resume" code path distinct from the "start" one — there is one
  path; a start is a replay over an empty journal;
- hand-written idempotence at every step — replaced by `expect`, one
  mechanism, in the fold.

## The four rules

1. **The journal is the only state.** A deadline, a lease, a status
   line are things to *look at*, never things to *decide from*.
2. **Every question the library adds is a `Sys` question**, so the
   author's own question type never grows and no consumer's `match`
   gains a case it does not own.
3. **A worker may always be killed.** Every move is idempotent or
   guarded by `expect` — an answer carries the position it expected, so
   a second worker's answer for a filled position is rejected by the
   fold. Two workers produce one journal. The failure mode is a
   repeated attempt, never a doubled answer.
4. **The driver's row is not the program's.** The program's row must be
   `Replayable` (chapter 21). The oracle is the half that reaches
   outside and lives in the wider row. A workflow whose own row is
   `Async` does not compile.

## What it cost

The honest list, from the guide's own "what this is NOT":

- **No scheduler process.** `tick(now)` is a call you make from your
  own loop.
- **Retirement is a tool you run**, not a policy that runs. `Retire`
  tells you what is still there and deletes nothing.
- **The lease is advisory.** Two workers collide harmlessly either way;
  `expect` is what makes that safe. The lease only makes collisions
  rare.
- **Cancellation is cooperative**, not pre-emptive.
- **A parent does not spawn a child.** Starting one is an activity —
  a shape to know rather than a gap.
- **`Timers.due` and `Signals.next` scan a topic.** Honest for
  thousands of runs, wrong for millions.

> A team adopting this gets Temporal's core idea with none of its
> operations, and should plan for the difference rather than discover
> it.

That sentence is in the guide because discovering it in production is
how this kind of thing ordinarily goes.

## What went wrong

The part a guide will not tell you. Stage 4 shipped eleven lanes, every
one gated green — and afterwards an audit found seven defects in the
application layer. **None was found by a test or a gate.** They were
found by reading assertions and by counting instead of remembering.

Three that transfer:

**A test that passed against a sabotaged implementation.** The
`continueAs` test stood a stale writer at position two — where the
arithmetic rejects it anyway. Moved to position one, it failed
properly: *a stale answer was accepted into the new chapter*. A test
whose scenario cannot distinguish the bug is green for the wrong
reason, and the only way to find out is to break the code on purpose
and watch.

**An assertion describing an impossible race.** A lease test claimed a
race that sequential calls cannot produce. Its own failure message was
what gave it away — the message described a situation the test could
not create. Rewritten around the real hole (expiry and fencing), it
tested something.

**A warning that survived five green gates.** `E176` sat in
`TestWorker.scala` through five GREEN verdicts, because every one of
those gates ran warm and never recompiled that module. My first
conclusion — "the gate tolerates test-source warnings" — was wrong. The
correct statement is **no gate had seen it**. A green verdict covers
what was compiled, and a warm build compiles less than you think.

And one design defect worth its own line: the same verdict had to be
produced at **four** doors, and three of them had it. Counting the
doors after a fix — rather than remembering how many there were —
turned four call sites into one `placed(d)`.

## The same program as a TERM

Everything above is the monadic shape, and it is the default. Since
`static-workflow` (2026-09-18) the same workflow can also be written
as a **value** — `Proc`, the free arrow over the questions this engine
already journals — and the two share one journal, one worker and one
envelope, so a run started by either is carried on by the other.

What the term buys is what a closure cannot give: `leaves` before the
run, a position that is a PATH (with a counter inside a loop), a
deploy check that asks live journals whether they still fit the new
code without starting one, and a picture drawn from the program rather
than from a projection somebody has to keep in step.

What it costs is `ArrowApply`: a step chosen by a value the program
binds is refused, because that is a monad by Hughes' proof. Branching
and looping are not — the loop the appendix said a static shape could
not have is an `Iter` node, and the appendix carries the correction.

[docs/static-workflows.md](../static-workflows.md) is the page.

## When to reach for this

**Yes** when a business process outlives its process: onboarding,
settlement, anything with "and then we wait for".

**No** for a request handler, a job that finishes in a second, or
anything where the answer is wanted before the caller goes away.
Replay is not free — chapter 22 counted it — and durability you do not
need is a journal you must still operate.

**Not yet** if you need a scheduler, pre-emptive cancellation, or
millions of concurrent runs. The model would hold; the operations are
younger than the model, and that gap is stated above rather than
discovered.

---

← [22 · Saving and restoring](22-checkpoints.md) ·
[Contents](index.md) ·
[24 · A debugger for agents →](24-a-debugger-for-agents.md)
