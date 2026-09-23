# Continuations in practice

The literature examples in [theory/2](theory/02-continuations.md) prove
that delimited control *works*. They do not prove it is *useful* — they
are all from papers. This page is the other half: the four shapes that
earn a capture in ordinary code, each under a name, each next to the
way the same thing is usually written, and a section on when it makes
code worse.

Everything here is in `TestDelimPatterns`, so the code below is
checked rather than claimed.

## The rule

> **Reach for an effect first. Write `shift` only when no effect fits.
> And when you do write one, the first thing you do is wrap it in a
> name.**

Three claims, each with a reason.

**Reach for an effect first.** A continuation is not a feature you
choose, it is what effect handlers are made of: a handler in this
library is a Cont-valued natural transformation (`F !> S = F ==> ([X]
=>> X /> S)`, `Effects.scala:120`), so `Logic`'s backtracking,
`Once`'s call-by-need cell, `State`'s threading and `Choice`'s
multi-shot are already captures — with names, laws, and handlers you
can swap. When one of them fits, using it gets you the capture *and*
the ability to reinterpret it later. Raw `shift` gets you the capture
only.

**Only when none fits.** The honest test is: can you name the effect
your code needs — "it can fail", "it remembers", "it may branch"? If
yes, that noun is an effect, and it belongs in the row. `shift` is
for control flow that is *not* an effect of the program but a shape
of it: leaving early, coming back later, running the rest twice.

**Wrap it in a name.** A `shift` at a call site makes the reader stop
and reconstruct what the continuation is. A name does not:

```scala
if r(t) then !Delim.exit(Some(t))              // obvious
if r(t) then !Delim.shift[Unit](_ => pure(Some(t)))   // a puzzle
```

Both compile to the same thing. The second one asks every future
reader to notice that `k` is dropped and work out what that means.
The name is also where the *evidence* goes: `exit` can only be
written inside a `delimited`, because holding the `Prompted[R]`
requires it — so the wrapper is what turns a runtime `NoPrompt` into
a compile error. Writing a capture and not naming it throws away the
only part of the work that is reusable.

The four names below are what came out of applying that rule to the
patterns people actually hit.

## The second rule: one machine

> **One `Delim.run` per program. The outermost pattern runs the
> machine; everything nested inside it only installs a delimiter.**

`delimited`, `collect` and `resumable` each end by running the
machine. A machine owns one prompt stack, and a capture can only
reach a prompt on the stack of the machine that is running it — so
two machines is two stacks, and a capture that crosses from one into
the other dies with `NoPrompt` at run time. It is not a rare shape:
"a producer that pauses for an answer" is `resumable` around
`collect`, and written with those two names it throws.

Each of the three therefore has a half that does not run:

| runs the machine (outermost) | installs a delimiter (nested) |
|---|---|
| `Delim.delimited` | `Delim.scope` |
| `Delim.collect` | `Delim.collecting` |
| `Delim.resumable` | `Delim.pausing` |

```scala
def half(using Delim.Asking[String, Int, List[Int], Delim + Pure]) =
  Delim.collecting[Int, Pure]:        // nested: installs only
    direct:
      !Delim.emit(1)
      val more = !Delim.pause("more?")  // crosses the collect's delimiter
      !Delim.emit(more)
      !Delim.emit(3)

!.run(Delim.drive(!.run(Delim.resumable(half)))(_ => pure(2)))  // List(1, 2, 3)
```

Under one machine the delimiters compose the way multi-prompt
promises: the `pause` names the dialogue's prompt, the capture takes
the collect's delimiter *with it*, and resuming re-installs it, so the
emits after the answer land in the same list. Both halves are in
`TestDelimNesting`, the wrong spelling pinned beside the right one.

The type system catches the wrong spelling where the row is concrete:
`collect[A, Delim + F]` puts **two** `Delim` in one row, and the
combinators that run a machine refuse that with a message naming the
nested form. It cannot catch an ABSTRACT row — a row-polymorphic
helper still compiles — so the failure is still reachable, and when
it happens the error says everything it knows:

```
the capture at Booking.scala:31 named the prompt 'prompt @ Service.scala:12',
which is not on the stack of the machine running it.
Installed here, innermost first:
  collecting @ Walk.scala:12
  delimited @ Job.scala:40

ONE `Delim.run` PER PROGRAM. …
```

A prompt knows what made it and where; the machine knows which
delimiters it holds. That is the program's own structure, which is
what a JVM stack trace cannot show you once a continuation has been
resumed somewhere else.

### The stack in the type

That error is a run-time one, and it need not be. `Delim.Stacked`
carries the stack of installed prompts as a lexical given: `delimited`
starts it empty and runs the machine, `reset` pushes the prompt it
makes for its body only, and `shift` asks the compiler for evidence
that its prompt is on the stack in force:

```scala
val r = !.run(delimited[Int, P] { s =>
  import s.given
  shift[Int, Int, P](s.p)(k => k(5).map(_ * 2))
})   // 10
```

One `import s.given` per delimiter is the whole cost at a call site;
the type arguments on `shift` are the ones the unstacked door takes
today. Three programs that throw `NoPrompt` at run time are refused by
the compiler: a shift with no reset, a shift to a prompt another reset
made, and a prompt that ESCAPED its reset into a `var` and is shifted
to after it returned — once the reset has returned, the stack in force
is the outer one, and the leaked prompt is not on it. The machinery
underneath is the ordinary machine (`push`, `run`, the same prompts);
the stack is a claim about the program and erases entirely (`okay.Prog`,
specs/freer-base.md stage 2). Not stacked: `shift0`/`control0`, whose
body runs with the delimiter CONSUMED — their index is the stack below
the prompt, which this stage leaves unpriced.

> Gunter, Rémy & Riecke, *A generalization of exceptions and control in
> ML-like languages*, FPCA 1995,
> [doi:10.1145/224164.224173](https://doi.org/10.1145/224164.224173) —
> prompts with a typed identity. Dyvbig, Peyton Jones & Sabry, *A
> monadic framework for delimited continuations*, JFP 17(6), 2007,
> [doi:10.1017/S0956796807006259](https://doi.org/10.1017/S0956796807006259)
> — the multi-prompt machine, and the run-time check this replaces.
> Kiselyov & Shan, *Lightweight static capabilities*, ENTCS 174(7),
> 2007, [doi:10.1016/j.entcs.2006.10.039](https://doi.org/10.1016/j.entcs.2006.10.039)
> — evidence as a value the types carry, the shape `Has` follows.

## 1 · Leave early with an answer

**The shape:** you are deep in nested loops, or inside a lambda, and
you have the answer. You want out, with it.

```scala
Delim.delimited[Option[Int], Pure]:
  direct:
    for t <- txs; r <- rules do
      if r(t) then !Delim.exit(Some(t))   // out of BOTH loops, with a value
    None
```

**How it is usually written** — the loops become a fold, and "am I
done already" is now a question every iteration has to ask:

```scala
txs.foldLeft(Option.empty[Int]): (acc, t) =>
  if acc.isDefined then acc
  else if rules.exists(_(t)) then Some(t) else acc
```

That fold is fine at this size. It stops being fine when the work
inside the loop is not a predicate, when there are three levels, or
when the exit is inside a callback that the loop does not own. The
alternatives at that point are an exception thrown for control flow
(untyped, and it walks past everything that was counting on an
orderly exit) or a sentinel threaded through every caller.

`exit` is a capture that **drops** its continuation — that is all an
early return is. `Delim.abort` is the same thing outside a direct
block.

## 2 · A push producer, read as a pull

**The shape:** the producer wants to call you; you want a sequence.

```scala
def walk(t: Tree[Int])(using Delim.Emitting[Int]): Unit ! R = direct:
  t match
    case Tree.Leaf(a)    => !Delim.emit(a)
    case Tree.Node(l, r) => !walk(l); !walk(r)

Delim.collect[Int, Pure](walk(t))    // List(1, 2, 3)
```

**How it is usually written:** a `ListBuffer` threaded through the
producer as a parameter, or a callback parameter that turns the
producer inside out and makes it someone else's job to know when it
ends.

The walk above is ordinary recursion and knows nothing about lists:
`emit` builds the list out of *the rest of the walk*, which is why
nothing has to be inverted. The same shape covers paginated APIs, a
`ResultSet`, a watcher, a parser — anything that pushes when you
wanted to pull.

**And stopping it.** A `collect` reads the producer to its end;
`collectUntil` reads it until a `FoldUntil` says it has seen enough,
and the rest of the producer never runs — the same `walk`, unchanged:

```scala
Delim.collectUntil[Int, Vector[Int], Vector[Int], Pure](using FoldUntil.take(2))(walk(t))   // Vector(1, 2) — the third leaf is never visited
Delim.collectUntil[Int, Option[Int], Option[Int], Pure](using FoldUntil.find[Int](_ > 1))(walk(t))  // Some(2)
```

Why `exit` inside a `collect` could not do this: `collect` builds its
list on the way *back*, in the continuation frames, so an early exit
drops exactly the prefix it would want to answer with. `collectUntil`
passes the fold's state on the way *down* instead — the prompt's
answer is a function of the state, Filinski's trick for state over
`shift`/`reset` — and a stop simply does not call the rest of the
walk. `take(0)` runs no body at all; a fold that is never done answers
what `collect` answers. `collectingUntil` is the nested half, as
`collecting` is of `collect`.

## 3 · Stop in the middle, carry on later

**The shape:** the program needs an answer from outside — a person, a
different service, the next HTTP request.

```scala
def booking(using Delim.Asking[String, String, String, R]): String ! R = direct:
  val city   = !Delim.pause("Which city?")
  val nights = !Delim.pause(s"How many nights in $city?")
  val pay    = !Delim.pause(s"Pay ${nights.toInt * 90} for $city?")
  if pay == "yes" then s"Booked $city for $nights nights" else "Cancelled"

val start = !.run(Delim.resumable[String, String, String, Pure](booking))
!.run(Delim.drive(start)(answering(List("Kyiv", "3", "yes"))))
```

`resumable` answers with a `Paused[Q, A, R, G]`: either `Ask(question,
resume, at)` — and `resume` **is** the rest of the program — or
`Done(value)`. `asking` is the question, `where` is the line the
`pause` was written on: a dialogue that has not moved since Tuesday
reads as *waiting at `Booking.scala:31` on "Pay 270 for Kyiv?"*,
which is the difference between an incident and a puzzle.

**How it is usually written:** a state machine with a `step` column
and a hand-rolled record of everything the process knew so far, which
has to be written, migrated, and kept in sync with the code by hand.
Here the process is straight-line code and the record is the
continuation.

A `Paused` is a *value*, so resuming it does not consume it — the test
answers the same start page twice, differently.

### Outliving the process

A continuation is a closure, and a closure cannot be written to disk.
So the thing you persist is not the `Paused` — it is the **journal**,
the answers given so far, in order. Where the dialogue stands is then
re-derived:

```scala
val (p1, j1) = !.run(Delim.answer(p0, Nil)("Kyiv"))
val (p2, j2) = !.run(Delim.answer(p1, j1)("3"))    // j2 = List("Kyiv", "3")

// ---- the process dies here. p0, p1, p2 go with it; j2 was written down.

val back = !.run(Delim.replay(booking)(j2))
back.asking    // Some("Pay 270 for Kyiv?") — the same place
```

This is what durable workflow engines do (Temporal, Cadence, Durable
Functions), and here it is nine lines in `Delim` rather than a
runtime. What has to be storable is the answers — ordinary data, not
code.

It is exact under one discipline:

> **Everything the outside world tells the program enters through
> `pause`.**

Since `dialogue-replay-discipline` that sentence is a TYPE, not a
hope: `Delim.replay` and `okay.persist.Dialogue` ask for
`Replayable[F]`, and a row holding `Async`, `Writer` or `Resource`
does not have it — so a body that calls a service between two pauses
does not compile as a durable dialogue. Breaking it on purpose is
still possible and has to be written down: `Replayable.unchecked`,
which is a method rather than a given precisely so that it appears in
the diff.

Then the program is a pure function of its journal and replay cannot
diverge from the original run. Break it — read a clock, call a
service, roll a die anywhere but a `pause` — and replay re-runs that.
Both halves are in `TestDelimPersist`, measured rather than promised:
one test watches a `Writer` log say the same thing twice across two
runs, and the next one writes the same program to the discipline and
watches the driver perform each outside call exactly once.

### It is event sourcing, with the fold already written

`okay-persist` puts that journal in a topic:
`Dialogue[Q, A, R, F](topic, id, program)(booking)` — `at` is where the
program stands, `answer(a)` advances and then appends durably,
`run(oracle)` drives it to the end, calling the oracle once per
question and never for one the journal already answered. A second
process over the same topic stands exactly where the first one stood.

Four things it does that are invisible until they matter, each because
probing found the failure first (specs/durable-workflow.md, stage 0):

- **an answer the program refuses is not journalled.** The advance
  happens first; only a value reaches the append. Before that, one bad
  answer killed a dialogue permanently — every later process replayed
  it and threw.
- **a journal written by a different `program` stops the fold** and
  names both ids, instead of mapping old answers onto new questions.
  That is the deploy problem, and a loud stop is the honest half of
  it; `patch` (Temporal's `getVersion`) is stage 2.
- **a second writer is told it lost.** Each record carries the
  position its writer expected, and the fold takes only the one that
  fits; the loser gets `Answered.Lost` and is shown where the dialogue
  actually stands.
- **the oracle gets an idempotency key** — `(id, index)`, stable
  across restarts. It needs one: a crash between performing the call
  and journalling its answer re-asks that question, which is the
  at-least-once contract every workflow engine has.

Which is event sourcing, with one difference worth naming:

| Event sourcing | Here |
|---|---|
| events | the answers — the only non-determinism the discipline allows |
| the aggregate's state | where the program stands |
| `apply(state, event)`, written by hand | **the program itself** |
| rebuild = fold the events | rebuild = run the program on its journal |

The fold you would otherwise write, keep in step with the code, and
get subtly wrong is the straight-line program you already wrote. What
is stored is the answers; what interprets them is the code.

That discipline has a second payoff: a program whose every outside
call is a question is also a program you can test by answering the
questions — no mocks, no doubles, and the journal of a failed
production run replays on a laptop.

### A durable program, as it actually reads

```scala
def booking(using w: Wf.Asks[String, String, String, Pure]) = direct:
  val city = !w.pause("city?")          // the world answers
  val when = !w.now                     // the RUNTIME answers, once, and it is journalled
  val n    = !w.pause("nights?")
  if !w.patch("promo") then s"$city/$n/promo at $when" else s"$city/$n at $when"

Dialogue.workflow[String, String, String, Pure](topic, id, "booking/1")(booking)
  .run(Dialogue.asking(oracle))
```

Three things in that block are not ordinary code and all three are
invisible: `now` is read once in the life of the dialogue and replayed
from the log ever after, so a restart does not move the clock;
`patch("promo")` answers `false` for every run that began before the
branch existed and `true` for every run that began after, so a deploy
does not have to wait for the old runs to drain; and the whole thing
is a straight line, not a state machine.

A whole page follows this one where the engine is concerned:
[durable workflows](durable-workflows.md) — the worker, durable
timers, signals, the status index, and the rules that keep them from
becoming a second source of truth.

### What this is not

Say both halves when proposing it. The MODEL here is smaller and
better than a workflow engine's — the fold that rebuilds the state is
the program you already wrote. The OPERATIONS are YOUNGER, and the
list changed on 2026-09-17 when the operator asked for the engine:
durable timers, signals with a mailbox, retry policies on the
activity, a visibility index and a worker now exist
([durable workflows](durable-workflows.md)); cancellation, child
workflows, bounded history and a lease do not, and there is no
scheduler process — `tick(now)` is a call you make from your own loop.
specs/durable-workflow.md stage 4 tracks each one. A team adopting
this should plan for that difference rather than discover it.

## 4 · Do something on the way back

**The shape:** from somewhere in the middle, you want to act on what
the *rest* of the block produces.

```scala
Delim.delimited[String, Pure]:
  direct:
    !Delim.onReturn(s => if s.startsWith("failed") then s"$s; refunded $amount" else s)
    if ok then s"charged $amount" else "failed: card declined"
```

**How it is usually written:** wrap the remainder in a function and
pass it down (which restructures everything after this point), or a
`finally` — which cannot see the answer, so it cannot compensate
based on it.

This is the generalization of the reverse-mode AD example: run the
rest, then do something with what came back. Compensation in a saga,
undo, an audit line that records what the remainder produced,
measuring what the rest of a request cost — all the same two lines.

## What a capture does to everything else

The questions everyone asks in the first week, answered by running
rather than by reasoning — every line below is a test in
`TestDelimLimits`, and the ones that surprised us are marked.

| you write | what happens |
|---|---|
| `Resource.acquire`, then `exit` (k dropped) | **the release still runs.** Resource's handler is outside the machine, so it closes what was opened |
| `Resource.acquire` under a k invoked twice | two acquires, then two releases **at the end of the program**, LIFO — n branches hold n handles at once |
| a cleanup line written by hand after the capture point | **it does not run.** It was part of the continuation that was dropped. Cleanup goes in `Resource`, not in the block |
| `bracket` in a row containing `Delim` | **a compile error** ("no Handler"). Bracket runs its body to completion in one suspension, which is what a capture breaks — so the unsafe mix cannot be written |
| `try { !x } finally { … }` in a `direct` block | **a compile error**, naming the finalizer |
| `try { !x } catch { … }` in a `direct` block | **compiles, and catches nothing.** The catch guards the BUILDING of the program; the throw happens when it is run, one stack away. Failure belongs in the row: `Throws` |
| `raise` inside or instead of a captured `k` | reaches the handler normally; the abandoned part does not run |
| `State` around a multi-shot capture | **the branches share one timeline** — the second `k` sees what the first one wrote. If you want a fork, the effect for it is `Choice`/`Logic`, not `State` |
| a `var` touched by the rest of the block | same reason: the block runs once per invocation of `k`, and the `var` is shared across those runs |
| 10 000 `emit`s, 3 000 `pause`s, replay of 3 000 answers | all fine — the machine is a loop, not the JVM stack |
| `exit` from inside a lambda the block does not own (`xs.map { … }`) | works, with a typed answer |
| a `pause` on either side of an async operation | works; the row is `Delim + Async`, the machine suspends for the foreign operation and resumes with the same stack |

Two more costs that are not tests:

- **Stack traces stop describing your code.** The machine reifies the
  continuation, so a debugger shows the interpreter's loop, not the
  call chain you wrote.
- **A capture is not free.** Installing a delimiter costs about what
  an ordinary operation costs; CAPTURING and re-invoking the
  continuation measured 3.8x a purpose-built effect on the generator
  lane (specs/delimited-control.md). That is the argument for reaching
  for an effect first — and it is also why the price only shows up in
  a hot loop, never in a dialogue that pauses for a human.
- **`shift` where `flatMap` would do.** If the control flow is a
  sequence, `direct` is the whole answer. A capture for elegance is a
  cost with no matching benefit.

## Deciding

| The control flow you need | Reach for |
|---|---|
| a sequence, possibly failing | `direct` over your row |
| it can fail / branch / remember | the effect: `Fail`, `Choice`, `State`, `Once` |
| leave from the middle with an answer | `Delim.exit` (`Delim.abort` in `for`) |
| a producer that pushes, a consumer that pulls | `Delim.collect` / `Delim.emit` |
| ...pulled lazily, or stopped early | `Generate` / `Producer` for the lazy pull; `Delim.collectUntil(using fo)` for a push producer stopped by a `FoldUntil`; on a stream the fold itself (`Stream.foldUntil`, `Writer.foldUntil`, `Chunks.foldUntil`, `Source.runFoldUntil`) |
| stop now, resume when the answer arrives | `Delim.resumable` / `pause` / `drive` |
| ...and survive a restart | `Delim.answer` + `Delim.replay` over the journal |
| ...and keep the journal in a durable log | `okay.persist.Dialogue` |
| act on what the rest of the block answers | `Delim.onReturn` |
| any of the above INSIDE another one | the nested half: `scope` / `collecting` / `pausing` |
| none of the above | `Delim.shift`, and then give it a name |

## Introducing it to a codebase that has none

The order below is the one that keeps every step defensible to a
reviewer who has never read a continuations paper. Nothing in it
requires a rewrite: the doctrine for both control facilities
(specs/delimited-control.md, "Adoption doctrine") is *additive by
default* — a consumer who ignores the new door loses nothing.

1. **Nothing.** Write `direct` over your effect row. Most code needs
   no capture at all, and a team that has not yet felt the pain will
   not keep a tool it did not need.
2. **A named exit.** The first capture anyone should write is
   `Delim.exit`, because the thing it replaces — an exception thrown
   for control flow, or a sentinel threaded through five signatures —
   is already in the codebase and already disliked. One `delimited`
   at the top of a function, one `exit` in the middle.
3. **A producer read as a list.** `collect`/`emit`, where a callback
   API or a `ListBuffer` parameter is being threaded today.
4. **The one that pays for the rest: a resumable process.**
   `resumable`/`pause`, for anything that waits on a human, another
   service, or the next request — approvals, wizards, onboarding,
   any two-phase protocol. This is where the alternative is a
   hand-written state machine with a `step` column, and where the
   straight-line version is not a cleverness but a smaller thing to
   own.
5. **Durable, when step 4 must survive a restart**: the journal, and
   `okay.persist.Dialogue` if it belongs in a log. Adopt the
   discipline with it — *everything the outside world tells the
   program enters through `pause`* — because that single rule is what
   makes replay exact, testing mock-free, and a production journal
   replayable on a laptop.

The honest stopping point is step 1 for most modules and step 4 for
the one or two places that have a waiting process in them. A library
that is only ever used for step 4 has still paid for itself, because
step 4 is where teams otherwise build a workflow engine.

---

← [Direct style](direct-style.md) · [Docs](README.md) ·
[2 · Continuations (theory)](theory/02-continuations.md) →
