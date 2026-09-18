# Static workflows: the same program, as a term you can read

A durable workflow here is ordinarily a **monadic** program — you
write it with `direct`, it runs on `Dialogue.workflow`, and where it
stands is re-derived by replaying it over its journal
([durable workflows](durable-workflows.md)). That is the right default
and it is not going anywhere.

This page is about the other half: the same workflow written as a
**term**, so that its shape is a value you can walk before you run it.

> Every block on this page is a test. The terms and the block are
> `src/test/scala/TestProc.scala` and `TestProcDirect.scala`; the
> journal-sharing ones are `okay-persist`'s `TestWorkflowProc`.

## Why a second shape exists at all

The design record for the monadic engine ends on a sentence worth
repeating, because it is what this shape is for:

> Past a `Bind`, walking **is** running.

A monadic program's continuation is a host closure. You cannot ask it
what it will do next without doing it — so "which questions may this
run ask", "would this deploy strand it", "draw me where it is" are all
questions that can only be answered by starting it.

A term has no closure in its spine, so all three become ordinary
folds:

| question | monadic | as a term |
|---|---|---|
| what may it ask? | run it | `leaves` |
| where does it stand? | replay it | `walk`, which performs nothing |
| would this deploy strand it? | find out in production | `accepts(journal)`, before the deploy |
| draw it | a status projection a worker keeps in step | `render` |

## The block

The straight-line spelling is the same one the monadic workflow uses,
and it compiles to the term:

```scala
val booking: Wf.Proc[String, String, Unit, String] =
  Proc.direct[Sig, Unit, String]: _ =>
    val city = !ask("city?")
    val t    = !now
    val id   = !uuid
    s"$city/$t/$id"
```

Beside its monadic twin, which asks the same three questions:

```scala
def bookingMonadic(using w: Wf.Asks[String, String, String, Pure]): String ! (Delim + Pure) =
  direct:
    val city = !w.pause("city?")
    val t    = !w.now
    val id   = !w.uuid
    s"$city/$t/$id"
```

**They write the same journal, record for record**, and that is
asserted rather than hoped: a run started by one is carried to the end
by the other over one topic. Pick a shape per program, not per system.

What `!` marks here is an **operation** — a question of the signature
— never a `Proc`. That distinction is the whole of what makes the
spine static, and the macro refuses the other case by name:

```
Proc.direct: this mark's value is a Proc, so the STEP is chosen by a
value this block binds — that is `app`, and an arrow with `app` is a
monad (Hughes 2000, §4.5).
```

## What you get for it

```scala
booking.leaves.map(_.name)        // Vector("ask", "now", "uuid")
```

Before anything runs, and the names are **the author's own**: the leaf
is named after the door the block called, so a term reads in the
vocabulary of the program rather than of the library.

```scala
Wf.Proc.walk(booking)((), journal)
// Right(Standing.Done("Kyiv/1700000000000/id-1"))
// Right(Standing.Asking(path, Question.Uuid(), accepted = 2))
// Left(Stranded(path, record = 1, "Now() cannot take Right(Kyiv)"))
```

`walk` has **no runtime, no row and no monad in its signature**, which
is the proof it performs nothing — the same way `Wf.replay` taking no
`Runtime` is the proof it cannot read a clock. It is what lets a
deploy ask ten thousand live runs whether they still fit the new code
without starting one of them.

And a journal that does not fit is **data**, not an exception: the
path says where, the record number says which, and the question says
what it could not take.

## Two readings of one position, and they must agree

This is the property the whole shape rests on:

```
walk(term)(x, journal)   ==   Wf.replay(program(term)(x))(journal)
```

`walk` is what the deploy check and the picture trust; `replay` is
what the engine trusts. They are asserted equal on **every prefix** of
several journals, including one written before a `patch` existed. A
disagreement is a bug found before a journal is.

It has been watched failing, which is the half that makes it evidence.
Breaking the fold three ways:

| break | what goes red |
|---|---|
| the loop stops counting its rounds | the position test, printing the path it got |
| a leaf reads an answer without consuming it | six tests, both agreement properties among them |
| a patch eats the record after it | the old-journal pair |

## Branches

An `if` whose branches ask questions is part of the term, both sides
of it:

```scala
val districted = Proc.direct[Sig, Unit, String]: _ =>
  val city = !ask("city?")
  if city == "Kyiv" then
    val d = !ask("which district?")
    s"$city/$d"
  else s"$city/whole"

districted.leaves.map(_.name)   // Vector("ask", "ask") — BOTH branches
```

`leaves` reports both because which side runs is decided by a value
that does not exist yet; a run asks only the taken one. That
over-approximation is the whole point — a capability list, a dry run
and a deploy check all want the upper bound.

The condition may be a question too (`if !patch("promo") then …`),
and an `if` over values already bound is ordinary code inside an
`Arr`, costing no node.

**Top-level only**: a val's right-hand side, a statement of its own,
or the block's answer. An `if` nested inside a larger expression is
refused, and the message says why — it would have to hoist, and a
hoisted mark RUNS whether or not its branch is taken.

## Loops, and the one node the literature lacks

The classic objection to a static workflow is that its shape cannot
depend on an answer. That objection is about `Selective`, whose
`whileS` is a recursive *definition* — an infinite term. **Elgot
iteration is a node**, so the shape stays finite:

```scala
val rooms = Proc.direct[Sig, Unit, List[String]]: _ =>
  val n = !ask("nights?")
  var got = List.empty[String]
  while got.length < n.toInt do
    got = got :+ !ask(s"room ${got.length + 1}?")
  got
```

Ask how many nights, then one question per night — in a term whose
`leaves` are two, because the body is counted once.

**Nothing mutates.** An assignment compiles to a rebuild of the
environment with one slot replaced, so the value that goes round the
loop travels on the arrow's edge. That is why a replay re-derives it
exactly, and why the position inside a loop is a path with a counter:

```scala
Wf.Proc.walk(rooms)((), List(Right("3"), Right("a"), Right("b")))
// Asking(at = ".../round2/...", Question.Ask("room 3?"), accepted = 3)
```

`Iter` is not `ArrowLoop` — Paterson's `loop` is lazy *value* feedback
and cannot say "run the body again".

## What it does not do, stated

- **A `for` or a `foreach` over a collection** is a lambda, and a
  question under a lambda is the corner `direct`'s v1 refuses too.
  Write the loop as a `while` over values the block binds; the
  refusal says exactly that.
- **A question in a `while` condition.** The test would have to ask
  once per round inside the loop — expressible, not wired.
- **An `if` nested inside a larger expression**, for the hoisting
  reason above. Bind it to a val first.
- **The environment is not pruned.** Every bound name rides on the
  edge until the end of the block, as a left-nested tuple. A liveness
  pass would drop the dead ones; nothing has asked for it, and the
  cost is tuple allocation between leaves that are outside calls.

## Which shape to pick

**A term** when the program's shape is fixed — a booking, an
onboarding, a settlement with a known set of steps — and when any of
these matters: knowing the questions in advance, checking a deploy
against live runs, drawing the process, or an exhaustive crash test
(a finite set of leaves means "crash before and after every one of
them" is a property, not a sample).

**A monadic program** when the shape genuinely depends on the answers
beyond a loop — "read the next workflow's name from an answer and run
it" — or when the straight-line code is the point and nothing above is
wanted. Replay is cheap; the design record measured 40 steps for 40
answers.

**They compose one way.** A term becomes a program (`Wf.Proc.program`)
and can be an activity of a monadic workflow; a monadic program can
answer a term's question as an activity. There is one journal format,
one envelope and one worker underneath.

## The algebra, for the curious

`Proc[F, X, Y]` is the **free arrow** over a signature — `Static`'s
neighbour one rung up Lindley, Wadler and Yallop's ladder (*idioms are
oblivious, arrows are meticulous, monads are promiscuous*):

| rung | type here | what it can do |
|---|---|---|
| applicative | `Static[F, A]` | effects fixed before the run, no input |
| **arrow** | **`Proc[F, X, Y]`** | a step may look at what came before it |
| arrow + `app` | a monad | a step may BE what came before it |

The line is `ArrowApply`: Hughes (2000, §4.5) proves an arrow with
`app : P[(P[A, B], A), B]` is exactly a monad, and `Free.Bind`'s
`f: A => Free[F, B]` is `app` spelled as a closure. So the rule for
the spine is not "no monads" but **no operation that takes a
computation as data** — inside a leaf a monad is welcome and is the
point, since an activity runs whatever it likes between two journal
records.

The instance is `Optic.Arrow & Optic.Choice`, so every optic in the
library applies to a step with no new machinery, and the laws are the
shared suite `okay.laws.ArrowLaws` — fifteen of them, instantiated in
three lines, the same suite `Mealy` uses.

---

The design records: [specs/static-workflow.md](../specs/static-workflow.md)
(the shape and what it buys), [specs/proc-notation.md](../specs/proc-notation.md)
(the notation and its translations), [specs/arrows-plan.md](../specs/arrows-plan.md)
(how these lanes were ordered and what each one tested).
