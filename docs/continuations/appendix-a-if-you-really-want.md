# Appendix A · If you really want to, you can

> **After the last chapter, because it is the question that comes
> after the last chapter.** Chapter 22 says a paused program cannot be
> written down, and chapter 23 builds an engine around not trying.
> Readers do not accept this, and they are right not to: "cannot" is
> too strong. This appendix is the full answer — every route that
> actually exists, and what each one charges.
>
> The short version: **you can, three ways, and each one takes
> something away that you probably wanted to keep.**

---

## The static route: never be a monad in the first place

The first route, and the one chapter 22 raises: if a monadic program
cannot be written down, what about an applicative one?

The reason a monadic program cannot be written down is one line of
`Free`:

```scala
case Bind[F[+_], A, B](a: Free[F, A],
                       f: A => Free[F, B]) extends Free[F, B]
```

`f` is a host closure. What the program does after its first operation
cannot be read without running it. That opacity is the price of
`flatMap`, and most programs are worth it.

An applicative has no such function. Its structure is fixed before any
value exists, which is why `Static` — the free applicative with
`select` on top — can offer something `Free` cannot:

```scala
def leaves: Vector[F[Any]]    // every operation the program MAY perform,
                              // before it runs
```

Those leaves are operations **as values**. They serialise.

### So: run it as a monad, save it as an applicative?

No, and the reason is one sentence in `Static.scala`:

> The bridge is one-way and cheap: `toFree`.

You can turn a static program into a monadic one in order to run it.
You cannot recover a static program from a monadic one, because
recovering it would mean reading the closure. The moment a `flatMap` is
written, the shape is gone — not hidden, **gone**, in the sense that no
amount of inspection recovers it.

The direction that works is the other one: **start static**.

### What starting static would buy

Everything about checkpointing changes if the structure is known in
advance:

> If a program's shape is static, "where it stands" is an **index**,
> not a closure.

A checkpoint becomes a cursor plus the answers, and a restore is O(1) —
**no replay at all**. That is not a hypothetical design; it is exactly
why BPMN engines, step functions and workflow DSLs look the way they
do. A static graph with a cursor is checkpointable for free, and the
whole family of tools arrived at that shape because it is the only one
that does not have to re-run history.

### What it costs, which is the reason this engine did not

No data-dependent structure. `Select` buys a conditional with **both
sides written down**, and the source names the approximation honestly:

> `leaves` reports both sides of every `Select`, because which side
> runs is decided by a value that does not exist yet. It is an upper
> bound.

An upper bound is what a batcher, a capability list and a dry run all
want. It is not what a workflow wants. You cannot write

```scala
val nights = !w.pause("how many nights?")
for _ <- 1 to nights do !w.pause("which room?")
```

because the *shape* now depends on an answer. The five-line booking of
chapter 23 ends on `if !w.patch("promo")`, and the reason its programs
are worth writing at all is that the line between two pauses is
ordinary Scala.

So the trade is real and it is a trade:

| | position | restore | language |
|---|---|---|---|
| monadic | a closure — cannot be named | replay, O(answers) | anything |
| applicative / selective | an index into a known shape | a cursor, O(1) | no data-dependent structure |

This engine took the left column deliberately, because the code between
pauses is straight-line and replaying it is cheap — chapter 22 measured
it: 40 answers, 40 steps.

### The caveat that survives either choice

An applicative does not make the *program* serialisable either.
`Pure(a)` may hold any value, functions included, and the spine of
combining lambdas is host code whichever functor you chose. What
serialises is the **leaves**, not the spine.

The code still has to be on disk, and it is still addressed by name —
which is what `"booking/1"` in the journal has been doing all along.
That is the deep version of chapter 22's first answer:

> Nobody serialises the code. Not for monads, not for applicatives.
> The difference is whether you can name your **position** in it
> without running it.

### And the hybrid that is already here

`continueAs` is the third answer, and it sits between the two columns:
it does not make the past inspectable, it declares it **superseded**. A
seed replaces the history, so the next replay starts from a value
rather than from an epoch. Not a cursor, not a full replay — a way to
stop paying for a history that no longer describes anything.

## But can we not just walk the tree?

The next question, always, and it deserves a long answer because the
short one ("no") is false.

**Yes, you can walk it, and the machine already does.** `Free` has four
cases:

```scala
case Pure(a: A)                                   // a value
case Inject(a: F[A])                              // one operation
case Bind[F[+_], A, B](a: Free[F, A],
                       f: A => Free[F, B])        // ...and a function
case Delay(thunk: () => Free[F, A])               // ...and a thunk
```

`Pure` and `Inject` are data: an operation is an enum case and it
serialises. `Bind`'s **left** side is a program and walks fine. What
stops the walk is `f`, and it stops it completely, for a reason worth
stating precisely:

> Past a `Bind`, walking **is** running. The only way to learn what `f`
> produces is to apply it to a value.

That is not an implementation limit. It is what a function *is*.

## The surprise: it is not the continuations

The reflex is to blame delimited control, and it is wrong. In this
implementation a captured continuation is **already a data structure**:

```scala
private enum Segs[F[+_], A, Z]:
  case Done[F[+_], Z]()
  case K[F[+_], X, Y, Z](f: X => Y ! (Delim + F), rest: Segs[F, Y, Z])
  case Mark[F[+_], X, Z](p: Prompt[X], rest: Segs[F, X, Z])
```

A list of frames. You can walk it, count the frames, and see which
prompts are installed — `Mark` is a delimiter, sitting in the
continuation as an ordinary node. A continuation here is **more**
inspectable than a native stack, not less, and that is exactly why it
is multi-shot (chapter 13).

The opacity is `K`'s `f`. An ordinary host function — the same one
`Bind` was already carrying before `Delim` existed.

> **The problem was never continuations. It is `flatMap`.** Captures
> only made it visible.

## Three ways to serialise a function

Three ways. All three are real, all three are in production somewhere,
and each pays a different price.

### (a) Serialise the JVM lambda

It works. **This is what Spark does** — it ships closures to executors
as serialised objects, and has for a decade.

*Costs:* the deserialising side needs the same class with the same
synthetic name (`$anonfun$foo$1`). Insert a line above it and the
numbering shifts; recompile with a different compiler and it moves
again.

So it survives **a network hop between identical binaries**, and dies
at **a deploy**. Which is precisely the axis a durable workflow exists
to survive. Spark's problem is space; a workflow's problem is time, and
the same mechanism answers one and not the other.

### (b) Defunctionalise

Reynolds, 1972, and it is the complete answer. Replace every function
with a **data tag plus its captured environment**, and write one
interpreter:

```
apply(tag, env, argument)
```

Every continuation becomes a case of a finite enum. Nothing is opaque,
everything serialises, a position is a tag and a restore is a lookup.

*Costs:* you must enumerate every continuation shape **in advance**. No
host lambdas — you are writing terms of a language you interpret, not
embedding Scala.

That is not a hypothetical either. **BPMN, step functions and every
workflow DSL are defunctionalised programs.** The boxes in the diagram
are the tags, and the engine is `apply`. They can checkpoint without
replay because they gave up being a programming language.

### (c) Name the code, journal the answers

What this engine does — and here is the reframing worth carrying away:

> **Replay is defunctionalisation with one coarse tag.**

The tag is `"booking/1"`. The environment is the journal. The
interpreter is the JVM, re-running the program. We did not escape
defunctionalisation by being clever; we chose a granularity of **one
tag per program** instead of one per continuation, and paid for the
coarseness by re-running the spine.

Seen that way, the three options stop looking like different ideas.

## The trade, in one table

| | what is fixed | what is stored | survives a deploy |
|---|---|---|---|
| serialise the lambda (Spark) | **the binary** | the closure | no |
| defunctionalise (BPMN) | **the shapes** | a tag + environment | yes |
| replay (here) | **nothing** | the answers | yes |

And the claim that unifies them:

> A position is only meaningful **relative to code**. So you either
> freeze the code, enumerate its shapes, or re-run it. There is no
> fourth option — not for engineering reasons, but because a position
> with nothing to be a position *in* does not mean anything.

Worth knowing: **Temporal replays too.** The reference implementation
of this whole category does not serialise stacks either. When a
mechanism that looks like an obvious win is missing from every mature
system in a category, that is usually evidence rather than an
opportunity.

---

## So what would you actually build?

If, having read all that, you still want checkpoint-without-replay,
here is the honest design advice.

**Do not try to recover the shape of a monadic program.** It is gone,
and every hour spent there is spent re-deriving why `Bind` carries a
function.

**Do decide, per program, which half it belongs to.** Most systems have
both kinds and gain by saying which is which:

- A program whose shape is fixed — fetch these twenty keys, run these
  five stages, apply this rule pack — is **static**, and a `Static`-like
  type gives you `leaves`, batching, a dry run, a capability list and,
  if you want it, a cursor.
- A program whose shape depends on its answers — anything with `if
  paid` or `while retries < n` in it — is **monadic**, and replay is
  what it costs. Keep the code between pauses straight-line so replay
  stays cheap; chapter 22 measured 40 steps for 40 answers.

**Do put the boundary between them somewhere you can name.** The two
halves compose in one direction: `toFree` turns the static half into a
program the monadic half can run. Not the other way.

**And if you find yourself defunctionalising by hand** — writing an
enum of "what to do next" cases and an `apply` over it — stop and
notice what you are building. It is a workflow DSL, it will work, and
the reason it works is the reason it is not Scala any more. That is a
legitimate choice; it is just not a free one, and it is better made on
purpose than arrived at after six months.

## The one-paragraph answer, for when somebody asks again

A continuation is a closure, and a closure is code plus captured
values. The values serialise; the code does not, because "this code"
means "this compiled artefact", and the whole point of a durable system
is that the artefact changes. So the three ways to keep a position are
to freeze the artefact (Spark), to replace the code with data you
enumerate (BPMN, and any free applicative), or to keep only the inputs
and run the artefact again (replay, and Temporal, and this engine).
Everything else is one of those three wearing a different name.

---

← [28 · A short history](28-a-short-history.md) ·
[Contents](index.md)
