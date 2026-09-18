# proc-notation: the `direct` macro at an arrow — one block, four translations

## Overview

specs/static-workflow.md priced its own design before building it,
and the first line of the bill was *combinators, not straight-line
code: Scala 3 has no `proc` notation*. The operator's answer the same
day (2026-09-18): that price is to be designed away NOW, not paid and
then gated on complaints — "for applicatives and monads something was
already done; what stops us adding it where we need it, the way we
need it?" This spec is the answer, and it is shorter than the question
suggests, because most of the mechanism exists.

### What `proc` notation is

Paterson (2001, "A New Notation for Arrows"): a block whose bound
names are threaded through an ARROW instead of a monad.

```haskell
proc x -> do
  y <- f -< x + 1        -- an arrow f applied to an argument
  z <- g -< y            -- the argument may use y; f and g may NOT
  returnA -< (y, z)
```

Its translation is mechanical and it is the whole of the idea: the
ENVIRONMENT (the tuple of names bound so far) travels as the arrow's
input, `first`/`second` carry it past each step, `arr` reshapes it, a
conditional becomes `left`/`|||` (Choice), and a loop becomes `loop`
(ArrowLoop — here `Iter`, static-workflow's Decisions say why). The
one rule that makes it an arrow block and not a monad block: **the
arrow left of `-<` may not mention a name the block binds** — that
would be `app`, which Hughes proved is a monad. Everything to the
RIGHT of `-<` may.

### What this repository already has, which is most of it

`direct` is not a monad macro. It is a block normaliser with the
translation chosen by the algebra the carrier PROVES at expansion:

| the carrier has | the macro emits | since |
|---|---|---|
| `Monad` | `flatMap` binds (ANF, statement-level) | direct-macro v1 |
| `Applicative` only | the idiom bracket for a run of independent binds; a DEPENDENT bind is refused BY NAME | applicative-do, 2026-09-18 |
| `Selective` | `ifS` for an `if` whose condition is an effect | selective-do, 2026-09-18 |
| `Async` in the row, opt-in | independent binds spawned and joined flat | applicative-static stage 3 |

and, target-independent: mark hoisting out of subexpressions (ANF),
`for`/`while` with marks inside (direct-loops), `try` (direct-try),
auto-colouring behind a capability, the dependency analysis
(`independentRun`, `mentionsAny` — does this right-hand side mention
a name bound earlier). That analysis is Turner's K-versus-S question
(applicative-static Overview), it is ApplicativeDo's question (Marlow
2016), and it is Paterson's `-<` rule: **the same question decides
every rung.** An arrow target is one more translation of the SAME
normalised block, not a second macro.

The ladder, so the rung is named (Lindley, Wadler & Yallop 2011,
"Idioms are oblivious, arrows are meticulous, monads are
promiscuous"): static arrows ≅ applicatives, arrows + `app` ≅ monads,
and between them sit `ArrowChoice` (Selective's cousin) and iteration.
`direct` already serves the two ends; this spec adds the middle.

### What it buys against static-workflow's bill

| the price as stated | after this spec |
|---|---|
| combinators, not straight-line code | the SAME block text as the `Wf` booking, at a different entry: `Proc.direct` |
| the state is threaded explicitly | the macro threads it — environment tuples live in `Arr`, which is pure and never journalled, so no `Schema` is asked for any of them |
| shape only through `left` and `iter` | `if` and `while`/`for` in the block ARE `left` and `iter`; what is refused is refused by name: "`f` is chosen by `city`, which this block binds — an arrow cannot run a step it does not know before it starts; bind the choice into the question instead" |

What stays a price, and it is the same price every `direct` block
already pays: a mark under a lambda that is not a whitelisted loop
shape is refused (direct-macro v1), and a nested block is its own
block.

## Interface

```scala
object Proc:
  /** THE ENTRY: a straight-line block over questions, compiled to a
   *  Proc. The block's parameter is the procedure's input; the
   *  block's value is its output. */
  inline def direct[Q, A, X, Y](inline block: Proc.In[Q, A, X] ?=> X => Y): Proc[Q, A, X, Y]

  /** the capability, like `DirectCtx`: exists ONLY inside a Proc
   *  block, and every door below asks for it */
  final class In[Q, A, X] private[Proc] ()

  // the doors AS WRITTEN IN A BLOCK: each takes the question VALUE
  // (an expression over the block's names) and stands as the mark;
  // the macro closes the expression over the environment into an
  // `Arr` and turns the door into its leaf. Outside a block they
  // are the combinator doors of static-workflow, unchanged.
  //   val city  = !ask("which city?")
  //   val start = !now
  //   !sleep(day)
  //   val ok    = !awaitSignal("payment")
  //   if !patch("promo") then s"$city/promo" else city
```

**The translation, per shape** (the environment `E` is the tuple of
the block's live names at that point; `E+y` is `E` with `y` added):

| block shape | emitted term |
|---|---|
| `val y = !ask(e)` | `Arr(env => (env, e(env))) >>> Second(Ask(identity, read)) >>> Arr(E+y)` |
| a pure `val y = e` | folded into the next `Arr` — no node of its own |
| `if c then t else u` with leaves inside | `Arr(env => if c(env) then Left(env) else Right(env)) >>> (T ||| U)` where `T`, `U` are the branches compiled at the same environment |
| `if !p then t else u` | the scrutinee leaf first, then the same |
| `while c do body` with leaves inside | `Iter(Arr(env => if c(env) then Left(env) else Right(env)) >>> Left(BODY))` where the names the body ASSIGNS are the loop-carried part of `env` |
| `for x <- xs do body` | the same `Iter` over `(env, rest)` — the collection rides on the edge |
| the block's result `r` | `Arr(env => r(env))` |
| `!f(x)` where `f` mentions a block-bound name | REFUSED, naming the name and the line — this is `app` |
| a mark under any other lambda; `try` | REFUSED, direct-macro v1's message |

**Liveness keeps the tuple small**: a name is carried only while a
later statement mentions it. This is the one place the arrow
translation does work the monad translation never had to, and it is
measured in Behavior rather than assumed cheap.

## Behavior

### Stage 1 — the arrow road (`proc-notation-road`)

- [ ] the five-line booking of docs/continuations/23, with `Proc.direct`
      in place of `direct` and NO OTHER CHANGE to the text, compiles to
      a `Proc` whose `leaves` are `[Ask, Now, Timer, Signal, Patch]`
      in that order, and whose `toProgram` run through
      `Dialogue.workflow` produces the SAME journal as the monadic
      booking — pinned by comparing the two journals byte for byte
- [ ] the same text at `direct` (monadic) and at `Proc.direct` is ONE
      test source with two expected types, so a spelling that works at
      one and not the other fails the build
- [ ] "ask `nights?`, then one `room?` per night" written as a `for`
      over `1 to nights` compiles to an `Iter`, and the position after
      the second room is `Iter(2, …)` under `walk`
- [ ] `if` on a pure condition over a bound name with a leaf inside a
      branch compiles to `Left`; `leaves` reports both branches; a run
      asks only the taken one
- [ ] a leaf chosen by a bound name — `val f = if city == "Kyiv" then
      askA else askB; !f(x)` — is refused with a message naming `f`,
      `city`, the line and the rewrite (`!ask(if city == "Kyiv" then
      qa else qb)`); the refusal is `compileErrors`-pinned
- [ ] every existing `direct` test passes untouched and the emitted
      tree for a monadic carrier is unchanged (bytes: the direct lanes
      move by 0 B/op) — the first risk of touching a macro the whole
      repository uses, stated first as applicative-do did
- [ ] a pure `val` between two leaves emits NO node: the term for
      `val a = !ask(q); val b = a + 1; val c = !ask(b.toString)` has
      exactly two leaves and its `Arr`s are folded
- [ ] liveness: a block binding ten names of which the result uses one
      carries a one-element environment at its last leaf (asserted on
      the term's shape, and the tuple arity measured, not assumed)

### Stage 2 — one front end, four back ends (`direct-targets`)

Gated on stage 1 showing duplication — the third road in Direct.scala
(monadic, applicative-only, arrow) is the point at which "count the
doors" applies:

- [ ] the block normaliser (ANF, dependency analysis, loop and `if`
      shapes) is ONE function producing a small IR; each target is
      a translation of the IR — `Target.Monad`, `Target.Applicative`
      (+ `Selective`), `Target.Arrow` (+ `Choice`, + `Iter`)
- [ ] the target is chosen by evidence summoned at expansion, strongest
      available, exactly as `Monad`-then-`Applicative` is today; a
      shape the chosen target cannot express is refused by the
      target with the name of the rung it would need
- [ ] adding a fifth target is one file that pattern-matches the IR,
      and the spec names the first candidate: `Static` (an input-less
      Proc), so that a `direct` block at `Static` gets `Select` for
      its `if` where today it gets `ifS` through the carrier

### Stage 3 — the doors that read as statements (`proc-doors`)

- [ ] `!sleep(d)`, `!awaitSignal(n)` and the other `Unit`-typed doors
      stand as STATEMENTS without `: Unit` ascriptions, the way
      `w.tell` does (direct-tell): the door is `transparent inline`
      and decides by `In` in scope
- [ ] `patch(id)` in an `if` condition is the Selective shape at the
      arrow rung: the `Patch` leaf, then `Left` — and `walk` obeys the
      non-consuming rule through it

## Out of scope

- `ArrowLoop`/`rec` — value recursion; no consumer, and static-
  workflow refused it for the spine.
- Nested `Proc.direct` blocks composing through a bound name — a
  nested block is a leaf (`toProgram`) or a sub-procedure applied with
  `>>>`; the refusal names both.
- `try` in a Proc block — an error is an answer, and the spine has no
  `Throws`.
- Changing the monadic emission in any way. Stage 1's first behavior
  item is that it does not move.

## Design

**Why an arrow block needs an INPUT and a `direct` block does not.**
A monadic block is `F[A]`: no input, its environment is the closure.
An arrow is `P[X, Y]`, and its environment is a VALUE on the edge —
which is the whole point (static-workflow: the position is data). So
`Proc.direct` takes `X => Y` and the parameter `x` is the first name
in the environment. A procedure with no input is `Proc.direct[…, Unit, Y]`.

**Why the door takes the question value, not a function.** In the
combinator form a leaf is `Ask(q: X => Q, read)` because the input is
all the leaf can see. In a block the leaf sees every bound name, so
the natural spelling is the question as an EXPRESSION — `!ask(s"room
$i?")` — and the macro is what closes it over the environment. The
author never writes `env =>`; the `Arr` that does is emitted. That
`Arr` is a Scala closure over the block's constants only (never over a
value that arrives at run time — those are in `env`), which is exactly
the kind of closure static-workflow's journal never stores.

**Why the analysis is the existing one.** `mentionsAny(rhs, bound)`
already answers "does this term mention a block-bound name". The
arrow road asks it of two DIFFERENT subterms of a marked call: the
argument (allowed; it becomes part of the `Arr` before the leaf) and
the callee (refused; it would be `app`). applicative-do asked it of
the whole right-hand side. Same helper, one more call site.

**Why `if` with a PURE condition must become `Left` here and rides
free at a monad.** At a monad a branch containing a leaf is just code
inside a continuation. At an arrow there is no continuation: both
branches must exist as terms before the run, so the macro compiles
each branch at the current environment and joins with `|||`. That is
`leaves` reporting both sides — the over-approximation static-workflow
named — and it is what makes the deploy check able to see a branch a
run has not taken yet.

**Why loops are `Iter` over the environment.** direct-loops already
finds `while`/`foreach`/`map` with marks inside and emits a recursive
`def` over `flatMap`. At an arrow the recursion is a NODE: the body is
compiled at an environment whose loop-carried part is the set of names
the body assigns (`sum += …`, `i += 1` — the `Assign` shape
direct-loops already binds); `Left` goes round with the new
environment, `Right` leaves with it. The collection of a `for` rides
on the edge as the remaining elements, immutable, which is
direct-loops' multi-shot rule kept for a different reason: a journal
replay walks the same list.

**The refactor waits for the duplication.** applicative-do landed as
a separate small road (`applicativeOnly`) rather than refactoring the
monadic pipeline, and was right to: the shared pieces were three
helpers. The arrow road is the third; if it shares more than the
helpers, stage 2's IR is earned. If it shares only the helpers, stage
2 is not built and the spec says so. Deciding that by writing the
third road rather than by predicting is the cheaper order.

**What the neighbour's lanes are, checked against this** (specs/optics.md
stage 12, optics-arrows-effects, landed the same morning):
`optics-guide-page` (docs), `optics-arrow-instances` (`Arrow` for
`Function1` and the Kleisli — laws in `TestMealy`'s shape),
`optics-cont-profunctor` (`Cont` as `Strong & Choice`),
`optics-prism-selective` (a `Star` over `Selective` so a prism reports
both arms), `optics-field-fuse`. None touches Direct.scala, none
touches `Proc`, none is a notation. Two contacts, both stated in the
room: the arrow LAWS should be one generic suite both `Function1` and
`Proc` instantiate, whoever lands first; and `optics-prism-selective`
is the same idea as `Proc`'s `Left` + `leaves` — both arms visible
before the run — at a different carrier, so the two Results should
cite each other.

## Decisions

- **One macro, one more road** — chosen because the normalisation and
  the dependency analysis are target-independent and already exist.
  Rejected: a separate `proc` macro (a second ANF, a second loop
  whitelist, a second lambda rule, drifting).
- **The question as an expression, closed by the macro** — chosen so
  the block text is the `Wf` text. Rejected: doors taking `X => Q`
  (the author threads the environment by hand, which is the price
  this spec exists to remove).
- **Refuse `app` by name, do not emulate it** — an arrow that ran a
  step it did not know is a monad; the deploy check, `leaves` and the
  exhaustive cut all rest on the spine being known. Rejected: a
  fallback to `toProgram` for the offending step (silently turns a
  static procedure into a monadic one, which is failure B's shape).
- **Ungated, by the operator's decision** — static-workflow stage 5
  had this behind "a consumer writes ten leaves by hand and says so";
  the operator ruled (2026-09-18) that the price is designed away
  before stage 1 ships, and that entry now points here.

## Results

(none yet — stage 0 is this document, 2026-09-18)
