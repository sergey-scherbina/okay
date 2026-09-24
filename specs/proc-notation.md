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

### Stage 1 — the arrow road (`proc-notation-road`) — LANDED 2026-09-18

- [x] the five-line booking of docs/continuations/23 compiles to a
      `Proc` whose `leaves` are `[ask, now, timer, signal, patch]` in
      that order, it SUSPENDS at the timer, and `walk` agrees about
      where it stopped. **AMENDED**: "NO OTHER CHANGE to the text" was
      not achievable and the reason is worth keeping — a monadic body
      marks `w.pause(q)` (a program) and a block marks a QUESTION, so
      the door spellings differ by one word each. What is pinned is
      the property that phrase was standing in for: the two journals
      are equal record for record
- [x] one journal from both front ends, and the stronger form with it:
      a run started monadically is carried to the end by a term over
      the same topic (`TestWorkflowProc`, landed with lane 2)
- [~] ONE test source for both spellings — **NOT POSSIBLE while the
      doors differ**, see above; the journal equality is the property
      and it is asserted. Reopen if the doors are ever unified
- [x] a loop whose trip count is an ANSWER compiles to an `Iter`
      (v1.1, `proc-notation-branches`, the same day): written as a
      `while` over values the block binds, with the loop-carried state
      being the environment itself. A `for`/`foreach` is a lambda and
      stays refused, with `while` named in the message
- [x] an `if` with a question inside a branch compiles to `OnRight`
      (v1.1): both branches compiled at the SAME environment, `leaves`
      reporting both, and a run asking only the taken one. Top-level
      only — a val's right-hand side, a statement of its own, or the
      block's answer; one nested in a larger expression still refuses,
      because it would have to hoist
- [x] a leaf chosen by a bound name is refused, `compileErrors`-pinned,
      with `app`, the monad and the two rewrites in the message
- [x] every existing `direct` test passes untouched — trivially and on
      purpose: `Direct.scala` was not touched at all, the road is its
      own file (`ProcMacro.scala`). The whole core suite (1237) is
      green
- [x] a pure `val` between two leaves emits NO leaf, and two `Arr`s in
      a row fold into one at construction (`Proc.andThen`)
- [x] liveness — BUILT, MEASURED AND REFUTED (proc-notation-liveness,
      2026-09-24): every bound name still rides to the end of the block,
      because dropping the dead ones made blocks SLOWER at every length
      measured. See the Results. (The "filed with its trigger" of the
      first cut never reached a board: it lived in the queue entry that
      was deleted when the lane landed.)

### Stage 2 — one front end, four back ends (`direct-targets`)

Gated on stage 1 showing duplication — the third road in Direct.scala
(monadic, applicative-only, arrow) is the point at which "count the
doors" applies:

**REFUSED FOR NOW, with the evidence in this spec's Results and in
specs/arrows-plan.md**: the arrow road shares TWENTY LINES with the
other two, and nothing else was reusable, because a statement NESTS A
CONTINUATION there and APPENDS TO AN ENVIRONMENT here. One IR would be
two IRs with one name. The boxes are kept, unticked, as the shape a
fourth road would have to want:

- [x] WHAT IS SHARED IS SHARED (direct-macros-shared-syntax,
      2026-09-24): the mark syntax lives ONCE, in `macros.MarkSyntax`.
      That is the spellings of a mark, the colouring dispatch,
      `stripped`, `asMark` and `hasMark`. `Direct`'s compiler has it
      through `DirectPhase`, and `ProcMacro` makes an instance whose one
      difference is `procColor`. The arrow road's own copy is gone, so a
      spelling added to one road can no longer be silently missing from
      the other.
- [ ] the block normaliser is ONE function producing a small IR; each
      target is a translation of it — STILL REFUSED, re-examined when the
      syntax was unified: after it, what the two roads share is exactly
      that trait, and the rest is translation
- [ ] the target is chosen by evidence summoned at expansion
- [ ] adding a fifth target is one file that pattern-matches the IR

### Stage 3 — the doors that read as statements (`proc-doors`)

- [x] a `Unit`-typed door stands as a STATEMENT with no ascription —
      `!timer(t)` always did, and the five-line booking of
      `TestProcDirect` has one. **The door the box asked for was
      BUILT AND REMOVED**, see the Results: the coloured spelling
      cannot be a statement, for a reason that is the rule rather than
      an omission, and the refusal now names that case
- [x] `patch(id)` in an `if` condition is the Selective shape at the
      arrow rung — `TestProcColour`'s branch asks `patch("promo")` in
      the condition and `TestProc`'s v1 journal shows `walk` obeying
      the non-consuming rule through it

### Stage 4 — `match` with questions in its cases (`proc-notation-case-binders`)

Found by foreign-in-durable-workflow (2026-09-24). Branching on an
activity's `Either` is what a workflow does after every call, and the
natural spelling was refused: `x match { case Right(p) => !total(p) }`
failed at expansion with "a reference to value p was used outside the
scope where it was defined". Paterson's notation has `case` for exactly
this: the pattern's binders join the environment of their branch.

- [x] A `match` whose cases ask questions compiles in all three positions
      an `if` does: the right-hand side of a `val`, a statement of its
      own, and the block's answer.
- [x] Translation: the scrutinee's questions are asked first (hoisted,
      as a condition's are). ONE pure step runs the original `match`, its
      patterns and guards untouched, and each case answers an injection
      of the environment extended by that case's binders into a nested
      `Either`. Each case body is compiled at its own extended
      environment, and the bodies are joined by `|||`. No pattern is
      duplicated and no binder renamed.
- [x] Every case's questions are in `leaves`, and only the taken case
      asks. `walk` agrees with replay on every prefix.
- [x] Guards may read the binders and the block's names. A guard that
      asks a question is refused by name (a guard runs for cases not
      taken).
- [x] A match with no questions in its cases is ordinary code, as
      before.

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

### Stage 1 — landed 2026-09-18 (`proc-notation-road`)

`ProcMacro.scala` (a new file), `Proc.direct`, `Proc.andThen` and
`Proc.keeping`; `TestProcDirect` (13). **`Direct.scala` was not
touched**, which is the strongest form of the first behavior item: no
existing emission can have moved.

**THE TRANSLATION IS THE ENVIRONMENT, and nothing else.** What a
monadic body keeps in its closure, an arrow carries on its edge, so
the whole macro is: a left-nested tuple that starts as the block's
input and grows by one at every bound name; a reference becomes a
projection into it; a leaf becomes `Proc.keeping`; a run of pure
statements becomes one `Arr`. Two `Arr`s in a row fold at
construction, so a compiled block's nodes are its leaves and the
plumbing between them — and a hand-written term gets the same fold.

**THE BUG THE TESTS FOUND, and it is the one this encoding is prone
to.** A statement with TWO marks read the second answer twice — `r|r`
where `l|r` was meant. The residual expression was rewritten against
the depth AFTER its leaves, so the k-th mark projected one place too
high for each mark before it. The fix is that `emitLeaves` returns the
depth it STARTED at; the failure is the reason the test asks for
`"l|r"` rather than just asserting the questions were asked.

**THE REFUSAL TESTS FOUND A SECOND BUG BEFORE THEY COULD PIN
ANYTHING.** Inside `compileErrors` the macro answered "takes a lambda"
— because an inline argument arrives there wrapped in `Inlined` nodes
carrying `$proxy` bindings, and the lambda pattern only looked through
`Inlined(_, Nil, _)`. That is the same shape Direct.scala records for
`asMark`, met from the other side. `compileErrors` is a harsher
environment than an ordinary call site, and a refusal test is
therefore worth writing even when the refusal already works by hand.

**THE LEAF TAKES THE AUTHOR'S NAME.** `nameOf` reads the callee at the
root of the marked expression, so a block calling `patch("promo")`
gives a leaf named `patch` and not `Patched` — a term reads in the
vocabulary of the program rather than of the library. Found by a test
that expected the case's name and was wrong.

### The spellings, pinned (`proc-notation-forms`, 2026-09-18)

The operator asked the question `apdo-forms` asked of the applicative
road — **are the types required?** — and the answer is the same shape:
fewer than every test in this repository was writing.

```scala
val booking: Wf.Proc[String, String, Unit, String] =
  Proc.direct: _ =>                       // no type arguments at all
    val city = !ask("city?")
    val t    = !now
    s"$city/$t"
```

- **The three type arguments are needed only where there is NO
  expected type.** On a `val`'s or a `def`'s declared type they are
  all inferred — the signature, the input and the answer — and the
  block's parameter type comes with them.
- **The input is inferred too**, so `Proc.direct: who => …` at a
  `Wf.Proc[…, String, String]` gives `who: String`.
- **Branches and loops need none either**, which was worth checking
  separately: they are compiled by their own routines and could have
  lost the expected type on the way.
- **A MARK is still required.** `direct` has auto-colouring behind
  the `DirectCtx` capability; this road has none, so an operation
  used where a value is wanted is the ordinary type error it should
  be. Recorded as a decision rather than left as a surprise.

`TestProcForms` compiles the same block FOUR ways and asserts the
terms behave identically — the leaves, the answer and the questions
asked. Compiling was never the claim: a macro that infers its types
wrongly compiles too.

### Stage 3 — the doors, and a door that was built and removed (`proc-doors`)

Both boxes are met, and one of them is met by a REFUSAL.

`!timer(t)` on its own line has always compiled — the mark makes it a
leaf, and the five-line booking has one. The COLOURED spelling,
`timer(t)` alone, cannot, and the reason is the rule the whole feature
rests on: **auto-colouring fires where an ANSWER is expected, and a
statement expects nothing.** The conversion has nothing to convert.

**THE DOOR THE BOX ASKED FOR WAS BUILT, MEASURED AGAINST THE REST, AND
REMOVED.** `Question.asked` — `transparent inline`, gated on the
capability, ascription built in, exactly the shape `Direct.tell` has —
works as a piece of Scala and collides with the check that makes
colouring safe: it expands to `val _ = q.reflect`, the inliner leaves
a `$proxy` binding holding the question, and the stray-question check
sees a question nobody asked. Making the check tolerate that means
teaching it which bindings are consumed by which marks, which is more
machinery than a spelling is worth.

So the refusal stays and it now NAMES this case among the three that
reach it. That is the better outcome and not a consolation: a compile
error saying "mark it" is more useful than a door that compiles and a
warning nobody reads, which is what the half-built version produced
(E176 from the typer, before the macro ever sees the statement).

### Auto-colouring (`proc-auto-colour`, 2026-09-18)

The operator asked for it the hour `proc-notation-forms` said the
types were not required either. A question reads as its answer, with
no `!`:

```scala
import okay.Proc.given
import scala.language.implicitConversions

val booking: Wf.Proc[String, String, Unit, String] =
  Proc.direct: _ =>
    val city: String = ask("city?")
    val t: Long      = now
    s"$city/$t"
```

**The gate is the capability, exactly as `direct`'s is.** The entry's
block became `ProcCtx[F] ?=> X => Y`, the conversion requires
`ProcCtx[F]`, and the class's constructor is `private[okay]` — so
outside a block the conversion cannot resolve and a question used as a
value stays the compile error it always was. The macro looks through
the context lambda; taking it for the block's own is how the first cut
compiled a procedure whose input was a `ProcCtx`.

**ONE conversion where `Direct` has two, and that is the shape of the
two roads.** A monadic block distinguishes its own programs
(`selfColor`) from an effect signature's operations (`opColor`, gated
by a `Direct.Effect` marker) because those are different things there.
A term's leaves are operations of ONE signature, so there is one case
— and the marker gate has nothing to add, because the capability
already names `F`.

**IT IS AN IMPORT, NOT A DEFAULT**, and that fell out of where the
given can live rather than from taste. `Free.directColor` works with
no import because `Free`'s companion is in the implicit scope of
`Free[R, A]`; a `Proc` block's source type is the AUTHOR's signature,
whose companion this library does not own. So `import okay.Proc.given`
it is — and the consequence is worth having: a file that did not ask
for colouring cannot get it by accident, which `TestProcForms` pins by
NOT writing the import and asserting the error is still there.

**THE ONE FAILURE COLOURING CAN PRODUCE SILENTLY, made loud.** The
conversion fires where an ANSWER is expected. `"a" + q` expects
nothing in particular — `String.+` takes `Any` — so the question is
stringified and the program asks one where it reads as asking two.
Measured before the check: `ask("left?") + "|" + ask("right?")`
answered `l|Ask(right?)`. So after rewriting, NOTHING of the
signature's type may remain in a residual, and what does is refused
with both fixes named. The check cost three wrong cuts before it was
right — a positional "skip the root" skipped an `Inlined` wrapper and
then reported the question under it, which refused every marked val in
the repository — and the version that stuck counts QUESTIONS, with
wrappers transparent.

### Stage 1.1 — branches and loops, landed 2026-09-18 (`proc-notation-branches`)

`TestProcBranches` (13). The flat walk became a recursive body
compiler, because a branch is a block compiled at the environment the
`if` sees.

**AN `if` IS TWO `OnRight`s AND THREE `Arr`s**, and the shape is the
whole design:

```
Arr(env => if cond then Right(env) else Left(env))   // Left carries the else's env
  >>> OnRight(thenBranch)                            // Either[E, V]
  >>> Arr(_.swap)                                    // Either[V, E]
  >>> OnRight(elseBranch)                            // Either[V, V]
  >>> Arr(_.fold(identity, identity))
```

Both branches are in the term, so `leaves` reports both — the
over-approximation the whole shape is built on — and only the taken
one asks, which is asserted from both sides.

**AN ASSIGNMENT IS A REBUILD, AND THAT IS WHAT MAKES THE LOOP
HONEST.** `x = e` emits an `Arr` that reconstructs the environment
with x's slot replaced, so a name always lives at its own index and
the layout never depends on what has been assigned. Nothing mutates:
the value that goes round the loop travels on the arrow's edge, which
is why a replay re-derives it exactly. The alternative — remapping an
index per assignment — was rejected before it was written, because the
projection arithmetic would then depend on the assignment history.

**A `while` IS `Iter` WITH THE TEST IN FRONT:**

```
Iter( Arr(env => if cond then Right(env) else Left(env))
        >>> OnRight(body >>> rebuild)
        >>> Arr(_.swap) )
```

`Left` goes round and `Right` leaves, so the swap after the body is
what turns "ran the body" into "go again". A zero-trip loop asks
nothing, and that is a test because the obvious mis-compile runs the
body first.

**THE ARITHMETIC WAS WRONG A SECOND TIME, and the fix was to stop
having two copies of it.** An `if` whose CONDITION asks a question
hoists that question as a leaf, and the condition was then rewritten
against the depth AFTER the leaf — the same off-by-one that read
`r|r` for `l|r` in v1. `hoist` is now one function that answers the
depth it started at, used by every caller.

**THE UP-FRONT GUARD WAS THE BUG THAT HID THE FEATURE.** v1 swept
every statement with `guard` before compiling anything, which refused
the very shapes this stage compiles. The guard now runs on the
straight-line pieces only — a condition, a branch's statements — and
an `if` or a `while` is compiled by its own routine.

**SEEN FAILING, twice.** Inverting the branch selector reddens three
tests (the run asks the wrong question); running the loop's body
before its test reddens the zero-trip test alone, which is the one
written for it.

**WHAT IT SHARES WITH `Direct.scala`: twenty lines.** The mark symbols
and `asMark`/`hasMark`, and nothing else — no ANF machinery, no loop
whitelist, no lambda rule was reusable, because the arrow road's
"statement" is a different thing (it appends to an environment rather
than nesting a continuation). **That is the datum stage 2 was waiting
for, and it says stage 2 is NOT earned**: one IR serving three
translations would have to model the environment for one road and the
continuation for the other two, which is two IRs with a shared name.
The line stands until a fourth road wants the same normalisation.

### Stage 4 — landed 2026-09-24 (`proc-notation-case-binders`)

- `ProcMacro.compileMatch`. The selector is one `Proc.arr` running a
  `Match` built from the ORIGINAL `CaseDef`s' patterns and guards; the
  guards are rewritten against the environment. Each case's right-hand
  side becomes an injection into `Either[E0, Either[E1, …]]`, and `Ei` is
  the environment extended by the case's `Bind` symbols, in pattern
  order. Branches come from `compileValue` at `st` pushed and bound with
  those symbols, and a `fanin` built like compileIf's
  (`onRight`/`swap`/`onRight`/merge) joins them.
- Found beside it, and fixed. (1) The statement position of an `if` with
  questions handed a raw lambda to `thenTerm`; it had never been
  exercised, and compiling the `match` twin of it crashed the macro
  ("Expected type: okay.Proc…, Actual type: Function1"). Both are now
  wrapped in `arr`. (2) An assignment to an outer `var` INSIDE a branch
  compiled to "Reassignment to val _2", or to a lost write. It is
  refused by name (`noOuterAssign`), for `if` and `match`. (3) The
  refusal's own advice, `x = if …`, did not compile: an assignment's
  right-hand side went through hoisting. `x = if …` and `x = … match …`
  now compile, the branch's value as a slot and the environment rebuilt
  with `x` replaced.
- Tests: TestProcMatch (a three-case match with a guard; binders read
  after a question; walk against replay on every prefix; a val's
  right-hand side with two binders from one pattern; statements; the
  assignment forms; three refusals). The foreign workflow block in
  okay-foreign-workflow now branches with `match`.
- Mutants: injecting the cases in reverse was refused by the COMPILER,
  since the environments differ when binders do. Dropping the guards
  compiled, and failed the taken-case test.

### Liveness — measured and refuted, 2026-09-24 (`proc-notation-liveness`)

The operator asked for it. The performance rule asked for a number
first, so `ProcEnvBench` (okay-workflow's first JMH) was written: a block
of N statements where each name is read only by the next, run by
`foldMap` into the identity monad with every question answered at once,
so that only the environment's plumbing is timed. The baseline grows
faster than linearly, 23 ns per statement at 4 and 33 at 64: every
projection walks a tuple as deep as the block so far.

The experiment (commit 319f1576, reverted by the next one) pruned before
each statement to the slots a later statement or the answer still reads,
and not in a loop body, whose environment must come round in its shape.
All 137 okay-workflow tests passed with it. Two rounds, alternated:

| N | A: every name rides | B: dead names dropped | B / A |
|---|---|---|---|
| 4 | 91.4 ns | 108.4 ns | 1.19 |
| 16 | 426.1 ns | 467.7 ns | 1.10 |
| 64 | 2066.3 ns | 2579.9 ns | 1.25 |

Bytes per op were EQUAL (640, 2640, 12096). Rebuilding the environment
from its live slots allocates the pairs that appending did, and the
prune is one more `Arr` per statement, a node the interpreter walks, and
that costs more than the `_1` reads it saves. It is not a matter of
tuning: an extra node per statement is the price of pruning, and a
projection down a tuple is field loads. The one argument left, holding a
large dead value until the block ends, has no measurement behind it, and
a workflow's answers are small. The benchmark stays, as the instrument
for anyone who brings the question back, with the numbers in
src/jmh/history.tsv.

