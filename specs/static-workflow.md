# The durable spine without `ArrowApply`: a workflow built from everything except the monad

## Overview

Appendix A of the continuations book ends on one sentence that this
spec starts from: *the opacity is not the continuations — a captured
continuation is already a walkable list of frames (`Segs`: `Done`,
`K`, `Mark`); the closure is `flatMap`'s, and it was there first.
Captures only made it visible.* The operator's question the same
morning (2026-09-18): if continuations and effects are not the
problem and the monad is, what does a durable workflow look like
built from everything this library already has EXCEPT the monad —
applicatives, selective functors, profunctors, arrows, optics,
continuations, effects? Is that enough, and what makes such a system
reliable rather than merely different?

The answer is yes, with the boundary drawn one notch more precisely
than "no monads", with ONE addition to the list, and with a reuse
that makes stage 1 small.

### The boundary is `ArrowApply`, not "monads"

Hughes (2000, §4.5): an arrow with `app : P[(P[A, B], A), B]` — a
computation that receives a computation AS A VALUE and runs it — is
exactly a monad, and `Bind`'s `f: A => Free[F, B]` is `app` spelled
as a Scala closure. Everything short of `app` is a term whose shape
is fixed before a value exists: `arr`, `>>>`, `first` (Category +
Strong, already `Optic.Arrow` in Optic.scala), `left` (`Choice`, a
prism's requirement). So the rule for the durable spine is not "no
monads", it is: **no operation that takes a computation as data**.
Inside a leaf, a monad is fine and is the point — see below.

### What the existing list gives, and the one thing it lacks

| the operator's list | its role on the spine | where it already is |
|---|---|---|
| applicative | the input-less special case (`Static`): a spine with no `first` | Static.scala |
| selective | `Select` = `left` + `>>>`: a conditional with BOTH sides written down | Monad.scala, Static.scala |
| profunctor | the spine IS one: `dimap` = `arr` before and after | Optic.scala |
| arrow | the spine's algebra: `arr`, `>>>`, `first` | `Optic.Arrow` |
| optics | the STATE glue: a lens runs a sub-procedure on a part of the state, a prism on one variant, and they apply to the spine by its `Strong`/`Choice` instances — no new machinery | Optic.scala |
| continuations | NOT needed to suspend the spine (the position is structural); reused in stage 1 to run it through the engine that exists; available inside a leaf like any effect | Delim.scala |
| effects | the LEAVES: a leaf is a question (`Wf.Ask[Q]`), and the oracle that answers it runs in whatever row the activity needs — the two-row rule of durable-workflow, now structural | Wf.scala, Dialogue.scala |

**The addition: iteration as a node.** Appendix A refuses the
static route for exactly one reason — "no data-dependent structure:
you cannot write `for _ <- 1 to nights do pause(…)`". That objection
is about `Selective` as it stands: Mokhov et al.'s `whileS` is a
recursive definition, which in a strict free structure is an
infinite term. It dissolves with one constructor that the selective
literature does not have and the iteration-theory literature does
(Elgot; Bloom & Ésik; Capretta 2005's `iter`):

```scala
case Iter(body: Proc[…, X, Either[X, Y]]) extends Proc[…, X, Y]
```

run the body; on `Left(x)` run it again from `x`; on `Right(y)` stop.
The shape is finite, the position is a path with iteration counts,
and "ask N questions where N is an answer" is a loop with a counter
on its edge. This is NOT `ArrowLoop` (Paterson's lazy value
feedback) and the Decisions say why.

**And one thing that is not an algebra: `Schema` on the answers.**
Already the case — a `Dialogue`'s journal is `Entry[A]` under a
`Schema` with versions and upcasts. Nothing new is asked of the
author here, and that is the whole reason stage 1 is small.

### What it buys over the landed engine, and what it costs

The monadic engine (specs/durable-workflow.md, stage 4 closed
2026-09-17) is right for a program whose shape depends on its
answers, and stays. A static spine buys six things it cannot, and
each is a stage below:

1. **The deploy check is structural and runs BEFORE the deploy.**
   Failure B of durable-workflow (a changed program silently reading
   an old journal) became a loud stop at RUN time via the `program`
   field. A spine is a term, so "does this journal still fit this
   program" is a walk over the term with the answers, with NO
   effects performed — `Proc.accepts(journal)` — and a census over a
   topic answers "which live runs would this deploy strand" while
   the old build is still serving. `patch` stays available as a
   leaf; it is needed less often, and the check says when.
2. **No `Replayable` discipline to enforce.** The spine cannot reach
   outside by construction — its only effects are its leaves, and a
   leaf is a question the ORACLE answers in its own row. The thing
   `Replayable` and `unchecked` exist to police cannot be written.
3. **Fault injection is EXHAUSTIVE.** A finite term has finitely many
   leaves, so "crash before the append / after the append at every
   leaf, resume, compare with the uninterrupted run, count activities
   per (id, position)" is a property over a finite set, not a
   sample. The monadic engine can only sample.
4. **Two independent derivations of the position.** `Proc.walk` (the
   structural fold) and `Wf.replay(p.toProgram)` (the engine's
   replay) must agree on every journal — a law tested by property,
   and the memory rule "derive a third way before trusting either
   side" made a fixture.
5. **`leaves`, a dry run, a picture.** Every question the run MAY ask
   before it asks one; a rendering of the term with the run's
   position marked, from the term rather than from a `Statuses`
   projection the worker must keep.
6. **Room for what the closure forbade** (later, gated): parallel
   branches with two sub-positions, compensation as an undo arrow
   walked backwards along the path, and an O(1) cursor snapshot as a
   chapter format — each possible only because the position is data.

What it costs, said before any of it is built:

- **Combinators, not straight-line code — DESIGNED AWAY, not gated**
  (operator, 2026-09-18, the same day). specs/proc-notation.md puts
  the `direct` macro at an arrow: the block text of the `Wf` booking
  compiles to a `Proc` at `Proc.direct`, the macro threads the
  environment (Paterson 2001) in `Arr`s that are never journalled,
  `if` is `left`, `while`/`for` is `iter`, and the one thing an arrow
  cannot do — a leaf chosen by a bound value, which is `app` — is
  refused by name. The three lines that stood here said the state is
  threaded by hand and the shape is written in combinators; both are
  the macro's job now, and what remains is the price every `direct`
  block already pays (a mark under a lambda that is not a loop shape).
- **Shape only through `left` and `iter`.** "Run the workflow whose
  name arrives in an answer" is a leaf whose activity does that, not
  a spine that changes — and in a block that is a compile error naming
  the line, not a silent fallback.
- **A second way to write a workflow.** The two compose one way — a
  `Proc` is a program (`toProgram`) and can be an activity of a
  monadic one, and a monadic program can be the oracle of a leaf —
  and the docs must say which to pick: shape fixed → `Proc`; shape
  data-dependent beyond a loop → `Wf`.

## Interface

**AMENDED 2026-09-18 by specs/arrows-plan.md, Decision 1, before any
of it was written.** `Proc` is over a SIGNATURE `F`, beside `Static`
(the arrow to its applicative), and the workflow is one signature,
`Wf.Question[Q, A, *]`, with `Wf.program` (the bridge) and `Wf.walk`
(the structural fold) living where the journal is. The interface
below is the workflow instantiation as first drawn; every Behavior box
stands unchanged, read through `Wf.Proc[Q, A, X, Y] =
Proc[Wf.Question[Q, A, *], X, Y]`. A leaf is NAMED (`Eff(name, op)`)
because its operation value does not exist before the input does, and
`leaves` reports names and kinds, not question values.

```scala
// Proc.scala (core, beside Wf.scala) — SUPERSEDED, see the amendment

/** a procedure whose every step is known before it runs:
 *  questions of type Q answered by A, taking X to Y */
enum Proc[Q, A, X, Y]:
  /** pure; never journalled; re-run on every walk */
  case Arr(f: X => Y)
  /** the AUTHOR's question — the leaf that reaches outside,
   *  answered by the oracle, journalled as `Right(a)` */
  case Ask(q: X => Q, read: A => Y)
  /** the RUNTIME's question — the same leaf with the library's tag;
   *  `read` is total on the case it asked for */
  case Sys(q: X => Wf.Sys, read: Wf.SysA => Y)
  case Seq[Q, A, X, Y, Z](f: Proc[Q, A, X, Y], g: Proc[Q, A, Y, Z]) extends Proc[Q, A, X, Z]
  case First[Q, A, X, Y, C](f: Proc[Q, A, X, Y]) extends Proc[Q, A, (X, C), (Y, C)]
  case Left[Q, A, X, Y, C](f: Proc[Q, A, X, Y]) extends Proc[Q, A, Either[X, C], Either[Y, C]]
  /** Elgot iteration: Left goes round, Right leaves */
  case Iter(body: Proc[Q, A, X, Either[X, Y]])

object Proc:
  // ── the algebra, as instances the rest of the library already consumes
  given [Q, A]: Optic.Arrow[[X, Y] =>> Proc[Q, A, X, Y]]     // Category + Strong + arr
  given [Q, A]: Optic.Choice[[X, Y] =>> Proc[Q, A, X, Y]]    // left / right
  // ── the doors, one per Wf door, each a Sys leaf
  def ask[Q, A, X](q: X => Q): Proc[Q, A, X, A]
  def now[Q, A, X]: Proc[Q, A, X, Long]
  def uuid[Q, A, X]: Proc[Q, A, X, String]
  def random[Q, A, X]: Proc[Q, A, X, Double]
  def patch[Q, A, X](id: String): Proc[Q, A, X, Boolean]
  def timer[Q, A]: Proc[Q, A, Long, Unit]                      // wake at this instant
  def sleep[Q, A, X](millis: Long): Proc[Q, A, X, Unit]        // = now >>> arr(_ + millis) >>> timer
  def awaitSignal[Q, A, X](name: String): Proc[Q, A, X, String]
  def awaitChild[Q, A, X](id: String): Proc[Q, A, X, String]
  def cancelled[Q, A, X]: Proc[Q, A, X, Option[String]]
  def iter[Q, A, X, Y](body: Proc[Q, A, X, Either[X, Y]]): Proc[Q, A, X, Y]

  extension [Q, A, X, Y](p: Proc[Q, A, X, Y])
    /** every question this procedure MAY ask, in term order; both
     *  sides of every Left, the body of every Iter once — an upper
     *  bound, exact for a spine with neither */
    def leaves: Vector[Leaf[Q]]
    /** THE BRIDGE, one-way: the same procedure as a durable program
     *  the existing engine runs — `Dialogue.workflow`, `Worker`,
     *  timers, signals, children, retries, cancel, continueAs, all
     *  unchanged. Its row is `Delim + Pure`: trivially Replayable. */
    def toProgram(x: X): Wf.Asks[Q, A, Y, Pure] ?=> Y ! (Delim + Pure)
    /** THE STRUCTURAL FOLD: where the procedure stands after these
     *  answers, performing NOTHING. `Stranded` names the first record
     *  the term cannot take and the position it was at. */
    def walk(x: X, answers: Delim.Journal[Wf.Ans[A]]): Either[Stranded, Standing[Q, Y]]
    /** the deploy check: `walk` succeeded */
    def accepts(x: X, answers: Delim.Journal[Wf.Ans[A]]): Boolean
    /** the term, drawn; a position may be marked */
    def render(at: Option[Path] = None): String

  /** where a walk stopped: a path into the term with iteration counts */
  enum Path: …
  enum Standing[Q, Y]:
    case Done(y: Y)
    case Asking(at: Path, q: Wf.Ask[Q])
  final case class Stranded(at: Path, record: Int, why: String)
```

Optics apply to a `Proc` through the instances and nothing else:
`lens(step)` for an `Optic[Strong, S, T, A, B]` is `Proc[Q, A', S, T]`
when `step: Proc[Q, A', A, B]` — a sub-procedure on the part of the
state the lens sees; a prism runs its step on one variant and passes
the others through. `andThen` on optics composes the focus; `>>>` on
procedures composes the steps.

## Behavior

### Stage 1 — the type, the bridge, the laws (`static-workflow-proc`) — LANDED 2026-09-18

- [x] `Proc` is an `Optic.Arrow` and an `Optic.Choice`, and the
      category, arrow and choice laws hold — `TestProcLaws`, three
      lines over `okay.laws.ArrowLaws` (arrows-plan lane 1), at a
      signature whose leaf has a state so the laws have something to
      be wrong about
- [x] the booking written as a `Proc` runs through
      `Dialogue.workflow` with NOTHING in okay-persist changed
      (`TestWorkflowProc`, 7 tests): the clock read once across a
      restart, `sleep` suspending with `Waiting(Until(t))` at a
      deadline a second process with a different clock agrees on,
      `patch` taking the old branch on an old journal
- [x] a loop: "ask `nights?` and then one `room?` per night" as an
      `Iter`, with the position after two rooms being a path whose
      round is 2, both under `walk` and under the engine
- [x] `walk` and `Wf.replay` AGREE — on every PREFIX of three
      journals (the booking, the loop, and a v1 journal read by a
      term that gained a patch), and seen to FAIL under three
      deliberate sabotages of the fold
- [x] `walk` performs no effects — its signature is the proof, as
      `Wf.replay` taking no runtime was: no row, no monad, no runtime
- [x] `leaves` of a term with a choice reports both sides and of an
      `Iter` reports the body once, in term order
- [x] an existing `Wf` journal written by the monadic booking is
      accepted by the `Proc` booking that asks the same questions in
      the same order — and a run STARTED monadically is carried on by
      the term from the same topic, which is the stronger form

### Stage 2 — the deploy check and the exhaustive cut (`static-workflow-strands`)

- [~] a v2 term with a new first question REFUSES the v1 journal —
      **PARTLY, and the limit is the important half**: `walk` sees the
      SHAPE of an answer, so a new question whose answer is tagged (a
      clock, a patch) strands the old journal at record 0 as promised.
      Two AUTHOR questions of the same answer type are
      indistinguishable in a journal of answers, so inserting one
      before them reads every old answer one place across and carries
      on — failure B exactly, and `walk` cannot see it. The envelope's
      `program` field is still the mechanism for that case. Both are
      asserted, including the one that does not work
- [x] `Proc.strands(term)(x)(journals)` lists the runs a term would
      strand and where; a run it accepts is not listed. A PURE
      function — no row, no runtime — which is what lets it be asked
      of ten thousand journals before a deploy rather than during one
- [x] EVERY cut: crash at every leaf, resume from the journal, and the
      final answer equals the uninterrupted run's; every activity ran
      once except the one the crash caught in flight, which ran twice
      — the at-least-once floor with a window of exactly one call,
      measured over a REAL topic (`TestProcCut`, okay-persist)
- [x] a `Sys.Patch` leaf under `walk` obeys the non-consuming rule —
      `TestProc`, "a v1 journal takes the OLD branch, and its next
      answer is not eaten", and again through `strands` on a term
      whose change was made WITH a patch, which strands nobody

### Stage 3 — optics as the state glue (`static-workflow-optics`)

- [x] a lens applied to a step runs the step on the focused part and
      puts the result back; the surrounding state is untouched and
      the journal holds only the step's answers — and the term folds
      that journal back to the same whole
- [x] a prism applied to a step runs it on the matching variant and
      passes every other variant through with no leaf asked; `leaves`
      still reports the step, because which variant arrives is decided
      by a value that does not exist yet
- [~] the form of `ui-direct-example` rewritten as a `Proc` — NOT
      done, and named rather than quietly dropped. The two claims it
      was there to make are made by the tests above (an optic applies
      to a step with no new machinery; the journal holds the step's
      answers and nothing else) and by `TestProcCut` (a crash between
      two questions resumes). What a form would add is a CONSUMER, and
      that is `ui-`something's lane, not this one — filed as
      `proc-form-consumer`

### Stage 4 — the picture (`static-workflow-render`)

- [x] `Proc.mermaid` draws the term with every leaf named by the door
      the AUTHOR called, both sides of a choice, and a back edge for
      an `Iter` — drawn ONCE, because how often a body runs is not a
      fact the term has. Pure steps are not drawn. (`render`, the
      indented form from stage 1, stays: it is what a failure message
      wants and Mermaid is what a page wants)
- [x] `mermaid(Some(path))` marks the run's position, taken from
      `walk` — so a dashboard draws a position without replaying

### Stage 5 — gated: the rest the closure forbade

**THE ARC IS CLOSED FOR EVERY STAGE THAT WAS IN SCOPE** (2026-09-18):
stages 1-4 landed the same day, and what is left below is gated on
purpose. Each has a TRIGGER and lands only when it fires — a list of
things nobody has asked for is not a plan, and building one of these
without its trigger would be building it for the spec rather than for
a consumer.

- [x] `proc`-notation — MOVED to specs/proc-notation.md and UNGATED
      (operator, 2026-09-18), and LANDED there in full the same day:
      the block, branches and loops, the spellings (no type arguments
      where there is an expected type) and auto-colouring. It was
      never a stage-5 option; it is stage 1's companion.
- [ ] parallel branches: a `Par(f, g)` node whose position is a PAIR
      of paths, run as two sub-drives that join. TRIGGER: a workflow
      with two independent waits that today serialises them.
- [ ] compensation as structure: a leaf carrying an undo, and a
      failure walking the path backwards. TRIGGER: a saga written by
      hand on the cancelled branch twice.
- [ ] the cursor chapter: a snapshot format of `(path, carried
      values)` giving O(1) restore, with `Schema` demanded on the
      carried types at construction. TRIGGER: a term whose walk is
      measured to cost more than its next leaf's activity.

## Out of scope

- Replacing `Wf`/`Dialogue`. A program whose shape depends on its
  answers beyond a loop stays monadic and stays on the landed engine.
- A second journal format, a second envelope, a second worker. Stage
  1's whole design is that there is one of each.
- Serialising anything but answers. Appendix A's three columns stand;
  this is the "enumerate the shapes" column taken one notch — the
  shapes are the constructors — and it stores exactly what the
  monadic engine stores.
- Arrow notation in the language. A macro — specs/proc-notation.md.

## Design

**The spine is a term; the leaves are questions; the journal is the
one that exists.** `Proc.toProgram` maps `Ask` to `w.pause`, each
`Sys` case to its `Wf` door, `Seq` to `flatMap`, `First`/`Left` to
the obvious pure plumbing, `Iter` to a `Delay`-trampolined loop. The
program it produces is in `Delim + Pure`, so the engine's row
obligations are met by construction and every stage-4 feature of
durable-workflow works on day one. That is stage 1's whole bet, and
it is testable by the fixture in Behavior: the SAME journal, written
by the monadic booking, read by the static one.

**The position is a path, and two functions must agree about it.**
`walk` folds the term over the answers with no runtime and returns
`Done`, `Asking(path, q)` or `Stranded(path, record, why)`.
`Wf.replay(toProgram)` returns a `Paused` whose pending question must
be the same `q`. The property that they agree on every generated
term and every answer prefix is the reliability keystone: `walk` is
what the deploy check and the picture trust, `replay` is what the
engine trusts, and a disagreement is a bug in one of them found
before a journal was.

**`Iter` under `walk`** counts: entering the body appends an
iteration index to the path; `Left(x)` increments it; `Right(y)`
pops. A journal that ends inside the third iteration stands at
`Iter(3, inner)`. Under `toProgram` the loop is `Free.defer`-driven
so a loop of any length costs no host stack (the same move as
`Static.toFree`).

**Optics need no bridge.** `Optic[C, S, T, A, B]` is `P[A, B] =>
P[S, T]` for every `P` with `C[P]`; with `Strong`/`Choice` instances
for `Proc`, `lens(step)` type-checks today. What stage 3 adds is the
tests and the example, not machinery.

**The neighbouring lane, and the line between them.** specs/optics.md
stage 12 (optics-arrows-effects, landed the same morning) surveys
arrows against the tree: `Optic.Arrow` has ONE instance, `Mealy` in
okay-lex, and `TestMealy` states the arrow laws over an input — stage
1's law test takes that shape rather than inventing one. Its lanes
add instances for `Function1` and the Kleisli `A => B ! R`; this
spec adds the instances for `Proc` and the FIRST `Optic.Choice` for
an arrow, which stage 12 names as the trigger it defers to here
("a plan that must branch on a VALUE and still be drawn"). Neither
lane adds the other's instance.

**Why `Sys` is a case and not nine.** The nine doors differ only in
which `Wf.Sys` they ask and which `SysA` they read; the walk treats
them alike except `Patch` (the non-consuming rule, copied from
`Wf.replaying` rather than re-derived, for the reason that spec
gives: a second copy of that rule would drift).

## Decisions

- **`Iter` (Elgot), not `ArrowLoop`.** Paterson's `loop` is value
  recursion — a feedback wire whose meaning needs laziness — and it
  cannot express "run the body again". Elgot iteration is control
  iteration with a data-decided exit, finite as a term, and its
  position is an integer. Rejected: `Selective`'s `whileS`, which is
  the same thing written as an infinite term.
- **One question type per procedure, like `Wf`, not one per leaf.**
  Per-leaf answer types would need per-leaf `Schema`s and a journal
  record that says which leaf it answers — a second format. `Ask`'s
  `read: A => Y` is where a leaf narrows its answer, and a refusal
  there is failure A of durable-workflow, handled by the same
  trial-before-commit order.
- **Stage 1 runs through the existing engine (`toProgram`), not a
  cursor runtime.** A cursor runtime would buy O(1) restore at the
  price of `Schema` on every carried type and a second driver beside
  `Dialogue`'s; the appendix already priced O(1) restore against a
  microsecond prefix and found the benefit inverted. The cursor is
  stage 5, gated on a measurement.
- **`walk` exists beside `replay` even though `replay` would do.** A
  second derivation of the position, with no runtime in its
  signature, is what the deploy check runs and what the agreement
  property tests. Its cost is one fold written twice on purpose.
- **Named `Proc`**, after Paterson's `proc` keyword; `Flow` is
  okay-cluster's plan, `Step` is `Wf`'s result, `Route` is HTTP's.

## Results

### Stage 1 — landed 2026-09-18 (`static-workflow-proc`)

`Proc.scala` (the free arrow over a signature), the workflow half in
`Wf.scala` (`Question`, `Wf.Proc`, the doors, `program`, `walk`,
`tag`), `TestProc` + `TestProcLaws` (30) and `TestWorkflowProc` (7).

**THE BET PAID: okay-persist did not change by one line.** A term
becomes an ordinary durable program through `Wf.Proc.program`, so
`Dialogue.workflow`, the envelope, the races, the snapshots, the
suspension and `patch` all work as they stand. The decisive test is
not that the term runs but that the two front ends SHARE A TOPIC: a
run started by the monadic booking and half-answered is carried to the
end by the term, and their journals are equal record for record.

**THE ONE THING THE ENCODING COSTS, named where it is paid.**
`okay.Proc`'s signature parameter is `F[+_]`, so `Wf.Question` must be
covariant in its answer; matching `Ask` then proves `A <: Z` rather
than `A = Z`, and a program `A ! Row` is invariant in its value. Every
arm of the bridge therefore goes through `up`, which is a `map` — one
node per leaf, beside a leaf that is an outside call, and NOT an
`asInstanceOf`. The alternative was an invariant signature, which
`okay.Proc` could not accept at all.

**THE GADT REPLACED NINE PARTIAL FUNCTIONS WITH A TYPE.** The monadic
doors each carry `case SysA.Millis(v) => v` and throw `Mismatched`
when the shape is wrong. `Question[Q, A, R]` says it once — `Now` IS a
`Question[…, Long]` — so the fold's `readInto` is total over the pairs
that make sense and every other pair is DATA (a `Stranded` naming the
record), because a fold that throws cannot be a deploy check.

**THE KEYSTONE WAS SEEN TO FAIL, three ways.** A green property is
worth nothing until it has been red for the right reason, so `walk`
was sabotaged and the suite watched:

| sabotage | what went red |
|---|---|
| the `Iter` loop stops counting its rounds | the position test, naming the path it got |
| a leaf reads an answer without consuming it | SIX tests, including both agreement properties and the cross-front-end journal |
| the patch EATS the record that follows it | the v1-journal test and the agreement property over it |

The second is the one to keep: the failure message reads
`stranded at 1/1/1/2/2/1/2/in on record 1: Now() cannot take
Right(Kyiv)` — the path says where, the record says which, and the
question says what it could not take.

**DECISION 4 OF arrows-plan, ANSWERED BY TRYING IT.** Are `leaves`,
`walk` and `render` one path-indexed fold? **Two of the three are**:
both are one line over `Proc.nodes`, a walk that hands every node its
path, with `render`'s indentation READ OFF the path (the steps that go
inside a node are the levels). **`walk` is not**, and the reason is
structural rather than effort: it threads a VALUE and a JOURNAL
through the term and may stop in the middle, so it is an interpreter
over two inputs, not a traversal of one term; written as a fold its
accumulator would carry the value, the remaining journal, the count
and an early exit — the interpreter with a fold's spelling on top. So
the indexed-optics seat the optics board is watching for is worth ONE
entry here, not three.

### Stage 2 — landed 2026-09-18 (`static-workflow-strands`)

`Wf.Proc.strands`, `TestProcStrands` (5) and `TestProcCut` (4).

**THE DEPLOY CHECK IS TWO QUESTIONS, NOT ONE, and writing the test is
what said so.** The stage was specified as "a v2 term refuses the v1
journal". It does not, and the reason is the doctrine working exactly
as designed: a journal holds ANSWERS. Two author questions whose
answers have the same type are indistinguishable in it, so a term that
inserts one before them reads every old answer one place across and
carries on — failure B of specs/durable-workflow.md, silently. What
`walk` DOES catch is a shape: a clock reading where an author's answer
sits, a tagged answer where an untagged one belongs. So:

| the question | the tool | what it costs |
|---|---|---|
| does this journal's SHAPE still fit the code? | `Proc.strands` | nothing — a pure fold, no runtime |
| was it written by THIS program? | the envelope's `program` field / `Retire.census` | one pass over the envelopes |

Neither is enough alone, and the honest version of the stage is that
sentence rather than a green test. The test asserts BOTH — including
the mis-mapping that is not caught, because a limit nobody has written
down is a limit somebody will discover in production.

**`strands` IS PURE, and that is the whole point.** No row, no monad,
no `Runtime` in its signature — the same proof `walk` carries. A
deploy can ask ten thousand live journals whether they still fit the
code about to ship, before shipping it. `Retire.states` answers a
neighbouring question and cannot do this: it replays, so it needs a
row and a runtime and costs a run apiece.

**THE EXHAUSTIVE CUT IS A PROPERTY, WHICH IS MOST OF WHY THE SHAPE
EXISTS.** "A crash resumes correctly" is normally a sample — somebody
picks a plausible moment. A term has finitely many leaves, so it is a
loop: crash at every one, resume, compare. Over a real topic, every
activity runs once except the one the crash caught in flight, which
runs twice. That is the at-least-once floor with a window of exactly
one call, measured rather than asserted.

**THE FIRST CUT OF THAT TEST MODELLED THE WRONG CRASH.** It threw
BEFORE counting the activity, which models a process that died without
making its outside call — not a window anybody worries about. The
window is that the call HAPPENED, the card was charged, and the answer
never reached the log. One line, and the test failed loudly at cut 1
until it was right.

**TWO MACRO BUGS FELL OUT, both invisible to every earlier test.**
Writing a term whose slot is a `Long` and whose leaf is a GADT case
named directly broke `Proc.direct` twice:

- a projection typed as the path-dependent `env._2` unifies with a
  reference type by luck and NOT with a primitive ("Expected type:
  scala.Long, Actual type: env._2"). Every projection is now ascribed
  at the slot type the compiler recorded when the slot was pushed.
- the answer type of a leaf is NOT the last type argument.
  `Wf.Question.Now[Q, A]` extends `Question[Q, A, Long]`, so reading
  `args.last` off the CASE gives `A`. It is read off the base type at
  the signature's own symbol now.

Every earlier test went through a door whose declared result type was
the parent, and every earlier slot was a reference. Two coincidences
held the encoding up.

### Stages 3 and 4 — landed 2026-09-18 (`static-workflow-optics`)

`Proc.mermaid` and `TestProcOptics` (11).

**STAGE 3 BUILT NOTHING, WHICH WAS THE CLAIM.** An optic is a function
polymorphic in a profunctor constrained by what it needs — a lens asks
for `Strong`, a prism for `Choice` — and `Proc` has had both since
stage 1, so `lens(step)` type-checked before this lane existed. What
the lane adds is the answer to whether it BEHAVES, which is not the
same question: the step sees only the focused part, the surrounding
state comes back untouched, the journal holds the step's answers and
nothing else, and the term folds that journal back to the same whole.
A prism's absent case asks nothing and journals nothing, while
`leaves` still reports its step — the over-approximation, once more,
and in the one place where it is most obviously right.

**THE PICTURE IS DRAWN FROM THE TERM, and that is the whole feature.**
A monadic engine can only draw a process from a status projection
somebody keeps in step with the code by hand, so the picture and the
program drift and the picture is the one nobody checks. Here they
cannot disagree — it is the same value the engine runs. Both sides of
a choice are drawn because which one runs is decided by a value that
does not exist yet; a loop is drawn ONCE with a back edge, because a
picture that unrolled it would be lying about a number the term does
not have.

**WHAT WAS NOT DONE IS NAMED.** The third box asked for the
`ui-direct-example` form as a `Proc`. Its two claims are already made
— an optic applies to a step with no new machinery, and a crash
between two questions resumes — so what a form would add is a
CONSUMER, which belongs to a UI lane. Filed rather than quietly
ticked.

**THE PRICE proc-notation EXISTS TO REMOVE IS NOW VISIBLE.** Every
test term needed a `keep` helper — `arr(x => (x, x)) >>> second(p)` —
because what a monadic body keeps in a local variable, a term carries
on its edge. Four uses in one booking. That is the evidence lane 3
was waiting for, and it arrived by writing the terms rather than by
arguing about them.
