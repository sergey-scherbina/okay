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

```scala
// Proc.scala (core, beside Wf.scala)

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

### Stage 1 — the type, the bridge, the laws (`static-workflow-proc`)

- [ ] `Proc` is an `Optic.Arrow` and an `Optic.Choice`, and the
      category, arrow and choice laws hold on a term with every
      constructor present (property, comparing `toProgram` runs
      against a pure interpreter)
- [ ] the five-line booking of docs/continuations/23 written as a
      `Proc` runs through `Dialogue.workflow` with NOTHING in
      okay-persist changed: the clock read once across a restart,
      `sleep` suspending with `Waiting(Until(t))`, a signal answered
      from the mailbox, `patch` taking the old branch on an old
      journal
- [ ] a loop: "ask `nights?` and then one `room?` per night" as an
      `Iter`, with the position after the second room being a path
      whose iteration count is 2, both under `walk` and under the
      engine
- [ ] `walk` and `Wf.replay(toProgram)` AGREE on every journal
      (property over generated terms and generated answer prefixes,
      including journals the term refuses)
- [ ] `walk` performs no effects — its signature is the proof, as
      `Wf.replay` taking no runtime was
- [ ] `leaves` of a term with a `Left` reports both sides and of an
      `Iter` reports the body once, in term order
- [ ] an existing `Wf` journal written by the monadic booking is
      accepted by the `Proc` booking that asks the same questions in
      the same order — the two front ends share one journal format,
      pinned

### Stage 2 — the deploy check and the exhaustive cut (`static-workflow-strands`)

- [ ] a v2 term with a new first question REFUSES the v1 journal at
      `walk`, naming record 0 and the position — failure B of
      durable-workflow, caught with no run started
- [ ] `Proc.strands(topic, term)` lists, over every live run in a
      topic, the ones the term would strand and where; a run the
      term accepts is not listed; the census reads envelopes and
      answers only
- [ ] EVERY cut: for a generated term and oracle, crash before the
      append and after the append at every leaf, resume from the
      journal, and the final answer equals the uninterrupted run's;
      every activity ran at most once per `(id, position)` when the
      cut was after the append and at most twice when before — the
      at-least-once floor, measured, not assumed
- [ ] a `Sys.Patch` leaf under `walk` obeys the non-consuming rule
      (answer `false`, entry NOT consumed, on a journal written
      before the patch existed) exactly as `Wf.replaying` does

### Stage 3 — optics as the state glue (`static-workflow-optics`)

- [ ] a lens applied to a step runs the step on the focused part and
      puts the result back; the surrounding state is untouched and
      the journal holds only the step's answers
- [ ] a prism applied to a step runs it on the matching variant and
      passes every other variant through with no leaf asked
- [ ] the form of `ui-direct-example` (validated applicatively, errors
      put back by an optic) rewritten as a `Proc` whose questions are
      the form's fields, resumed from a journal after a crash between
      two fields

### Stage 4 — the picture (`static-workflow-render`)

- [ ] `render` draws the term (Mermaid) with every leaf named, both
      sides of a `Left`, a back-edge for an `Iter`
- [ ] `render(Some(path))` marks the run's position, taken from
      `walk` — no `Statuses` projection consulted

### Stage 5 — gated: notation and the rest the closure forbade

Each of these has a TRIGGER and lands only when it fires:

- [ ] `proc`-notation — MOVED to specs/proc-notation.md and UNGATED
      (operator, 2026-09-18): it is stage 1's companion, not stage
      5's option. The lambda-boundary risk it named is stated there
      as the price that stays.
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

(none yet — stage 0 is this document, 2026-09-18)
