# Arrows, optics, notation and the static workflow: one plan

## Overview

Four documents landed on 2026-09-18 about the same corner of the
library, from two agents, within hours: specs/optics.md stage 12 and
its re-check (optics-arrows-effects, optics-arrows-recheck),
specs/static-workflow.md (the durable spine without `ArrowApply`) and
specs/proc-notation.md (the `direct` macro at an arrow). The operator
asked for a second reading of the optics spec and ONE plan — what is
best to do, in what order, by whom — so that nothing is built twice
and the pieces that are one thing are built once.

This spec is that plan. It takes three decisions the four documents
left implicit, corrects one interface in static-workflow.md that the
second reading showed to be the wrong factoring, and orders the lanes.
It adds no new feature.

## What the second reading of optics stage 12 found

Read against static-workflow and proc-notation, line by line:

1. **The re-check already did the reconciliation on their side.**
   `ArrowChoice`'s trigger is marked PULLED by `Proc`; `prism-selective`
   WAITS on static-workflow stage 3; indexed optics has three named
   seats and a restated trigger; `Grate` has no seat; the stream
   question is answered by `Mealy`. Nothing in stage 12 contradicts
   the two workflow specs, and the line "neither lane adds the other's
   instance" is quoted there. So the risk is not contradiction; it is
   TWO lanes each waiting for the other to write the shared piece.
2. **The shared piece is named in both boards and owned by neither:
   the arrow law suite.** `optics-arrow-instances` says "whichever
   lane lands first writes it reusable"; static-workflow stage 1 says
   "the laws hold … property". A piece two lanes need and nobody owns
   is the one that gets written twice or not at all. Decision 2 below
   gives it a lane of its own, FIRST, with no new instance in it.
3. **`Mealy` is the instance the suite already has**, and its shape
   is instructive: `Optic.Arrow[Mealy] & Optic.Choice[Mealy]` as ONE
   given (Mealy.scala). `Proc`'s instance should be spelled the same
   way, and the suite should take exactly that intersection.
4. **Static and Proc are the same idea at two arities**, and the
   static-workflow Interface hid it. `Static[F, A]` is a free
   applicative over a SIGNATURE `F` with `leaves: Vector[F[Any]]`,
   `toFree` and `foldMap` into any `Selective`. static-workflow's
   `Proc[Q, A, X, Y]` bakes the WORKFLOW's question type into the
   core structure, so the arrow that stage 12 says "earns its place
   at the static part" would exist only for workflows, and a second
   free arrow would be needed the day dataflow or `Tables.Plan` wants
   one. Lindley, Wadler & Yallop's ladder puts arrows beside
   applicatives, not beside workflows. Decision 1 refactors the
   interface before a line of it is written.
5. **Indexed optics may have a fourth seat in `Proc` itself.** The
   re-check says the family stays out until "a fourth path-carrying
   walk, or two of the three wanting one walk". `Proc.walk` returns a
   `Path`, `render(Some(path))` marks it, `Stranded(at: Path)` names
   it — three functions over one term, all carrying a path. Whether
   they are ONE indexed fold is a fact stage 1 of static-workflow can
   establish by trying, and Decision 4 makes it a deliverable of that
   lane rather than a lane.
6. **`optics-cont-profunctor` is independent of all of this** and
   stays theirs, in the order they chose; nothing here waits on it or
   feeds it.

## Decision 1 — `Proc` is over a signature, and the workflow is one signature

The core type loses the question and answer parameters:

```scala
// Proc.scala (core, beside Static.scala — the arrow to its applicative)
enum Proc[F[+_], X, Y]:
  case Arr(f: X => Y)
  /** the leaf: an operation of the signature, built from the input;
   *  NAMED, because the operation VALUE does not exist before the
   *  input does and the static reading needs something to report */
  case Eff(name: String, op: X => F[Y])
  case Seq[F[+_], X, Y, Z](f: Proc[F, X, Y], g: Proc[F, Y, Z]) extends Proc[F, X, Z]
  case First[F[+_], X, Y, C](f: Proc[F, X, Y]) extends Proc[F, (X, C), (Y, C)]
  case Left[F[+_], X, Y, C](f: Proc[F, X, Y]) extends Proc[F, Either[X, C], Either[Y, C]]
  case Iter(body: Proc[F, X, Either[X, Y]])

object Proc:
  given [F[+_]]: (Optic.Arrow[[X, Y] =>> Proc[F, X, Y]] & Optic.Choice[[X, Y] =>> Proc[F, X, Y]])
  extension [F[+_], X, Y](p: Proc[F, X, Y])
    /** every leaf this procedure MAY reach, by name, in term order —
     *  both sides of a Left, an Iter's body once; the same upper
     *  bound `Static.leaves` reports */
    def leaves: Vector[Leaf]
    /** the term interpreted into the Kleisli arrow of a monad — the
     *  general form of `Static.toFree`, and `Iter` is why it asks for
     *  a Monad rather than a Selective (an iteration's count is a
     *  value; only a monad can run a step it counts) */
    def foldMap[G[_]: Monad](nt: F ==> G): X => G[Y]
    /** the same into the program monad: `Free.Inject` per leaf */
    def toProgram: X => Y ! F
    def render(at: Option[Path] = None): String
  /** `Static[F, A]` IS `Proc[F, Unit, A]`: the door, one way, so
   *  that an applicative spine can be a step of an arrow */
  def ofStatic[F[+_], A](s: Static[F, A]): Proc[F, Unit, A]
```

And the workflow is a SIGNATURE plus the walk that only it can have:

```scala
object Wf:
  /** the questions a durable procedure asks — the author's and the
   *  library's, one enum, so the journal's Left/Right tag is a case */
  enum Question[Q, A, +R]:
    case Ask[Q, A](q: Q) extends Question[Q, A, A]
    case Sys[Q, A](s: Wf.Sys) extends Question[Q, A, Wf.SysA]

  type Proc[Q, A, X, Y] = okay.Proc[Question[Q, A, *], X, Y]

  /** the doors, each an Eff of the signature: ask, now, uuid, random,
   *  patch, timer, sleep, awaitSignal, awaitChild, cancelled */
  /** THE BRIDGE into the landed engine: foldMap with the natural
   *  transformation Question ==> (Delim.ask, tagged) — Dialogue.workflow,
   *  Worker, timers, signals, children, retries, cancel, continueAs
   *  unchanged */
  def program[Q, A, X, Y](p: Wf.Proc[Q, A, X, Y])(x: X): Asks[Q, A, Y, Pure] ?=> Y ! (Delim + Pure)
  /** THE STRUCTURAL FOLD — only this signature has a journal to fold */
  def walk[Q, A, X, Y](p: Wf.Proc[Q, A, X, Y])(x: X, answers: Delim.Journal[Ans[A]]): Either[Stranded, Standing[Q, Y]]
  def accepts(…): Boolean
```

What this changes and what it does not:

- **Nothing in static-workflow's Behavior changes.** Every box there
  is stated in terms of the booking, the loop, the journal, the deploy
  check; all are reachable through `Wf.Proc`. The interface section of
  that spec is amended to point here.
- **proc-notation's road becomes generic in `F`**, which is what the
  `direct` macro already is: `Proc.direct[F, X, Y]` closes a leaf
  expression `!op` (an `F[Y]` over bound names) into `Eff(name, env =>
  op(env))`. The workflow doors are ordinary `F` values of
  `Question`. One road serves every signature.
- **`Static` stays.** It has consumers, a measured cost and a
  stack-safe `foldMap` into `Selective` that `Proc` cannot offer (an
  `Iter` needs a monad). `ofStatic` is the door between; deleting
  `Static` is refused, for the same reason `Par` stays beside the
  macro's flat join: generic code over `Selective` wants the
  applicative shape.
- **The leaf's name is the static content.** `Static.leaves` reports
  operation VALUES because they exist before the run; `Proc`'s cannot
  — an `Eff`'s op is a function of the input. So `leaves` reports
  names and kinds, `render` draws names, and the journal's structural
  check compares names and order. For the workflow the name is the
  door (`ask`, `now`, `timer`, `signal:payment`, `patch:promo`); an
  author's `ask` may name itself (`ask("city", …)`) and the deploy
  check gets stronger with every name given.

## Decision 2 — the arrow laws are one lane, first, with no instance in it

`arrow-laws`: `okay.laws.ArrowLaws[P[_, _]]` (test scope) — the
Category laws (identity, associativity), the Arrow laws (Hughes'
nine, in Paterson's normalised form), the Choice laws — parameterised
by the carrier's instance and an OBSERVATION `P[A, B] => A => B` (how
to run one arrow on one input and read the answer), which is
`TestMealy`'s shape lifted out. Instantiated at `Mealy` alone, so the
lane changes no instance and its only claim is that `TestMealy`'s
statements now live in the suite and pass. Cost: a morning. Value: the
race between `optics-arrow-instances` and `static-workflow-proc`
disappears; each instantiates one line.

## Decision 3 — the order, and who

| # | lane | owner | depends on | what it proves |
|---|---|---|---|---|
| 1 | `arrow-laws` | whoever claims first; this plan's author will if unclaimed when static-workflow-proc starts | — | the suite exists, at `Mealy` |
| 2 | `static-workflow-proc` | the workflow side | 1 | `Proc` core + `Wf.Question` + `program`/`walk`; the booking on the landed engine; the same journal from both front ends; `walk == replay`; stage 3's lens/prism tests FOLDED IN (the instances make them one-liners, and `optics-prism-selective` closes on them) |
| 3 | `proc-notation-road` | the workflow side | 2 | the block text at both entries from one source; the `app` refusal pinned |
| 4 | `optics-arrow-instances` | the optics side | 1 | `Function1` and the Kleisli instantiate the suite; the "one Profunctor" sentence becomes a fact |
| 5 | `static-workflow-strands` | the workflow side | 2 | the deploy census; the exhaustive cut |
| 6 | `optics-cont-profunctor` | the optics side | — | independent experiment, refutation road stated |
| 7 | `static-workflow-render`, `proc-doors` | the workflow side | 2, 3 | the picture; statement-shaped doors |
| gated | `direct-targets` (the IR), `Par`, compensation, the cursor chapter; `optics-field-fuse`, `ui-path-two-walks` | as filed | their triggers | — |

The first three are one agent's sequence; 4 and 6 are the other's;
neither sequence waits on the other after lane 1. `optics-guide-page`
(claimed as this is written) is docs-only and touches none of it.

## Decision 4 — the indexed-optics seat is a question stage 1 answers

Deliverable of `static-workflow-proc`, one sentence in its Results
either way: **are `leaves`, `walk` and `render` one path-indexed fold
over the term?** If the three are written as one `foldIndexed[R](f:
(Path, Leaf) => R, …)` and each is a one-line instantiation, the
fourth seat exists and the trigger in optics stage 12 is pulled — to
be filed on the optics board, not built in the workflow lane. If
`walk`'s answer-threading or `render`'s structure make them
genuinely three walks, that is recorded and the family stays out with
its three seats. Trying is cheaper than predicting, and both answers
are useful.

## Refused, so nobody re-derives them

- **Deleting `Static` in favour of `Proc[F, Unit, A]`** — see
  Decision 1; the applicative `foldMap` into a `Selective` is a
  capability the arrow does not have.
- **A `Category` for the Kleisli used as the workflow spine.** `A => B
  ! R` composed with `>>>` is a monad in arrow's clothing (`app` is
  one line away), and the spine's guarantees rest on its absence.
  optics-arrow-instances adds the instance for what it is — a plain
  effectful function — not as a spine.
- **`Tables.Plan` branching, `Grate`, `ArrowLoop`** — each still has
  its recorded trigger, and this plan pulls none of them.
- **A second law suite.** One, in test scope, generic; a lane that
  needs a law the suite lacks adds it to the suite.

## Out of scope

- Any code. This is the plan; the lanes above carry the code.
- Re-ordering the neighbour's BACKLOG entries. Their board, their
  order; this plan cites them by slug.

## Results

(none — a plan is judged by whether the lanes above land in this
order without a duplicate; the first duplicate is a defect of this
document and is recorded here)
