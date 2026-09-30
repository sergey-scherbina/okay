# Indexed effects — the arc after the invariant base

## Overview

On 2026-09-30 the base became `Freer[G[_, _, +_], S, R, +A]` with `S`
and `R` INVARIANT (specs/freer-base.md, "The indexes INVARIANT"), a
`Diag` case put a unary operation on the diagonal by its node
("The diagonal leaf"), and `PState.Threaded` ran a type-changing state
through `State.handle`'s loop at 1.07x the untyped State against 1.79x
through the answer type ("PState as data"). Those three landings
answered the operator's question — where and how the effect system
uses the tree's two indexes — and left a list. This spec is that list
written down BEFORE it runs, as the scrumban rule asks, with the
operator's directive of the same day fixed at the top:

**Design and capability first, every stage executed regardless of its
number. Performance is measured and optimised LATER, in a lane of its
own.** No stage below runs a benchmark; a stage that would have asked
for one records the lane to run instead (Results, "Deferred
measurements"). What every stage does run is its tests and the
additive gate (`affected master Test/compile`), and a stage that
changes existing behaviour runs the full `affected master staged`.

The reading the arc builds on, from specs/freer-base.md, in one line
each:

- An index is a type the tree carries on every `Bind`; the tree does
  not know what it means. Two readings type on the invariant base: an
  ANSWER TYPE (Cont, `PState.get/set`, printf) and a CONSUMED STATE
  (McBride's, `PState.Threaded`, a resource typed by the index).
- A unary effect enters an indexed row on the diagonal through
  `Freer.diag`; its handler matches `Diag`. A three-ary signature
  moves the index through `Inject`.
- `Prog`'s phantom index and `Delim.Stacked` are claims on a facade;
  stage 4 asks whether Delim's claim can live on its signature instead.

## Interface

Stage 1 — the shared `Get` node (State.scala):

```scala
object PState:
  object Threaded:
    inline def get[S]: Threaded[S, S, S]   // ONE node for every S, as State.get: SharedOps.getT, the same cast
```

Stage 2 — `Tx` as an indexed data signature (okay-sql):

```scala
enum TxOp[S, R, +X]:                                  // package okay.sql
  case Begin(isolation: Isolation, readOnly: Boolean) extends TxOp[Tx.Open, Tx.Idle, Granted]
  case Commit() extends TxOp[Tx.Idle, Tx.Open, Unit]
  case Rollback() extends TxOp[Tx.Idle, Tx.Open, Unit]
  case Update[S](sql: String, params: Vector[SqlValue]) extends TxOp[S, S, Long]
  case Batch[S](sql: String, rows: Chunk[Vector[SqlValue]]) extends TxOp[S, S, Long]
  case Describe[S](sql: String) extends TxOp[S, S, Vector[Col]]
object Tx:
  type Data[A, S, R] = Freer[TxOp, S, R, A]           // the protocol as a program of its own ops
  final class Conn[S] private[sql] (val db: Sql)      // the connection, TYPED BY THE INDEX
  def begin(…): Data[Granted, Idle, Open]; def commit(): Data[Unit, Open, Idle]; …  // doors, no `transition`
  def interpret[A](p: Data[A, Idle, Idle])(db: Sql): A ! Async   // the handler: threads Conn[state], each op the driver's program
```

`Tx` (the `Prog` facade) stays as it is; `Tx.Data` is the second door,
additive, the PState/Delim policy (specs/freer-base.md Decisions).

Stage 3 — the three-ary row (core):

```scala
/** the row of indexed signatures; a UNARY member enters on the diagonal ONLY */
infix type +~[F[_, _, +_], G[_, _, +_]] = [S, R, X] =>> F[S, R, X] | G[S, R, X]
/** a unary effect as a member of an indexed row: the match type reduces only at S = R */
type Unary[F[+_]] = [S, R, X] =>> S match { case R => F[X] }
object !!:                                              // the doors of an indexed program, beside `!`'s
  def effect[G[_, _, +_], S, R, X](g: G[S, R, X]): Freer[G, S, R, X]          // a moving operation
  def unary[G[_, _, +_], R, X](e: …): Freer[G, R, R, X]                      // a unary one, on the diagonal (Freer.diag)
  def widen / translate at the indexes
/** split by class over an indexed row, as `split` does over a unary one */
inline def splitI[F[_, _, +_], G[_, _, +_]](op: (F +~ G)[S, R, X])(f: F[S, R, X] => B)(g: G[S, R, X] => B): B
```

`State.handle` (and the handlers that forward) are NOT rewritten over
the indexed row in this stage; the row probe in TestFreerPara is
moved into the library as the reference handler shape, and the row
probe's `moving` arm's throw becomes a compile error at the door.

Stage 4 — Delim's prompt stack on its signature (core, a spike with a
landing):

```scala
/** Delim's operations with the prompt stack as the index they move: Push installs p, a 0-capture pops it */
enum DelimOp[S <: Tuple, R <: Tuple, +X]:
  case Push[R, St <: Tuple](p: Prompt[R], body: Freer[…, p.type *: St, p.type *: St, R]) extends DelimOp[St, St, R]
  …
```

What lands is decided by the spike (Decisions): at least the payloads
typed instead of `Any` where the signature can say it, and
`Delim.Stacked` re-expressed as the doors of that signature rather
than a facade over `Prog`; the machine's continuation stack (`Segs`)
is out of scope.

Stage 5 — the user page:

`docs/typestate.md`: what an indexed effect is, the two readings, when
to take which road, `PState.Threaded` and `Tx.Data` as the examples,
every ```scala line pinned by `TestDocExamplesTypestate`, the
literature (Atkey 2009, McBride 2011, Danvy–Filinski 1989).

## Behavior

Stage 1:
- [ ] `PState.Threaded.get[S]` builds no node: two `get[Int]` are `eq`,
      and a `get[String]` is the same object; TestState's threaded
      protocol test is unchanged and green.

Stage 2:
- [ ] `TxOp` and `Tx.Data`: `begin.flatMap(_ => begin)` is a compile
      error, `commit()` alone cannot be interpreted (`Idle -> Idle` is
      the only shape `interpret` takes), a well-bracketed program runs
      against the recording fake `Sql` in the order the type promised
      (TestTx's shapes, on the data road).
- [ ] `interpret` threads `Conn[S]`: `Begin` is answered only from a
      `Conn[Idle]` and yields a `Conn[Open]`; `Commit`/`Rollback` only
      from `Conn[Open]`. A handler arm that commits from `Conn[Idle]`
      does not type (a `compileErrors` pin on a copy of the arm).
- [ ] A `Throws` abort inside a data-road transaction drops the
      continuation and the transition does not happen — the caveat
      stage 2 of freer-base asserts for the facade, asserted here for
      the data road.

Stage 3:
- [ ] `+~` and `Unary`: a program over `PSt +~ Unary[State[Int, *]]`
      types with `State` entering through `unary`, and putting a
      `State` operation through `effect` at a moving index is a
      compile error (the row probe's thrown `IllegalStateException` is
      gone, refused at the door).
- [ ] `splitI` dispatches by class over the indexed row; the reference
      handler (State's, over the indexed row) is in the library and
      TestFreerPara's row probe uses it.
- [ ] The `Row.In` crash class (row-membership-crash) is re-asked for
      the three-ary row: the membership witness is subtyping, never an
      inductive given, and `ProbeRowCrash` gains the three-ary shape.
- [ ] A `direct` block over an indexed program is OUT of this stage;
      the macro's symbol table is unchanged and a test pins that a
      `direct` block still compiles over `A ! F` exactly as before.

Stage 4:
- [ ] The spike's outcome recorded in Decisions with the compiler's
      words; what landed listed in Results.
- [ ] Every `Any` payload the signature can type is typed; every cast
      the machine keeps is named with its invariant.
- [ ] TestDelim and TestProg are green unchanged.

Stage 5:
- [ ] `docs/typestate.md` exists, every example line pinned
      (`TestDocExamplesTypestate`), `TestDocSnippets` green, the page
      indexed where the docs index lives.

## Out of scope

- **Benchmarks, in every stage** (the operator's directive). The lane
  that measures the arc afterwards is named in Results.
- **Value-dependent post-states** (McBride's `a : I -> Set`): the
  sum-typed state is the encoding (specs/freer-base.md, "McBride's
  reading").
- **The Delim machine's continuation stack as a `Freer`** — `Segs`
  stays; stage 4 types the signature, not the machine.
- **Rewriting the unary handlers over the indexed row.** `A ! F` stays
  the diagonal at `Unit` for the 95% case; the indexed row is a door
  beside it.

## Decisions

- **Design first, numbers later** (operator, 2026-09-30): every stage
  lands on its tests; the measurement of the whole arc is one later
  lane, so the numbers are read once, on a quiet box, against one
  base.
- **Additive everywhere**: `Tx` keeps its `Prog` facade beside
  `Tx.Data`; `Delim.Stacked`'s doors keep their spelling where the
  spike keeps them; `!` is untouched and `!!` is beside it.
- **The unary member of an indexed row is a match type**, not a
  wrapper (`At`, refuted by freer-diag-leaf) and not a claim: the type
  reduces only on the diagonal, so the door refuses what the handler
  could not answer. If dotty cannot carry it through a handler's
  existential middle index, the fallback is the door's own check
  (`unary` builds `Diag`, `effect` requires a `NotUnary` witness) and
  the decision is recorded.

## Results

### Stage 1 — LANDED (indexed-effects-1-shared-get)

`SharedOps.getT`, `PState.Threaded.get` under the same cast as
`State.get`, `TestState` pinning `get[Int] eq get[String]`. The
measurement is deferred (below).

### Deferred measurements

The lane to run after stage 5: `stateThreaded` after the shared node
(expected 244 904 B, the State count), a `Tx.Data` interpretation
against `Tx`'s facade (expected within noise: the same driver
programs), the row's `splitI` against `split` on a forwarding handler,
and the Delim lanes (`DelimBenchmark`) before and after stage 4.
