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

Stage 6 — one machine (the follow-up stage 4 named):

```scala
object Delim:
  def run[R, F[+_]](prog: R ! Delim + F)(using OneMachine[F]): R ! F =
    Stacked.machine(Stacked.at[F, R, EmptyTuple](prog), forward = false)   // the unstacked door enters the typed machine
  def runNested[R, F[+_]](prog: R ! Delim + F)(using Row.In[Delim, F]): R ! F   // forward = true, embedded captures only
  // Segs, Frames, Cut, split/copy, reify, loop, step over `Delim` alone: DELETED
```

Stage 5 — the user page:

`docs/typestate.md`: what an indexed effect is, the two readings, when
to take which road, `PState.Threaded` and `Tx.Data` as the examples,
every ```scala line pinned by `TestDocExamplesTypestate`, the
literature (Atkey 2009, McBride 2011, Danvy–Filinski 1989).

## Behavior

Stage 1:
- [x] `PState.Threaded.get[S]` builds no node: two `get[Int]` are `eq`,
      and a `get[String]` is the same object; TestState's threaded
      protocol test is unchanged and green.

Stage 2:
- [x] `TxOp` and `Tx.Data`: `begin.flatMap(_ => begin)` is a compile
      error, `commit()` alone cannot be interpreted (`Idle -> Idle` is
      the only shape `interpret` takes), a well-bracketed program runs
      against the recording fake `Sql` in the order the type promised
      (TestTx's shapes, on the data road), and an `Async` program runs
      inside the body.
- [x] `interpret` threads `Conn[S]`: `Begin` is answered only from a
      `Conn[Idle]` and yields a `Conn[Open]`; `Commit`/`Rollback` only
      from `Conn[Open]`. `closed` on a `Conn[Idle]` and `opened` on a
      `Conn[Open]` do not type (`compileErrors` pins).
- [x] A failure inside a data-road transaction drops the continuation
      and the transition does not happen — the caveat stage 2 of
      freer-base asserts for the facade, asserted here for the data
      road (the log ends at `begin`).

Stage 3:
- [x] `+~` and `Unary`: a program over `PSt +~ Unary[State[Int, *]]`
      types with `State` entering through `unary`, and putting a
      `State` operation through `effect` at a moving index is a
      compile error (the row probe's thrown `IllegalStateException` is
      gone, refused at the door).
- [x] `splitI` dispatches by class over the indexed row; the reference
      handler (State's, over the indexed row) is in the library and
      TestFreerPara's row probe uses it.
- [x] The `Row.In` crash class (row-membership-crash) is re-asked for
      the three-ary row: no inductive given over `+~` exists, the
      membership is the class test and exclusion (Results).
- [x] A `direct` block over an indexed program is OUT of this stage;
      the macro's symbol table is unchanged, and every existing
      `direct` suite is the pin (Results).

Stage 4:
- [x] The spike's outcome recorded (Results): the positional witness
      refuted by `shift`'s re-installation, the segments typed by
      answer types, `rebase` the one claim.
- [x] Every `Any` payload the signature can type is typed (`Op`'s
      three cases); the casts the machine keeps are named: the two on
      embedded unstacked operations (their payloads are `Any` by
      construction), `rebase`, and `erase`'s excluded middle.
- [x] TestDelim and TestProg are green unchanged (TestProg's stacked
      shapes run on the new machine; its facade tests on `Prog`).

Stage 6:
- [x] `Delim.run` and `runNested` run on `Stacked.machine` through
      `Stacked.at`; the unstacked machine's chain, cut, reify and loop
      are gone from Delim.scala; `runNested` forwards an EMBEDDED
      capture whose prompt is not on this machine (a typed one is never
      forwarded: its `Has` says the prompt is here).
- [x] Every Delim consumer runs unchanged on the one machine: TestDelim
      and its family, collect/resumable/pausing, okay-ui's Scope and
      Screen, okay-agent's Stepper, okay-llm's Cut — the full `affected
      master staged`, both stages.
- [x] The measurement lane's first question is restated (Results):
      `at` is an identity; the one machine's shape is what to price.

Stage 5:
- [x] `docs/typestate.md` exists, every example line pinned
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

### Stage 2 — LANDED (indexed-effects-2-tx-data), on the row

Built on `TxOp +~ Unary[Async]` from the start (the operator's answer
to "is the row worth building": a transaction body wants effects
inside it, and `reflect` is not a way out — it needs a Cont). What the
compiler said:

- **The alias reads left to right and the tree the other way.**
  `Data[A, From, To] = Freer[Row, To, From, A]`: the first cut wrote
  `Data[Granted, Idle, Open]` for `begin` over `Freer[Row, S, R, A]`
  and got `Open -> Idle`; the alias is where the readable order and
  the tree's meet, once.
- **A match type with provably disjoint indexes WARNS, E184** ("Match
  type reduction failed since selector Open matches none of the
  cases") at every door whose two indexes are different concrete
  types. `Unary` gained `case _ => Nothing`: `Nothing` where the
  indexes are disjoint, stuck where they are abstract, `F[X]` on the
  diagonal — the refusal holds in all three.
- **The connection's moves are found lexically.** `opened`/`closed`
  as extensions in `Conn`'s companion were not applied to the
  GADT-refined `c: Conn[R]` (the companion's implicit scope does not
  try the refinement); `import Conn.{opened, closed}` inside the
  handler does, and the wrong arm is a compile error as the box asks.
- **`Indexed.lift`** (core): a unary program onto the diagonal, one
  `Diag` per operation as the interpreter reaches it, the recursion
  under `flatMap`.
- The caveat holds and is asserted: a failure inside the body drops
  the `commit`; the log ends at `begin`.

### Stage 6 — LANDED (indexed-effects-6-one-machine): one machine

`Delim.run` and `runNested` enter `Stacked.machine` through `at`; the
unstacked machine — 326 lines of Delim.scala: its `Segs`, `Frames`,
`Cut`, `split`/`copy`, `reify`, `loop`, `step` over `Delim` alone —
is deleted. What the run found, each a red before it was green:

- **The embeddings must be identities.** The first cut's `at` and
  `erase` were lazy rewrites, a node per operation each way; every
  capture's continuation went out through one and came back through
  the other, each resumption wrapped the rest of the program in one
  more layer, and Lexical's depth test (10 000 performs through one
  deep instance) walked a quadratic number of nodes into an
  OutOfMemoryError. The facade's `.free` had been an identity. Both
  are casts now, with the argument in `Stacked`: the nodes are the
  same classes, an unstacked operation is a member of the indexed
  row, this machine reads `Inject` and `Diag` alike, and nothing but
  this machine interprets a `Delim` program. `Indexed.lift` stays for
  `Tx.Data.async`, whose handler needs `Diag` to mean diagonal.
- **A function is re-based, never wrapped.** `reify`'s dollar arm
  wrapped the frame's `ret` in a closure to move its index; a deep
  instance's 10 000 performs nested 10 000 of them around one `ret`,
  unwound on the stack at the end (StackOverflowError, the same test).
  `rebaseF` casts the function value; the embedded dollar arms pass
  their `ret` through as the old machine did.
- **`runNested` forwards embedded captures only**, with the one cast
  the old machine had for it; a typed capture is never forwarded.
- Gate: the Delim family (TestDelim and its suites, TestDollarProbe,
  TestProg, TestStackedShift0, TestLexical, TestLexicalStacked,
  TestLexicalTail, TestLayered, ProbeRowInference) 79/79, no
  warnings; the full `affected master staged`.

Not measured, by the arc's rule; the deferred lane's first question
is now moot in its stated form (`at` costs nothing) and becomes: the
one machine's own shape against the old one on `DelimBenchmark`.

### Stage 4 — LANDED (indexed-effects-4-delim-signature): the typed Delim machine

The operator's word ("Да ок"): `Delim.Stacked` is a real machine over
the indexed tree, not a facade over `Prog`; `Lexical.Stacked` and
`Layered.Stacked` moved with it; the unstacked `Delim` and its machine
are untouched. What was built, and what the compiler and the
semantics said on the way:

- **The signature, with its types.** `Delim.Stacked.Op[F, S, R, +X]`
  — `Push[St, R, P0 <: Prompt[R]](p: P0 & Prompt[R], body: Under[F, R,
  P0 *: St])`, `Dollar`, `Capture[St, R, P0, B, S0, A](p, f: (A =>
  Under[F, R, S0]) => Under[F, R, S0], underPrompt, delimitK, at)` —
  parameterised by the rest of the row `F` (fixed at `run`, so it may
  be named), every operation on the DIAGONAL: the tree's index is the
  stack the operation runs under, and a `Push` nests its body one
  deeper through the payload's own index. The two casts the unstacked
  machine makes (a `Push`'s body and a `Capture`'s `f` erased to
  `Any`) do not exist for these operations; the row is `Op[F] | Delim
  | F`, the unstacked operations embedded on the diagonal, and
  `Under[F, A, S] = Freer[Row[F], S, S, A]` IS the tree.
- **The positional witness is REFUTED for the machine**, by `shift`'s
  semantics rather than by the compiler (ProbeDelimTyped, question
  3): `reset(E[shift f]) = reset(f(x => reset E[x]))` keeps the outer
  reset while `k` installs an inner one, so when `E` runs again the
  stack is one prompt deeper than when `E`'s witnesses were built —
  a positional witness is a de Bruijn index and re-installation
  shifts it. The machine keeps the identity search (`===` with
  `Same`) as its cut; `Has` stays what it was, compile-time evidence
  of presence, unchanged in shape so Lexical and Layered keep their
  `Has.Aux`. Question 4 fell the same way: `P0 <: Prompt[x]` and `P0
  <: Prompt[R]` do not make `x = R` for dotty.
- **The segments are typed by answer types, not by the stack**, for
  the same reason: a re-installed segment runs under more prompts
  than it was typed at. The stack index is evidence of presence, and
  presence is monotone under the identity search, so a segment may
  run at any stack extending its own — said ONCE, in `rebase`, the
  machine's one claim (erased, costs nothing), used in `reify` and at
  a capture's re-push. `Segs.K` carries its continuation at the two
  indexes a `Bind`'s continuation has (the middle one is existential),
  and the loop is index-polymorphic.
- **Two embeddings meet Lexical's clauses**, which are written over
  `A ! Delim + F` with answer types that embed unstacked programs:
  `Delim.Stacked.at`/`under` lift an unstacked program onto the
  diagonal (`Indexed.lift`, a `Diag` per operation as the machine
  reaches it), and `erase` turns a typed program into its unstacked
  twin — lossy on purpose, each typed operation to the `Delim` case
  the machine handles alike, bodies deferred so nested resets erase
  in constant stack. `Prog.diag` became `at`/`under`, `.free` became
  `erase`, `Prog.pure` became `Freer.Return` (index-polymorphic); the
  facade `Prog` itself stays for its own consumers (`Tx`, TestProg's
  facade tests).
- **A matched case's singleton loses its bound**: `case pu: Op.Push[F,
  st, r, p0]` binds `p0 <: Prompt[?]`, so the fields are `p: P0 &
  Prompt[R]` and a re-push names `c.p.type`, the value's own singleton,
  which carries `<: Prompt[r]`.
- Gate: TestProg, TestStackedShift0, TestLexical, TestLexicalStacked,
  TestLayered, TestDelim, ProbeRowInference, ProbeDelimTyped 48/48,
  no warnings; the full `affected master staged`.

Not done, and named: routing the unstacked `Delim.run` through this
machine (via `at`) and deleting the old one — the unification the
arc points at, a lane of its own with the full matrix as its gate,
and the first thing the deferred measurement should price. And
`control0` stays unstacked, for specs/shift0-dollar.md's reason.

### Stage 3 — LANDED (indexed-effects-3-row)

`src/main/scala/Indexed.scala`: `+~`, `Unary[F]`, `TypeableI`,
`splitI`, `Indexed.pure/effect/unary/offDiagonal`;
`State.handleIndexed` in State.scala; TestFreerPara's row on them. What
the compiler said:

- **The match type does its job at both doors.** `Indexed.unary[Row,
  R, Int](State.Modify(…))` types: `R match { case R => State[Int, X] }`
  reduces for an abstract `R` (the scrutinee is a subtype of the
  pattern by identity). `Indexed.effect[Row, Int => Unit, String =>
  Unit, Int](State.Modify(…))` is refused: the member is stuck between
  two function types dotty does not prove disjoint, and no value
  conforms to a stuck match type. Pinned by `compileErrors`.
- **Inside the handler the reduced member pattern-matches as
  `State[S, X]`**: `splitI`'s exclusion arm at the `Diag` case is
  written `{ case Get() => … }` directly, the GADT binding `S` through
  it. At the `Inject` case the member is stuck (existential middle
  index) and the arm is `Indexed.offDiagonal` — the design's one
  throw, unreachable through the doors.
- **A lone `Diag`/`Inject` needs its type arguments spelled** when
  rebuilt as a bind with a pure continuation: dotty does not infer the
  row from a union whose second member reduced to `State[S, X]`.
- **Box 3 (the crash class) needs no re-asking**: no inductive given
  over `+~` exists, membership is by the class test and exclusion, as
  `split`'s is. Box 4 (`direct`) holds by construction: the macro's
  symbol table is untouched, and every existing `direct` suite is the
  pin.

### Stage 5 — LANDED (indexed-effects-5-docs)

`docs/typestate.md`, linked from docs/README.md; `TestDocExamplesTypestate`
pins the core examples verbatim, `TestTxData` the transaction's;
TestDocSnippets and TestDocsIndex green.

### Deferred measurements

The lane to run after stage 6, and its first question: `DelimBenchmark`
before and after stage 6 — the one machine's own shape against the
old one (`at` and `erase` are identities, so the embedding costs
nothing; what could move is the loop's extra `Diag`/`Op` type tests
and the two-index `Segs`). Then, from the earlier stages: `stateThreaded` after the shared node
(expected 244 904 B, the State count), a `Tx.Data` interpretation
against `Tx`'s facade (expected within noise: the same driver
programs), the row's `splitI` against `split` on a forwarding handler,
and the Delim lanes (`DelimBenchmark`) before and after stage 4.
