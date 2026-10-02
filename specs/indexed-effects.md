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

Stage 7 — Cont's leaf on the machine:

```scala
object Delim.Stacked:
  enum Op[F[+_], S, R, +X]:
    case Shift[F[+_], St <: Tuple, R, P0 <: Prompt[R], X](p: P0 & Prompt[R], body: (X => R) => R, at: At) extends Op[F, St, St, X]
  def contShift[R, A, F[+_]](p: Prompt[R])(using Stack[?])[B <: Tuple](using Has.Aux[st.S, p.type, B])(f: (A => R) => R)(using At): Under[F, A, st.S]
```

Stage 8 — the `Prog` facade removed: `Tx.Data` becomes `Tx` (`Tx.begin/
commit/rollback/update/batch/describe/async`, `Tx.interpret`), Prog.scala
deleted, the guide's section rewritten over it.

Stage 9 — Lexical's clauses over the program type:

```scala
trait Ops[F[+_], R, P[_]]:    def op[X](e: F[X], k: X => P[R]): P[R]
trait Clauses[F[+_], A, R, P[_]] extends Ops[F, R, P]:    def ret(a: A): P[R]
// unstacked instances at P = [A] =>> A ! Delim + G; stacked ones at P = [A] =>> Under[G, A, St]
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

Stage 7:
- [x] `contShift(p)(k => k(1) + k(10))` under a `delimited` answers 11
      on the machine; a list reflected through a shift called once per
      element; a typed `shift0` inside the segment a synchronous `k`
      runs; a segment holding a foreign operation refused by name.
- [x] Decisions carry the two refutations: answer-type modification
      cannot sit under a mark (its answer is its prompt's), and a
      `Cont[A, R, R]` VALUE cannot be converted (a leaf's inner answer
      type is erased, and a diagonal-typed program may hold
      non-diagonal binds).

Stage 8:
- [x] Prog.scala is gone; `okay.sql.Tx` is the data road; TestTx's
      shapes hold on it; docs/guide.md's "Typestate on a program" is
      rewritten over `Tx` with its lines pinned; TestProg keeps only
      the stacked shapes.

Stage 9:
- [x] `Ops`/`Clauses`/`ShallowClauses` take the program type `P[_]`;
      the unstacked instances are unchanged in behaviour (TestLexical
      green), the stacked ones take clauses over `Under[G, *, St]` with
      no `erase` on their road (TestLexicalStacked green on typed
      clauses).

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
- **Additive everywhere, until the facade had no consumer**: stages
  1-7 kept `Tx`'s `Prog` facade beside `Tx.Data`; stage 8 removed it
  once both of its consumers (okay-sql's `Tx`, the stacked `Delim`)
  ran on the indexed tree. `Delim.Stacked`'s doors keep their spelling
  where the spike keeps them; `!` is untouched and `!!` is beside it.
- **Cont on the machine is the diagonal fragment, and a door, not a
  conversion** (stage 7). A mark's answer type is its prompt's, fixed
  when it is pushed; Danvy–Filinski's answer-type modification makes
  the same reset answer `S` without a shift and `R` with one, which no
  frame of the machine can carry — so the full `Cont[A, S, R]` stays on
  Cont's runner, and what the machine hosts is `(A => R) => R` at the
  prompt's `R`. And a `Cont[A, R, R]` VALUE is not converted, even
  though its outer type is diagonal: a bind's middle index is
  existential, a leaf `(X => T) => R` with `T ≠ R` is invisible after
  erasure, and a program composed of two answer-type-modifying halves
  types as diagonal — the conversion would hand its body a `k` of the
  wrong type and the error would be a `ClassCastException` at run
  time. The door `contShift` is typed diagonal at its site, which is
  where the two runners meet.
- **The unary member of an indexed row is a match type**, not a
  wrapper (`At`, refuted by freer-diag-leaf) and not a claim: the type
  reduces only on the diagonal, so the door refuses what the handler
  could not answer. If dotty cannot carry it through a handler's
  existential middle index, the fallback is the door's own check
  (`unary` builds `Diag`, `effect` requires a `NotUnary` witness) and
  the decision is recorded.

## Results

### Stage 9 — LANDED (indexed-effects-9-lexical-typed): clauses over the program type

`Lexical.Ops[F, R, P[_]]`, `Clauses[F, A, R, P[_]]` and
`ShallowClauses[F, A, R, P[_]]` answer in `P[R]`; the unstacked
instances are `P = Lexical.Unstacked[G]` (`[X] =>> X ! G`) and behave
as before (TestLexical, TestLexicalTail, TestLexicalDefault,
TestLexicalWalk green unchanged; `Lexical.State.deep/shallow` and the
three test clause objects moved to the new spelling — the one visible
change to a caller is the fourth type argument, `Unstacked[Delim + G]`
for `Delim + G`). The stacked deep instance takes
`Clauses[F, A, R, Lexical.Stacked.Below[G, St]]` (`[X] =>> Under[G, X,
St]`, St the stack below the instance's prompt): `perform` asks
`Has.Aux[st.S, p.type, S]` with `S` FIXED to the class's stack, so the
evidence proves the stack below rather than finding it, and hands the
clause the stacked `k` as it is; the installation is one `Op.Dollar`
over `c.ret` and the body. `at` and `erase` are gone from that road
(both stay in `Delim.Stacked` for `Lexical.Stacked.Tail`'s guard, which
is the unstacked `dollarResumed`, and for `Layered.Stacked`). The price:
stacked clauses name their stack, written `def deep[St <: Tuple]` and
instantiated by the installation (inference finds `St = st.S` from the
expected clause type, so the call is `Lexical.Stacked.deep(clauses)`
with no type arguments), and a clause object typed at another stack is
refused at the installation (TestLexicalStacked, 3 tests). Full
`affected master staged`. Performance: not measured here; the stacked
road lost two identity casts per operation and gained nothing, and the
one pass after this stage prices everything (Deferred measurements).
### Stage 8 — LANDED (indexed-effects-8-prog-facade): the facade removed

Prog.scala is deleted, with its `Prog[F, A, S, R]` opaque type,
`diag`/`pure`/`transition`/`.free` and the `flatMap` import trap.
`okay.sql.Tx` is the data road alone: the doors `Tx.Data.begin/commit/
rollback/update/batch/describe/async/interpret` are `Tx.begin` etc.
(`Tx.Data[A, From, To]` stays as the program type's name, `TxOp`,
`Conn`, `Row` unchanged); the facade class `Tx(db)`, `Tx.Step` and
`Tx.run` are gone, and TestTx with them — its shapes (nested begin,
orphan commit, a program left open, all compile errors) were already
TestTxData's. TestProg keeps the stacked shapes only. docs/guide.md's
"Typestate on a program" is rewritten over `Tx` with every line pinned
by TestTxData; docs/theory/03 names the indexed signature as the third
instance; the roadmap's Road 3 record says the facade is gone.
Nothing is faster or slower by this: the facade was an identity over
the untyped tree, and `Tx` was already measured on the data road
(Deferred measurements).

### Stage 7 — LANDED (indexed-effects-7-cont-on-machine): Cont's leaf on the machine

`Op.Shift[F, St, R, P0, X](p, body: (X => R) => R, at)` is a fourth
case of the typed signature, diagonal (`Op[F, St, St, X]`) at the
prompt's own answer type, and `contShift(p)(f)` is its door beside
`shift`/`control`/`shift0`. The machine's arm splits the stack at the
prompt like `Capture` does and answers with `body(k)` where `k` is
SYNCHRONOUS: it reifies the captured segment at the value and runs it
on a nested `machine(_, forward = false)` to its `Return`, so the body
may call it zero, one or many times (`k(1) + k(10)` answers 11; a list
reflected by calling `k` once per element). The nested run is one
machine frame per `k` call, nested only where the body nests its calls
(`k(k(v))`): the program's text, never its data — that is the bound the
four inventory rows carry (specs/stack-safety-okay.tsv, `loop`,
`machine`, `step`, `sync` mutual through `answer`). A segment that
performs a FOREIGN operation cannot be run by a synchronous `k`, and
the machine refuses it by name (`UnsupportedOperationException` naming
the site and the prompt) rather than losing the operation — the fourth
test pins it; a typed `shift0` inside the segment is a machine
operation, not foreign, and runs. The runner is one: Cont's own
`Cont.run` still exists for the full `Cont[A, S, R]` (Decisions: the
answer-type-modifying fragment cannot sit under a mark), and everything
diagonal runs where Delim runs. TestContOnMachine (4 tests); the delim
family (TestDelim*, TestProg, TestStackedShift0, TestLexical*,
TestLayered, TestCont) green unchanged; additive gate. Performance: not
measured in this lane — the arm is a new `case` in `step` that an
unstacked program never reaches, and the one measurement pass after
stages 8 and 9 prices the machine as a whole (Deferred measurements).

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

MEASURED (indexed-effects-measure, the same day, the operator's word
that a regression is fixed at once): the one machine against the old
on the five `DelimBenchmark` lanes that exercise it, alternating
rounds, MIN, `jmh-lane.sh -f2 -wi3 -i5 -prof gc`, every lane quiet,
bytes identical to the byte on every lane (history.d
`2026-09-30T134711Z-indexed-effects-measure.tsv`). The first cut read
level on four lanes (0.990 / 0.993 / 1.000 / 1.007) and 1.035-1.044
on `stateLexDeep`, two rounds, bars 1-2 µs: one type test more per
operation — `step` tested the typed `Op` before `Delim`, the loop
tested `Diag` before `Inject` — on the lane with the most operations
per capture. REORDERED in the lane: Delim's total class test first
(one test per unstacked operation, what the old machine paid), `Op`
second, `F` by exclusion; `Inject` before `Diag` (no Delim program
builds a `Diag` since the embeddings are identities). After it:

| lane | one machine | old | ratio |
|---|---|---|---|
| `stateLexDeep` (MIN of 3) | 100.98 µs | 100.00 µs | 1.010 |
| `delimGenerator` | 69.06 µs | 69.23 µs | 0.997 |
| `delimPushOnly` | 17.16 µs | 17.67 µs | 0.971 |
| `delimDollarResume` | 39.09 µs | 38.98 µs | 1.003 |
| `writerTellUnderDelim` | 25.28 µs | 25.63 µs | 0.986 |

Inside the bars on every lane. The lesson is the one
delim-machine-allocs already recorded for this loop: on a lane that
does thousands of operations per capture, the ORDER of the type tests
in the dispatch is a measurable quantity, and the common case goes
first.

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

### The measurement pass — LANDED (indexed-effects-measure-2), the arc priced

One pass after stage 9, every question the stages deferred, master
e574034d8 against the arc's base d31fc94f1 (the old Delim machine, the
unshared Get, the facade), alternating arms, one lane per `jmh-lane.sh`
(`scripts/history.sh indexed-effects-measure-2`):

| lane | arc | base | ratio | bytes |
|---|---|---|---|---|
| delimGenerator | 68.80 | 68.13 | 1.010 | same |
| delimPushOnly | 17.18 | 17.44 | 0.985 | same |
| delimDollarResume | 38.58 | 39.12 | 0.986 | same |
| writerTellUnderDelim | 25.36 | 25.47 | 0.996 | same |
| stateLexDeep | multi-modal, see below | 100.1 | 1.01-1.02 (common mode) | same |
| stateThreaded | 16.52 | 18.05 | 0.915 | 244 832 vs 276 904 |
| stateEffect (control) | 16.97 | 17.31 | 0.980 | same |
| stateIndexedForward vs stateForward | 32.85 | 31.37 | 1.047 | 369 128 vs 368 280 |
| Tx.Data vs the facade (facade's last tree) | 1.680 | 1.261 | 1.332 | 20 560 vs 13 032 |

- **The one machine costs nothing on four of five Delim lanes** (0.985-1.010,
  bytes identical). `stateLexDeep` is the one that MOVES, and it moves PER
  JVM FORK: master reads one of {91.7, 100.5, 101.5, 103.6, 109.9} us/op,
  each fork tight (±0.3), where the base reads 99.7-101.5 over twelve
  forks. Two fixes were tried in the lane. Slimming `step` back to the
  old machine's size (the typed `Op` arms moved to `typed`, 1282 -> 672
  bytes) changed nothing but removed the 91.7 mode. A `PrintInlining`
  probe of four forks per tree found the cause: the ONE fast fork is the
  one where `Freer.resume`'s rotation closure (`f(_).flatMap(g)`) was
  NOT inlined into the loop ("already compiled into a medium method");
  every base fork and every slow fork inlines it. That is a base-tree
  design lead, not a machine defect — backlog
  `freer-rotation-closure-jit-modes` carries the evidence and the road
  (the rotation as a node the loops walk). Kept: the slimmed `step` (the
  old shape, no worse, one fewer thing to blame).
- **The shared Get is the number pstate-threaded promised**: -32 B per
  step, 244 832 B = the untyped State's count minus 72, and 0.915x the
  base's time — `stateThreaded` now reads 0.973x `stateEffect`. The typed
  protocol on the data road is no longer 1.07x the untyped effect; it is
  under it.
- **`splitI` against `split` on a forwarding handler: 1.047x, and it is
  not the test.** Two fixes landed here: `handleIndexed` forwards the
  NODE it holds (`forwardedI`, the indexed `forwarded`) instead of
  rebuilding it (-16 B per forwarded operation, measured), and
  `TypeableI.derived` emits a constant-class `instanceof` where
  `byClass` read a field and called `Class.isInstance` (the residual
  `TypeableK.derived` removed on the unary side). Time did not move:
  `stateIndexedForward` is multi-modal per fork (32.2/32.6/33.1/33.7/35.1)
  where `stateForward` is stable (30.8-31.7) — the same shape as
  `stateLexDeep`, the same closure, the same backlog item. The 848 B
  that remain are per run, not per operation.
- **`Tx.Data` against the facade: 1.33x and +74 B per statement, and that
  is the tree.** The facade was an identity over the driver programs'
  own `Async` chain; the data road builds one node, one bind and one
  closure per operation on top of that chain, and `interpret` walks
  them. Against a driver round trip (tens of microseconds at best) four
  nanoseconds per statement is noise, and the facade is gone (stage 8)
  because the tree is what checks the protocol. Refuted alternative: a
  fast path in `interpret` for a driver program that is already a
  `Return` — it would collapse the residue only for a silent Sql, which
  no caller has.
