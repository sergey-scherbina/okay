# Freer, minimal: a three-node core with answer-type modification and prompts

Status: hypothesis, stage 0 (2026-10-06). Lane `freer-min`, branch
`feature/freer-min`, worktree `../okay-wt-freer-min`.

## Overview

The operator's ask (2026-10-06): cut the core to the absolute minimum. The
core — the Freer monad — must support continuations with answer-type
modification and prompts; effect rows are BUILT by `pure`/`perform`/
`flatMap`, never declared. Diagonal effect types come after, if needed.

This spec records the definition and what the stage-0 probe
(`specs/probes/freer-min/Freer.scala`, scala-cli, Scala 3.9.0,
`-Werror -Wunused:all`) established. Nothing on master is touched.

## The definition

```scala
/** `(A => S) => R` over the signature `G`: a computation of `A` that, given a continuation into `S`, answers `R`.
 *  The freer monad, indexed for answer-type modification (Danvy–Filinski on the node, Atkey's parameterised
 *  monad as the algebra). `G` is COVARIANT: a program over a row is a program over any wider row, and `flatMap`
 *  joins the two sides' rows — the row is BUILT by `pure`/`perform`/`flatMap`, never declared. */
enum Freer[+G[_, _, +_], S, R, +A]:
  /** the value: the inner answer is the outer one (the diagonal), over the empty row */
  case Return[R, A](a: A) extends Freer[Pure, R, R, A]
  /** one operation of the signature, at its own indexes */
  case Perform[G[_, _, +_], S, R, A](op: G[S, R, A]) extends Freer[G, S, R, A]
  /** sequencing as data: the answer types meet at `T` */
  case Bind[G[_, _, +_], S, T, R, A, B](m: Freer[G, T, R, A], k: A => Freer[G, S, T, B]) extends Freer[G, S, R, B]

  /** the row of the result is the join of the rows of the two sides */
  def flatMap[H[_, _, +_], S2, B](f: A => Freer[H, S2, S, B]): Freer[G + H, S2, R, B] = Bind(this, f)
  def map[B](f: A => B): Freer[G, S, R, B] = Bind(this, a => Return(f(a)))

  /** head form — `Return`, `Perform`, or `Bind(Perform, k)` — in constant stack; associativity is the proof */
  @tailrec final def resume: Freer[G, S, R, A] = this match
    case Bind(Bind(m, f), g) => Bind(m, x => Bind(f(x), g)).resume
    case Bind(Return(a), f)  => f(a).resume
    case p                   => p

/** the empty row: no operation at all */
type Pure = [S, R, A] =>> Nothing
/** the row join */
infix type +[G[_, _, +_], H[_, _, +_]] = [S, R, A] =>> G[S, R, A] | H[S, R, A]

def pure[A, R](a: A): Freer[Pure, R, R, A] = Freer.Return(a)
def perform[G[_, _, +_], S, R, A](op: G[S, R, A]): Freer[G, S, R, A] = Freer.Perform(op)
/** a deferred program is not a node: a bind off the unit, forced by `resume`'s loop */
def delay[G[_, _, +_], S, R, A](t: () => Freer[G, S, R, A]): Freer[G, S, R, A] = Freer.Bind(Freer.Return(()), _ => t())
```

Prompts are NOT in the tree. They are two operations of one signature,
written over the row their bodies use (λ$, Materzok–Biernacki; the typing
is specs/freer-kont.md's, where the prompt carries its answer index and
value). The machine that answers them is the next stage; the tree knows
nothing of it, and a captured `k` is an ordinary `A => Freer`.

```scala
/** a delimiter's identity: by allocation, compared by `eq` */
final class Prompt[S, Y](val label: String)

enum Control[+G[_, _, +_], S, R, +A]:
  /** `ret $_p body`: delimit at `p`, `ret` the frame first above it (`reset p body = Return(_) $_p body`).
   *  Typed as `Bind(body, ret)` is — the delimiter sits between the two */
  case Reset[G[_, _, +_], S, T, R, X, Y](p: Prompt[S, Y], ret: X => Freer[G, S, T, Y], body: Freer[G, T, R, X])
    extends Control[G, S, R, Y]
  /** capture up to `p`, the delimiter included: `k` is the segment as a function, `ret`'s shape; `f(k)` goes
   *  on in the delimiter's place */
  case Shift0[G[_, _, +_], S, T, R, X, Y](p: Prompt[S, Y], f: (X => Freer[G, S, T, Y]) => Freer[G, S, R, Y])
    extends Control[G, T, R, X]

/** `Control` over a row, as a row */
type Ctl[G[_, _, +_]] = [S, R, A] =>> Control[G, S, R, A]
```

## What is gone, against master's `Freer` (Free.scala)

- `Delay`: derived, `Bind(Return(()), _ => t())`; the loop forces it where
  it forced `Delay`. (`Freer.Suspended`, `resumeRun` go with it.)
- `Mapped`: `map` is a plain `Bind`.
- `Unary[F]`, `Free[F, A]`, `object Free`'s four re-exported names and ITS
  ONE CAST (`Free.Bind.unapply` pinning the middle index to `Unit`):
  the index a matched `Bind` leaves unknown is recovered by the GADT
  match on a DIAGONAL operation instead (Results, 3).
- `handle`/`handleOne`, the `Monad`/`ParaMonad`/`TailRecM` givens, the
  `direct` colouring: not the core's.
- Invariance of `G`: the row is covariant, so `pure` is a program of
  every row and `(F + G) + F` is accepted where `F + G` is expected.

## Results (stage 0 probe, 2026-10-06)

1. The definition compiles on 3.9.0 under `-Werror`: covariant
   higher-kinded `+G` with the union `flatMap` — the mechanism
   `freer-two-part-row` is blocked on (scala/scala3#27234) — is accepted
   in this shape. A `for` over two signatures infers `Ask + Say`; a
   further `flatMap` into `Ask` gives `(Ask + Say) + Ask`, accepted at
   `Ask + Say`; `pure(3)` is accepted at `Ask + Say`.
2. 100 000 left-nested `map`s run in constant stack through `resume`.
3. A `Bind` matched through a covariant `G` leaves `G$6 <: G` and the
   middle index `T` existential. Dispatch that takes `op: Ask[T, Unit, X]
   | Say[T, Unit, X]` and `k: X => Freer[Row, Unit, T, B]` — a type
   test on the union picks the signature, and the GADT match on its
   diagonal operation (`Number() extends Ask[Unit, Unit, Int]`) recovers
   `T = Unit` and `X = Int` — compiles WITH NO CAST. The first cut, with
   `T` fixed to `Unit` in `step`'s signature, did not: `G$6[T$4, Unit,
   A$8]` is not `Ask[Unit, Unit, ?] | Say[Unit, Unit, ?]`. So the
   middle index must be carried to the match that pins it.
4. `Control` typed as above sits in a row with the effects:
   `Freer[Ctl[Row] + Row, Unit, Unit, Int]` builds.

## Open (the operator's call)

- The home of the kernel: a new module with no dependencies (proposed:
  `okay-kernel`, package `okay.min`), or master's `Free.scala` in place.
- `Shift0`'s body: typed at the delimiter's `(S, R)` as above, after
  freer-kont — or diagonal in an unknown outer index. The probe does not
  decide it; the machine will.
- "Diagonal effect types": read as operations declared at `F[T, T, A]`
  (`Unary[F]` today), which Results 3 already relies on for dispatch.

## Results, continued: the arity of `G` (2026-10-06, probe `FreerDiag.scala`)

The operator asked whether the three-place `G[S, R, A]` earns its place
beyond continuations, or whether a unary `F[A]` with the monad still
three-place would do. Measured on the model:

5. A unary signature lifted to the row by an index-ignoring lambda
   (`Diag[F] = [S, R, A] =>> F[A]`) through a node of its own,
   `Op[F, T, A](op: F[A]) extends Freer[Diag[F], T, T, A]`, recovers
   `T = R` from the node but LOSES the dispatch: from `Diag[F$] <: Row`
   the compiler does not invert the lambda, `F$`'s bound is
   `[_] =>> Any`, and `op: F$[X]` is not `Ask[X] | Say[X]`. That is
   master's cast, seen from the other side.
6. The DIAGONAL node over the three-place row instead —
   `Op[G, T, A](op: G[T, T, A]) extends Freer[G, T, T, A]` — compiles
   cast-free: the node gives `T = R`, and the pointwise bound
   `G$[T, T, X] <: Row[T, T, X] = Ask[X] | Say[X]` gives the dispatch.
   Unary effects stay `enum Ask[+A]`, enter by
   `effect(op): Freer[Diag[F], T, T, A]`, and a mixed program over
   `Diag[Ask] + Diag[Say] + PState` with the index moving `Int ->
   String` through `PState.Put` builds beside them. 100 000 left-nested
   maps, constant stack, as before.

So the four-node form, `Return | Op (diagonal) | Perform (index-moving)
| Bind`, is what keeps unary effects unary AND keeps the signature
open to index-moving operations. See "The arity of G" in Decisions.

## Results, continued: a handler's rest, and what the row's `+` can and cannot infer (2026-10-06)

Probes `RestCo.scala` (covariant kernel), `Inv.scala` (the same kernel
with `G` invariant; its union `flatMap` then needs one cast, the claim
foreign-effects-in-tree's probe C made), `RestPos.scala` (both):

7. A handler typed `p: Freer[Diag[Ask] + G, …]` against a three-member
   row `(Diag[Ask] + Diag[Say]) + Cnt`: `G` is NOT inferred without an
   expected type, on EITHER variance (`G <: [_, _, _] =>> Any`); with an
   expected type (`val rest2: Freer[Diag[Say] + Cnt, …] = runAsk(three)`)
   both compile. The unary baseline `FreeU[Ask + G, A]` against
   `(Ask + Say) + Cnt` fails the same way: this is the union lambda, not
   the arity — a beta-reduced union gives nothing to solve `G` from
   (Indexed.scala:62's own comment).
8. POSITIONALLY — the handled member left, the rest one application
   on the right, `three: Freer[Diag[Ask] + Rest, …]` — `G` infers
   EXACTLY on both variances with no expected type: invariant prints
   the reduced lambda, covariant prints `Diag[Say] + Cnt` as written.
   So the kernel keeps master's inference, which was positional all
   along (`handle`'s `=:=` evidence, `Remove`), and foreign-effects-in-
   tree's "covariance widens to `Object & Enum`" was the lower-bound
   `flatMap` road, not this one.

What follows for the operator's second question (ZIO in the tree): a
row joined by union gives SUBTYPING — `pure` into any row, `(F + G) + F`
at `F + G`, a ZIO handler reading `R`/`E` off the row by `<:<` as ZIO's
own `for` would — and gives SUBTRACTION (a handler's rest) only by
position or evidence. ZIO itself never subtracts: `provide`/`catchAll`
move `R` to `Any` and `E` to `Nothing` by subtyping. See Decisions.

## Decisions (2026-10-06, the operator's)

- **The diagonal node is `Inject`**, the index-moving one `Perform`.
  Four nodes: `Return | Inject | Perform | Bind`.
- **`G` stays three-place.** Not for continuations alone: for every
  operation that moves the index — the prompt stack in the index (a
  `Shift0` naming a prompt not on the stack becomes a compile error,
  today `NoPrompt` at run time), resources and transactions (acquire
  and release as index moves, a leak a type error), session-typed
  dialogues (`Paused.Ask/Done` typed by step) — and `PState`, which
  exists. ZIO's three places are NOT these: ZIO is graded (`R` by `&`,
  `E` by `|`, a semilattice), the row's own law, so a ZIO is a row
  member through `Inject`, raw, and never an index.
- **`Delay` is not a node.** `delay(t)` is `Bind(Return(()), _ => t)`;
  `defer(t)(f)` is `Bind(delay(t), f)`. Costs against master's node:
  the same allocations per `delay` (one node and the thunk; `Return(())`
  is the node), and for `defer` ONE rule in `resume` — a `Bind` whose
  left side is `Bind(Return(a), f)` applies `f` instead of composing a
  closure over it — which is the case master wrote for `Bind(Delay(t),
  g)`. What master used `Delay` for beyond laziness: `Suspended`, a
  thunk the machine steps into instead of forcing — the same by class
  on the continuation of a `Bind(Return(()), k)`, as `Mapped` and
  `Frames` already are functions known by class. Speed is measured
  after, against master's lanes (the `performance` skill).
- **The machine is the next stage, in this module, as a LIBRARY over
  the tree**: cont-core.md's two type-aligned lists (`Frames`, a
  segment; `Stack`, the segments at their delimiters) and the one loop
  with five rules, nothing else of master's `Delimited` (`Kept`,
  `nearest`, `Shots`, `strict`, `barrier`, `Outer`, `Step`). A captured
  `k` is a `Stack`, which is an `A => Freer` by class. `resume` stays
  for handlers over rows without `Control`; the machine never rotates,
  it pushes frames.
- **The module's name.** `okay-kernel` / `okay.kernel` is the
  microkernel of plugins and ports (specs/kernel.md, build.sbt, docs/
  README.md), so this lane is `okay-freer`, package `okay.freer`,
  until the operator says which of the two moves.

## Open, after stage 0

- `Control`'s row parameter: the BODY's row as probed (`Control[+G]`,
  `G` the row the body is written in, so a nested delimiter has a
  deeper type), or the base row with the bodies over `Ctl[F] + F`
  (freer-kont's `Row[F]`, closed under nesting, which `run` needs to
  strip all control at once). The machine decides; the first reset
  sugar will show which infers.

## Stage 1 — the machine (DONE on the JVM, 2026-10-06)

`Machine.scala`: cont-core.md's model, nothing else. `Frames` (a segment,
contravariant in what it consumes), `Stack = Done | Run | Delim` (what
closes a level into the run's result), `Piece = Hole | Over | Under` (a
capture built outward from the hole), `Captured` (the piece and the
delimiter's `ret`, an `X => Freer[Row[F], S, T, Y]` by class), `Resume`
(a pending `k(x)`: the lazy form `Bind(Return(()), r)`, spliced by the
machine, run alone by any other interpreter), `Resumption` (the rest of
a run after an operation handed out). One loop, `go`, five rules; `cut`
and `link` are tail loops with an accumulator; `step` answers a
`Next`.

`Control` is over the BASE row `F`, its bodies over `Row[F] = Ctl[F] + F`
(the open question of stage 0, decided): closed under nesting, so
`Machine.run[F, S, R, A](p: Freer[Row[F], S, R, A]): Freer[F, S, R, A]`
takes every delimiter off at once and hands the rest out as head forms
over `F`. Sugar: `reset`, `dollar`, `shift0`.

### The typing that made it fit, and the casts (five)

- **The answer index is ONE across the stack.** `Stack[F, B, S, R, S0, Z]`
  closes a level `Freer[Row[F], S, R, B]` into the result
  `Freer[F, S0, R, Z]`: `R` is the same in every node and in the result,
  because an answer chains through a boundary as it chains through a
  `Bind` (`Done` fixes it). So an operation handed out stands at the
  run's answer BY THE TYPES, and the hand-out `Bind(Inject(op),
  Resumption(k, m))` needs no claim — master's `Outer.diagonal` plus
  `substituteCo` is this, proved instead of asserted.
- **A `Return` under a delimiter proves the body kept its index.** At
  `Return(a)` with the segment empty over `Delim(p, ret, out, rest)`,
  the GADT gives `T = R` from the node, so `go(ret(a), out, rest)` types
  with no cast. The same equation types `Resume`'s splice: the lazy form
  is `Bind(Return(()), r)`, and a `Return` on the left of a bind makes
  the bind's middle index the run's answer.
- The casts, each in one function with its reason: (1) `control`, a
  `Control` on the row is this machine's and its arguments the node's
  (`F` is abstract, so only the class can be tested); (2) `effect`, the
  union's other half (a match on a union does not narrow its
  fall-through); (3) `resumed`, a `Resume` is the function the `Bind`
  holds, by its own extends clause; (4) `answered`, an index-moving
  operation handed out is re-indexed to its `T`: whoever answers it with
  a value takes the move on itself, as `runState` takes `Put`'s, and `R`
  is phantom in every stack node; (5) `cut`, THE PROMPT CAST: a prompt
  is allocated once at one type, and `eq` is that allocation.
  A type test `c: Control[F, S, R, A]` on the union is refused as
  uncheckable (E092: the arguments cannot be determined from `F[S, R,
  A]`), which is why (1) tests the class and casts.

### Behavior (TestMachine, 8 green on the JVM)
- [x] shift0's `k`, delimiter included, applied twice: 4
- [x] multi-shot, three resumptions summed: 60
- [x] a delimiter of another prompt between a capture and its own is
      captured and put back: 64
- [x] a capture with no delimiter of its prompt throws `NoPrompt`
- [x] answer-type modification REALISED: a `PState.Put` in a shift0 body
      moves the state `Int -> String` across the control operator, the
      program typed `Freer[Row[PState], String, Int, Int]`, run by the
      machine then by `runState`: `("s", 6)`
- [x] an effect inside a delimiter is handed out, answered outside, and
      the run goes on (k twice around an `Ask`): 12
- [x] 100 000 captures and resumptions in one delimiter in constant
      stack: each `k(())` is the lazy form, spliced, never nested
- [x] a `k` that escaped its delimiter runs the piece alone, through
      `Machine.run` and through the tree's own `resume`

### What answer-type modification IS in this tree
`Freer[G, S, R, A]` reads `(A => S) => R`, but a run ends in a VALUE,
and `Return` is diagonal: a program with `S ≠ R` can never end by a
`Return`. The indexes are realised by handlers — `runState` turns
`Put`'s move into a value of the new type, the machine threads a
shift0 body's move to the enclosing level — not by a delimiter
answering a different type than its body. Danvy–Filinski's "reset
returns a string" is the case where the answer is the value; here the
answer is an index and the value stays `Y`. This is McBride's reading
(freer-base.md), and it is what lets `PState` and `Control` compose.

### Open
- [x] JS and Native: `okayFreerJS/Test/compile; okayFreerNative/Test/compile` GREEN (2026-10-06).
- Speed: nothing measured. Master's lanes to compare against:
  HandlerBenchmark (stepping), DelimDepthBenchmark (capture depth,
  k called 1 and 8 times), Fib/statePara (closure fusion, which this
  tree has not).
- `reset`/`shift0` inference: every call in the tests names `F`
  explicitly (`reset[Pure, Unit, Unit, Int]`); the body's row does not
  solve `F` from `Row[F]` (the union lambda, Results 7). An expected
  type does. The row-inference ergonomics are a stage of their own.

## Results 9 — row inference for `reset`/`shift0` (2026-10-06, probe `Inference.scala`)

Measured over the module's own sources with scala-cli 3.9.0, six shapes:

1. `reset(p)(body)`, nothing named, `body: Freer[Diag[Ask], …]`: `F` is
   NOT solved (`F <: [_, _, _] =>> Any`). The body's row has to satisfy
   `Diag[Ask] <: Row[F] = Ctl[F] + F`, a union with the variable in it,
   and a union gives nothing to solve from (Results 7).
2. The same under an expected type, `val b: Freer[Row[Diag[Ask]], …] =
   reset(p)(body)`: compiles. `Row[F]` against `Row[Diag[Ask]]` is one
   alias applied twice, matched application to application — the
   positional mechanism of Results 8.
3. The prompt names the row, `Prompt[F, S, Y]`: `F` is solved from the
   prompt alone, and a body with no `flatMap` on the way checks by
   subtyping.
4. BUT a `for` written in place under that `reset`, or `k(n).flatMap(k)`
   inside a `shift0` body, FAILS when its expected type is `Freer[Row[F],
   …]` with `F` known: `-explain` shows `flatMap`'s `H` already fixed to
   `Ctl[Diag[Ask]]` when `k` is checked. The mechanism: `flatMap`'s
   result `Freer[G + H, …]` is constrained against the expected
   `Freer[Ctl[F] + F, …]` BEFORE the argument is typed (dotty's
   `constrainResult`), and a type-variable application `H[s, r, a]`
   against a union `Control[…] | Ask[a]` COMMITS to the first alternative
   that can be made to fit. The two tests that passed earlier with
   `Row[Pure]` passed because `Ctl[Pure] + Pure` simplifies to one
   member (`X | Nothing`), so there was no union to commit to. Writing
   the row as `F + Ctl[F]` changes nothing (and breaks the machine's
   GADT chains). Naming the hole (`shift0[X]`, or `(k: Int => …)`) fixes
   `X` and `T` but not this.
5. THE ROAD TAKEN — master's own rule, "an obligation over a row is
   carried, never searched for at an abstract row", in its `Row.Sub`
   form: the body's row `G` is a type parameter of its own, inferred
   BOTTOM-UP (no expected row reaches the `for`, so `flatMap` joins what
   the steps are), and membership is a `using ev: Freer[G, S, S, Y] <:<
   Freer[Row[F], S, S, Y]` — no type variable in it once `F` comes from
   the prompt, and the evidence IS the widening (`ev(body)`). With it: a
   `for` in place under `reset` over two effects with `k` used twice,
   nested delimiters with a capture to the outer one, and an effect
   outside the prompt's row REFUSED with "Cannot prove … <:< …". The
   only things named are the prompt's row (once, where the prompt is
   made) and the hole, `shift0[Int](p)(k => …)`.
6. The same shapes compile as `TestMachine` (17 green): the diagonal
   `reset`/`shift0` by evidence; the index-moving forms stay
   `Control.Reset`/`Control.Shift0` through `perform`, every index
   named, which the ATM test writes in full.

So the row is BUILT by `flatMap` and CHECKED by evidence; it is never
pushed down into a `for`. That is the discipline for every door that
takes a body: a handler's rest is inferred positionally (Results 8), a
body's membership is proved by `<:<`.

## Stage 2 — the self-sufficient machine: eight nodes, no cast (DONE on the JVM, 2026-10-06)

The operator's direction: `Reset` and `Shift0` are NODES of the tree, as
intended from the start; the question was whether that alone makes the
machine self-sufficient and removes every cast. It does. The tree:

```
Return | Inject | Perform | Bind | Delay | Reset | Shift0 | Resume
```

`Delay` is a node again (its derivation saved no case and cost an
allocation and an explanation). `Reset`, `Shift0`, `Resume` are the
machine's three: a delimiter, a capture, a pending `k(x)`. `Control`,
`Row[F]`, `Ctl`, the evidence on `reset`/`shift0` are gone; `Prompt` is
`Prompt[G, S, Y]`, naming the row of the context it delimits.

### The one typing fact that shaped it
A continuation's row cannot be the enum's covariant `G`: in `Shift0`
the `k` sits in contravariant position, and the compiler refuses it —
rightly, since the frames between a capture and its delimiter may do
more than the capture's own row says. So `k` is typed at the PROMPT's
row `H`, a case parameter of its own, and the machine runs each
delimited level at that level's row. Between levels there is exactly
one fact to carry, `H` inside `G`, and it is a polymorphic identity
`Widen[H, G]` written where a covariant match has just proved it (the
typed pattern `case r: Reset[h, ?, ?, ?, ?, ?]` names `h`). It is
composed down the levels (`andThen`), stored in the delimiter it
belongs to, and used to lift a body's result or an operation handed
out into the row outside. No node ever claims a type it was not given.

### How each of stage 1's five casts went
1, 2 (`control`, `effect`, the union): gone with `Row[F]` — a delimiter
is matched as a node, by GADT.
3 (`resumed`, the resumption): `Resume` is a node, matched by GADT; its
`Captured` links itself (`under`), so its two existentials never leave
the class.
4 (`answered`, the answer index): the answer index is not a parameter of
the stack at all. No node held a value of it; the program carries it.
`Resumption(k, m, sub): A => Freer[F, S0, T, Z]` types as it is.
5 (the prompt): the type test `_: p.type` IS `eq`, and the compiler
refines the delimiter's `H`, `S`, `Y` to the prompt's (probe
`freer-sing`). `grep asInstanceOf|@unchecked` over the kernel: 0.

### What the levels look like
`Stack[F, G, B, S, S0, Z]` closes a level over `G` into the run's
result over `F`; `Done` is the top level, `G = F`; `Delim` joins the
body's `H` above to `G` below with its `Widen[H, G]`, and keeps the
`Widen[G, F]` of the level it returns to. `Piece` is the same shape
cut loose, so linking it back anywhere (another run, another row `F`)
recomputes the witnesses from the live stack's. `go` is one `@tailrec`
loop, polymorphic in the level's row (dotty accepts the polymorphic
tail call).

### Behavior (TestMachine 11 + TestFreer 8 = 19 green on the JVM)
- [x] the eight stage-1 scenarios unchanged in substance
- [x] a program WIDER than its prompt's row: the delimiter's `k` stays at
      the prompt's row, the rest of the program is over the wider one,
      and the machine runs both (`Diag[Ask] + Pure`, answered 12)
- [x] the machine runs plain programs: a million deferred calls, 100 000
      left-nested maps — so `Freer.resume` is now a CHOICE (the tree's
      rotation for interpreters of rows without control), not a need
- [x] a body outside the prompt's row refused at compile time
- [x] `reset`/`shift0[X]` name nothing but the prompt and the hole: all
      indexes diagonal from the prompt; an index-moving delimiter or
      capture is the node itself with its indexes written (the ATM test)

### Open
- [x] JS and Native compile (below).
- Which loop is THE loop: `Machine.run` handles every node; `resume`
  handles five and leaves the machine's three as head forms. One of
  them should go, after a measurement.
- Speed, unmeasured: frames per `Bind` (the machine) against closure
  rotation (`resume`); `Widen.andThen` allocates a closure per
  delimiter and per linked node.
- The stage-1 probes under `specs/probes/freer-min/` describe the old
  kernel; `kernel/` holds this one.

## Stage 3 — checked against itself (2026-10-06)

The operator asked whether the eight-node design can be simpler still.
Three of the doubts were checkable, and all three were checked
(`specs/probes/freer-min/kernel/`, 14 scenarios green, `Bench.scala`).

1. **`$` is derived; `reset` is the primitive.** `ret $_p body` is
   `reset_p (body >>= x => shift0_p (_ => ret x))`: the body's value
   leaves the delimiter by an abort and meets `ret` outside, so a `k`
   captured inside carries `ret` with the delimiter (λ$'s rule, `v $
   E[S0 k. e] → e[k := λx. v $ E[x]]`) and `f(k)` never meets `ret`
   (`v $ w → v w`). The test that tells the two apart: `ret = _ + 1`,
   body `shift0(k => k(1) >>= k).map(_ * 2)` answers 7 (`k(1) = 3`,
   `k(3) = 7`), where `reset(body) >>= ret` would answer 5. So `Reset`
   lost its `ret` field, `Delim` and `Under` lost theirs, `Captured` is
   the piece and the prompt and nothing else. Smaller kernel, same
   calculus. Cost: a `$` pays one capture of an empty piece and an
   abort per completion; a plain `reset` pays nothing new.
2. **One loop.** With `Reset`/`Shift0`/`Resume` as nodes the tree's own
   `resume` was no longer complete (it left three nodes as head forms),
   so it is gone: `Machine.run` is THE interpreter, and every consumer
   in the tests runs head forms through it. Measured first (one JVM,
   best of 7 after warm-up, NO JMH — a hypothesis for the `performance`
   skill's lanes, not a result):

   | shape | `resume` | machine |
   |---|---|---|
   | right-nested 1M (`delay` + `flatMap`) | 14.0 ms | 7.2 ms |
   | left-nested 100 000 `map`s | 2.2 ms | 1.5 ms |
   | 1M `Ask` handled OUTSIDE | 10.0 ms | 15.8 ms |

   Frames beat closure rotation on plain programs; the hand-out
   protocol (a `Resumption` per operation answered outside) costs ~1.6x
   on an effect loop. Handing out the node itself (`sub(c)`, not a
   rebuilt one) took 17.5 → 15.8. The rest of that gap is the one
   allocation per hand-out, and it is the next thing to measure
   properly, against master's HandlerBenchmark.
3. **One bundle, not two.** `Found` and `Linked` were the same shape —
   a value due at a segment over a stack at a level — so they are one
   `Next`, and a capture's result is a `Cut`, a `Next` plus the captured
   continuation. The machine also answers a head form it handed out
   with that head form itself (`Machine.run(head) eq head`), so a
   handler that runs the machine on every step allocates nothing extra
   for it.

What stays as it was, with its reason: `Inject` beside `Perform` (the
equation `S = R` on the node is what lets a unary effect's handler
recover the middle index; dropping it means every unary effect is
declared three-place); `Widen` (the price of a covariant row with no
cast: an identity per delimiter and per linked node); `NoPrompt` at run
time (the typestate road would make it static); the index pair's order.

### Behavior
- [x] TestFreer 8 + TestMachine 13 = 21 green on the JVM, through the one loop
- [x] `dollar` 7, not 5; a run over a handed-out head form is identity

## Stage 4 — the prompt stack in the index: `NoPrompt` static (probe, 2026-10-06)

`specs/probes/freer-min/typestate/`: a second kernel, 243 lines, 12
scenarios green under `-Werror`, `grep asInstanceOf|@unchecked|NoPrompt|eq`
over it: 0. It is NOT the module's kernel; it is the other end of a
fork the index forces (below).

### What it is
The program's ONE index is the stack of delimiters in force, a tuple
of `At[L, Freer[H, EmptyTuple, Y]]` entries: the prompt's label `L`
(a literal type — `Prompt("p")` is `Prompt["p", H, Y]`; two prompts of
one label are one delimiter, by type; the machine never looks at a
prompt value), its row and its value, the two riding inside a program
type because a match type refuses an uninhabited selector and `Pure`
applied is `Nothing`.

```
Freer[+G[+_], S <: Tuple, +A]            -- rows UNARY, the index the stack
Return | Inject | Bind | Delay | Reset | Shift0 | Resume     -- seven
Reset [H, S, Y, L](p, body: Freer[H, At[L, …] *: S, Y])            extends Freer[H, S, Y]
Shift0[H, S, X, Y, L, O](p, has: Has[S, At[L, …], O], f: (X => Freer[H, O, Y]) => Freer[H, O, Y])  extends Freer[H, S, X]
Resume[H, X, O, Y](x, k)                                           extends Freer[H, O, Y]
```

`Has[S, P, O]` is a GADT (`Head`, `Tail`) — the witness that `P` is on
`S` with `O` under it — summoned by givens at the capture and WALKED by
the machine: `cut` matches the witness first (`Head`: this level's
delimiter is the one; `Tail`: cross it), and the stack's type follows,
so `Done`, the empty stack, is no case of the walk. `run` takes a
program on `EmptyTuple` only: a program that claims a delimiter it has
not installed cannot be run. A capture to a prompt with no delimiter
in force finds no witness: a compile error (checked by
`typeCheckErrors`). `Captured` is diagonal on the stack OUTSIDE its
prompt (it carries the delimiter), so an escaped `k` is a program of
the outside and runs later as one. A handed-out operation is
re-injected on the empty stack — free, because a unary operation
carries no index.

### What it took to make inference work (each one measured by a failure)
- The stack at a capture is LEXICAL: `reset` gives its body a given
  `In[At[…] *: S]` and `shift0` reads `S` from it FIRST. An expected
  type does not reach a method's receiver (`shift0(…).map(…)`), and a
  given searched with `S` open instantiates it to `Tuple`. A program
  written outside its reset declares `(using In[…])`. The top has
  `given top: In[EmptyTuple]`.
- The stack under the prompt, `Under[S, P]`, is a match type on the
  concrete stack (labels are provably distinct; `p.type`s of two vals
  are not — `val q = p` would alias them), and the witness is searched
  LAST, when `S` and `O` are known.
- The delimiter's row and value are tied to the prompt's by GADT on
  `At`'s arguments, not by the prompt's singleton: dotty does not
  identify `a.H` and `b.H` for two values of one singleton-bounded `P`,
  and a dependent parent (`extends Stack[…, At[p.type, …] *: S, …]`)
  compares as `Delim.this.p`, never as `d.p`.
- Product patterns on enum cases give BOUNDS, typed patterns give
  EQUALITIES but E092 when an argument is not determined by the
  scrutinee: `Has` is matched by typed pattern, `Delim` by a product
  pattern bound whole (`d @ Delim(…)`) and handed to a helper typed by
  `d` alone.

### The fork
With the index the prompt stack, every node is diagonal: `Reset`,
`Shift0`, `Resume` keep it, only `Reset`'s BODY sits one entry deeper.
So the second index is redundant and goes, and with it answer-type
modification and `Perform`: `PState`'s `Put` has no index to move. The
module's kernel does the opposite: the index pair is the answer type
(state), prompts are dynamic, `NoPrompt` is thrown. Both at once is a
fifth parameter, `Freer[+G, Σ, S, R, A]`, the stack beside the pair —
the same machinery as this probe plus the pair as before. Not built;
it is the operator's call whether user-level index moves (`PState`,
typestate effects) are worth one more parameter on every signature, or
whether control's static safety is.

What the typestate kernel is smaller by: `Perform` (one node), prompt
identity (no `eq`, no singleton test, no label at run time), `NoPrompt`.
What it is larger by: `Has`, `In`, `Under`, `At` — four small types —
and the lexical discipline. `Widen` is the same in both.

## Stage 5 — both: the delimiter stack beside the answer pair (DONE, 2026-10-06)

The operator kept typestate effects as user signatures, so the answer
pair stays, and the static prompts come in as the fifth parameter:

```
Freer[+G[_, _, +_], Σ <: Tuple, S, R, +A]
```

`Σ` is which delimiters are in force (what `Reset` pushes and `Shift0`
captures to), the pair `S`, `R` is the answer type (what `Perform`
moves). Every node keeps `Σ` but `Reset`'s body. The module's kernel is
this now (`okay-freer`, 253 lines of kernel): 22 tests green on the
JVM, JS and Native compile, `grep asInstanceOf|@unchecked|NoPrompt|eq`
over the kernel: 0. The merge was mechanical: stage 4's stack machinery
(`Has`, `In`, `Under`, `At`, the witness-guided `cut`, `Done` at the
empty stack) with stage 3's pair threaded through `Frames`, `Stack`,
`Piece`, `Captured` and `Next`.

Decided on the way, each by a failure:
- The prompt names its answer index again, `Prompt[L, H, S, Y]`, and the
  stack entry is `At[L, Freer[H, EmptyTuple, S, S, Y]]`: a capture's
  result index is the PROMPT's, fixed, while the level's index changes
  across the `Run` nodes a walk crosses — so it cannot come from the
  walk.
- `Next` names the index its value is due at (`Next[F, A, T, S0, Z]`):
  the splice of a `Resume` continues at the hole's index, which the
  node's own type carries.
- A program with no capture is POLYMORPHIC in the stack (`def one[Σ <:
  Tuple]: Freer[Fx, Σ, …]`): the index is exact, so a program written
  at the empty stack is not a body for a delimiter. `pure`, `inject`,
  `perform` already are polymorphic; a `val` program is not.
- The walk crossing delimiters (`cut -> tail -> crossed -> cut`) is a
  mutual recursion BOUNDED by the length of the index, a static tuple;
  it is inventoried as such (specs/stack-safety-okay.tsv).

### Behavior (TestFreer 8 + TestMachine 14 = 22)
- [x] everything of stages 1–3, under the stack
- [x] answer-type modification WITH static prompts: `Put` in a shift0 body
      moves the state `Int -> String` across the delimiter; the nodes
      written out with their indexes, the witness `Has.Head()` by hand
- [x] a capture with no delimiter in force is a compile error (no
      witness); a program claiming a delimiter it did not install cannot
      be run (`run` takes the empty stack only)
- [x] the same prompt twice: the innermost delimiter (Head before Tail)
- [x] `dollar` 7, escaped `k` is a program of the outside, 100 000
      captures in constant stack, a head form idempotent under `run`

## Stage 6 — are prompts needed at all? (probe `five/Nearest.scala`, 2026-10-06)

The operator's question. Measured: the crossing capture of the nested
test — `reset_p (reset_q (shift0_p (k => k(1) >>= k) + 10) * 2)`, 64 —
is reproduced WITHOUT naming `p` from inside `q`, by two captures to the
NEAREST delimiter: `shift0_q (k1 => shift0_p (k2 => f(x => dollar_p (k2)
(k1(x)))))`. The inner continuation runs again under a fresh delimiter of
`p` whose `ret` is the outer continuation, which is exactly what the
machine's `Under` piece builds (λ$'s `$` rule: `k` carries `ret`); both
answer 64. This is Materzok–Biernacki's result that `shift0` expresses
the hierarchy, in the kernel's own terms.

So a named prompt is not a KERNEL concept. With the delimiter stack in
the index, the nearest delimiter's row, answer index and value are the
HEAD of the index, and a capture to it is typed with no name:
`Shift0[H, O, S, T, R, X, Y](f) extends Freer[H, Entry[H, S, Y] *: O, T,
R, X]`. Capturing across inner delimiters is a derived combinator over
`shift0` and `dollar`, typed by the index (each step's types are the
head). What the kernel would lose: `Prompt` and its labels, `Has` and
its givens, `Under`, `Piece.Under`, `cut`'s `tail`/`crossed` and their
inventoried mutual recursion, the `At` label. What stays: `Reset(body)`,
`Shift0(f)`, `Resume`, the `In` given (an expected type still does not
reach a receiver). Not done; the operator's call.

Found by the probe, FIXED in the module (gate green, 22): a `shift0`
body is WRITTEN inside the delimiters but RUNS outside its prompt's, so
the sugar must give it the stack under the prompt (`In[Under[Σ, …]]
?=>`), not the stack it is written in — with the stack it was written
in, a `dollar` inside the body typed at the wrong level.

## Stage 7 — no prompts; ternary effects only (DONE, 2026-10-06)

The operator's decisions: prompts out of the kernel, answer-type
modification kept; and effects in ZIO's shape, three-place, considered.
Both done; the module's kernel is `specs/probes/freer-min/seven/`,
197 lines, SEVEN nodes:

```
Return | Perform | Bind | Delay | Reset | Shift0 | Resume
Freer[+G[_, _, +_], Σ <: Tuple, S, R, +A]
Reset [H, Σ, S, R, Y](body: Freer[H, Entry[H, S, Y] *: Σ, S, R, Y])            extends Freer[H, Σ, S, R, Y]
Shift0[H, O, S, T, R, X, Y](f: (X => Freer[H, O, S, T, Y]) => Freer[H, O, S, R, Y]) extends Freer[H, Entry[H, S, Y] *: O, T, R, X]
```

`Entry[H, S, Y] = At[Freer[H, EmptyTuple, S, S, Y]]`: an entry of the
delimiter stack is the delimiter's row, answer index and value, and the
NEAREST delimiter is the HEAD of the stack — a capture is typed by it,
with no name. `Prompt`, labels, `Has` and its givens, `Under`,
`Piece.Under`, `cut`'s `tail`/`crossed` and their inventoried mutual
recursion: gone. `cut` walks `Run` nodes to the first `Delim`, a tail
loop; `Done` is no case of it (the stack's type says there is a
delimiter). `grep asInstanceOf|@unchecked|NoPrompt|eq|Prompt|Has` over
the kernel: 0. 20 tests on the JVM, JS and Native compile.

What a prompt was doing in the SUGAR and what replaced it: naming the
delimiter's types at `reset` (a body's type cannot say what delimits
it) — `Delimiter[H, S, Y]`, a value with no identity, `@unused` in the
kernel's sugar, only there so `reset(d)(…)` and `shift0[X](d)(…)`
infer. Two delimiters of one kind are told apart by position alone.

A capture across an inner delimiter is DERIVED (stage 6's law, now a
test): `shift0(d)(k1 => shift0(d)(k2 => f(x => dollar(d)(k2)(k1(x)))))`
answers 64 as the walk did.

### Ternary effects
`Inject` is gone: every operation is `Perform`, three-place. A unary
effect is declared in ZIO's shape with its operations on the diagonal,
polymorphic in the index:

```scala
enum Ask[S, R, +A]:
  case Number[T]() extends Ask[T, T, Int]
```

The dispatch of a union of such rows recovers the middle index and the
value by the GADT match on the operation (Results 3, now the test's
`step`): no cast, no `Inject`. The costs, stated: two phantom parameters
per declaration and one per constructor, written at each use
(`Ask.Number[Unit]()` — the index does not flow into a receiver); and a
foreign unary type (`IO`, `ZIO[R, E, *]`) cannot be declared so and
needs a lifting signature of its own, one case class, one allocation
per foreign operation. `Inject` would be the one node that pays both
at once; it is a one-node addition if the costs prove too high.

## Stage 8 — unary effects and `Inject` back (DONE, 2026-10-06)

The operator's decision after stage 7's costs: effects are unary again,
`enum Ask[+A]`, entering the row as `Diag[F]` through `Inject`, the one
node that carries `S = R` so a handler recovers the middle index with no
cast; `Perform` stays for the index-moving operations (`PState`, a
protocol's steps). Eight nodes. 20 tests on the JVM, JS and Native
compile, no cast in the kernel.

## Stage 9 — what the delimiter stack means for effects (DONE, 2026-10-06)

The operator's question: with the fifth parameter kept, what is the whole,
and how does it touch effects? Measured by building the one thing it
touches: a handler INSIDE a delimiter, crossed by a capture.

- Effects themselves: not at all. An operation carries no stack; `inject`
  and `perform` are polymorphic in it; a handler is a loop over head forms.
- A program with no capture is written polymorphic in the stack (`def
  one[Σ <: Tuple]`), or at the stack it is for; a `val` is at one stack.
- A handler can live inside a delimiter: `Machine.run` runs at ANY stack
  (`Done` at any `Σ`, the run's own stack `Σ0` a parameter of `Stack`),
  and a capture that reaches the run's bottom is handed out as a head
  form, `Bind(shift, rest)`, exactly as an operation is (`Cut.Gone`; the
  equality of the run's stack and the capture's is a field, an `=:=`,
  because the reachability check treats a method's type parameter as
  rigid). The handler passes it through with its own continuation
  re-wrapped, state in the closure, so the capture is multi-shot through
  the handler: TestHandlers resumes twice through a counting handler and
  the count replays (answer 3).
- To let a capture through, a handler that REMOVES an effect from the row
  needs one fact: every delimiter in force has a row inside what it
  leaves (`Within[Σ, G]`, a GADT found by givens on the concrete stack and
  walked on the abstract one); it yields the delimiter's own `Widen`, and
  the capture's node widens by it — no cast. This is what the stack buys
  for effects: a handler KNOWS the rows of the delimiters around it. The
  capture's body runs at its delimiter, outside the handler, so it may
  only use what the delimiter's row allows, and the type says so.
- What stays a claim, outside the kernel: a handler answering `Counter` by
  class on a row `Diag[Counter] + G` claims `G` holds no `Counter` and
  that the union's other half is `G`'s — master's `Distinct[E + F]`; the
  test states it in one function, twice `@unchecked`, with the reason.

`run` taking any stack replaces "a program under a claimed delimiter cannot
be run" with "its capture is handed out"; a consumer at the top types its
parameter at `EmptyTuple` to forbid it. 21 tests on the JVM, JS and Native
compile, no cast in the kernel.

## Stage 10 — THE MINIMAL BASIS: effects, handlers, continuations with answer-type modification (DONE, 2026-10-06)

The operator named what is needed — effects, handlers, continuations
with ATM — and left the rest to judgement. The judgement, after the
detours of stages 4–9, and what each cost to learn:

```
enum Freer[+G[+_], S, R, +A]:                       -- (A => S) => R over the row G; seven nodes
  Return [R, A](a)                                    extends Freer[Pure, R, R, A]
  Inject [G, T, A](op: G[A])                          extends Freer[G, T, T, A]
  Bind   [G, S, T, R, A, B](m, k)                     extends Freer[G, S, R, B]
  Delay  [G, S, R, A](t)                              extends Freer[G, S, R, A]
  Reset  [H, S, R, U](p: Prompt[H, S], body: Freer[H, S, R, S])                               extends Freer[H, U, U, R]
  Shift  [H, Sp, T, R, X, V](p, f: ([U] => X => Freer[H, U, U, T]) => Freer[H, V, R, V])        extends Freer[H, T, R, X]
  Resume [H, X, T, U](x, k: Captured[H, X, T, ?])                                              extends Freer[H, U, U, T]
```

- The rows are UNARY, built by `flatMap`; `Inject` is the one operation
  node, diagonal. No `Perform`: no sequential typestate (stage 7's
  finding, the operator's "I do not know whether I need it").
- The answer pair is Danvy–Filinski's, typed as they typed it: a
  delimiter's VALUE IS ITS FINAL ANSWER (`reset body` with `body : S [S,
  R]` answers `R`, at any `U` outside); a capture's `k : X => T [U, U]`
  for every `U` is PURE — their `τ/t → α/t`, here a polymorphic function
  — and delivers the answer at the hole; the shift's body runs inside
  the delimiter put back, at its own `V`, answering `R`. This is the
  real thing: `reset(1 + shift(_ => "a"))` answers a String, and
  `shift(k => k(1) >>= k >>= (n => "n=" + n))` answers `"n=3"`. Stage
  3's pair had NO answer-type modification: it was McBride's state
  reading, realised only by `Perform`; with `Perform` gone it was
  phantom (found by the first ATM test written for it).
- `Prompt[H, S]`: identity by allocation, the row `H`, and the answer
  `S` the body is written at — the latter for INFERENCE ONLY (an
  expected type does not reach a receiver, so a shift cannot learn the
  answer at its hole from its context; a lexical `In[S]` was tried and
  fails the same way, the given instantiating to `Any` before the body
  is typed). The machine does not tie the delimiter to `S`: after a
  shift the delimiter is put back at the shift body's `V`. A prompt
  that pinned it (Gunter–Rémy–Riecke's) loses ATM; this one does not.
  The row is what makes the prompt NECESSARY: `k` is the context's
  frames, typed at the context's row, and nothing but the delimiter's
  identity (`_: p.type`, a type test that IS `eq`) ties the shift's row
  to it — without it the compiler refuses `Captured(piece)`.
- A capture goes to the NEAREST delimiter, which must be its prompt's
  (`NotNearest` otherwise). With answer types that move, a capture
  ACROSS another delimiter needs the stack of answer types — the CPS
  hierarchy, Materzok–Biernacki — which is stage 5's `Σ`. Not in the
  basis. A capture reaching the bottom of a nested run (a handler's) is
  handed out as a head form, re-closed by a fresh `Done` at the hole's
  answer (`Cut.Gone`), so handlers live inside delimiters and a capture
  crosses a HANDLER, multi-shot (TestHandlers: 3).
- The stack carries the run's answer (`R0`) beside its initial one, so a
  delimiter popping changes the program's answer without changing the
  loop's result type; an operation handed out is re-injected at the
  run's answer (it carries none).

216 lines of kernel, `grep asInstanceOf|@unchecked`: 0. 21 tests on the
JVM, JS and Native compile. The one claim outside the kernel is the
handler's, in one function (`Distinct`'s job).

What was learned the expensive way, kept here so it is not learned
again: (1) with a data stack, the shift and its delimiter are two nodes,
and the compiler needs the proof they match — identity, `Σ`, or a cast;
(2) a nested run is not a fourth way, it removes the search, not the
proof; (3) `Perform`-style ATM and DF-style ATM are two different things
on the same pair, and only one can be the delimiter's typing; (4) a
captured continuation must be answer-polymorphic or every bind after it
is pinned to the hole's answer; (5) polymorphic function types have no
variance.

## PState on the basis (2026-10-06)

Master's `PState` is already Danvy–Filinski's state: `get: Cont[S, S => R, S => R]`, `set: Cont[S, S2 => R, S => R]`,
the state's type carried by the answer type (Asai–Kameyama). On the basis it is the same two shifts,
`okay-freer/src/test/scala/okay/freer/TestState.scala`: the answer is `St[H, S, W] = S => Freer[H, W, W, W]`
(monadic, since `k(x)` is a program, not a value), `get[S] : S [St[S], St[S]]`, `put[S](s2: S2) : Unit [St[S2], St[S]]`,
and `Bind` composes the moves: `put[Int]("x") >>= get[String]` types, `put[String](2) >>= get[Int]` does not.
No `Put[S, T]` signature, no indexed effect in the row: the "sequential typestate" IS the answer-type pair.
Found on the way: a type that appears only in a lambda parameter's type (`State[H, S1, W]`) is instantiated to
`Any` before the body is typed, so the capability carries `Prompt[H, ?]` and the run names `S1` from the body's
answer; the sugar `shift[X](p)` pins the hole's answer to the prompt's `S`, so `put` is the node `Freer.Shift`
with `k`'s type written (the prompt's `S` is the run's, not the operations').

## Stage 11, tried and not taken: no prompt, the capture to the nearest delimiter (2026-10-06)

`NotNearest` is the runtime side of the one proof the prompt gives: that the delimiter reached has the shift's row
`H`. The machine always cuts to the NEAREST delimiter; the name can only agree or disagree with it, and disagreement
is a multi-prompt program (`reset(p)(reset(q)(shift(p)…)))`, out of the basis), reported at run time. So: can the
name go? The shift's body would be written for EVERY row the delimiter may have, `k.Row >: H`, a path-dependent
row (`specs/probes/freer-min/nearest/`): `Continue[H, X, T] { type Row[+A] >: H[A]; def apply[U](x: X): Freer[Row, U, U, T] }`,
`Shift[H, T, R, X, V](f: (k: Continue[H, X, T]) => Freer[k.Row, V, R, V])`, the machine instantiating `Row := G`
at the level it meets (the GADT bound `h <: G` of the match on the node is the proof). It compiles, with no prompt,
no `Delim(_: p.type, …)`, no `NotNearest`; 3.9.0 elaborates no monomorphic lambda into a polymorphic function type
(even `val f: [A] => A => A = x => x` is refused), which is why the row is a type member and not `[G >: H] => …`.
What it costs, found by the tests: a body knows `k`'s row only as `k.Row`, so it cannot EXPORT `k` at a concrete
row. The escaped `k` is `Int => Freer[k.Row, …]`, not `Top[Pure, Int]`; and `PState`'s answer `S => Freer[H, W, W, W]`
wraps `k` in a function at the run's row `H` — `k(s).flatMap(f => f(s))` is at `k.Row + H`, and nothing says
`k.Row <: H`. That is exactly the proof the prompt's identity gives (`Row = H`), and the only proof there is:
answer types and rows are independent indices, Freer is covariant in the row, so the delimiter's row is above the
shift's, never known equal to it. The one way without a name is an invariant row, which costs `pure` fitting
anywhere and rows built by `flatMap`. So the prompt stays, as the NAME OF THE ROW; `NotNearest` is its one runtime
failure, a missing-handler kind of error, and a shift whose body does not export `k` would not need it.

## Stage 12: the prompt is the reset's context (2026-10-06)

The operator's call: a shift inherits its delimiter from the reset it is written in, through a context function.
`reset[H, S](body: Prompt[H, S] ?=> Freer[H, S, R, S])` makes the prompt and gives it to its body as the given;
`shift[X](using Prompt[H, S])(k => …)` takes it from the context. Nested resets resolve to the inner given
(`reset[Pure, Int](reset[Pure, Int](shift[Int](k => k(1)).map(_ + 10)).map(_ * 2))` is 22), so the shift's delimiter
is the nearest by construction, and a capture to the outer one is written only by naming the outer given
(`reset[Pure, Int]: p ?=> reset[Pure, Int](shift[Int](using p)(…))`), the one way to reach `NotNearest`. A fragment
with a shift declares its delimiter: `def loop(n: Int)(using Prompt[Pure, Int])`. `Prompt` has no label and no
user constructor in the sugar; the row `H` and the answer `S` are written on the reset, Danvy–Filinski's annotation
of the delimiter — a type in a context function's parameter is fixed before its body is typed, so `reset[H, S]` is
a two-stage call like `shift[X]`. `PState`: `State[H, W](using Prompt[H, ?])`, `reset[H, St[H, S1, W]](body(State()) …)`.

## Stage 13: the delimiter's row IN THE INDEX — no prompt (DONE, 2026-10-06)

The operator's call: the stack of rows in the index. Built first as the operator pictured it,
`Freer[+G, Σ <: Tuple, S, R, A]`, `Σ` empty at the top, `Reset` pushing `Lvl[H]`, `Shift` typed at `Lvl[H] *: Σ`
with `k : X => T [U, U]` under `Σ` (the delimiter is in `k`, so `k` is a program of the stack OUTSIDE it). The
kernel compiled with no prompt and no identity: the Delim's index `Lvl[H] *: Σ` meets the shift's by GADT, and
`h = H` is an equality, not a bound, because the index is invariant. The tests then measured the one thing the
tail of the stack does: it pins `k`. A shift's body runs INSIDE the delimiter put back, at `Lvl[H] *: Σ`, and
`k(1)` in it is at `Σ` — `k(1).flatMap(k(_))`, the first test, does not type, by the tail alone, which nothing in
the basis ever reads (a capture reaches the nearest delimiter and no further). The full stack belongs to
shift0/`$`, whose body runs OUTSIDE the delimiter at the same `Σ` as `k` (stage 7), and to captures across
delimiters, which are not in the basis.

So the index is the HEAD alone: `Freer[+G, D, S, R, A]`, `D = Lvl[H]` the delimiter in force, `EmptyTuple` at the
top, and `k` is polymorphic in it as it is in the answer — `[U, E] => X => Freer[H, E, U, U, T]` — because it
brings its delimiter along: a captured `k` is a program under any delimiter. `Resume[H, X, T, D, U]` at any `D`.

```
Freer[+G[+_], D, S, R, +A]
Return | Inject | Bind | Delay | Reset | Shift | Resume
Reset [H, D, S, R, U](body: Freer[H, Lvl[H], S, R, S])                                           extends Freer[H, D, U, U, R]
Shift [H, T, R, X, V](f: ([U, E] => X => Freer[H, E, U, U, T]) => Freer[H, Lvl[H], V, R, V])  extends Freer[H, Lvl[H], T, R, X]
Resume[H, X, T, D, U](x: X, k: Captured[H, X, T, ?])                                         extends Freer[H, D, U, U, T]
```

Gone: `Prompt`, its identity, `_: p.type`, `NotNearest`, the prompt's `S`. `Cut.Gone` builds the handed-out
program inside `cut`, where the `Done` match knows the run's types, so the `=:=` field is gone too. What stays for
inference only: `In[D, S]`, the delimiter and answer a body is written at, a given from `reset`, read by `shift`;
the machine never sees it. The handler test: a capture through a handler inside a delimiter is at the delimiter's
row, which the index says (`Lvl[D]`), and the handler asks `D[A] <: G[A]` — the capture passes with NO claim; the
one `@unchecked` left is `Distinct`'s, on the operation. 228 lines of kernel (77 + 151), 25 tests on the JVM, JS
and Native compile, `grep asInstanceOf|@unchecked|eq|Prompt|NotNearest` over the kernel: 0.

A program with no capture is written for any `D` (`def one[D]: Freer[Fx, D, …]`): the index is exact, so a `val`
at the top is no body for a delimiter. A fragment with a shift declares its delimiter twice, as its context and as
its index: `def loop(n: Int)(using In[Lvl[Pure], Int]): Freer[Pure, Lvl[Pure], Int, Int, Int]`.

## Stage 14: THE STACK OF ROWS (DONE, 2026-10-06)

The operator's call: the stack of rows is what is needed. Built, with what it forces, each by a failure:

```
Freer[+G[+_], Σ <: Tuple, S, R, +A]             -- Σ the delimiters in force, each by its row, the nearest first
Return | Inject | Bind | Delay | Reset | Shift0 | Resume
Reset [H, Σ, S, R, U](body: Freer[H, Lvl[H] *: Σ, S, R, S])                                     extends Freer[H, Σ, U, U, R]
Shift0[H, Σ, T, R, X](f: (k: Continue[H, Σ, X, T]) => Freer[H, Σ, k.Out, k.Out, R])          extends Freer[H, Lvl[H] *: Σ, T, R, X]
Resume[H, X, T, Σ, U](x: X, k: Captured[H, X, T, ?, Σ])                                       extends Freer[H, Σ, U, U, T]
trait Continue[H, Σ, X, T] { type Out; def apply[U](x: X): Freer[H, Σ, U, U, T] }
```

- `k` is the piece WITH its delimiter, so it is a program under `Σ`, the stack outside — and the index is exact,
  so it is a program there only. A shift body that used `k` inside the delimiter put back (Danvy–Filinski's shift)
  would run `k`'s frames one level deeper than their type: `k(1).flatMap(k(_))` did not type, by the tail alone.
  So the body runs OUTSIDE the delimiter, under `Σ`, in its place: shift0. Everything the basis asked for is the
  same — `k` twice, multi-shot, answer-type modification at the nearest delimiter (`reset(1 + shift0(_ => "a"))`
  is `"a"`), abort, `PState` (its `k` is used outside, in the function answer), the escaped `k`, 100 000 captures,
  a handler inside a delimiter crossed by a capture. Lost: a second shift to the SAME delimiter from a body after
  it used `k`; such a body is written with `reset` around the part that needs it.
- The body runs at the answer of the context it replaces, which the node does not know (the reset is at any `U`):
  the answer is the abstract member `k.Out`, the body is diagonal at it, so polymorphic by construction — no
  polymorphic lambda (3.9.0 elaborates none). `Cut.Found` is built in `found`, where the delimiter's `U` is a
  named type, and carries the next program; `Cut.Gone` likewise, where `Done` names the run's types.
- Answer-type modification is the nearest level's: the outer levels are diagonal through a shift0 body (`k.Out`,
  `k.Out`). The CPS hierarchy — a pair per level, two stacks `Σin`, `Σout` whose heads today's `S`, `R` are — would
  type a `k` that moves outer levels, but its outer movement is the piece's, which the node cannot name; not in
  the basis.
- `In[Σ, Ss]`: the stack in force and, beside it, the answers the bodies are written at; `reset` gives its body
  `In[Lvl[H] *: Σ, S *: Ss]`, `shift0` reads the head and gives its own body `In[Σ, Ss]` (the body is written inside
  the delimiter and runs outside it). Inference only; the machine never sees it.
- A program with no capture is written for any stack (`def one[Σ <: Tuple]`); a fragment with a shift declares its
  delimiter as its context and its index.

Gone from the kernel: `Prompt`, identity, `_: p.type`, `NotNearest`, the prompt's `S`, the `=:=` field. 25 tests on
the JVM, JS and Native compile, `grep asInstanceOf|@unchecked|eq|Prompt|NotNearest` over the kernel: 0.

On the operator's question whether `E <: Tuple` alone would do, without `S`, `R`: answer-type modification is a
pair by nature, `(A => S) => R`, and `Bind` cancels the middle. The pair of the nearest level could sit in the head
entry, `Lvl[H, S, R]`, at the cost of `Bind` taking the head apart and a second rule at the empty stack; the
information is the same. Two tuples, in and out, with `S`, `R` their heads, are the full hierarchy. The basis is the
one pair beside the stack.

## Stage 15: the stack stays; simplified (DONE, 2026-10-06)

- `Cut` (an enum, `Found` | `Gone`, matched in `go`) is one class `Step`: the machine's next state WITH its program
  (`c`, `k`, `m`, `sub`), built by `cut` where the level's types are names — the shift's body at the delimiter's
  level, or at the run's bottom the capture handed out, as a program over `End`/`Done`: the head-form rule of `go`
  returns it as it is, so `Gone` is no case of anything.
- `Next` (a value due: `k`, `m`, `sub`) IS the data of `Resumption`; one class, a function of the value, built by
  `link`; `Inject` hands out with it.
- The identity `Widen[h, G]` built twice in `go` is `Widen.sub[H <: G, G]`, the compiler's knowledge made a value.
- Aliases for what a user writes: `Top[G, A]` (no delimiter in force), `Under[H, S]` (the context of a fragment for
  the body of a top `reset[H, S]`), `Body[H, S, A]` (a program in that body): `def loop(n: Int)(using Under[Pure,
  Int]): Body[Pure, Int, Int]`.

Kernel 268 lines (94 + 174), 25 tests on the JVM, JS and Native compile, no cast, no warning.

## Stage 16: THE INDEX ONE STACK — the pair in the head (DONE, 2026-10-06)

The operator's call: everything in the stack. `Freer[+G, Σ <: NonEmptyTuple, +A]`, an entry `Lvl[H, S, R]` a level's
row and answer pair, the run a level too (`Top[G, A] = Freer[G, Lvl[Pure, A, A] *: EmptyTuple, A]`), `Bind` composing
the head's pairs as states: `m` from `T` to `R`, `k` from `S` to `T`, the whole from `S` to `R`.

```
Return[H, R, Σ, A](a)                                          extends Freer[Pure, Lvl[H, R, R] *: Σ, A]
Inject[G, H, T, Σ, A](op)                                      extends Freer[G, Lvl[H, T, T] *: Σ, A]
Bind[G, H, Σ, S, T, R, A, B](m: Freer[G, Lvl[H, T, R] *: Σ, A], k: A => Freer[G, Lvl[H, S, T] *: Σ, B]) extends Freer[G, Lvl[H, S, R] *: Σ, B]
Reset[H, H2, Σ, S, R, U](body: Freer[H, Lvl[H, S, R] *: Lvl[H2, U, U] *: Σ, S])                          extends Freer[H, Lvl[H2, U, U] *: Σ, R]
Shift0[H, H2, Σ, T, R, X, U](f: (X => Freer[H, Lvl[H2, U, U] *: Σ, T]) => Freer[H, Lvl[H2, U, U] *: Σ, R])
                                                               extends Freer[H, Lvl[H, T, R] *: Lvl[H2, U, U] *: Σ, X]
Resume[H, X, T, H2, Σ, U](x, k)                                extends Freer[H, Lvl[H2, U, U] *: Σ, T]
```

What it bought: three parameters; `k` a plain function (`Continue` and `k.Out` gone: the answer of the level outside
is the second entry); `Return` and `Inject` diagonal by their head, as they must be (a node claiming a move it does
not make would be believed by `reset`). What it cost, each by a failure:
- Every node and every machine type matches the head, `Lvl[h, s, r] *: σ`; a typed pattern must NAME every argument
  (`Reset[h, h2, σ, s, rr, u]`), a `?` leaves the GADT without the equality, and an unused name is bound by an
  ascription.
- A program with no capture is polymorphic in three things, `def one[H, R, Σ]`, not one.
- THE LEVEL OUTSIDE IS IN THE INDEX OF EVERY NODE OF A BODY, and it is not lexical: the reset's `H2`, `U`, `Σ`
  come from the reset's context. So the context given carries them as ABSTRACT MEMBERS (`in.H2`, `in.U`, `in.Σi`,
  `in.Out`), the body is written at them, nothing is inferred inside, and `reset` makes them its own parameters
  when it builds the given — the trick of `k.Out`, for the whole level. A fragment's index is the context's:
  `def loop(n: Int)(using in: Under[Pure, Int]): in.Body[Pure, Int, Int]`.
- Hence an escaped `k` is a program at `in.Out`, and is run where `in` is; it cannot be exported as `Top[Pure, Int]`
  from inside the body (stage 15's `k`, polymorphic in the answer outside, could).
- `PState` names the level outside concretely (`Outer = Lvl[Pure, W, W] *: EmptyTuple`) and uses the raw nodes.

Two stacks (`Σin`, `Σout`, the CPS hierarchy, answer-type modification across delimiters), the operator's next
thought, measured on paper: `k : X => T [Σd, Σ1]` moves the levels outside as the piece's frames do — `Σ1`, at the
hole, the node knows; `Σd`, at the delimiter, it does not, it is the context's. Either `k` is required diagonal
outside, and the machine can prove it only if `Frames` and `Bind` are diagonal outside by construction — which IS
this stage — or `Σd` is taken lexically and written into the node, which the machine cannot check: a prompt again.
So two stacks collapse to one for the basis.

Kernel 285 lines, 25 tests on the JVM, JS and Native compile, no cast, no warning.

## Stage 17: TWO STACKS — THE HIERARCHY (DONE, 2026-10-06)

The operator's thought: Atkey's `S` and `R` become two stacks, one each. Right, and what makes it typeable for
NODES (stage 16's objection was wrong) is a LEVEL CONSTANT in the entry: `At[H, D, S]` — the delimiter's row, the
stacks OUTSIDE its delimiter `D` (where its continuation lives, so where a capture to it is a program), and the
answer at this point. `D` is constant along a level by construction, as the row is, so a shift node knows it from
its own head, and `k : X => T [D, I]` is typed by the node alone; the body, running outside, is `[D, O]` and may
MOVE the levels outside — answer-type modification across delimiters, the CPS hierarchy.

```
Freer[+G[+_], I <: Tuple, O <: Tuple, +A]       -- (A => I) => O, a level each
Return[Σ, A](a)                                          extends Freer[Pure, Σ, Σ, A]
Inject[G, Σ, A](op)                                      extends Freer[G, Σ, Σ, A]
Bind[G, I, T, O, A, B](m: Freer[G, T, O, A], k: A => Freer[G, I, T, B]) extends Freer[G, I, O, B]
Reset[H, D, O, S, R](body: Freer[H, At[H, D, S] *: D, At[H, D, R] *: O, S]) extends Freer[H, D, O, R]
Shift0[H, D, I, O, T, R, X](f: (X => Freer[H, D, I, T]) => Freer[H, D, O, R]) extends Freer[H, At[H, D, T] *: I, At[H, D, R] *: O, X]
Resume[H, D, I, X, T](x, k)                              extends Freer[H, D, I, T]
```

`Return`, `Inject`, `Bind`, `Delay`, `Frames`, `Run`, `Piece` look at no head at all: the stacks compose as
tuples. Only the three nodes that move a level, and `Delim`, match `At[h, d, s] *: σ`. The machine is the same
loop; `cut`'s `Delim` match gives the node's `D` from the delimiter (`At[h, d, …]`), and `Captured(piece) : X =>
Freer[h, d, i, t]` is exactly `f`'s parameter. Measured: `reset_2(reset_1(shift0_1(_ => shift0_2(_ => "a")) + 10)
* 2)` answers `"a"` — the inner shift's body, outside the inner delimiter, shifts to the outer and moves ITS answer
from Int to String; and the same with `k` resumed inside and `k2` twice outside answers 44. With the nodes AND with
the sugar.

The sugar, to write the hierarchy: the context is STRUCTURAL. `In[H, S, Oc]` is the context of a body: the
delimiter's row and answer, `D` the stacks outside it, `Here = At[H, D, S] *: D` the stacks at this position (what a
reset written here has outside), `outer: Oc` the context outside by its own type. A nested reset's `D` is the
outer's `Here`, down to the top, so inside a body the levels outside are known types, not abstract members (stage
16's `in.D` could not be moved: a move is written in the structure of the outside). The top is named: `top[A](…)`
— a polymorphic given for the run's answer is instantiated to `Any` by the search before any expected type reaches
it, and a type in a context function's parameter is fixed before its body is typed, so nothing but a name could
say it (`value[Int](…)` in the tests). `Under[H, S] = In[H, S, ?]`, `in.Body[A]`; no lexical stack at all: the
outer context is `outer`, with its type. `shift0`'s `k` leaves the outside as it is, `D` to `D` (a piece that
moved it could not be resumed twice); the body may move it, `[D, O]`.

Kernel 280 lines, 28 tests on the JVM, JS and Native compile, no cast, no warning, no prompt.

## Stage 18: the top is empty; the machine's three control rules one shape (DONE, 2026-10-06)

- The top is `EmptyTuple`: `Top[G, A] = Freer[G, EmptyTuple, EmptyTuple, A]` — Danvy–Filinski's `⟨e⟩ : τ` has nothing
  outside, and there is nothing to name. `top[A]`, `Root[A]`, `Run[A]` are gone; `value(reset(…))` is plain. The
  context is `Ctx { type Here }`: `Root` with `Here = EmptyTuple`, a global given, and `In[H, S, Oc <: Ctx]` the
  body of a delimiter, `Here = At[H, D, S] *: D` with `D = o.Here` for the outer context `o`, down to the top. A
  reset takes `using o: Ctx`. `reset[Fx, Int](one)` with `one[Σ]` polymorphic now infers: `o.Here` is a type.
- The three rules that move a level are one shape: `val n = enter | cut | resume(…); go(n.c, n.k, n.m, n.sub)` —
  each helper typed by its node, where the types are names; no explicit type-argument lists in `go`.
  `Captured.under` is `Machine.resume`. The hand-out check in `go` is any `Bind` whose rest is a `Resumption` at a
  run's bottom (the `Inject | Shift0` test was redundant); it must stay in `go`, not `run`: a capture handed out at
  the bottom re-enters the loop once, and a boolean cannot give the GADT what `sub(c)` needs.

Kernel 276 lines, 28 tests on the JVM, JS and Native compile, no cast, no warning, no prompt, no named run.

## Stage 19: HANDLERS AS DELIMITERS (DONE, 2026-10-06)

The operator's call. A handler is a delimiter, an operation a capture to it, the clause the shift's body, running
outside; deep: `k` brings the delimiter, so the handler is in force through a resumption, and a clause may resume
more than once. What it took, and what it found:

- THE ROW OUTSIDE is in the level: `At[H, Hf, D, S]` — the body's row `H`, what the delimiter leaves `Hf`, the stacks
  outside `D`, the answer. `Reset[H, Hf, …] extends Freer[Hf, D, O, R]`: the discharge of an effect is `H = E + G`,
  `Hf = G`. A shift's body and its `k` are at `Hf` (they run outside). `enter` builds `Widen[hf, G]` from the GADT
  bound of the node's row in the level's, as before.
- `Inject` IS GONE, six nodes: with a delimiter that discharges `E`, an operation handed out past it to the run's loop
  would need `Widen[E + G, F]`, which the compiler refused to build in `enter` — the types found that the two
  models cannot coexist. Every effect goes through a delimiter now; the run's loop handles nothing; `Machine.run`
  answers `Return(z)` or a capture whose delimiter is outside the run. With it went `sub: Widen[G, F]` on every
  level, `Widen.andThen` and `refl`: a capture reaching the run's bottom is at the run's row by the `Done` match.
- `Handler[E, G, A, Ans]`: `ret` and the clauses, `apply[X, Oc <: Ctx](using o: Oc)(op: E[X], k: X => Freer[G,
  o.Here, o.Here, Ans]): Freer[G, o.Here, o.Here, Ans]` — written outside the delimiter, in the context outside,
  at its stacks. `handle(h)(body)` is `Reset[E + G, G, …](body.map(h.ret))` with the context `Handling[E, …]` holding
  `h`. A handler's state with replay across resumptions is the answer type's business (`PState`, TestState), not a
  closure's: `k` holds the handler, and a resumption cannot swap it.
- `perform(op)`: the handler is FOUND IN THE CONTEXT AT COMPILE TIME — `Perform[E, H, C]` for the context `C`:
  `direct` when `C` is the handler's (`Handling[E, …]`), `forward` when it is another delimiter's, whose row
  outside includes the outer body's (`H2 <: Hf`): a shift to it whose body performs outside and resumes `k` after —
  `Shift0(k1 => o(op, c.outer).flatMap(k1))`. Static evidence passing, by the structure of the context; the
  delimiters between forward. No handler, no program (a compile error, tested). Inference of a higher-kinded
  parameter from a BOUND on another parameter instantiates it to `Nothing` before the bound is checked (twice met):
  the handler is typed by the context's member `c.Out`, and `forward`'s bound names only what it uses.
- Measured: a reader (`Number` is `n`), a writer (the lines said, in front of the value), a reader outside a writer
  with the inner forwarding, `(List("41", "82"), 42)`; a reader outside a plain `reset` forwarding through it, with
  a capture to the reset in the same body, 12; nondeterminism (`Flip`, both ways, `List(a)`), four worlds; a
  capture out THROUGH a handler to a reset by hand, `Perform.forward`'s law, the reset resumed twice, 66; 100 000
  operations in constant stack.

Kernel 300 lines, 30 tests on the JVM, JS and Native compile, no cast, no `@unchecked` anywhere — the `Distinct`
claim of a handler over a union is gone with the loop: a handler never dispatches an operation by class, the
delimiter it is the clause of is the one its operations reach.

## Stage 20: THE BASIS AGAINST MASTER — JMH (DONE, 2026-10-06)

`okay-freer/src/jmh/scala/okay/freer/FreerBenchmark.scala`: master's HandlerBenchmark and DelimDepthBenchmark
workloads on handlers that are delimiters. One lane per `Jmh/run`, each side from the same worktree (the branch
rebased onto master for it), 2 forks × 5 iterations:

| lane (same workload both sides)                           | master                | okay-freer            | freer / master |
|-----------------------------------------------------------|-----------------------|-----------------------|---------------:|
| handlePrebuilt — 10 000 ops, 1 % handled, 99 % forwarded   | 133.2 ± 2.5 µs        | 347.6 ± 36.5 µs       | 2.6×           |
| handleForward — the same, built each call                  | 156.2 ± 0.4 µs        | 363.9 ± 2.8 µs        | 2.3×           |
| buildOnly — the 10 000-node tree alone                     | 28.8 ± 0.1 µs         | 28.5 ± 1.6 µs         | 1.0×           |
| tailcallChain — 10 000 mutual tail calls                   | 26.4 ± 0.1 µs         | 22.0 ± 0.7 µs         | 0.83×          |
| state — 1 000 × get, set (master `statePara`, PState)       | 41.7 ± 1.3 µs         | 46.1 ± 0.6 µs         | 1.1×           |
| state — 1 000 × get, set (master `stateEffect`, the handler)| 18.4 ± 0.3 µs         | 46.1 ± 0.6 µs         | 2.5×           |
| delimCaptureDepth — 16 binds, k once                       | 244 ± 1 ns            | 203 ± 2 ns            | 0.83×          |
| delimCaptureDepth — 256 binds, k 8 times                   | 11 275 ± 708 ns       | 10 856 ± 126 ns       | 0.96×          |

What the numbers say:
- THE MACHINE is on par with master or ahead: construction equal, a capture through binds and its resumptions
  0.83–0.96×, the deferred-call loop 0.83×, the state-passing answer 1.1× of master's own PState. The index in the
  type costs nothing at run time, as it must — it is erased.
- THE HANDLER PATH is 2.6× — and that is the price of "every operation is a capture": each of the 10 000 operations
  cuts the stack to its delimiter, builds a `Piece`, a `Captured`, a `Resume`, and pushes a `Delim` back when the
  clause resumes; the 9 900 forwarded ones do it twice (to the inner delimiter, then to the outer). Master's loop
  answers a tail-resumptive operation by applying `k` in place, no capture. The remedy is known (Koka, Effekt):
  a clause that resumes exactly once as its last act needs no capture — the machine can answer it in place, the
  delimiter untouched; the handler would declare it (an `answer: op => value` clause beside the general one), and
  `perform` for it would be a node that is not a capture. Not in this stage; it is the next measurement.
- Master's own `BuildShapeBenchmark.scala` does not compile on master (5 errors, `Free.Bind`/`Free.Mapped` gone from
  `Free.scala`; `Test/compile` does not reach Jmh sources, AGENTS.md): set aside, uncommitted, for the master lanes.
  A dotty 3.9.0 assertion (`wildApprox failed to remove uninstantiated G`, implicit search) on a `?` for the outer
  context in a fragment's using-parameter: the fragment is polymorphic in it and `handle` takes explicit type
  arguments.

## Stage 21: it is `Cont`; the rows joined in `Bind`; `Delay` measured (DONE, 2026-10-06)

- The operator: this monad is no longer `Freer` but `Cont`. Right — without a node for an operation there is no
  functor it is free over: it is the monad of delimited continuations, `shift0` and `reset` as nodes, typed by the
  two stacks. The enum is `Cont` (`okay-freer/src/main/scala/okay/freer/Cont.scala`); the module and the package
  keep their names for now, the operator's call.
- The operator's `Bind`: `Bind[F, G, I, T, O, A, B](m: Cont[F, T, O, A], k: A => Cont[G, I, T, B]) extends
  Cont[F + G, I, O, B]` — the rows joined IN THE NODE, each side its own, instead of one row widened by covariance.
  The machine is unchanged (its level row is abstract, and `F + G <: G` is what the bind rule needs); `flatMap` is
  `Bind(this, f)` with nothing widened. Measured: `handlePrebuilt` 343.0 ± 7.7 µs against 347.6 before — nothing.
  One cost: dotty's reachability holds a `Bind` pattern unreachable on a scrutinee at the row `Pure` (`Pure + Pure`
  is `Pure`, but not to that check); the one test that matches a head form on a `Pure` program ascribes the
  scrutinee at any row. Nobody else matches `Bind` at a concrete row: handlers are delimiters, the machine's row is
  abstract.
- `Delay`, asked again: derivable, `Bind(Return(()), _ => t)`, the lambda is the laziness. Removed and measured:
  `tailcallChain` 61.2 ± 0.6 µs against 22.0 ± 0.7 with the node — 2.8×, three steps of the machine and two nodes
  where one. Kept, with the number on it.

Kernel 308 lines, six nodes, 30 tests on the JVM, JS and Native compile.

## Stage 22: `okay-cont`; TAIL-RESUMPTIVE HANDLERS ANSWER IN PLACE (DONE, 2026-10-06)

- The operator: the module is `okay-cont`, the package `okay.cont`. Renamed (`okayCont` in build.sbt, `TestCont`,
  `ContBenchmark`). The JMH generator's cache survives a rename and fails with `NoClassDefFoundError` on the old
  name: `rm -rf okay-cont/.jvm/target`, as AGENTS.md says.
- THE HANDLER OPTIMISATION, Koka's and Effekt's: a clause that resumes once, last, with a value of the operation
  alone needs no continuation — `Answering[E, G, A, Ans]` declares `value(op): X`, its `apply` is `k(value(op))`.
  `handle` of an `Answering` gives its body the context `Answers[E, …]`, and `perform` in such a context — or in any
  context INSIDE it, through any delimiters between — is `Delay(() => Return(value(op)))`: no capture, no
  delimiter touched, no forwarding capture; the value when the machine gets there, in its order. Chosen AT COMPILE
  TIME: `Perform.answered` (the object, high priority) needs `Answered[E, C]`, found through the context's types
  (`here` on an `Answers` context, `outside` through the outer); where it is not found the low-priority `direct`
  and `forward` stand, unchanged. First tried at run time — a `Delay` and an `Option` check on every `perform` —
  and measured on the general path: 394.9 ± 9.9 µs against 343.0, 15 % for a path that does not use it; withdrawn.
- Measured, the 10 000 operations, 1 % handled by the inner handler, 99 % forwarded to the outer, prebuilt:

  | handlers                       | master (`handlePrebuilt`) | okay-cont            | ratio |
  |--------------------------------|---------------------------|----------------------|------:|
  | general clauses, `k(a)`        | 133.2 ± 2.5 µs            | 338.1 ± 4.5 µs       | 2.5×  |
  | `Answering`, `value(op) = a`   | 133.2 ± 2.5 µs            | 136.4 ± 1.7 µs       | 1.02× |

  Parity with master's handler loop, for the handlers that are tail-resumptive — which the benchmark's, and most,
  are. The general clause keeps its capture, and its price.

30 tests on the JVM, JS and Native compile, no cast, no warning.

## Stage 23: the capture path trimmed; STATE answering in place (DONE, 2026-10-06)

The operator: do it. Of the three named, two; the third stated for what it is.

- ONE CAPTURE THROUGH THE DELIMITERS BETWEEN — not in this machine's types. The level constants (`H`, `Hf`, `D` of
  each level) are preserved by every frame by construction, but `Frames[G, A, B, I, O]` does not say so, and at the
  second delimiter crossed the one found is not tied to the node's claim; the proof needs either a cast or a
  machine whose frames carry the head apart (stage 16's shape, at every frame). The forwarding by one capture a
  level is what the types support; a general clause through `n` delimiters pays `n` captures. Not done.
- THE CAPTURE PATH, one allocation less: `Frames` IS a piece (`Piece` a sealed trait, `Frames` extends it, `Over` a
  case class over the next segment) — no `Hole` wrapper. `link` matches the enum's cases by product pattern (a typed
  pattern on `Frames[G, A0, A, I, O]` is unchecked at run time, E092). Measured: `handlePrebuilt`, general clauses,
  309.5 ± 3 µs against 338.1 — 8.5 %, 2.3× master now. `Step` stays: it is the typed pair of existentials a walk
  answers with, and a scratch cell could not re-pair them without a cast.
- STATE ANSWERING IN PLACE: `State[S, +A]` (`Get`, `Put`), `StateCell` an `Answering` handler with the state in a
  cell of its own, one per `handle`; `state(s0)(body)` answers the last state with the value. `get`/`put` are
  answered where performed, no capture. A resumption shares the cell: a body resumed twice sees ONE state, the
  second resumption the first's last — tested, `(2, List(0, 1))` under `every` — not a replay; the replay is the
  answer type's (`PState`, TestState). Measured, 1 000 × get, put: 22.1 ± 0.4 µs against master's `stateEffect`
  18.4 ± 0.3 (1.2×) and the state-passing answer's 46.1 (2.1× faster than it).
- A name clash to know: `org.openjdk.jmh.annotations.*` has a `State`; in a benchmark `okay.cont.State` is imported
  under another name.

32 tests on the JVM, JS and Native compile, no cast, no warning.

## Stage 24: GENERAL CLAUSES, FASTER WITHOUT CROSSING (DONE, 2026-10-06)

The operator: how, without a capture through the delimiters? Four things, each measured on `handlePrebuilt`, the
10 000 operations with general clauses, 1 % handled by the inner handler, 99 % forwarded (master 133.2 ± 2.5 µs):

| step                                                                        | µs/op           | of master |
|-----------------------------------------------------------------------------|-----------------|----------:|
| stage 23                                                                    | 309.5 ± 3.0     | 2.3×      |
| `Widen` gone (the bound `Hf <: G` on `Delim`'s type parameter carries what `enter` knew; the clause's body goes on outside by covariance), one `Step` per resume (`under` answers with `Return(x)` directly), `flatMap(k)` in `forward` | 292.5 ± 5 | 2.2× |
| `Op`, the diagonal shift (`T = R`, `I = O`: an operation moves nothing), and THE TAIL RESUMPTION SEEN: a clause whose whole body is `k(x)` is answered under the delimiter as it stands — `under(x, piece, d)`, the same `Delim`, no `Resume` through the loop, nothing built | 261.5 ± 1.7 | 2.0× |

- The tail resumption is seen by identity: `Captured.apply(x)` keeps the `Resume` it made, and `tail(body)` is
  `body eq` that one — then `x` is read FROM THE RESUME, typed by the capture (no cast; a race could only make the
  check fail, never pass wrongly). It types only on the diagonal node: on `Shift0`, `T` and `R` are two parameters
  and `under(x, piece, d)` would need them equal; `Op` has them equal, and every `perform` is an `Op`. Master's
  loop does the same dynamically ("forwarding keeps the states per resumption; Halt drops the inside").
- What is left is the shape: a forwarded general clause is two captures — to the inner delimiter, whose body
  performs outside and resumes `k1` after (a `Bind`, not a tail `k(x)`), and to the outer; the outer's resumption
  is now in place, the inner's is a `Resume` through the loop with a new `Delim`. One capture for both is the
  crossing (stage 23, not typeable here). The next smaller thing: `Op` without a closure, the operation and the
  handler as fields (one allocation of about thirteen a forwarded operation).

Seven nodes. 32 tests on the JVM, JS and Native compile, no cast, no warning.

## Stage 25: ONE CAPTURE THROUGH THE DELIMITERS BETWEEN (DONE, 2026-10-06)

The operator asked again, and the proof was there: not on a level's IN index, which its frames move freely, but on
its OUT index, `O` of `Stack`, which is one along the level — `Run` keeps it — and to which the next delimiter's
record is tied: `At[H2, Hf2, D2, R2] *: O2 = D1`. And `D1` the node knows structurally, from the context. So an
operation can name how far its handler is, and the machine can cross the delimiters between, proving each one is
the one named. Stage 23's "not typeable" looked at the wrong index.

- `Reach[N, Hfn, Dn, Ansn]`: from the index `N` the operation is performed at, `Here` (the nearest delimiter is the
  handler's: its row left, stacks outside and answer are `N`'s head's) or `Out(next)` (the nearest's outside IS
  the next level's index, the reach goes on from there). `Op[N, X, Hfn, Dn, Ansn](at: Reach[N, …], f)` is the
  operation, at the row `Pure`: the index places it, the row of its level is the index's head, and so it is a
  program of any run it is handed out of (the hand-out at a run's bottom is `Bind(Op(at, f), rest)` — the reach
  left, from the run's level).
- The machine's `cutN` walks the reach in lockstep with the stack: at a delimiter with `Here` the clause; with
  `Out` the delimiter is CROSSED, its record into the piece (`Crossed(inner, out)`: the inner level's piece to the
  delimiter's value, and the delimiter's own frames outside), the walk going on at `d.rest`, whose out index the
  reach names as the next level's. `under` and `link` put a `Crossed` back as a `Delim` over the level outside.
  The tail resumption is now answered at the HOLE'S own `(k, m)`, kept through the walk — nothing re-linked at
  all, at any depth. A dotty note: the recursive call across a level must go through a helper typed by the
  delimiter (`crossed(d)[X](piece)`): inference of the piece's new index through the GADT aliases fails inline.
- `Reaches[E, C]` finds the handler through the context's types and builds the reach and the clause from the
  context VALUE (`target(c)`, dependent: the levels' indices are paths); `here` for the handler's own context,
  `out` for one level further. No row bound any more: the clause runs at the handler's outside, nothing of an
  inner level runs anywhere else. `Perform` chooses `answered` (in place), else `reaches` (one capture).
- Measured, `handlePrebuilt`, general clauses, 99 % forwarded through a delimiter: 184.4 ± 3.8 µs, from 261.5 —
  1.4× master's 133.2 (was 2.0×). Three handlers with a multi-shot one inside, `Ask` crossing two delimiters,
  `Say` one: every world, every line, in order (TestCont).

What this shows, for the next stage: an operation's effect is checked by the CONTEXT CHAIN alone — `perform` needs
a `Handling[E, …]` reachable through the context's types — and the `Op` node is at `Pure`. The row `G` of a program
no longer says what it performs; the rows `H`, `Hf` in a level's entry are consulted by nothing but `handle`'s own
annotation. Rows may leave the monad: `Cont[I, O, A]`, the index the handlers in force, as the operator said at
the start — two stacks.

Seven nodes, 33 tests on the JVM, JS and Native compile, no cast, no warning.

## Stage 26: ROWS OUT OF THE MONAD — `Cont[I, O, A]` (DONE, 2026-10-06)

The operator's "yes". With operations typed by the context (stage 25: `perform` needs a `Handling[E, …]` reachable
through the context's types, and `Op` is at `Pure`), the row said nothing any more. Gone: the row parameter of
`Cont`, `Pure`, `+`, the rows `H`, `Hf` of a level's entry (`At[D, S]`: the stacks outside the delimiter and the
answer), the row parameters of `Frames`, `Stack`, `Piece`, `Captured`, the row bound of `Delim` (nothing to widen
any more), `Handler[E, G, A, Ans]`'s `G`, `In[H, Hf, S, Oc]`'s rows, `Reach`'s rows, `Top[G, A]`'s `G`, the
`Row`/`Out` members. The kernel compiled on the first try, 399 lines from 458; the numbers are the same within
noise (`handlePrebuilt` 186.6 ± 6, answering 139.0 ± 1.6, state 22.2 ± 0.5).

```
Cont[I <: Tuple, O <: Tuple, +A]                 -- (A => I) => O: two stacks of answer types, a level each
At[D <: Tuple, S]                                -- a level: the stacks outside its delimiter, its answer
Return | Bind | Delay | Reset | Shift0 | Op | Resume
Reset [D, O, S, R](body: Cont[At[D, S] *: D, At[D, R] *: O, S])                       extends Cont[D, O, R]
Shift0[D, I, O, T, R, X](f: (X => Cont[D, I, T]) => Cont[D, O, R])                    extends Cont[At[D, T] *: I, At[D, R] *: O, X]
Op    [N, X, Dn, Ansn](at: Reach[N, Dn, Ansn], f: (X => Cont[Dn, Dn, Ansn]) => Cont[Dn, Dn, Ansn]) extends Cont[N, N, X]
Resume[D, I, X, T](x: X, k: Captured[D, I, X, T, ?])                                  extends Cont[D, I, T]
Top[A] = Cont[EmptyTuple, EmptyTuple, A]
```

What a program may do is now what its context reaches: a fragment declares its effects as the `Perform[E, c.type]`
it needs — `def loop(n: Int, acc: Int)(using c: In[Int, Root.type], p: Perform[Ask, c.type]): c.Body[Int]` —
evidence, not a union; the handlers in force ARE the effect row, ordered, and the index is their stacks. A reader's
body performing `Say` is refused for having no handler of `Say` in its context (TestCont), which is what the row
test said and one test more precisely. The index is the delimiters in force, each by its stacks outside and its
answer; two delimiters alike in both are told apart by nothing but their nesting — and by the context a program is
written in, which is where its operations' handlers are, as a closure's environment is.

Seven nodes, 32 tests on the JVM, JS and Native compile, no cast, no warning, no prompt, no row.

## Stage 27: `Free[R, A]` WITH ROWS, OVER `Cont` (DONE, 2026-10-06)

The operator: "теперь, имея такую монаду Cont — как нам сделать Free[F[_],A] с рядами поверх нее?" The kernel's
programs are typed by their context (stage 26); a `Free` is a program typed by a ROW, built with no context in
sight. The answer: `Free[R, A]` is a FUNCTION of a context whose capabilities reach `R`, into `Cont` —

```
trait Free[R <: Row, +A]:
  def run(using c: Ctx, has: Has[R, c.type]): Cont[c.Here, c.Here, A]
```

and a row is a nominal list, `Ask :+: Say :+: RNil`. Three probes chose the list (ProbeRow, deleted):

- a UNION in a type lambda (`+ = [A] =>> F[A] | G[A]`) unifies by subtyping, loosely — `Member[Cnt, Ask + (Say
  + Cnt)]` was found as `Cnt + Any`, and two instances were ambiguous;
- a nominal pair `Join[+X, +Y]` makes `Member` exact, but a GADT walk over `Has` is not total for the compiler
  (three exhaustivity warnings): a pair is not a list, its tail may be anything;
- a nominal list `E :+: T` / `RNil` is two classes: every walk is total, no claim, no cast, and the join `R1 ++
  R2` is a match type that reduces as the row is known.

The parts (Free.scala, 122 lines): `Member[E, R]` a path to the effect, the compiler builds it, the first by
priority; `Cap[E, C]` a capability — `Answers(Answered)` in place or `Reaching(Reaches)` by a capture — and its
`lift` to a context one level inside, which is the kernel's own `Answered.outside` / `Reaches.out` applied by hand;
`Has[R, C]` the row's capabilities, a list of the row's shape, `at(member)` total by the two shapes being one;
`Shape[R]` with `split` IN EACH CASE, where the row is the case's own constructor and `(E :+: T) ++ R2` reduces
(a match type in a sealed trait's method does not reduce on an abstract `R`); `Sub[R1, R2]` every effect of `R1`
in `R2`, for `widen`. Then `pure: Free[RNil, A]`, `inject(op): Free[E :+: RNil, X]`, `flatMap` joining rows and
splitting the capabilities by the first row's shape, `widen`, `handle(h)(p: Free[E :+: R, A]): Free[R, Ans]` —
the body under the handler's delimiter, its context given `Cap` for `E` at `Reaches.here` (or `Answered.here` for
an `Answering`) and the rest's capabilities lifted one level in — and `top(p: Free[RNil, A]): Top[A]` at `Root`.

What the list gives: the row says what the program performs; a program's `for` joins the rows as it is built,
keeping every occurrence (`Ask :+: Ask :+: RNil` for a loop), `widen` folds them into the row a person wrote; a
handler takes the HEAD effect off, so the order of handlers is the order of the row, and `widen` reorders. A
`Free` refused: an effect not in its row (`inject` into `Free[Ask :+: RNil, …]` of a `Say`), and a run with a
handler missing (TestFree, `compileErrors`). Five tests: the row under two handlers, head first; `widen` with the
handlers the other way round; the two refusals; `every` over `Free` under a reader and a writer (every world,
every line, in order); 100 000 operations in constant stack.

Dotty, this stage: `Has[R, C]` with `C` the context's TYPE does not reach a `Handling[E, Ans, c.type]` for its
`lift` — `In[S, Oc]` is invariant in `Oc`, so `run` takes `Has[R, c.type]`, the context's singleton; a value
extending the kernel's sealed `Target` from another file is refused — `Cap.lift` applies the kernel's own givens
instead, which is the shorter answer anyway. Two recursions by the row (`Has.at`, `Has.lift`), named in the
inventory: per effect of a row the program's TYPE names.

Seven nodes in the kernel, 37 tests on the JVM, JS and Native compile, no cast, no warning.

## Stage 28: `Op` WITHOUT A CLOSURE, THE REACH MADE ONCE PER CONTEXT (DONE, 2026-10-06)

The two things named at stage 26 as left on the general path, both promised. (1) `Op` held a closure, `k =>
c.handler(using c.outer)(op, k)`, built at each operation over the operation and the handler; now it holds the
OPERATION and the handler's CLAUSE as fields — `Op[N, X, Dn, Ansn, E](at, op: E[X], clause: Clause[E, Dn, Ansn])`,
`Clause.apply[X](op, k)` one virtual call at the delimiter — and the machine passes the two down `cutN` to
`foundN`, which calls `clause(op, captured)`. (2) `Perform.reaches` built a `Target` at each operation —
`r.target(c)`: the `Reach` chain, `Out` by `Out`, and the clause — though for a context they are constant; now a
`Perform` is OF ITS CONTEXT: `trait Perform[E, C] { val c: C; def apply[X](op: E[X]): Cont[c.Here, c.Here, X] }`,
its givens take the context where they are summoned (`using c0: C`), and `reaches` keeps `val t: Target[E,
c.Here]` — reach and clause made once per summoning of the capability, which is once per fragment, not per
operation. `perform(op)(using c, p: Perform[E, c.type]) = p(op)`: dotty takes `p.c.Here` as `c.Here` when `C` is
the singleton `c.type`, so nothing is said twice. The `Target`'s `reach` and `clause` are `val`s.

Measured, the same lanes, the box at load 5–7 (noisier than the stage-26 run):

| lane | stage 26 | now | master |
|---|---|---|---|
| handlePrebuilt, general clauses | 186.6 ± 6 | 171.4 ± 2.0 | 133.2 |
| handlePrebuiltAnswering | 139.0 ± 1.6 | 133.5 ± 1.2 | 133.2 |
| handleForward | 211.5 | 200.2 ± 2.1 | 156.2 |
| tailcallChain | 22.0 | 22.6 ± 0.6 | 26.4 |
| stateAnswering | 22.2 | 22.1 ± 0.4 | 18.4 |

The general path 8 % off (1.29× master from 1.40×), answering at master's. What is left on the general path is
the capture itself: `Captured`, the piece, the `Stack.Delim` put back on resumption — one capture per operation,
which is the semantics, not an allocation to spare. The `Free` layer's `Cap.perform` still builds its target at
each operation (it has no context value at `lift`); when `Free` is measured, that is where to look.

Seven nodes, 37 tests on the JVM, JS and Native compile, no cast, no warning.

## Stage 29: THE FREER MONAD AS A MODULE BELOW THE CORE — `okay-freer` (DONE, 2026-10-06)

The operator: the freer monad out of the core, into package `okay.freer` and a module `okay-freer`; an `Effects`
instance for the machine's `Cont`; the encoding chosen at compile time, by the given — "и сделать аккуратно и
красиво", in this branch. The core was one knot: `Free.scala` named `Effects`, `Handler`, `HandleFrames`,
`Distinct`, `DirectCtx`; `Monad.scala` named nothing of the core (its mentions of `Free`, `Choose`, `Row` were
in doc strings). So the cut: the MONAD is `Freer` (the indexed tree, its `resume`, `Suspended`, `Mapped`, `defer`,
`delay`, its `ParaMonad`/`Monad`/`TailRecM` givens), `Free` (the effect tree at `Unary`, the four names, `fold`,
`loop`), the bridge `Unary`/`Diagonal` with its extractor, the direct marker `DirectCtx` (the colouring given
lives in `Freer`'s companion, where it is found with no import — so the marker is the tree's), and the type
classes they instantiate (Monad.scala, still package `okay`); the LIBRARY over it — `handleOne`, the `handle`
extensions, `run` — stays in the core at the package's top. The core names the tree at its door
(src/main/scala/Free.scala): an alias each, the companions by a stable path, so `Free.Return(a)` builds, `case
Free.Bind(Free.Inject(e), k)` matches and `Freer.Return[G, R, A]` is a type, as before — 19 files of the core
and 105 across the modules untouched; what changed is five sites of `Freer[?, ?, ?, ?]` (an alias with a
higher-kinded parameter takes no wildcard there: written at its home) and `okay-direct`'s macros, which name
the symbols by their home. `Effects.loop` is `Free.loop`, exported with the four names.

The build: `okayFreer` (JVM, JS, Native, no dependency), the core on it. The module rule "the core is
dependency-free (specs/modules-infra.md)" was `forbid("okay(JVM|JS|Native)", ".*")`; now the freer monad is the
dependency-free one and the core may depend on it alone, the kernel on either — the rule's sense (no library)
kept, its letter moved one module down, in the spec.

THE SECOND CUT, after the pre-merge gate (the same day): `Free`, `Unary`, `Diagonal` and `DirectCtx` went BACK to
the core, and the module is `Freer` with the type classes alone. The gate found what the first cut had moved
out of reach: `p.handle` and `p.run` as top-level extensions of package `okay` were lost to a selective
`import okay.{!, …}` (okay-cats, TestIOMembers) and a top-level `run` was ambiguous beside any other
wildcard-imported `run` (okay-ui, TestPWizard) — they had been found in `Freer`'s companion, the implicit scope of
every `A ! F`, with no import at all. The core's own anchor in that scope is `Diagonal`: `Unary[F]` is
`Diagonal[F]#L`, a member's projection, and a member's prefix anchors its implicit scope (checked by the build:
the two sites compile again with the doors in `object Diagonal`). So the doors — the three `handle`s, `run`,
`directColor`, the tree's `Monad` and `TailRecM` — are `Diagonal`'s companion's, and `okay-direct` names
`directColor` there. The core's files import `Freer` from its home (an explicit import beats the package alias,
and costs no getter: through the alias `Effects[Free].handle`'s loop had grown past the inlining budget,
TestInlineBudget). The alias `Freer` stays at the door for everyone else.

Checked: the core and the module on the JVM, JS and Native; the nearest core suites (TestEager, TestReflect,
TestFreerPara, TestBangLoop, TestFoldCont, TestFoldMap, TestFoldUntil) and the whole `okay-direct` suite — 500
green; the test classes of `okay-cont`, `okay-spring` and the core compile. Not run: the rest of the build's
tests, for a change that is a rename of a home; the landing's whole-build pass is theirs.

## Stage 30: THE MACHINE AS AN `Effects` ENCODING — `Prog`, `Head` (DONE, 2026-10-06)

`Effects[M]` asks five things of an encoding — `pure`, `perform`, `defer`, `flatMap`, `foldCont` — and gives the
rest by `reify`/`reflect` through `Free`; `Eager` is that shape. For the machine: `Prog[F, A]` is a program over
a signature `F` of the core's kind (a union) as a FUNCTION of how `F` is performed at a context, into `Cont`:
`def run(using d: Dispatch[F]): Cont[d.c.Here, d.c.Here, A]`, `Dispatch[F]` the one capability for the whole
row where `Perform` is one per effect. `Effects[Prog]` is in the companion: `Effects[Prog]`, `prog[Prog]`,
`summon[Effects[Prog]]` find it with no import, beside `Effects[Free]` and `Effects[Eager]`, and the encoding is
chosen where the program is run — the extension syntax rides `import okay.cont.Prog.given`, as `Eager`'s does.

Native here: `runWith(using Answers[F])` answers every operation in place at the top; `tailcall` is `Delay`;
`foldCont[S](h)` runs the program under ONE delimiter at the top, every operation a capture to it, whose clause
answers with `h(op).flatMap(x => Machine.value(k(x)))` — the captured rest is a run of its own, inside the
continuation the core's `Cont` is given, and the next operation's clause returns from it at once, so the fold is
constant-stack (100 000 operations, TestProg) and a multi-shot `h` works (every choice, in order). `handle`,
`shift`, `reset`, `foldMap` are the interface's own, through `foldCont` and `reify`: a native `handle` against
`h: F !> M[G, B]` would have to put a machine continuation, which is at ONE context, into a `Prog`, which is at
any — not typeable without a claim, so not done; the machine's own `handle` is the fast path, and `Prog` is
the bridge.

What `foldCont` needed of the machine: a VALUE out of a run. `Machine.run` answered the head form it ran to, a
`Cont`, which a match could read as a value only with a fallback arm; now it answers `Head[I, O, A]` — `Value(a)`
at stacks unchanged, or `Out(c)` a capture handed out whole for a machine outside, whose index says the stacks
have a level to go to (`At[D, R] *: O`): at the top there is none, so `Machine.value(p: Top[A]): A` is one
total arm. The proof for `Out` is the capture's node — `cut`'s `Shift0` carries it, an `Op`'s `Reach` does — read
in `out`, a method of its own: inside the loop, the nested match cost 18 % on `handlePrebuilt` (202.7 from
171.4, a loop past the inlining budget, as the core found at `resume`); off it, 172.4 ± 3.0.

Seven nodes, 42 tests on the JVM, JS and Native compile, no cast, no warning.
