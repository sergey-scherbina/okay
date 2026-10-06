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
