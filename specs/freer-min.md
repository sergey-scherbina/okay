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
