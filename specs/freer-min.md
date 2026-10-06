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
