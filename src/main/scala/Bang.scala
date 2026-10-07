package okay

import okay.cont.{Handler, Op}

/**
 * A PROGRAM: `A ! R` is the machine's `Free[R, A]` (okay-cont) — a program of `A` over the row `R`, `State % Int +:
 * Throws % String +: Pure`, written as every program of okay reads (specs/freer-min.md, stage 47; the classic's
 * `A ! F` is `okay.freer`'s). An operation is a program over any row that has its effect (`effect`, `op.perform`);
 * a handler takes its effect off the row wherever it is (`p.handle(h)`); a closed program has a value (`p.value`).
 */
infix type ![A, R <: Row] = okay.cont.Free[R, A]

/** THE SAME ROW, WRITTEN LEFT TO RIGHT: `Pure + State % Int + Throws % String` is `Throws % String +: State % Int
 * +: Pure` — `+` adds an effect to the row on its left, `+:` puts one in front of the row on its right; both are
 * the one list, and the order of effects in a row says nothing. A polymorphic rest is `F + State % S` or `State % S +: F` */
infix type +[R <: Row, E[+_]] = E +: R

/** fix the parameter of a binary signature: `State % Int`, `Throws % String` */
infix type %[F[_, +_], S] = [X] =>> F[S, X]

/** a value as a program, at any row */
def pure[R <: Row, A](a: A): A ! R = okay.cont.Free.pure(a)

/** an operation as a program, over any row that has its effect: the row is the program's it is bound into */
def effect[E[+_], X](op: E[X]): Op[E, X] = okay.cont.Free.inject(op)

extension [E[+_], X](op: E[X])
  /** an operation performed, postfix: `State.Get[Int]().perform` */
  def perform: Op[E, X] = effect(op)

extension [R <: Row, A](p: A ! R)
  /** the effect handled, wherever it is in the row: `p.handle(State(0))` */
  def handle[E[+_], Ans](h: Handler[E, A, Ans])(using rm: Removed[E, R]): Ans ! rm.Out = Effects.given_Effects_Free.handle(h)(p)

extension [A](p: A ! Pure)
  /** a program with nothing left to handle, run at the top: its value */
  def value: A = okay.cont.Machine.value(okay.cont.Free.top(p))
