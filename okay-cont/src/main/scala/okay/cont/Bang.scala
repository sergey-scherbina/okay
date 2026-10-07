package okay.cont

/**
 * A PROGRAM OF THE MACHINE, spelled as the classic spells it: `A ! R` is `Free[R, A]`, `R` a row of effects
 * `State % Int +: Throws % String +: Pure` — the machine's `Free` (nominal rows, handlers found by the compiler)
 * under the `!` every program of okay reads as (specs/freer-min.md, stage 41). `effect` makes one of an
 * operation; `run` is a closed program's value.
 */
infix type ![A, R <: Row] = Free[R, A]

/** THE SAME ROW, WRITTEN LEFT TO RIGHT: `Pure + State % Int + Throws % String` is `Throws % String +: State % Int
 * +: Pure` — `+` adds an effect to the row on its left, and `+:` puts one in front of the row on its right; both
 * are the one list, and the order of effects in a row says nothing (`Removed`, `Union`). A polymorphic rest is
 * `F + State % S` or `State % S +: F`. Not an operator on two bare effects: that would be a tree at the effects'
 * kind, with one arm no type can close (specs/freer-min.md, stage 38) */
infix type +[R <: Row, E[+_]] = E +: R

/** fix the parameter of a binary signature: `State % Int`, `Throws % String` */
infix type %[F[_, +_], S] = [X] =>> F[S, X]

/** an operation as a program, over any row that has its effect: the row is the program's it is bound into (`Op`) */
def effect[E[+_], X](op: E[X]): Op[E, X] = Free.inject(op)

extension [A](p: A ! Pure)
  /** a program with nothing left to handle, run at the top: its value (`run` is the trait's own, at a context) */
  def value: A = Machine.value(Free.top(p))
