package okay.cont

/**
 * A PROGRAM OF THE MACHINE, spelled as the classic spells it: `A ! R` is `Free[R, A]`, `R` a row of effects
 * `State % Int +: Throws % String +: Pure` — the machine's `Free` (nominal rows, handlers found by the compiler)
 * under the `!` every program of okay reads as (specs/freer-min.md, stage 41). `effect` makes one of an
 * operation; `run` is a closed program's value.
 */
infix type ![A, R <: Row] = Free[R, A]

/** fix the parameter of a binary signature: `State % Int`, `Throws % String` */
infix type %[F[_, +_], S] = [X] =>> F[S, X]

/** an operation as a program: its effect alone is the row */
def effect[E[+_], X](op: E[X]): X ! (E +: Pure) = Free.inject(op)

extension [A](p: A ! Pure)
  /** a program with nothing left to handle, run at the top: its value (`run` is the trait's own, at a context) */
  def value: A = Machine.value(Free.top(p))
