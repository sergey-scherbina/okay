/** The two aliases that let Scala 2 write a program's type the way okay
 * does (specs/scala2-facade.md, stages 16 and 17). okay writes
 * `Int ! (State % Int + Writer % String)`; the facade's type is
 * `A ! R` with `R` an intersection of capabilities, which Scala 2
 * spells `with`. With these two it is `Int ! (State[Int] + Writer[String])`
 * — the parentheses as okay writes them, and here they are REQUIRED:
 * Scala 2 gives every infix type operator one precedence, left-assoc,
 * so `A ! R + S` is `(A ! R) + S` (SLS 2.13 §3.2.8; measured, stage
 * 17). They are declared HERE, in Scala 2 source — a user declares them
 * once in their own package object — because a Scala 3 top-level alias
 * is invisible to scalac 2.13. */
import okay.scala2.Eff

package object scala2probe {
  type +[R, S] = R with S
  type ![A, R] = Eff[R, A]

  /** `State % Int` is `State[Int]`, as in okay (stage 18). An alias may
   * TAKE a type constructor and answer a plain type; what Scala 2
   * refuses is an alias that ANSWERS one (stage 16). In a row it needs
   * its own parentheses — `(State % Int) + (Writer % String)` — for
   * the one-precedence reason above, so the rows here stay
   * `State[Int] + Writer[String]`. */
  type %[F[_], A] = F[A]
}
