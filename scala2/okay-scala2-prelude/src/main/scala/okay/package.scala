package okay

/**
 * The TOP-LEVEL names of okay, for a Scala 2.13 program — the part of
 * `okay.scala2` that only a Scala 2 compiler can provide
 * (specs/scala2-facade.md, stage 21).
 *
 * The facade is written in Scala 3, and scalac 2.13's TASTy reader does
 * not see a Scala 3 TOP-LEVEL definition: `type +`, `type !`, `pure`,
 * the runner `!` were each something a 2.13 user had to declare in
 * their own package object, or spell differently (`Eff.run`,
 * `Eff.pure`). This module is compiled BY scalac 2.13, so its package
 * object for `okay.scala2` is an ordinary Scala 2 package object: one
 * `import okay.scala2._` brings the aliases and the top-level words
 * with everything else, exactly as `import okay2._` does on okay2 and
 * as the Scala 3 core spells them. The Scala 3 facade defines no
 * top-level members, so nothing collides with this file.
 */
package object scala2 {

  /** a row of several capabilities: `State[Int] + Writer[String]` */
  type +[R, S] = R with S

  /** a program: `Int ! (State[Int] + Writer[String])` */
  type ![A, R] = Eff[R, A]

  /** `State % Int` is `State[Int]` */
  type %[F[_], A] = F[A]

  /** the row that requires nothing — okay2's `Pure` */
  type Pure = Any

  /** a value as a program that performs nothing */
  def pure[A](a: A): A ! Pure = Eff.pure(a)

  /** one of the given alternatives (okay's top-level `choose`) */
  def choose[A](as: A*): A ! Choose = Choose.choose(as: _*)

  /** every answer of a search, the rest of the row forwarded (okay's
   * top-level `runChoice`) */
  def runChoice[A, R](e: Eff[Choose with R, A]): Seq[A] ! R = Choose.runChoice[A, R](e)

  /** the runner, spelled as okay and okay2 spell it: `!.run(p)` for a
   * program with nothing left to perform */
  object ! {
    def run[A](e: A ! Pure): A = Eff.run(e)
  }

  /** `p.runWith` for an `Async` program, as okay2's `runWith` — the
   * facade's `Eff.runAsync` */
  implicit final class AsyncRunWith[A](private val e: A ! Async) extends AnyVal {
    def runWith: A = Eff.runAsync(e)
  }
}
