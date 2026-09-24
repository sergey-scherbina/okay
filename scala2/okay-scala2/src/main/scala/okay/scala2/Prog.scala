package okay.scala2

import okay.{!, %, +, Async, Throws, async, raise}
import okay.Row.at
import okay.given
import scala.util.control.NonFatal

/**
 * A program over `Async + Throws % Throwable`, as a type a SCALA 2.13
 * compiler can read (specs/scala2-facade.md).
 *
 * The library's own programs are `A ! Row`, and a row is a union —
 * which Scala 2 cannot spell — and its combinators are `inline`, which
 * Scala 2's TASTy reader refuses to call ("Unsupported Scala 3 inline
 * method flatMap; found in class okay.Free"). This class is the same
 * program behind a signature with neither: plain methods, one type
 * parameter, no row. The row is still there — it is the type of
 * `program` — and a Scala 3 caller crosses back through `Bridge.lift` and
 * `Bridge.program`; a 2.13 caller never touches either.
 *
 * WHAT FAILS WHERE: an exception thrown inside `delay` becomes a
 * `Throws` failure, the same as `Prog.fail`, so `recover`, `attempt`
 * and `runEither` see both. An exception thrown by a function passed
 * to `map` or `flatMap` is not suspended by anything and escapes
 * `run()` as a throw — `delay` is the door for code that may throw.
 */
final class Prog[A] private[scala2] (private val body: ProgBody[A]) {

  private[scala2] def program: A ! Prog.Row = body.program

  def map[B](f: A => B): Prog[B] = Prog.of(program.map(f))

  def flatMap[B](f: A => Prog[B]): Prog[B] = Prog.of(program.flatMap(a => f(a).program))

  /** the failure as a value; the result never fails */
  def attempt: Prog[Either[Throwable, A]] =
    Prog.of(okay.runEither[A, Async, Throwable](program).at[Prog.Row])

  /** on failure, continue with `h` */
  def recover(h: Throwable => Prog[A]): Prog[A] =
    attempt.flatMap {
      case Right(a) => Prog.pure(a)
      case Left(e) => h(e)
    }

  /** run to the answer on this thread, throwing a failure (JVM) */
  def run(): A = runEither().fold(e => throw e, identity)

  /** run to the answer on this thread, a failure as a value (JVM) */
  def runEither(): Either[Throwable, A] = okay.runEither[A, Async, Throwable](program).runWith
}

/**
 * THE ROW, OUT OF THE CONSTRUCTOR. scalac 2.13 reads a class's
 * primary-constructor parameter types when it first loads the class,
 * and a type naming `okay.+` — a union — makes it refuse the class
 * outright: "Unsupported Scala 3 union in bounds of type +; found in
 * object okay.Effects$package", reported at the user's `package` line
 * before any of their code. Measured 2026-09-22 by bisection, and all
 * four spellings failed alike: a public `val`, a `private val`, a plain
 * parameter kept in a `def`, a plain parameter kept in a `val`. METHODS
 * are different — they are read only when called, so `Prog.of` and
 * `Bridge.program` may name the row freely (a public method returning
 * the row was tried against the cold 2.13 probe and compiled). Holding
 * the program in THIS class is the fix, because a reference to a class
 * is not a reading of its constructor. A value class, so `Prog` still
 * allocates one object per combinator.
 */
private[scala2] final class ProgBody[A](val program: A ! Prog.Row) extends AnyVal

object Prog {

  private[scala2] def of[A](p: A ! Row): Prog[A] = new Prog(new ProgBody(p))

  /** the row every `Prog` runs in */
  private[scala2] type Row = Async + Throws % Throwable

  def pure[A](a: A): Prog[A] = Prog.of(okay.pure(a))

  /** suspend `a`: nothing runs until the program does, and a throw
   * inside it is this program's failure */
  def delay[A](a: => A): Prog[A] =
    Prog.of(async(try Right(a) catch { case NonFatal(e) => Left(e) }).at[Row]
      .flatMap(e => fromEither(e).program))

  def fail[A](e: Throwable): Prog[A] = Prog.of(raise[Throwable, A](e).at[Row])

  def fromEither[A](e: Either[Throwable, A]): Prog[A] = e.fold(fail, pure)

  /** run in order, answers in order; stack-safe at any length */
  def sequence[A](ps: List[Prog[A]]): Prog[List[A]] =
    ps.foldLeft(pure(List.empty[A]))((acc, p) => acc.flatMap(xs => p.map(_ :: xs)))
      .map(_.reverse)
}

/**
 * The Scala 3 side of the seam, kept OUT of `Prog`'s own signature so
 * a Scala 2 compiler never has to read it: a 2.13 caller that does not
 * name `Bridge` never loads it.
 */
object Bridge {

  /** an Async program as a `Prog` */
  def lift[A](p: A ! Async): Prog[A] = Prog.of(p.at[Prog.Row])

  /** and back: the program a `Prog` stands for */
  def program[A](p: Prog[A]): A ! (Async + Throws % Throwable) = p.program
}
