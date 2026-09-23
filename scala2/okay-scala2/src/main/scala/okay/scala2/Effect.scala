package okay.scala2

import okay.{Effects, Free, TypeableK}
import okay.given
import scala.reflect.ClassTag
import Rows.Top

/**
 * A Scala 2.13 user's OWN effect (specs/scala2-facade.md, stage 3).
 *
 * okay declares an effect with `derives Effect`, which is a Scala 3
 * derivation. What a row split actually needs from it is one method,
 * `TypeableK.test(x: Any): Boolean`, and a `ClassTag` answers that
 * question. So in Scala 2 an effect is ordinary code:
 *
 * {{{
 * sealed trait Console[A] extends Op[A]
 * final case class PrintLn(s: String) extends Console[Unit]
 * case object ReadLn extends Console[String]
 * object Console extends Effect[Console]
 * }}}
 *
 * The capability in a row is `Effect[Console]`:
 * `Eff[Effect[Console] with State[Int], A]`.
 */
trait Op[+A]

/** a handler: the operation AND the rest of the program after it. Call
 * `k` once to resume, never to abort, more than once for several
 * answers. */
trait Handler[F[_], R, B] {
  def apply[X](op: F[X], k: X => Eff[R, B]): Eff[R, B]
}

abstract class Effect[F[_]](implicit tag: ClassTag[F[Any]]) { self =>

  private val test: TypeableK[Op] = new TypeableK[Op] {
    def test(x: Any): Boolean = tag.runtimeClass.isInstance(x)
  }

  /** THE SECOND CAST, and why it is right: `handle` calls this only on an
   * operation that `test` accepted, and `test` is an instance check
   * against F's own class. */
  private def narrow[X](op: Op[X]): F[X] = op.asInstanceOf[F[X]]

  /** perform one operation */
  def send[A](op: F[A] & Op[A]): Eff[Effect[F], A] = Eff.of(Rows.coerce(okay.effect[Op, A](op)))

  /** handle this effect: `ret` for the answer, `h` for each operation */
  def handle[R, A, B](e: Eff[Effect[F] & R, A])(ret: A => Eff[R, B])(h: Handler[F, R, B]): Eff[R, B] =
    Eff.of(Effects[Free].handle[Op, Top](using test)[A, B](Rows.coerce(e.program))(a => ret(a).program)(
      [X] => (op: Op[X]) =>
        okay.shift[X, Free[Top, B], Free[Top, B]](k => h(narrow(op), (x: X) => Eff.of[R, B](k(x))).program)))

  /**
   * handle the LAST effect and answer. Not a convenience: `handle` at the
   * last position leaves scalac 2.13 nothing to infer `R` from but the
   * first argument, it solves `R = Any`, and `-Xlint` reports "a type was
   * inferred to be `Any`" — an error under `-Werror`. Here there is no
   * `R` to infer (measured in the probe, 2026-09-23).
   */
  def run[A, B](e: Eff[Effect[F], A])(ret: A => Eff[Any, B])(h: Handler[F, Any, B]): B =
    Eff.run(handle[Any, A, B](e)(ret)(h))
}
