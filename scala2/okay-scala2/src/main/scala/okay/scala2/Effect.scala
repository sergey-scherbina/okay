package okay.scala2

import okay.{Foreign, Run, TypeableK, effect}
import okay.core.Paths
import okay.one.row.In
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
  def send[A](op: F[A] & Op[A]): Eff[Effect[F], A] = Eff.of(effect[Top, A](op))

  /** handle this effect: `ret` for the answer, `h` for each operation — the kernel's `Deep` written out, this
   * effect's operations moved to `Op` beside `Top` (`Rows.moved`), the clauses' programs at `Top`'s place outside */
  def handle[R, A, B](e: Eff[Effect[F] & R, A])(ret: A => Eff[R, B])(h: Handler[F, R, B]): Eff[R, B] =
    val p = Rows.moved[Op, A](e.program)(using Foreign.Member.of[Op](using test))
    val answer = ret
    Eff.of(new Run[Top, B]:
      def on[T <: okay.core.Row](ps: Paths[Top, T]): okay.core.Eff[T, B] =
        val here = Paths.Both(Paths.One(In.Here[Op, T]()), ps.under[Op])
        okay.one.Cont.handle(p.fit(here))(new okay.one.handler.Deep[Op, T, A, B]:
          def ret(a: A) = answer(a).program.fit(ps)
          def apply[X](op: Op[X], k: X => okay.one.![B, T]) =
            h(narrow(op), (x: X) => Eff.of[R, B](okay.Program.lift(k(x)).at[Top])).program.fit(ps)))

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
