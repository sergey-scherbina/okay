package okay2.simple

import scala.reflect.ClassTag
import okay2._
import okay2.Free.{Return, Inject, Bind}

/**
 * THE SIMPLE FORM OF A USER'S OWN EFFECT, as okay-scala2 writes it
 * (specs/okay2.md, stage 12) — OPTIONAL, beside okay's form (a `Row`
 * with an `Op` member, a `Signature`-style `Effect` instance and the four
 * handler shapes). Three names, and a Scala 2 file written for the
 * facade compiles here with its imports changed:
 *
 * {{{
 *   import okay2._
 *   import okay2.simple.{Effect, Handler, Op}
 *
 *   sealed trait Console[A] extends Op[A]
 *   final case class PrintLn(s: String) extends Console[Unit]
 *   case object ReadLn extends Console[String]
 *   object Console extends Effect[Console]
 *
 *   Console.send(ReadLn)                               // String ! Effect[Console]
 *   Console.handle(prog)(a => pure(a))(handler)         // the rest of the row
 * }}}
 *
 * The imports are SELECTIVE on purpose: okay2 has its own `Effect` and
 * `Handler` (okay's), and an explicit import outranks the wildcard, so
 * a file says which form it uses.
 *
 * What it is underneath: the effect's ROW is the class type
 * `Effect[Console]`, whose operations are recognised by the class of
 * `Console` (a `ClassTag`), and a handler is the one shape that can do
 * everything — it gets each operation with the rest of the program as
 * a plain function, and may resume it once, many times or never. That
 * is `Interpr` with the `shift` done for you; the loop is `handle`'s,
 * with no `Cont` entered: the continuation handed over is the rest of
 * the program under a `Delay`, so deep programs trampoline.
 */
trait Op[+A]

/** a handler of the effect F: each operation, and the rest of the
 * program as a function, answering in the rest of the row R */
trait Handler[F[_], R, B] {
  def apply[X](op: F[X], k: X => B ! R): B ! R
}

abstract class Effect[F[_]](implicit tag: ClassTag[F[Any]]) extends Row {
  /** the operations as the row types them: the class test below is
   * what tells them apart, so the member says nothing more */
  type Op[+A] = Any

  private val cls: Class[_] = tag.runtimeClass

  /** THE ONE CAST: an operation that passed this effect's class test
   * IS an `F[X]` at the answer the continuation expects — the only way
   * one enters the tree is `send[X](op: F[X])` */
  private def narrow[X](op: Any): F[X] = op.asInstanceOf[F[X]]

  /** perform one operation */
  def send[A](op: F[A] with okay2.simple.Op[A]): A ! Effect[F] = Inject[Effect[F], A](op)

  /** handle this effect by `h`, the values by `ret`, forwarding the rest
   * of the row R */
  def handle[R, A, B](e: Free[Effect[F] with R, A])(ret: A => B ! R)(h: Handler[F, R, B]): B ! R = {
    def loop(x: Free[Effect[F] with R, A]): B ! R = Free.resume(x) match {
      case Return(a) => ret(a)
      case Inject(op) => loop(Bind(Inject[Effect[F] with R, A](op), (v: A) => Return[Effect[F] with R, A](v)))
      case Bind(Inject(op), k) =>
        if (cls.isInstance(op)) h[Any](narrow[Any](op), (v: Any) => Free.delay(() => loop(k(v))))
        else Inject[R, Any](op).flatMap(v => loop(k(v)))
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }
    loop(e)
  }

  /** handle a program whose only effect is this one, to its answer */
  def run[A, B](e: A ! Effect[F])(ret: A => B ! Pure)(h: Handler[F, Pure, B]): B =
    Effects.run(handle[Pure, A, B](e)(ret)(h))
}
