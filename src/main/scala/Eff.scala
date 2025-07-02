package okay

import scala.annotation.tailrec

infix type +[+F[_], +H[_]] = [A] =>> F[A] | H[A]

trait Handler[F[_]]:
  def apply[A, B](a: F[A]): A \ B

infix type ![A, F[_]] = Eff[A, F]
def pure[A, F[_]](a: A): A ! F = Eff.Yield(a)
def raise[A, F[_]](e: F[A]): A ! F = Eff.Raise(e)

enum Eff[A, F[_]] {
  case Yield(a: A)
  case Raise(a: F[A])
  private case Continue[A, B, F[_]](a: A ! F,
                                    f: A => B ! F)
    extends Eff[B, F]

  def flatMap[B](f: A => B ! F): B ! F = this match
    case Continue(a, g) => Continue(a, g(_).flatMap(f))
    case a => Continue(a, f)

  inline def map[B](f: A => B): B ! F = flatMap(f.andThen(Yield(_)))

  @tailrec final def run(using handler: Handler[F]): A = this match
    case Yield(a) => a
    case Raise(e) => handler(e)(identity)
    case Continue(c, f) => c match
      case Yield(a) => f(a).run
      case Raise(e) => handler(e)(f).run
      case Continue(a, g) => a.flatMap(g(_).flatMap(f)).run

  inline def handle(handler: Handler[F]): A = run(using handler)

  def end: Boolean = this match
    case Continue(_, _) => true
    case _ => false

  @tailrec final def ? : ? ! F = this match
    case a@Yield(_) => a
    case e@Raise(_) => e
    case Continue(a, _) => a match
      case Continue(a, _) => a.?
      case _ => a.?

  @tailrec final def skip(n: Long = 0)(using handler: Handler[F]): A ! F = {
    if (n == 0) this else this match
      case Continue(Continue(a, f), g) => a.flatMap(f(_).flatMap(g)).skip(n - 1)
      case Continue(Raise(e), f) => handler(e)(f).skip(n - 1)
      case Continue(Yield(a), f) => f(a).skip(n - 1)
      case _ => this
  }

}
