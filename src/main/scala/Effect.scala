package okay

import scala.annotation.tailrec

trait Effect[F[_]]:
  def apply[A, B](effect: F[A]): A \ B

inline def handler[F[_] : Effect as h]: Effect[F] = h

type Pure[A] = A

trait Impure extends Effect[Pure]

given Impure with
  override inline def apply[A, B](a: A): A \ B = _(a)

object Impure:
  def print(suffix: String = "", prefix: String = ""): Effect[Pure] = new:
    override inline def apply[A, B](a: A): A \ B = k => {
      Predef.print(prefix + a + suffix)
      k(a)
    }

import !.*

inline def result[A, F[_]](a: A): A ! F = Yield(a)
inline def raise[A, F[_]](e: F[A]): A ! F = Raise(e)

type Producer[A] = A ! Pure
inline def produce[A](a: A): Producer[A] = raise(a)

infix enum ![A, F[_]] {
  case Yield(a: A)
  case Raise(a: F[A])

  private case Continue[A, B, F[_]](a: A ! F,
                                    f: A => B ! F)
    extends (B ! F)

  final def flatMap[B](f: A => B ! F): B ! F = this match
    case Continue(a, g) => Continue(a, g(_).flatMap(f))
    case a => Continue(a, f)

  inline final def map[B](f: A => B): B ! F =
    flatMap(f.andThen(Yield(_)))

  @tailrec final def run: Effect[F] ?=> A = this match
    case Yield(a) => a
    case Raise(e) => handler(e)(identity)
    case Continue(c, f) => c match
      case Continue(a, g) => a.flatMap(g(_).flatMap(f)).run
      case Yield(a) => f(a).run
      case Raise(e) => handler(e)(f).run

  final inline def end: Boolean = this match
    case Continue(_, _) => true
    case _ => false

  @tailrec final def ? : ? ! F = this match
    case Continue(a, _) => a match
      case Continue(a, _) => a.?
      case _ => a.?
    case a => a

  @tailrec final def next(n: Long = 1): Effect[F] ?=> A ! F =
    if (n < 1) this else this match
      case Continue(Continue(a, f), g) => a.flatMap(f(_).flatMap(g)).next(n - 1)
      case Continue(Yield(a), f) => f(a).next(n - 1)
      case Continue(Raise(e), f) => handler(e)(f).next(n - 1)
      case _ => this
}
