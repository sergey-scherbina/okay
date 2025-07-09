package okay

import scala.annotation.tailrec

import Free.*

infix type +[F[_], G[_]] = [A] =>> F[A] | G[A]

inline def handler[F[_] : Effect as F]: Effect[F] = F

trait Effect[F[_]]:
  def apply[A](a: F[A]): A

given [F[_] : Comonad]: Effect[F] with
  inline def apply[A](a: F[A]): A = a.extract

extension [A, F[+_]](a: A ! F)

  @tailrec def next(steps: Long = 1): Effect[F] ?=> Free[F, A] = a match
    case Continue(a, f) if steps > 0 => a match {
      case Continue(b, g) => b.flatMap(g(_).flatMap(f)).next(steps - 1)
      case Yield(a) => f(a).next(steps - 1)
      case Raise(e) => f(handler(e)).next(steps - 1)
    }
    case a => a

  def ? : Effect[F] ?=> ? = a match
    case Yield(a) => a
    case Raise(e) => handler(e)
    case Continue(a, _) => a match {
      case Continue(b, _) => b.?
      case Yield(a) => a
      case Raise(e) => handler(e)
    }
