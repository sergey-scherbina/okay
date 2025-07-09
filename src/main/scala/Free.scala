package okay

import scala.annotation.tailrec

infix type ![A, F[+_]] = Free[F, A]

inline def result[F[+_], A](a:A): A ! F = Free.Yield(a)
inline def raise[F[+_], A](e: F[A]): A ! F = Free.Raise(e)

object Free {
  inline def pure[F[+_], A](a: A): Free[F, A] = Yield(a)
  @tailrec def get[F[+_] : Comonad, A](e: A ! F): A =
    e.fold(identity)(a => get(a.extract))

  given [F[+_]]: Monad[Free[F, *]] with
    extension [A](a: Free[F, A])
      override inline def flatMap[B](f: A => Free[F, B]): Free[F, B] = a.flatMap(f)
    override inline def pure[A](a: A): Free[F, A] = Free.Yield(a)
}

/**
 * https://blog.higher-order.com/assets/trampolines.pdf
 *
 * "Stackless Scala With Free Monads"  Runar Bjarnason
 *
 */
enum Free[F[+_], +A] {
  case Yield(a: A)
  case Raise(e: F[A])
  case Continue[A, B, F[+_]](a: Free[F, A],
                             f: A => Free[F, B]) extends Free[F, B]

  final def flatMap[B](f: A => Free[F, B]): Free[F, B] = this match
    case Continue(a, g) => Continue(a, g(_).flatMap(f))
    case a => Continue(a, f)

  inline def map[B](f: A => B): Free[F, B] = flatMap(f.andThen(Yield(_)))

  inline def fold[B](inline f: A => B)
                    (inline g: F[Free[F, A]] => B): Functor[F] ?=> B =
    unfold match
      case Right(a) => f(a)
      case Left(e) => g(e)

  @tailrec final def unfold: Functor[F] ?=> Either[F[Free[F, A]], A] =
    this match
      case Yield(a) => Right(a)
      case Raise(e) => Left(e.map(Yield(_)))
      case Continue(a, f) => a match {
        case Continue(b, g) => b.flatMap(g(_).flatMap(f)).unfold
        case Yield(a) => f(a).unfold
        case Raise(e) => Left(e.map(f))
      }

  inline def pure: Boolean = this match
    case Yield(_) => true
    case _ => false

  inline def raise: Boolean = this match
    case Raise(_) => true
    case _ => false

  inline def end: Boolean = this match
    case Continue(_, _) => false
    case _ => true

}