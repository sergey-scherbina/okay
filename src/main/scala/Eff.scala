package okay

import scala.annotation.tailrec

/**
 * https://okmij.org/ftp/Haskell/extensible/more.pdf
 * "Freer Monads, More Extensible Effects" Oleg Kiselyov
 */

infix type +[F[_], G[_]] = [A] =>> F[A] | G[A]

type Eff[F[_], A] = (A ! F)
inline def effect[F[_], A](a: F[A]): A ! F = Eff.Effect(a)

enum ![A, F[_]] {
  case Pure(a: A)
  case Effect[F[_], A, X](x: F[X],
                          k: X => A ! F = Pure[A, F](_))
    extends (A ! F)

  final def flatMap[B](f: A => B ! F): B ! F = this match
    case Effect(x, k) => Effect(x, k(_).flatMap(f))
    case Pure(a) => f(a)

  inline def map[B](f: A => B): B ! F = flatMap(f.andThen(Pure(_)))

  inline final def unfoldF: Functor[F] ?=> Either[F[A ! F], A] = this match
    case Effect(a, f) => Left(a.map(f))
    case Pure(a) => Right(a)

  inline def foldF[B](inline f: A => B)
                     (inline g: F[A ! F] => B): Functor[F] ?=> B = unfoldF match
    case Left(e) => g(e)
    case Right(a) => f(a)

  @tailrec final def fold[B](f: A => B)(g: [X, Y] => F[X] => (X => Y) => Y): B = this match
    case Effect(e, k) => g(e)(k).fold(f)(g)
    case Pure(v) => f(v)

  import !.*

  inline def run[B](f: A => B): Handler[F] ?=> B = fold(f)(handler)

  @tailrec final def next(steps: Long = 1): Handler[F] ?=> A ! F = this match
    case Effect(a, k) if steps > 0 => handler(a)(k).next(steps - 1)
    case a => a

  inline def ? : Handler[F] ?=> ? = this match
    case Effect(a, _) => handler(a)(identity)
    case a => a

}

val Eff = !

object ! {
  inline def pure[F[_], A](a: A): A ! F = Pure(a)
  inline def effect[F[_], A, B](a: F[A], k: A => B ! F): B ! F = Effect(a, k)

  @tailrec def extract[F[_] : Comonad, A](e: A ! F): A =
    e.foldF(identity)(a => extract(a.extract))

  given [F[_]]: Monad[[A] =>> A ! F] with
    override inline def pure[A](a: A): A ! F = Pure(a)
    extension [A](a: A ! F)
      override inline def flatMap[B](f: A => B ! F): B ! F = a.flatMap(f)

  trait Handler[F[_]]:
    def apply[A, B](a: F[A], k: A => B): B

  given [F[_] : Comonad]: Handler[F] with
    inline def apply[A, B](a: F[A], k: A => B): B = k(a.extract)

  inline def handler[F[_] : Handler as H]: [A, B] => F[A] => (A => B) => B =
    [A, B] => e => k => H(e, k)
}
