package okay

import scala.annotation.tailrec
import scala.reflect.Typeable

infix type +[F[+_], G[+_]] = [A] =>> F[A] | G[A]

/**
 * https://okmij.org/ftp/Haskell/extensible/more.pdf
 * "Freer Monads, More Extensible Effects" Oleg Kiselyov
 */

type Eff[F[+_], A] = (A ! F)
inline def effect[F[+_], A](a: F[A]): A ! F = Eff.Effect(a)
inline def effect[F[+_], A, B](a: F[A], k: A => B ! F): B ! F = Eff.Effect(a, k)
inline def pure[F[+_], A](a: A): A ! F = Eff.Pure(a)

enum ![A, F[+_]] {
  case Pure(a: A)
  case Effect[F[+_], A, X](x: F[X],
                           k: X => A ! F = Pure[A, F](_))
    extends (A ! F)

  final def flatMap[B](f: A => B ! F): B ! F = this match
    case Effect(x, k) => Effect(x, k(_).flatMap(f))
    case Pure(a) => f(a)

  inline def map[B](f: A => B): B ! F = flatMap(f.andThen(Pure(_)))

  final def fold[B](f: A => B)(g: [X] => F[X] => X \ B): B = this match
    case Effect(e, k) => g(e)(k(_).fold(f)(g))
    case Pure(v) => f(v)

  final def foldG[B, G[+_]](f: A => B ! G)(g: [X] => F[X] => X \
    (B ! G)): B ! G = this match
    case Effect(e, k) => g(e)(k(_).foldG(f)(g))
    case Pure(v) => f(v)

  private def _produce_ = produce
  @tailrec final def produce(f: [X] => F[X] => X / (A ! F) /
    (A ! F, Option[A])): Producer[A] = this match
    case Effect(e, k) => f(e)(k) match
      case (x, Some(a)) => effect(a, _ => x._produce_(f))
      case (x, _) => x.produce(f)
    case Pure(v) => effect(v)

  inline final def unfoldF: Functor[F] ?=> Either[F[A ! F], A] = this match
    case Effect(a, f) => Left(a.map(f))
    case Pure(a) => Right(a)

  inline def foldF[B](inline f: A => B)
                     (inline g: F[A ! F] => B): Functor[F] ?=> B = unfoldF match
    case Left(e) => g(e)
    case Right(a) => f(a)

  import !.*

  @tailrec final def next(steps: Long = 1): Handler[F] ?=> A ! F = this match
    case Effect(a, k) if steps > 0 => handler(a)(k).next(steps - 1)
    case a => a

  inline def ? : Handler[F] ?=> ? = this match
    case Effect(a, _) => handler(a)(identity)
    case a => a

}

val Eff = !

object ! {
  inline def pure[F[+_], A](a: A): A ! F = Pure(a)
  inline def effect[F[+_], A](a: F[A]): A ! F = Effect(a)
  inline def effect[F[+_], A, B](a: F[A], k: A => B ! F): B ! F = Effect(a, k)

  inline def <|>[F[+_], G[+_]]: [A] => (e: F[A] | G[A]) => (Typeable[A],
    Typeable[F[A]], Typeable[G[A]]) ?=> Either[F[A], G[A]] = [A] => e => e match
    case e: F[A] => Left(e)
    case e: G[A] => Right(e)

  def handle[A, B, F[+_], G[+_]](f: A => B)(g: [X] => F[X] => X \ (B ! G))
                                (a: A ! F + G): B ! G = {
    def loop(x: A ! F + G): B ! G = x match
      case !.Pure(a) => pure(f(a))
      case !.Effect(x, k) => <|>[F, G](x) match
        case Left(e) => g(e)(k.andThen(loop))
        case Right(e) => effect(e, k.andThen(loop))

    loop(a)
  }

  @tailrec def extract[F[+_] : Comonad, A](e: A ! F): A =
    e.foldF(identity)(a => extract(a.extract))

  given [F[+_]]: Monad[[A] =>> A ! F] with
    override inline def pure[A](a: A): A ! F = Pure(a)
    extension [A](a: A ! F)
      override inline def flatMap[B](f: A => B ! F): B ! F = a.flatMap(f)

  trait Handler[F[_]]:
    def apply[A, B](a: F[A]): A \ B

  given [F[_] : Comonad]: Handler[F] with
    inline def apply[A, B](a: F[A]): A \ B = _(a.extract)

  inline def handler[F[_] : Handler as H]: [A, B] => F[A] => A \ B =
    [A, B] => e => H(e)

}
