package okay

import scala.annotation.tailrec
import scala.reflect.Typeable

infix type +[F[+_], G[+_]] = [A] =>> F[A] | G[A]

/**
 * https://okmij.org/ftp/Haskell/extensible/more.pdf
 * "Freer Monads, More Extensible Effects" Oleg Kiselyov
 */

type Eff[F[+_], A] = (A ! F)
inline def pure[F[+_], A](a: A): A ! F = Eff.pure(a)
inline def effect[F[+_], A](a: F[A]): A ! F = Eff.effect(a)

enum ![A, F[+_]] {
  case Pure(a: A)
  case Effect(x: F[A])
  case Continue[F[+_], A, X](x: X ! F,
                             k: X => A ! F)
    extends (A ! F)

  inline def flatMap[B](f: A => B ! F): B ! F = Continue(this, f)
  inline def map[B](f: A => B): B ! F = flatMap(f.andThen(Pure(_)))

  @tailrec final def foldMap[B](f: A => B)(g: [X, Y] => F[X] => X \ Y): B = this match
    case Continue(Continue(a, h), k) => a.flatMap(h(_).flatMap(k)).foldMap(f)(g)
    case Continue(Effect(e), k) => g(e)(k).foldMap(f)(g)
    case Continue(Pure(a), k) => k(a).foldMap(f)(g)
    case Effect(e) => g(e)(f)
    case Pure(a) => f(a)

  @tailrec final def unfold: Functor[F] ?=> Either[F[A ! F], A] = this match
    case Continue(Continue(a, h), k) => a.flatMap(h(_).flatMap(k)).unfold
    case Continue(Effect(e), k) => Left(e.map(k))
    case Continue(Pure(a), k) => k(a).unfold
    case Effect(e) => Left(e.map(Pure(_)))
    case Pure(a) => Right(a)

  inline def fold[B](inline f: A => B)
                    (inline g: F[A ! F] => B): Functor[F] ?=> B = unfold match
    case Left(e) => g(e)
    case Right(a) => f(a)

  private def _run[M[_] : Monad as M](h: [X] => F[X] => M[X]): M[A] = run(h)
  @tailrec final def run[M[_] : Monad as M](handler: [X] => F[X] => M[X]): M[A] = this match
    case Continue(Continue(a, h), k) => a.flatMap(h(_).flatMap(k)).run(handler)
    case Continue(Effect(e), k) => handler(e).flatMap(k(_)._run(handler))
    case Continue(Pure(a), k) => k(a).run(handler)
    case Effect(e) => handler(e)
    case Pure(x) => M.pure(x)

  import !.*

  @tailrec final def next(steps: Long = 1): Handler[F] ?=> A ! F = this match
    case a if steps <= 0 => a
    case Continue(Continue(a, h), k) => a.flatMap(h(_).flatMap(k)).next(steps - 1)
    case Continue(Effect(e), k) => handler(e)(k).next(steps - 1)
    case Continue(Pure(a), k) => k(a).next(steps - 1)
    case a => a

  @tailrec final def ? : Handler[F] ?=> ? = this match
    case Pure(a) => a
    case Effect(a) => handler(a)(identity)
    case Continue(a, _) => a.?
}

val Eff = !

object ! {
  given [F[+_]]: Monad[[A] =>> A ! F] with
    override inline def pure[A](a: A): A ! F = Pure(a)
    extension [A](a: A ! F)
      override inline def flatMap[B](f: A => B ! F): B ! F = a.flatMap(f)

  inline def pure[F[+_], A](a: A): A ! F = Pure(a)
  inline def effect[F[+_], A](e: F[A]): A ! F = Effect(e)

  @tailrec def runF[F[+_] : Comonad, A](e: A ! F): A = e.fold(identity)(a => runF(a.extract))
  inline def run[A](e: A ! Nothing): A = runF(e)

  inline def <|>[F[+_], G[+_]]: [A] => (e: F[A] | G[A]) => (Typeable[A],
    Typeable[F[A]], Typeable[G[A]]) ?=> Either[F[A], G[A]] = [A] => e => e match
    case e: F[A] => Left(e)
    case e: G[A] => Right(e)

  def handle[A, B, F[+_], G[+_]](a: A ! F + G)(f: A => B ! G)
                                (g: [X, Y] => F[X] => X \ Y): B ! G = {
    @tailrec def loop(x: A ! F + G): B ! G = x match
      case Continue(Continue(x, h), k) => loop(x.flatMap(h(_).flatMap(k)))
      case Continue(Pure(a), k) => loop(k(a))
      case Pure(a) => f(a)
      case Continue(Effect(e), k) => <|>[F, G](e) match
        case Left(e) => loop(g(e)(k))
        case Right(e) => loop(Effect(e).flatMap(k))
      case Effect(e) => <|>[F, G](e) match
        case Left(e) => g(e)(f)
        case Right(e) => Effect(e).flatMap(f)

    loop(a)
  }

  trait Handler[F[_]]:
    def apply[A, B](a: F[A]): A \ B

  given [F[_] : Comonad]: Handler[F] with
    inline def apply[A, B](a: F[A]): A \ B = _(a.extract)

  inline def handler[F[_] : Handler as H]: [A, B] => F[A] => A \ B =
    [A, B] => e => H(e)

}
