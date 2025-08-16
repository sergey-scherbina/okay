package okay

import scala.annotation.tailrec
import scala.reflect.Typeable

/**
 * https://okmij.org/ftp/Haskell/extensible/more.pdf
 * "Freer Monads, More Extensible Effects" Oleg Kiselyov
 *
 * https://blog.higher-order.com/assets/trampolines.pdf
 * "Stackless Scala With Free Monads"
 * Rúnar Óli Bjarnason runarorama@gmail.com
 */

type Eff[F[+_], A] = (A ! F)
inline def pure[F[+_], A](a: A): A ! F = Eff.pure(a)
inline def effect[F[+_], A](a: F[A]): A ! F = Eff.effect(a)

enum ![A, F[+_]] {
  case Pure(a: A)
  case Effect(x: F[A])
  case FlatMap[F[+_], A, X](x: X ! F,
                            k: X => A ! F)
    extends (A ! F)

  inline def flatMap[B](f: A => B ! F): B ! F = FlatMap(this, f)
  inline def map[B](f: A => B): B ! F = flatMap(f.andThen(Pure(_)))

  @tailrec final def foldMap[B](f: A => B)(g: [X, Y] => F[X] => X \ Y): B = this match
    case FlatMap(FlatMap(a, h), k) => a.flatMap(h(_).flatMap(k)).foldMap(f)(g)
    case FlatMap(Effect(e), k) => g(e)(k).foldMap(f)(g)
    case FlatMap(Pure(a), k) => k(a).foldMap(f)(g)
    case Effect(e) => g(e)(f)
    case Pure(a) => f(a)

  @tailrec final def unfold: Functor[F] ?=> Either[F[A ! F], A] = this match
    case FlatMap(FlatMap(a, h), k) => a.flatMap(h(_).flatMap(k)).unfold
    case FlatMap(Effect(e), k) => Left(e.map(k))
    case FlatMap(Pure(a), k) => k(a).unfold
    case Effect(e) => Left(e.map(Pure(_)))
    case Pure(a) => Right(a)

  inline def fold[B](inline f: A => B)
                    (inline g: F[A ! F] => B): Functor[F] ?=> B = unfold match
    case Left(e) => g(e)
    case Right(a) => f(a)

  private def _run[M[_] : Monad as M](f: [X] => F[X] => M[X]): M[A] = run(f)
  @tailrec final def run[M[_] : Monad as M](f: [X] => F[X] => M[X]): M[A] = this match
    case FlatMap(FlatMap(a, h), k) => a.flatMap(h(_).flatMap(k)).run(f)
    case FlatMap(Effect(e), k) => f(e).flatMap(k(_)._run(f))
    case FlatMap(Pure(a), k) => k(a).run(f)
    case Effect(e) => f(e)
    case Pure(x) => M.pure(x)

  import !.*

  @tailrec final def next(steps: Long = 1): Eval[F] ?=> A ! F = this match
    case a if steps <= 0 => a
    case FlatMap(FlatMap(a, h), k) => a.flatMap(h(_).flatMap(k)).next(steps - 1)
    case FlatMap(Effect(e), k) => k(eval(e)).next(steps - 1)
    case FlatMap(Pure(a), k) => k(a).next(steps - 1)
    case a => a

  @tailrec final def ? : Eval[F] ?=> ? = this match
    case Pure(a) => a
    case Effect(e) => eval(e)
    case FlatMap(a, _) => a.?
}

trait Eval[F[_]]:
  def apply[A](a: F[A]): A

inline def eval[F[_] : Eval as F]: [A] => F[A] => A =
  [A] => a => F(a)

given [F[_] : Comonad]: Eval[F] with
  inline def apply[A](a: F[A]): A = a.extract

infix type +[F[+_], G[+_]] = [A] =>> F[A] | G[A]

val Eff = !

object ! {
  inline def pure[F[+_], A](a: A): A ! F = Pure(a)
  inline def effect[F[+_], A](e: F[A]): A ! F = Effect(e)

  given [F[+_]]: Monad[[A] =>> A ! F] with
    override inline def pure[A](a: A): A ! F = Pure(a)
    extension [A](a: A ! F)
      override inline def flatMap[B](f: A => B ! F): B ! F = a.flatMap(f)

  inline def run[A](e: A ! Nothing): A = runEval(e)
  @tailrec def runEval[F[+_] : {Functor, Eval}, A](e: A ! F): A =
    e.fold(identity)(a => runEval(eval(a)))

  inline def <|>[F[+_], G[+_]]: [A] => (e: F[A] | G[A]) =>
    (Typeable[F[A]], Typeable[G[A]]) ?=> Either[F[A], G[A]]
  = [A] => e => e match
    case e: F[A] => Left(e)
    case e: G[A] => Right(e)

  def handle[A, B, F[+_], G[+_]](a: A ! F + G)(f: A => B ! G)
                                (g: [X, Y] => F[X] => X \ Y): B ! G = {
    @tailrec def loop(x: A ! F + G): B ! G = x match
      case FlatMap(FlatMap(x, h), k) => loop(x.flatMap(h(_).flatMap(k)))
      case FlatMap(Pure(a), k) => loop(k(a))
      case Pure(a) => f(a)
      case FlatMap(Effect(e), k) => <|>[F, G](e) match
        case Left(e) => loop(g(e)(k))
        case Right(e) => loop(Effect(e).flatMap(k))
      case Effect(e) => <|>[F, G](e) match
        case Left(e) => g(e)(f)
        case Right(e) => Effect(e).flatMap(f)

    loop(a)
  }
}

