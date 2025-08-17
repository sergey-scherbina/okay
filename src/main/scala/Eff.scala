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

  @tailrec final def resume: A ! F = this match
    case FlatMap(FlatMap(a, h), k) => a.flatMap(h(_).flatMap(k)).resume
    case FlatMap(Pure(a), k) => k(a).resume
    case a => a

  inline def unfold: Either[A ! F, A] = resume match
    case Pure(a) => Right(a)
    case a => Left(a)

  inline def fold[B](inline f: A => B)
                    (inline g: A ! F => B): B = unfold match
    case Right(a) => f(a)
    case Left(e) => g(e)

  inline def unfoldF: Functor[F] ?=> Either[F[A ! F], A] = resume match
    case FlatMap(Effect(e), k) => Left(e.map(k))
    case Effect(e) => Left(e.map(Pure(_)))
    case Pure(a) => Right(a)

  inline def foldF[B](inline f: A => B)
                     (inline g: F[A ! F] => B): Functor[F] ?=> B = unfoldF match
    case Left(e) => g(e)
    case Right(a) => f(a)

  final def runM[M[_] : Monad as M](f: [X] => F[X] => M[X]): M[A] = resume match
    case FlatMap(Effect(e), k) => f(e).flatMap(k(_).runM(f))
    case Effect(e) => f(e)
    case Pure(x) => M.pure(x)

  import !.*

  @tailrec final def next(steps: Long = 1): Eval[F] ?=> A ! F = resume match
    case FlatMap(Effect(e), k) if steps > 0 => k(eval(e)).next(steps - 1)
    case a => a

  @tailrec final def ? : Eval[F] ?=> ? = this match
    case FlatMap(a, _) => a.?
    case Effect(e) => eval(e)
    case Pure(a) => a
}

trait Eval[F[_]]:
  def apply[A](a: F[A]): A

inline def eval[F[_] : Eval as F]: [A] => F[A] => A =
  [A] => a => F(a)

given [F[_] : Comonad]: Eval[F] with
  inline def apply[A](a: F[A]): A = a.extract

infix type +[F[+_], G[+_]] = [A] =>> F[A] | G[A]

inline def <|>[F[+_], G[+_]]: [A] => (e: F[A] | G[A]) =>
  (Typeable[F[A]], Typeable[G[A]]) ?=> Either[F[A], G[A]]
= [A] => e => e match
  case e: F[A] => Left(e)
  case e: G[A] => Right(e)

val Eff = !

object ! {
  inline def pure[F[+_], A](a: A): A ! F = Pure(a)
  inline def effect[F[+_], A](e: F[A]): A ! F = Effect(e)

  inline def run[A](e: A ! Nothing): A = runEval(e)
  @tailrec def runEval[F[+_] : {Functor, Eval}, A](e: A ! F): A =
    e.foldF(identity)(a => runEval(eval(a)))

  given [F[+_]]: Monad[[A] =>> A ! F] with
    override inline def pure[A](a: A): A ! F = Pure(a)
    extension [A](a: A ! F)
      override inline def flatMap[B](f: A => B ! F): B ! F =
        a.flatMap(f)

  given Comonad[Nothing] with
    override inline def fmap[A, B](a: Nothing, f: A => B): Nothing = a
    extension [A](a: Nothing) {
      override inline def extract: A = a
      override inline def coflatMap[B](f: Nothing => B): Nothing = a
    }

  def handle[A, B, F[+_], G[+_]](a: A ! F + G)(f: A => B ! G)
                                (g: [X, Y] => F[X] => X \ Y): B ! G = {
    @tailrec def loop(x: A ! F + G): B ! G = x.resume match
      case FlatMap(Effect(e), k) => <|>[F, G](e) match
        case Left(e) => loop(g(e)(k))
        case Right(e) => loop(Effect(e).flatMap(k))
      case Effect(e) => <|>[F, G](e) match
        case Left(e) => g(e)(f)
        case Right(e) => Effect(e).flatMap(f)
      case Pure(a) => f(a)

    loop(a)
  }

}

