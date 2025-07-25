package okay

import scala.annotation.tailrec
import scala.reflect.Typeable
import scala.util.control.TailCalls.*

infix type +[F[+_], G[+_]] = [A] =>> F[A] | G[A]

/**
 * https://okmij.org/ftp/Haskell/extensible/more.pdf
 * "Freer Monads, More Extensible Effects" Oleg Kiselyov
 */

type Eff[F[+_], A] = (A ! F)
type !![A, F[+_]] = TailRec[A ! F]
inline def pure[F[+_], A](a: A): A ! F = Eff.pure(a)
inline def effect[F[+_], A](a: F[A]): A ! F = Eff.effect(a)
inline def effect[F[+_], A, B](a: F[A], k: A => B !! F): B ! F = Eff.effect(a, k)

enum ![A, F[+_]] {
  case Pure(a: A)
  case Effect[F[+_], A, X](x: F[X], k: X => A !! F) extends (A ! F)

  final def flatMap[B](f: A => B ! F): B ! F = this match
    case Effect(x, k) => Effect(x, x => tailcall(k(x).map(_.flatMap(f))))
    case Pure(a) => f(a)

  inline def map[B](f: A => B): B ! F = flatMap(f.andThen(Pure(_)))

  final def fold[B](f: A => B)(g: [X] => F[X] => X \ B): B = this match
    case Effect(e, k) => g(e)(x => tailcall(k(x).map(_.fold(f)(g))).result)
    case Pure(v) => f(v)

  final def foldG[B, G[+_]](f: A => B ! G)(g: [X] => F[X] => X \ (B ! G)): B ! G = this match
    case Effect(e, k) => g(e)(x => tailcall(k(x).map(_.foldG(f)(g))).result)
    case Pure(v) => f(v)

  inline final def unfoldF: Functor[F] ?=> Either[F[A ! F], A] = this match
    case Effect(a, f) => Left(a.map(f(_).result))
    case Pure(a) => Right(a)

  inline def foldF[B](inline f: A => B)
                     (inline g: F[A ! F] => B): Functor[F] ?=> B = unfoldF match
    case Left(e) => g(e)
    case Right(a) => f(a)

  import !.*

  @tailrec final def next(steps: Long = 1): Handler[F] ?=> A ! F = this match
    case Effect(a, k) if steps > 0 => handler(a)(k).result.next(steps - 1)
    case a => a

  inline def ? : Handler[F] ?=> ? = this match
    case Effect(a, _) => handler(a)(identity)
    case a => a

  def interprete[G[_] : Monad as M](handle: [X] => F[X] => G[X]): G[A] = this match
    case Pure(x) => M.pure(x)
    case Effect(x, k) => handle(x).flatMap(x => tailcall(k(x).map(_.interprete(handle))).result)
}

val Eff = !

object ! {
  inline def pure[F[+_], A](a: A): A ! F = Pure(a)
  inline def effect[F[+_], A](a: F[A]): A ! F = effect(a, x => done(pure(x)))
  inline def effect[F[+_], A, B](a: F[A], k: A => B !! F): B ! F = Effect(a, k)

  @tailrec def runF[F[+_] : Comonad, A](e: A ! F): A =
    e.foldF(identity)(a => runF(a.extract))

  inline def run[A](e: A ! Nothing): A = runF(e)

  given [F[+_]]: Monad[[A] =>> A ! F] with
    override inline def pure[A](a: A): A ! F = Pure(a)
    extension [A](a: A ! F)
      override inline def flatMap[B](f: A => B ! F): B ! F = a.flatMap(f)

  inline def <|>[F[+_], G[+_]]: [A] => (e: F[A] | G[A]) => (Typeable[A],
    Typeable[F[A]], Typeable[G[A]]) ?=> Either[F[A], G[A]] = [A] => e => e match
    case e: F[A] => Left(e)
    case e: G[A] => Right(e)

  def handle[A, B, F[+_], G[+_]](a: A ! F + G)(f: A => B)
                                (g: [X, Y] => F[X] => X \ Y): B ! G = {
    def loop(x: A ! F + G): B !! G = x match
      case !.Pure(a) => done(pure(f(a)))
      case !.Effect(x, k) => <|>[F, G](x) match
        case Left(e) => tailcall(g(e)(k(_).flatMap(loop)))
        case Right(e) => done(effect(e, x => tailcall(k(x).flatMap(loop))))

    loop(a).result
  }

  trait Handler[F[_]]:
    def apply[A, B](a: F[A]): A \ B

  given [F[_] : Comonad]: Handler[F] with
    inline def apply[A, B](a: F[A]): A \ B = _(a.extract)

  inline def handler[F[_] : Handler as H]: [A, B] => F[A] => A \ B =
    [A, B] => e => H(e)

}
