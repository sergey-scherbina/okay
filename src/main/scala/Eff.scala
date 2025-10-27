package okay

import scala.annotation.tailrec
import scala.reflect.Typeable

/**
 * https://okmij.org/ftp/Haskell/extensible/more.pdf
 * https://blog.higher-order.com/assets/trampolines.pdf
 */

infix type ![A, F[+_]] = Free[F, A]
inline def pure[F[+_], A](a: A): A ! F = Free.pure(a)
inline def effect[F[+_], A](a: F[A]): A ! F = Free.inject(a)

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

object ! {
  export Free.*
  import Free.*

  type Effect[F[+_], A] = Inject[F, A]
  val Effect = Inject

  extension [F[+_], A](self: A ! F) {

    @tailrec def resume: A ! F = self match
      case Bind(Bind(a, h), k) => a.flatMap(h(_).flatMap(k)).resume
      case Bind(Pure(a), k) => k(a).resume
      case a => a

    inline def unfoldF: Functor[F] ?=> Either[F[A ! F], A] = resume match
      case Bind(Effect(e), k) => Left(e.map(k))
      case Effect(e) => Left(e.map(Pure(_)))
      case Pure(a) => Right(a)

    inline def foldF[B](inline f: A => B)
                       (inline g: F[A ! F] => B): Functor[F] ?=> B = unfoldF match
      case Left(e) => g(e)
      case Right(a) => f(a)

    @tailrec def next(steps: Long = 1): Eval[F] ?=> A ! F = resume match
      case Bind(Effect(e), k) if steps > 0 => k(eval(e)).next(steps - 1)
      case a => a

    @tailrec def ? : Eval[F] ?=> ? = self match
      case Bind(a, _) => a.?
      case Effect(e) => eval(e)
      case Pure(a) => a
  }

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
      case Bind(Effect(e), k) => <|>[F, G](e) match
        case Left(e) => loop(g(e)(k))
        case Right(e) => loop(Effect(e).flatMap(k))
      case Effect(e) => <|>[F, G](e) match
        case Left(e) => g(e)(f)
        case Right(e) => Effect(e).flatMap(f)
      case Pure(a) => f(a)

    loop(a)
  }

}
