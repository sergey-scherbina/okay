package okay

// Robert Atkey. Parameterised notions of computation. (2009)
// https://bentnib.org/paramnotions-jfp.html
// Parametrised monad M[A, S, R] represents a computation of value A
// which changes a state from S to R, i.e. it's indexed by
// an arrow S -> R in a category S of "states".
trait Monad[M[_, _, _]] {
  // identity: R -> R
  def pure[A, R](a: A): M[A, R, R]

  // composition: (S -> S2) o (S2 -> R) = S -> R
  inline def flatten[A, S, S2, R](m: M[M[A, S, S2], S2, R]): M[A, S, R] =
    m.flatMap(identity)

  extension [A, S, R](m: M[A, S, R])
    inline def map[B](f: A => B): M[B, S, R] = m.flatMap(x => pure(f(x)))
    // composition: (S -> R) o (S2 -> S) = S2 -> R
    def flatMap[B, S2](f: A => M[B, S2, S]): M[B, S2, R]
}

/*
trait Functor[F[_]]:
  extension [A](fa: F[A])
    def map[B](f: A => B): F[B]

trait Applicative[F[_]] extends Functor[F]:
  def pure[A](a: A): F[A]
  extension [A, B](f: F[A => B])
    def apply(a: F[A]): F[B]
  extension [A](fa: F[A])
    override def map[B](f: A => B): F[B] = pure(f)(fa)

trait Alternative[F[_]] extends Applicative[F]:
  def empty[A](): F[A]
  extension [A](x: F[A])
    def append(y: F[A]): F[A]

trait Selective[F[_]] extends Applicative[F]:
  extension [A, B](fe: F[Either[A, B]])
    def select(f: F[A => B]): F[B]
    def branch[C](fa: F[A => C])(fb: F[B => C]): F[C] =
      fe.map(_.map(Left(_))).select(fa.map(_.andThen(Right(_)))).select(fb)
  extension (x: F[Boolean])
    def ifS[A](t: F[A])(e: F[A]): F[A] = x.map(Either.cond(_, (), ()))
      .branch(t.map(Function.const))(e.map(Function.const))

trait Monad[F[_]] extends Selective[F]:
  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B]
    inline def >>=[B](f: A => F[B]): F[B] = flatMap(f)
    override def map[B](f: A => B): F[B] = fa.flatMap(f.andThen(pure))
  extension [A, B](f: F[A => B])
    def apply(a: F[A]): F[B] = a.flatMap(a => f(pure(a)))
  extension [A, B](fe: F[Either[A, B]])
    override def select(f: F[A => B]): F[B] =
      fe.flatMap(_.fold(a => f.map(_(a)), pure))

/**
 * Kleisli composition, is the composition of effectful functions:
 */
extension [M[_] : Monad, A, B](f: A => M[B])
  def >>>[C](g: B => M[C]): A => M[C] = f(_).flatMap(g)

trait MonadPlus[F[_]]
  extends Alternative[F], Monad[F]:
  def mzero[A](): F[A] = empty()
  extension [A](x: F[A])
    def mplus(y: F[A]) = x.append(y)

trait Comonad[F[_]] extends Functor[F]:
  extension [A](a: F[A])
    def extract(): A
    def coflatMap[B](f: F[A] => B): F[B]
*/
