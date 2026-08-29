package okay

/**
 * A Loop R means (A => R) => (A => R): the body of an open-recursive
 * function A => R, whose continuation is the recursive call itself.
 * take is the argument of the current iteration.
 * loop ties the knot, i.e. it is the fixpoint.
 */
infix type Loop[A, R] = Cont[A, R, A => R]
infix type <<[A, R] = Loop[A, R]
extension [A](a: A) inline def apply[R](f: A Loop R): R = loop(f)(a)
inline def take[A, R]: A Loop R = shift(identity)
def loop[A, R](f: A Loop R): A => R =
  lazy val step: A => R = f / (step(_))
  step

trait Put[F[_]]:
  def put[A](a: A): A /> F[A]

inline def put[A, F[_] : Put as F](a: A): A /> F[A] = F.put(a)

def generate[A, B, F[_] : Put](a: A)(f: A => B)
                              (g: A => A): F[B] = a:
  for a <- take[A, F[B]]; _ <- put(f(a)) yield g(a)

inline def lazyList[A, B](a: A)(f: A => B)
                         (g: A => A): LazyList[B] =
  generate[A, B, LazyList](a)(f)(g)

given Put[LazyList] with
  final override inline def put[A](a: A): A /> LazyList[A] =
    shift(a #:: _(a))

import scala.util.chaining.*

type Produce[A] = Pure[A]
type Producer[A] = A ! Produce
inline def produce[A](a: A): Producer[A] = effect(a)

given Put[Producer] with
  final override inline def put[A](a: A): A /> Producer[A] =
    shift(produce(a).flatMap(_))

object Producer {
  def log(prefix: String = "", suffix: String = "\n"): Handler[Produce] = new:
    inline def handle[A](a: A): A = a.tap(_.pipe(prefix + _ + suffix).tap(print))
}

import scala.math.Numeric.Implicits.given

inline def num[N: Numeric as N, F[_] : Put]: F[N] =
  generate(N.zero)(identity)(_ + N.one)

inline def fib[N: Numeric as N, F[_] : Put]: F[N] =
  generate((N.zero, N.one))(_._1):
    (x, y) => (y, x + y)
