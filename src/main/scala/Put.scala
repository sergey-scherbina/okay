package okay

trait Put[F[_]]:
  def put[A](a: A): A \ F[A]

inline def put[A, F[_] : Put as F](a: A): A \ F[A] = F.put(a)

def generate[A, B, F[_] : Put](a: A)(f: A => B)
                              (g: A => A): F[B] = a:
  for a <- <<[A, F[B]]; _ <- put(f(a)) yield g(a)

inline def lazyList[A, B](a: A)(f: A => B)
                         (g: A => A): LazyList[B] =
  generate[A, B, LazyList](a)(f)(g)

given Put[LazyList] with
  final override inline def put[A](a: A): A \ LazyList[A] =
    shift(a #:: _(a))

import scala.math.Numeric.Implicits.given

inline def num[N: Numeric as N, F[_] : Put]: F[N] =
  generate(N.zero)(identity)(_ + N.one)

inline def fib[N: Numeric as N, F[_] : Put]: F[N] =
  generate((N.zero, N.one))(_._1):
    (x, y) => (y, x + y)
