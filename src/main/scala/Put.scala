package okay

trait Put[F[_]]:
  def put[A](a: A): A \ F[A]

inline def put[A, F[_] : Put as P](a: A): A \ F[A] =
  P.put(a)

def gen[A, B, F[_] : Put](a: A)(g: A => B)
                         (f: A => A): F[B] = a:
  for a <- <<[A, F[B]]; _ <- put(g(a)) yield f(a)

import scala.math.Numeric.Implicits.given

def num[N: Numeric as N, F[_] : Put]: F[N] =
  gen(N.zero)(identity)(_ + N.one)

def fib[N: Numeric as N, F[_] : Put]: F[N] =
  gen((N.zero, N.one))(_._1):
    (x, y) => (y, x + y)

given Put[LazyList] with
  override inline def put[A](a: A): A \ LazyList[A] =
    shift(a #:: _(a))

inline def lazyGen[A, B](a: A)(g: A => B)
                        (f: A => A): LazyList[B] =
  gen[A, B, LazyList](a)(g)(f)

inline def lazyNum[N: Numeric] = num[N, LazyList]
inline def lazyFib[N: Numeric] = fib[N, LazyList]
