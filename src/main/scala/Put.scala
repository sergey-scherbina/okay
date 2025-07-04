package okay

trait Put[F[_]]:
  def put[A](a: A): A \ F[A]

inline def put[A, F[_] : Put as P](a: A): A \ F[A] = P.put(a)

def generator[A, B, F[_] : Put](a: A)(g: A => B)
                               (f: A => A): F[B] = a:
  for a <- <<[A, F[B]]; _ <- put(g(a)) yield f(a)

given Put[LazyList] with
  final override inline def put[A](a: A): A \ LazyList[A] =
    shift(a #:: _(a))

inline def stream[A, B](a: A)(g: A => B)
                       (f: A => A): LazyList[B] =
  generator[A, B, LazyList](a)(g)(f)

given Put[Producer] with
  final override inline def put[A](a: A): A \ Producer[A] =
    shift(produce(a).flatMap(_))

inline def producer[A, B](a: A)(g: A => B)
                         (f: A => A): Producer[B] =
  generator[A, B, Producer](a)(g)(f)

import scala.math.Numeric.Implicits.given

inline def num[N: Numeric as N, F[_] : Put]: F[N] = generator(N.zero)(identity)(_ + N.one)
inline def fib[N: Numeric as N, F[_] : Put]: F[N] = generator((N.zero, N.one))(_._1):
  (x, y) => (y, x + y)
