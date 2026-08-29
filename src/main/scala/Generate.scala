package okay

/**
 * Generators from delimited control: take is the input of a loop
 * iteration, put is the output, and generate ties them into an
 * unfolding stream. One program — two semantics, by the Put instance:
 * LazyList materializes the stream by laziness (the continuation is
 * captured in the #:: tail, no effect runtime at all), and Producer
 * materializes it by effects (each put is an operation of the freer
 * tree, stepped by next and interpreted by a Handler, e.g.
 * Producer.log).
 */

/**
 * A Loop R means (A => R) => (A => R): the body of an open-recursive
 * function A => R, whose continuation is the recursive call itself.
 * take is the argument of the current iteration.
 * loop ties the knot, i.e. it is the fixpoint.
 */
/** the aesthetic alias of Loop */
infix type <<[A, R] = Loop[A, R]
infix type Loop[A, R] = Cont[A, R, A => R]

/** run a loop from this seed */
extension [A](a: A) inline def apply[R](f: A Loop R): R = loop(f)(a)
/** the argument of the current iteration: shift identity captures the loop context */
inline def take[A, R]: A Loop R = shift(identity)
/** tie the knot: the fixpoint of the loop body, with a memoized stepper */
inline def loop[A, R](f: A Loop R): A => R =
  lazy val step: A => R = f / (step(_))
  step

/** the interface of an F that accepts produced values */
trait Put[F[_]]:
  def put[A](a: A): A /> F[A]

/** put a value through the instance of F */
inline def put[A, F[_] : Put as F](a: A): A /> F[A] = F.put(a)

/** unfold: take the seed, put f(a), continue with the seed g(a) */
inline def generate[A, B, F[_] : Put](a: A)(f: A => B)
                                     (g: A => A): F[B] = a:
  for a <- take[A, F[B]]; _ <- put(f(a)) yield g(a)

/** generate, materialized by laziness */
inline def generateLazy[A, B](a: A)(f: A => B)
                             (g: A => A): LazyList[B] =
  generate[A, B, LazyList](a)(f)(g)

/** put captures the continuation in the lazy tail */
given Put[LazyList] with
  final override inline def put[A](a: A): A /> LazyList[A] =
    shift(a #:: _(a))

/** the identity signature: an operation is the value it produces */
type Produce[A] = Pure[A]

/** the freer monad over Produce: a computation that emits as it goes */
type Producer[A] = A ! Produce

/** emit a value as an effect operation */
inline def produce[A](a: A): Producer[A] = effect(a)

/** put suspends the value as an effect operation */
given Put[Producer] with
  final override inline def put[A](a: A): A /> Producer[A] =
    shift(produce(a).flatMap(_))

object Producer {

  import scala.util.chaining.*

  /** a Handler printing each produced value on the way through */
  def log(prefix: String = "", suffix: String = "\n"): Handler[Produce] = new:
    inline def handle[A](a: A): A = a.tap(_.pipe(prefix + _ + suffix).tap(print))

}

/**
 * A producer is a stream: it observes by stepping its next operation —
 * one op per element, on demand, no further than the observer asks
 * (the eager fronts crash here, see compare/TestLaziness). The end is
 * its Pure, observed as None. The cast is sound for producers built
 * by put/produce at one element type: Produce is the identity
 * signature, an operation IS its element, but the element types of
 * the operations are erased by the identity — the value type A is the
 * only witness left.
 */
given Stream[Producer] with
  import !.*

  def uncons[A](p: Producer[A]): Option[(A, Producer[A])] = p.resume match
    case Free.Pure(_) => None
    case Effect(e) => Some((e, Free.Pure(e)))
    case Bind(Effect(e), k) => Some((e.asInstanceOf[A], k(e)))

import scala.math.Numeric.Implicits.given

/** the naturals: 0, 1, 2, ... */
inline def nats[N: Numeric as N, F[_] : Put]: F[N] =
  generate(N.zero)(identity)(_ + N.one)

/** the Fibonacci numbers */
inline def fibs[N: Numeric as N, F[_] : Put]: F[N] =
  generate((N.zero, N.one))(_._1):
    (x, y) => (y, x + y)
