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

/** the first n elements of a stream (lives here to overload with the
 * Loop take above — toplevel overloads must share a file) */
extension [S[_], F[+_], A](s: S[A])(using Stream[S, F], Handler[F])
  def take(n: Int): LazyList[A] = s.toLazyList.take(n)
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
type Produce[A] = Id[A]

/** the freer monad over Produce: a computation that emits as it goes */
type Producer[A] = A ! Produce

/** emit a value as an effect operation */
inline def produce[A](a: A): Producer[A] = effect(a)

/**
 * The ANSWER of a produce operation is the value it produced.
 *
 * `Produce` is the identity signature, and `produce(a): Producer[A]`
 * is its only injector, so the answer type equals the element type
 * for every operation that can exist. The type system does not record
 * that — the answer is phantom on purpose — so this is the one place
 * that asserts it, and the walks over produced streams take it from
 * here rather than each writing an `asInstanceOf`.
 *
 * The twin of `okay.answer` for `Writer`, for the same reason and at
 * the same price: nothing at run time, and one place to be wrong.
 */
def produced[A](e: Any): A = e.asInstanceOf[A]

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
given Stream[Producer, okay.Pure] with
  import !.*
  import scala.annotation.tailrec

  def uncons[A](p: Producer[A]): Option[(A, Producer[A])] ! okay.Pure = pure((p.resume: @unchecked) match
    case Free.Pure(_) => None
    case Effect(e) => Some((e, Free.Pure(e)))
    case Bind(Effect(e), k) => Some((produced[A](e), k(e))))

  /** the specialized linear view: a direct walk of the freer tree —
   * no Option, no tuple per element (measured; the generic default
   * pays both). What remains per element is the stepping itself. */
  override def iterator[A](p: Producer[A])(using Handler[okay.Pure]): Iterator[A] =
    new Iterator[A]:
      private var cur: Producer[A] = p
      private var ready = false
      private var ended = false
      private var elem: A = scala.compiletime.uninitialized

      @tailrec private def advance(): Unit = cur match
        case Free.Pure(_) => ended = true
        case Effect(e) =>
          elem = e
          ready = true
          cur = Free.Pure(e)
        case Bind(Effect(e), k) =>
          elem = produced[A](e)
          ready = true
          cur = k(e)
        case _ =>
          cur = cur.resume
          advance()

      def hasNext: Boolean =
        if !ready && !ended then advance()
        ready

      def next(): A =
        if !hasNext then throw java.util.NoSuchElementException("empty producer")
        ready = false
        elem

/** a producer folds as a stream without a result (push consumption) */
given Foldable[Producer] with
  def fold[A, S](p: Producer[A])(using f: Fold[A, S]): S = Stream.fold(p)

/**
 * An EFFECTFUL producer is a stream in the effect G: the program
 * emits its elements (the identity Produce side) and performs G along
 * the way — uncons steps to the next element, carrying the performed
 * G-operations in its answer. With G = Async this is the asynchronous
 * stream: the next element may have to be awaited, and on Loom the
 * consumer's Handler[Async] just blocks a virtual thread for it. G is
 * split from the elements by its runtime class (TypeableK), so G's
 * operations must be class-distinct from the element values.
 */
given [G[+_] : TypeableK]: Stream[[A] =>> A ! Produce + G, G] with
  import !.*

  def uncons[A](p: A ! Produce + G): Option[(A, A ! Produce + G)] ! G = (p.resume: @unchecked) match
    case Free.Pure(_) => pure(None)
    case Effect(e) => <|>[G, Produce](e) match
      case Left(g) => Effect(g).map(_ => None)
      case Right(w) => pure(Some((produced[A](w), Free.Pure(produced[A](w)))))
    case Bind(Effect(e), k) => <|>[G, Produce](e) match
      case Left(g) => Effect(g).flatMap(x => uncons(k(x)))
      case Right(w) => pure(Some((produced[A](w), k(w))))

import scala.math.Numeric.Implicits.given

/** the naturals: 0, 1, 2, ... */
inline def nats[N: Numeric as N, F[_] : Put]: F[N] =
  generate(N.zero)(identity)(_ + N.one)

/** the Fibonacci numbers */
inline def fibs[N: Numeric as N, F[_] : Put]: F[N] =
  generate((N.zero, N.one))(_._1):
    (x, y) => (y, x + y)
