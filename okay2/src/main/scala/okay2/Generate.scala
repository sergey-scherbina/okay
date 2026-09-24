package okay2

import scala.annotation.tailrec
import Free.{Return, Inject, Bind}
import Split.split

/**
 * THE PRODUCER: an operation that IS its answer — okay's
 * `type Produce[A] = Id[A]`. `produce(a)` hands `a` out and answers it,
 * so a program is a stream of the values it produces, read by a fold,
 * stepped by `uncons`, or run by a Handler (`Producer.log` prints each).
 * A row needs a class to split on, so the value rides in `Emit`.
 */
sealed trait Produce extends Row { type Op[+A] = Produce.Emit[A] }

object Produce {
  final case class Emit[+A](a: A)

  implicit val effect: Effect[Produce] = Effect.of[Produce]

  /** each operation answers with its own value */
  implicit val handler: Handler[Produce] = new Handler[Produce] {
    def handle[A](a: Emit[A]): A = a.a
  }

  def produce[A](a: A): A ! Produce = Free.inject[Produce, A](Emit(a))

  /** a producer as a stream: `uncons` steps it one production at a time */
  implicit val stream: Stream[Producer, Pure] = new Stream[Producer, Pure] {
    def uncons[A](p: Producer[A]): Option[(A, Producer[A])] ! Pure = pure(Free.resume(p) match {
      case Return(_) => None
      case Inject(e) => val a = Split.only[Produce, A](e).a; Some((a, Return[Produce, A](a)))
      case Bind(Inject(e), k) => val a = Split.only[Produce, Any](e).a; Some((produced[A](a), k(a)))
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    })
  }

  /** a producer whose row also holds G: the productions arrive in G */
  def streamIn[G <: Row]: Stream[({ type L[A] = A ! (Produce + G) })#L, G] =
    new Stream[({ type L[A] = A ! (Produce + G) })#L, G] {
      def uncons[A](p: A ! (Produce + G)): Option[(A, A ! (Produce + G))] ! G = Free.resume(p) match {
        case Return(_) => pure[G, Option[(A, A ! (Produce + G))]](None)
        case Inject(e) => uncons(Bind(Inject[Produce + G, A](e), (v: A) => Return[Produce + G, A](v)))
        case Bind(Inject(e), k) =>
          split[Produce, G, Any, Option[(A, A ! (Produce + G))] ! G](e) { w =>
            pure[G, Option[(A, A ! (Produce + G))]](Some((produced[A](w.a), k(w.a))))
          } { g => Inject[G, Any](g).flatMap(x => uncons(k(x))) }
        case other => throw new IllegalStateException("resume left a non-head form: " + other)
      }
    }

  /** THE ONE CLAIM of the stream instances: the value a `produce[A]`
   * emitted IS an A — the tree typed it when `produce` built it, and
   * `Inject` holds the operation as `Any` */
  private[okay2] def produced[A](x: Any): A = x.asInstanceOf[A]
}

/** folds, stops and prints over producers — okay's `Producer` object */
object Producer {

  /** fold every production, the rest of the row forwarded */
  def fold[W, S, A, G <: Row](p: Free[Produce with G, A])(z: S)(f: (S, W) => S): (S, A) ! G = {
    def _loop(acc: S)(x: Free[Produce with G, A]): (S, A) ! G = loop(acc)(x)
    @tailrec def loop(acc: S)(x: Free[Produce with G, A]): (S, A) ! G = Free.resume(x) match {
      case Return(a) => pure[G, (S, A)]((acc, a))
      case Inject(e) => loop(acc)(Bind(Inject[Produce + G, A](e), (v: A) => Return[Produce + G, A](v)))
      case Bind(Inject(e), k) =>
        split[Produce, G, Any, Either[(S, Free[Produce with G, A]), (S, A) ! G]](e) { w =>
          Left((f(acc, Produce.produced[W](w.a)), k(w.a)))
        } { g => Right(Inject[G, Any](g).flatMap(x => _loop(acc)(k(x)))) } match {
          case Left((s2, next)) => loop(s2)(next)
          case Right(done) => done
        }
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }
    loop(z)(p)
  }

  /** a fold that STOPS: `done` is asked before each production; a G
   * operation before the stop is performed, one after it never is */
  def foldUntil[W, S, R, A, G <: Row](p: Free[Produce with G, A])(k: FoldUntil[W, S, R]): R ! G = {
    def _loop(s: S)(x: Free[Produce with G, A]): R ! G = loop(s)(x)
    @tailrec def loop(s: S)(x: Free[Produce with G, A]): R ! G =
      if (k.done(s)) pure[G, R](k.end(s))
      else Free.resume(x) match {
        case Return(_) => pure[G, R](k.end(s))
        case Inject(e) => loop(s)(Bind(Inject[Produce + G, A](e), (v: A) => Return[Produce + G, A](v)))
        case Bind(Inject(e), c) =>
          split[Produce, G, Any, Either[(S, Free[Produce with G, A]), R ! G]](e) { w =>
            Left((k.add(s, Produce.produced[W](w.a)), c(w.a)))
          } { g => Right(Inject[G, Any](g).flatMap(x => _loop(s)(c(x)))) } match {
            case Left((s2, next)) => loop(s2)(next)
            case Right(done) => done
          }
        case other => throw new IllegalStateException("resume left a non-head form: " + other)
      }
    loop(k.init)(p)
  }

  /** a producer of chunks as one Vector of their elements */
  def concat[X, G <: Row](p: Free[Produce with G, IndexedSeq[X]]): Vector[X] ! G =
    fold[IndexedSeq[X], Vector[X], IndexedSeq[X], G](p)(Vector.empty)((acc, c) => acc ++ c).map(_._1)

  /** run `f` on every production, the rest of the row forwarded */
  def each[W, A, G <: Row](p: Free[Produce with G, A])(f: W => Unit): A ! G = Free.resume(p) match {
    case Return(a) => pure[G, A](a)
    case Inject(e) => each[W, A, G](Bind(Inject[Produce + G, A](e), (v: A) => Return[Produce + G, A](v)))(f)
    case Bind(Inject(e), k) =>
      split[Produce, G, Any, A ! G](e) { w =>
        f(Produce.produced[W](w.a)); each[W, A, G](k(w.a))(f)
      } { g => Inject[G, Any](g).flatMap(x => each[W, A, G](k(x))(f)) }
    case other => throw new IllegalStateException("resume left a non-head form: " + other)
  }

  /** a Handler that prints every production and answers it */
  def log(prefix: String = "", suffix: String = "\n"): Handler[Produce] = new Handler[Produce] {
    def handle[A](a: Produce.Emit[A]): A = { print(prefix + a.a + suffix); a.a }
  }
}

/**
 * GENERATORS FROM DELIMITED CONTROL: `take` is the input of a loop
 * iteration, `put` is its output, and `generate` ties them into an
 * unfolding stream. One program, two semantics, by the `Put` instance:
 * `LazyList` materialises the stream by laziness (the continuation is
 * captured in the `#::` tail, no effect runtime at all), and `Producer`
 * by effects (each put is an operation, stepped by `uncons` or run by a
 * Handler).
 *
 * A `Loop[A, R]` is `Cont[A, R, A => R]`: the body of an open-recursive
 * function A => R whose continuation is the recursive call itself.
 */
trait Put[S[_]] {
  def put[W](w: W): Unit /> S[W]
}

object Put {
  implicit val lazyList: Put[LazyList] = new Put[LazyList] {
    def put[W](w: W): Unit /> LazyList[W] = shift[Unit, LazyList[W], LazyList[W]](k => w #:: k(()))
  }

  implicit val producer: Put[Producer] = new Put[Producer] {
    def put[W](w: W): Unit /> Producer[W] =
      shift[Unit, Producer[W], Producer[W]](k => Produce.produce(w).flatMap(_ => k(())))
  }
}

object Generate {
  /** the input of the current iteration */
  def take[A, R]: Loop[A, R] = shift[A, R, A => R](identity)

  /** tie the knot: the body's continuation is the next iteration */
  def loop[A, R](f: Loop[A, R]): A => R = {
    lazy val step: A => R = f / (step(_))
    step
  }

  /** emit one value into the stream `S` */
  def put[W, S[_]](w: W)(implicit S: Put[S]): Unit /> S[W] = S.put(w)

  /** the stream f(a), f(g(a)), f(g(g(a))), … — in whichever `S` */
  def generate[A, B, S[_]](a: A)(f: A => B)(g: A => A)(implicit P: Put[S]): S[B] =
    loop[A, S[B]](take[A, S[B]].flatMap(x => put[B, S](f(x)).map(_ => g(x))))(a)

  /** the same, as a LazyList */
  def generateLazy[A, B](a: A)(f: A => B)(g: A => A): LazyList[B] = generate[A, B, LazyList](a)(f)(g)

  /** the naturals: 0, 1, 2, … */
  def nats[N, S[_]](implicit N: Numeric[N], P: Put[S]): S[N] = generate[N, N, S](N.zero)(identity)(n => N.plus(n, N.one))

  /** the Fibonacci numbers */
  def fibs[N, S[_]](implicit N: Numeric[N], P: Put[S]): S[N] =
    generate[(N, N), N, S]((N.zero, N.one))(_._1) { case (x, y) => (y, N.plus(x, y)) }
}
