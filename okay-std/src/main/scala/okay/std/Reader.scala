package okay.std

import okay.{!, %, Effect, Handler, effect}
import okay.core.Split
import okay.one.handler.{Answering, Handler as OneHandler}

/**
 * The Reader effect: ask for an environment of type R. The handler answers every Ask with the same value, in place —
 * the kernel's `Answering`, nothing captured.
 *
 * On the kernel's rows an operation goes to its handler by its PATH, so two readers of one type in a row are two
 * places, the nearer one answering — no misroute to forbid (okay-freer's `Distinct` and `TestRowIdentity` were the
 * union's).
 */
enum Reader[R, +A] derives Effect {
  /** read the environment */
  case Ask() extends Reader[R, R]

  /** read the environment through `f`, as ONE operation (reader-asks-op, 2026-09-27) */
  case Asks[R, A](f: R => A) extends Reader[R, A]
}

object Reader {
  /** the environment */
  def ask[R]: R ! Reader % R = effect(Ask[R, R]())

  /**
   * E HOLDS A T, and here is how to take it out — the accessor, as a typeclass, so that membership is a
   * COMPILE-TIME fact and nothing casts (reader-read, 2026-09-16). Ordered by specificity: a tuple's element, then
   * the environment as a whole, then a product's fields.
   */
  trait Has[E, T]:
    def get(e: E): T

  object Has extends HasWhole:
    /** the head IS the T */
    given head[H, T <: Tuple]: Has[H *: T, H] with
      def get(e: H *: T): H = e.head
    /** otherwise, look further down the tuple */
    given tail[H, T <: Tuple, X](using h: Has[T, X]): Has[H *: T, X] with
      def get(e: H *: T): X = h.get(e.tail)

  trait HasWhole extends HasProduct:
    /** reading the environment itself: `read[Env][Env]` is `ask` */
    given whole[E]: Has[E, E] with
      def get(e: E): E = e

  trait HasProduct:
    /** a case class environment: its FIELDS are the tuple */
    given product[E <: Product, T](using m: scala.deriving.Mirror.ProductOf[E],
                                   h: Has[m.MirroredElemTypes, T]): Has[E, T] with
      def get(e: E): T = h.get(Tuple.fromProductTyped(e)(using m))

  /** the T inside this environment: `read[Env, User]` — `ask` with a projection, one operation */
  def read[E, T](using h: Has[E, T]): T ! Reader % E = effect(Asks[E, T](h.get))

  /** a context function `E ?=> A` as a Reader program: ask once and apply */
  def lift[E, A](cf: E ?=> A): A ! Reader % E = effect(Asks[E, A](e => cf(using e)))

  /** a Reader program as a context function: run under the ambient E */
  def unlift[E, A, Es[_]](p: A ! Es)(using s: Split[Es, Reader % E]): E ?=> A ! s.Rest = run[E, A, Es](summon[E])(p)

  /** the handler as a value: `p.handle(Reader(r))`, each ask answered `r` in place */
  def apply[R](r: R): Handler[Reader % R, [A] =>> A] = new Handler[Reader % R, [A] =>> A]:
    def kernel[T <: okay.core.Row, A]: OneHandler[Reader % R, T, A, A] = new Answering[Reader % R, T, A, A]:
      def ret(a: A) = okay.one.Cont.pure(a)
      def value[X](op: Reader[R, X]): X = op match
        case Ask() => r
        case Asks(f) => f(r)

  /** `p`'s asks answered `r`, wherever Reader is among its effects */
  def run[R, A, Es[_]](r: R)(p: A ! Es)(using s: Split[Es, Reader % R]): A ! s.Rest = p.handle(Reader(r))

  /**
   * SCOPED override: `p`'s own asks answer `f(r)`, `r` the AMBIENT environment (specs/scoped-effects-laws.md),
   * asked once. An interceptor: a handler of Reader for `p` whose own ask goes to the reader outside.
   */
  def local[R, A, Es[_]](f: R => R)(p: A ! Es)(using okay.Within[Reader % R, Es]): A ! Es =
    ask[R].at[Es].flatMap(r => p.intercept(Reader(f(r))))
}
