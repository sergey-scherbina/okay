package okay

import okay.!.*

/**
 * The Reader effect: ask for an environment of type R. The handler
 * answers every Ask with the same value, which makes it exactly
 * tail-resumptive — run is the relay, at relay speed.
 */
/**
 * PARAMETERISED, so the derived test is by CLASS only: the operations
 * carry no runtime trace of R, and a row may therefore hold ONE
 * Reader. Two — `Reader % Int + Reader % String` — misroute, loudly
 * (TestRowIdentity): the first handler answers both asks and the
 * second continuation gets a ClassCastException, rather than a
 * plausible wrong answer.
 */
enum Reader[R, +A] derives Effect {
  /** read the environment */
  case Ask() extends Reader[R, R]
}

object Reader {
  /** the environment */
  inline def ask[R]: R ! Reader % R = effect(Ask())

  /**
   * E HOLDS A T, and here is how to take it out — the accessor, as a
   * typeclass, so that membership is a COMPILE-TIME fact and nothing
   * casts (reader-read, 2026-09-16).
   *
   * This is the induction `HMap.Select` already does over a tuple,
   * keyed by the VALUE's type instead of by a key val: the head
   * matches at its own type, otherwise look further down. The three
   * derivations are ordered by specificity — a tuple's element wins
   * over the environment as a whole, and both win over a product's
   * fields, which a tuple would otherwise also match (a tuple IS a
   * Product).
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

  /**
   * The T inside this environment: `read[Env, User]`.
   *
   * NO new effect and no new handler — it is `ask` with a projection,
   * so the row holds one `Reader % Env`, `Reader.run` handles it, and
   * a component may declare exactly what it reads and nothing more:
   *
   *     def banner[E](using Has[E, User]): String ! Reader % E = direct:
   *       s"hello ${(!read[E, User]).name}"
   *
   * An application names its environment once and the call site keeps
   * one type argument:
   *
   *     type Env = (Users, Feeds)
   *     def read[T](using Reader.Has[Env, T]): T ! Reader % Env = Reader.read[Env, T]
   *
   * (A `[T] => Has[E, T] ?=> …` polymorphic value would have read
   * `read[Env][Users]`, and was tried: under a Direct mark its answer
   * type erases to `Any`.)
   *
   * which runs in ANY environment holding a User. The alternative —
   * `Reader % User + Reader % Feed` — does not work and is refused by
   * `Distinct`: both asks carry no value, so no runtime test can tell
   * them apart (the note above, and `Tag.Of` is the answer when two
   * readers really must be separate members).
   */
  def read[E, T](using h: Has[E, T]): T ! Reader % E = ask[E].map(h.get)

  /** answer every Ask with r, forwarding the effects F */
  def run[R, A, F[+_]](r: R)(a: A ! Reader % R + F): A ! F =
    relay[A, A, Reader % R, F](a)(pure(_)):
      [X, Y] => e => e match
        case Ask() => Cont.Pure(r)
}

/** by class only: `Ask()` carries no trace of R, so a row may hold
 * ONE Reader — see TestRowIdentity and typeableK */
