package okay

import okay.!.*
import okay.Row.at

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
   *       s"hello \${(!read[E, User]).name}"
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

  /**
   * THE ROW OF A BLOCK'S PROGRAM TYPE, recovered by the compiler:
   * `[X] =>> Free[R, X]` gives back R (reader-env, 2026-09-16).
   */
  trait RowOf[F[_]]:
    type R[+_]
  object RowOf:
    given [R0[+_]]: RowOf[[X] =>> Free[R0, X]] with
      type R[+A] = R0[A]

  /**
   * THE ENVIRONMENT OF THE READER INSIDE A ROW. Structural, like
   * `Row.In` — and it works here because the search runs at TYPER
   * time, while the row is still the alias the user wrote; by the time
   * the macro holds a row it has been beta-reduced into a union and
   * matches no `F + G` shape (measured, direct-narrow-colour).
   */
  trait EnvOf[R[+_]]:
    type E
  object EnvOf extends EnvOfDeeper:
    given here[E0, G[+_]]: EnvOf[Reader % E0 + G] with
      type E = E0
  trait EnvOfDeeper extends EnvOfOnly:
    given later[F[+_], G[+_]](using r: EnvOf[G]): EnvOf[F + G] with
      type E = r.E
  trait EnvOfOnly:
    given only[E0]: EnvOf[Reader % E0] with
      type E = E0

  /**
   * `!Reader.ask` with NO type argument, inside a `direct` block: the
   * environment comes from the BLOCK'S ROW, so a block names its
   * environment once — in its own row — and never again:
   *
   *     type Row = Writer % String + Reader % (Users, Feeds) + State % Long
   *     def f: String ! Row = direct:
   *       val (users, feeds) = !Reader.ask
   *
   * An OVERLOAD of `ask`, not a second name, because it is the same
   * operation: it expands to `ask[E]`. `ask[R]` keeps working
   * everywhere, in or out of a block; this one is chosen exactly when
   * no type argument is written and a `DirectCtx` is in scope.
   *
   * INLINE, and that is not a performance choice: the `DirectCtx` that
   * pins F is a value parameter, the macro strips the context lambda it
   * belongs to, and a reference to it surviving into the output is a
   * dangling parameter ("used outside the scope where it was defined").
   * Inlining removes the parameter at expansion and leaves `ask[E]`,
   * which is what the body was all along.
   */
  inline def ask[F[_]](using inline ctx: DirectCtx[F])
                      (using f: RowOf[F])(using e: EnvOf[f.R]): e.E ! Reader % e.E =
    ask[e.E]

  /**
   * THE BRIDGE TO CONTEXT FUNCTIONS (specs/context-functions.md,
   * ctx-reader-bridge — shipped 2026-09-23 at the operator's word,
   * "полезная штука, пусть будет"). A context function `E ?=> A` IS
   * a pure Reader program: `lift` asks once and applies. `unlift`
   * is the other way — a Reader program becomes a context function
   * that runs it under the ambient `E`, forwarding the rest of the
   * row. Named FUNCTIONS, not `Conversion`s, and that was measured
   * rather than chosen (E10, 2026-09-01): a `Conversion` from a
   * context function never fires, because the context function
   * auto-applies at the ascription site first; the other direction
   * as a SAM lambda hits an implementation restriction and the
   * `with apply` form inherits the eagerness. One line each, so the
   * library carries them rather than every call site.
   */
  def lift[E, A](cf: E ?=> A): A ! Reader % E = ask[E].map(e => cf(using e))

  /** a Reader program as a context function: run under the ambient E */
  def unlift[E, A, F[+_]](p: A ! Reader % E + F): E ?=> A ! F = run[E, A, F](summon[E])(p)

  /** answer every Ask with r, forwarding the effects F */
  def run[R, A, F[+_]](r: R)(a: A ! Reader % R + F): A ! F =
    relay[A, A, Reader % R, F](a)(pure(_)):
      [X, Y] => e => e match
        case Ask() => Cont.Pure(r)

  /**
   * SCOPED override: `p`'s own asks answer `f(r)`, `r` the AMBIENT
   * environment (specs/scoped-effects-laws.md). Asked ONCE, closed
   * over — a Reader's environment is one value for the whole run,
   * so `f` is applied once and every ask `local` claims answers the
   * same `f(r)`, not a live re-read.
   *
   * Built on `Effects.handle`, the same tool `Throws.recover` uses:
   * peel this ONE signature's own operations, forward everything
   * else unchanged. That forwarding is why `local` composes
   * correctly through a `Delim` capture made and re-invoked from
   * INSIDE `p` — `handle`'s forwarding arm wraps a forwarded
   * operation's continuation with the SAME handling loop again, and
   * that wrapping is baked into the tree it returns, not a pass that
   * finishes before anyone else looks (Kiselyov, Shan & Sabry,
   * "Delimited dynamic binding", ICFP 2006, is the paper; the trap it
   * warns about is the OTHER composition — a continuation captured
   * OUTSIDE a `local` and invoked from inside it, which sees the
   * CALLING `local` and cannot see the capturing one, since that
   * scope was never part of the data. Documented, not fixed:
   * specs/scoped-effects-laws.md, Out of scope).
   *
   * `.at` widens the handled result back into the declared row: `p`
   * no longer performs any `Reader % R` op after this (they were all
   * answered), but code AFTER `local(f)(p)` still may, and still sees
   * the ambient `r` — `local` scopes `p`, not what follows it.
   */
  def local[R, A, F[+_]](f: R => R)(p: A ! Reader % R + F): A ! Reader % R + F =
    // the GADT refinement `Ask(): Reader[R, R]` fixing X=R holds in a
    // method's match, not inside the `[X] => ...` lambda handle wants
    // (gadt-on-a-covariant-enum: the same trap SharedOnce.answer met)
    def answer[X](e: Reader[R, X], r2: R): Cont[X, Free[F, A], Free[F, A]] = e match
      case Ask() => shift(k => k(r2))
    ask[R].at[Reader % R + F].flatMap { r =>
      val r2 = f(r)
      (Effects[Free].handle[Reader % R, F](p)(a => pure[F, A](a)):
        [X] => (e: Reader[R, X]) => answer(e, r2)
      ).at[Reader % R + F]
    }
}

/** by class only: `Ask()` carries no trace of R, so a row may hold
 * ONE Reader — see TestRowIdentity and typeableK */
