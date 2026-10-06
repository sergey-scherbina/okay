package okay.freer

import scala.annotation.implicitNotFound

/**
 * THE EFFECT TREE: `Free[F, A]`, the freer monad at the UNARY signature `F` — `Freer[Unary[F], Unit, Unit, A]`,
 * every index `Unit` — and the four names at that arity (`Return`, `Inject`, `Bind`, `Delay`), the eliminator
 * `fold`, the loop. The bridge `Unary` and the marker `DirectCtx` are here because the tree's own companion is
 * where a given over `Free` is found with no import (`Freer.directColor`). The library over the tree — handlers,
 * rows, `Effects` — is the core's, package `okay` (module `okay`), which names these at its door.
 */
/** a unary effect as a member of an indexed row: `F[X]` on the
 * diagonal, and nothing off it — see Indexed.scala in the core */
type Unary[F[+_]] = Diagonal[F]#L

/**
 * `Unary`'s body, a projection on a class rather than a bare type lambda, for inference: two lambdas applied to a
 * row `Users + F` beta-reduce to unions, which give nothing to solve `F1` and `G` from; a projection compares by its
 * prefix (`Diagonal[Users + F]` against `Diagonal[F1 + G]`), so the row's `+` matches application to application.
 * The member reduces to `F[X]` wherever its two indexes are one type — every `A ! F`, at `Unit` — and to nothing
 * elsewhere (one-bridge: `Free`'s bridge and the indexed rows' are this one; `Lift`, which ignored the indexes,
 * is gone).
 */
sealed trait Diagonal[F[+_]]:
  type L[S, R, +X] = S match
    case R => F[X]
    case _ => Nothing

/**
 * THE BRIDGE'S EXTRACTOR (unary-extractor): a node of a unary member's operation ON THE DIAGONAL, answered at the
 * operation's own type. Its field is typed by the bridge, `Unary[F][R, R, A]` — a match type, which reduces in an
 * expression but not as the scrutinee of a nested pattern, so `case Unary(Writer.Say(c))` on the plain node would
 * refine nothing; viewed at `Unary.Op[F]` the field IS `F[A]`, and a constructor pattern on it refines `A` as a GADT
 * match does. A product match: the node itself, nothing allocated. `Free.Inject` is this at `Unit, Unit`.
 */
object Unary:
  /** `F` with its two indexes ignored: what the bridge IS on the diagonal */
  type Op[F[+_]] = [S, R, X] =>> F[X]

  def unapply[F[+_], R, A](i: Freer.Inject[Unary[F], R, R, A]): Freer.Inject[Op[F], R, R, A] =
    // THE CLAIM, the bridge's own definition read back: on the diagonal `Unary[F]` reduces to `F` — the same
    // operation, the same node, read at the signature that says so
    i.asInstanceOf[Freer.Inject[Op[F], R, R, A]]

/** Evidence installed only while a `direct` block is being compiled (okay-direct): the tree's own companion
 * colours a program value as its answer under it (`Freer.directColor`), so the marker is the tree's */
@implicitNotFound("no DirectCtx[${F}]: auto-coloring works only INSIDE a direct block.\nWrap the code in direct[F] { ... } — or use the explicit marks (.reflect / .? / !prog),\nwhich need no capability.")
final class DirectCtx[F[_]] private[okay] ()

/** the effect program: the base at `Unary[F]`, every index `Unit`. `object Free` keeps the four names
 * (`Return`, `Inject`, `Bind`, `Delay`) at the arities the match sites and the `direct` macro use */
type Free[F[+_], +A] = Freer[Unary[F], Unit, Unit, A]

object Free {

  /**
   * `tailRecM` for programs (specs/fold-until.md): run `f` from `s`, continue from a `Left`, answer a `Right`.
   * Stack-safe with no trampoline of its own: the recursive call sits inside the `flatMap`'s continuation. The
   * core's `!.loop` is this, exported.
   */
  def loop[S, A, F[+_]](s: S)(f: S => Free[F, Either[S, A]]): Free[F, A] =
    f(s).flatMap {
      case Left(next) => loop(next)(f)
      case Right(a) => Return(a)
    }

  /** a value as a tree */
  inline def pure[F[+_], A](a: A): Free[F, A] = Freer.Return(a)

  /** an operation as a tree */
  inline def inject[F[+_], A](a: F[A]): Free[F, A] = Freer.Inject[Unary[F], Unit, Unit, A](a)

  /** `Freer.defer` at the effect tree's indexes */
  def defer[F[+_], A, B](thunk: () => Free[F, A])(f: A => Free[F, B]): Free[F, B] =
    Freer.defer(thunk)(f)

  /** `Freer.delay` at the effect tree's indexes */
  def delay[F[+_], A](thunk: () => Free[F, A]): Free[F, A] = Freer.Delay(thunk)

  /** the eliminator: `p` interprets values, `h` operations with their continuations, over the head form
   * `resume` leaves */
  def fold[F[+_], A, B](m: Free[F, A])(p: A => B)
                                     (h: [X] => F[X] => (X => Free[F, A]) => B): B =
    (m.resume: @unchecked) match
      case Return(a) => p(a)
      case Inject(a) => h(a)(Freer.Return(_))
      case Bind(Inject(a), f) => h(a)(f)

  /**
   * The four names at the effect tree's arity (`Inject[F, A](e)`, `case Bind(Inject(e), k)`). The patterns are
   * product matches — `unapply` answers the node itself — so a match allocates nothing. Plain `def`s, not
   * inline: the `direct` macro recognises a program by the symbols of these `apply`s (DirectRow.scala).
   */
  object Return:
    def apply[F[+_], A](a: A): Free[F, A] = Freer.Return(a)
    def unapply[G[_, _, +_], R, A](r: Freer.Return[G, R, A]): Freer.Return[G, R, A] = r

  object Inject:
    def apply[F[+_], A](a: F[A]): Free[F, A] = Freer.Inject[Unary[F], Unit, Unit, A](a)
    /** the node at its operation's own type, `F[A]` — `Unary`'s extractor, at the indexes every `Free` node
     * has (`Unit, Unit`); a product match, the node itself, nothing allocated (TestInlineBudget measures
     * `handle`'s loop) */
    def unapply[F[+_], A](i: Freer.Inject[Unary[F], Unit, Unit, A]): Freer.Inject[Unary.Op[F], Unit, Unit, A] =
      Unary.unapply(i)

  object Delay:
    def apply[F[+_], A](thunk: () => Free[F, A]): Free[F, A] = Freer.Delay(thunk)
    def unapply[G[_, _, +_], S, R, A](d: Freer.Delay[G, S, R, A]): Freer.Delay[G, S, R, A] = d

  /**
   * THE ONE CAST of the effect side. A `Bind` matched through a tree typed at `Unit` has a middle index `T`
   * the type forgot. Every door that builds an effect program puts `Unit` there, and `Lift` carries no answer
   * type for another `T` to come from, so `T` IS `Unit` — said once here, for every `Bind(Inject(e), k)` site.
   * The type variables sit in the PARAMETER, so the compiler's type test binds them (in the result alone they
   * would be inferred `Nothing`); the result is the node, so the pattern allocates nothing. Unreachable on a
   * `Cont`, which is opaque outside its companion.
   */
  object Bind:
    def apply[F[+_], A, B](a: Free[F, A], f: A => Free[F, B]): Free[F, B] = Freer.Bind(a, f)
    def unapply[G[_, _, +_], T, R, X, A](b: Freer.Bind[G, Unit, T, R, X, A]): Freer.Bind[G, Unit, Unit, R, X, A] =
      b.asInstanceOf[Freer.Bind[G, Unit, Unit, R, X, A]]
}
