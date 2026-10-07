package okay.freer


/**
 * THE EFFECT TREE: `Free[F, A]`, the freer monad (`Freer`, module okay-freer, package `okay` as everything of the
 * two monads below the core) at the unary signature `F`, every index `Unit`, and the four names at that arity.
 * The library's doors are in `Diagonal`'s companion (Indexed.scala), in the implicit scope of every `A ! F`.
 */
/** the effect program: the base at `Unary[F]`, every index `Unit`. `object Free` keeps the four names
 * (`Return`, `Inject`, `Bind`, `Delay`) at the arities the match sites and the `direct` macro use */
type Free[F[+_], +A] = Freer[Unary[F], Unit, Unit, A]

object Free {

  /**
   * `tailRecM` for programs (specs/fold-until.md): run `f` from `s`, continue from a `Left`, answer a `Right`.
   * Stack-safe with no trampoline of its own: the recursive call sits inside the `flatMap`'s continuation.
   * `!.loop` is this, exported.
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
   * `Cps`, which is opaque outside its companion.
   */
  object Bind:
    def apply[F[+_], A, B](a: Free[F, A], f: A => Free[F, B]): Free[F, B] = Freer.Bind(a, f)
    def unapply[G[_, _, +_], T, R, X, A](b: Freer.Bind[G, Unit, T, R, X, A]): Freer.Bind[G, Unit, Unit, R, X, A] =
      b.asInstanceOf[Freer.Bind[G, Unit, Unit, R, X, A]]

}
