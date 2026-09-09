package okay

/**
 * Row membership as a witness, and the one cast it licenses.
 *
 * A constructor builds at its OWN row — `State.get[Int] : Int !
 * (State % Int)` — and a program usually has a wider one. The only
 * spelling the library had was `!.widen`, which asks for the
 * COMPLEMENT, the part of the row you are NOT talking about, and
 * walks the tree rebuilding every node.
 *
 * Neither is needed for one operation. `+` is a union, `[A] =>> F[A]
 * | G[A]`, and unions ERASE: a `Free[F, A]` already IS a `Free[R, A]`
 * whenever F is a member of R, because every operation it holds is an
 * `F[X]` and every `F[X]` is an `R[X]`. The compiler cannot say so —
 * `Free` is invariant in its row, and that invariance is a measured
 * choice, not an oversight (specs/writer-covariance.md,
 * free-row-variance) — so `In` supplies the missing side condition
 * and one cast does the rest.
 *
 * `In` is an opaque `Unit`: a witness that is only ever SUMMONED need
 * not exist at run time, and opacity is what stops a caller conjuring
 * a proof. Beware that opacity holds OUTSIDE this scope only —
 * anything written inside `RowLift` sees `In[F, R]` as literally
 * `Unit`, and the givens below become invisible to implicit search.
 * That is the one trap here.
 *
 * The design space, the measurements (this is at the same B/op as
 * constructing the operation at R), and the routes that did not work
 * — a macro, `<:<`, a covariant `Free` — are in
 * specs/writer-covariance.md, rowlift.
 */
object RowLift:

  /** F is a member of the row R. */
  opaque type In[F[+_], R[+_]] = Unit

  trait InLow:
    given self[F[+_]]: In[F, F] = ()

  object In extends InLow:
    given left[F[+_], G[+_]]: In[F, F + G] = ()
    given deeper[F[+_], G[+_], H[+_]](using In[F, G]): In[F, G + H] = ()

  /** partially applied, the witness reads as a context bound —
   * `[R[+_] : Has[State % Int]]` rather than a using clause */
  type Has[F[+_]] = [R[+_]] =>> In[F, R]

  /**
   * THE ONLY CAST. Sound by the erasure argument above; each caller
   * supplies the side condition — `at` by a witness, `plus` by
   * construction.
   *
   * NOT a replacement for `!.widen`: on a streaming path the walk is
   * also a normalisation, and removing it is measurably slower.
   */
  private inline def coerce[A, F[+_], R[+_]](p: A ! F): A ! R =
    p.asInstanceOf[A ! R]

  /**
   * NEITHER SPELLING CAN LOSE AN EFFECT, which is worth saying because
   * `at[R]` names a whole row and so LOOKS like it replaces one:
   * `In[F, R]` is the demand that R CONTAIN F. Both add; they differ
   * only in what the caller must name.
   *
   * `plus` names the ADDITION, `at` names the TARGET, and both are
   * the same cast, so the choice is only about which is shorter to
   * say. One effect being added: `plus`. Several operations landing
   * in one row: `at`, because there the target has a name and each
   * operation's complement is different — inside an interpreter into
   * `R = State % S + Writer % W + F`, a State operation's complement
   * is `Writer % W + F` and a Writer operation's is `State % S + F`,
   * while the target is `R` for both.
   *
   * `at` is REQUIRED only where the complement cannot be named at
   * all: an abstract row known only by membership, as in
   * `Fail.scala`'s `abort[A].at[F]`. Everywhere else both compile
   * (checked — including inside an interpreter, where the complement
   * mentions an abstract residual row and is still nameable).
   *
   * Row ORDER is not a reason to reach for either: `+` is a union and
   * `|` commutes, so `A ! (Users + Abort)` and `A ! (Abort + Users)`
   * are the same type, assignable in both directions with no coercion
   * at all. An `.at[...]` written to reorder is noise (checked — an
   * earlier version of this comment said otherwise).
   */
  extension [A, F[+_]](p: A ! F)
    /** land in row R, which must CONTAIN this program's row */
    inline def at[R[+_]](using In[F, R]): A ! R = coerce(p)

    /**
     * add R to whatever row this program already has: `A ! F` becomes
     * `A ! (F + R)`.
     *
     * The postfix counterpart of `!.widen`, and the one to reach for
     * inside a helper: the row you are IN is already in the type, so
     * naming it again is noise — say only what you are adding.
     *
     *     Users.find(id).plus[Abort]   :  Option[String] ! (Users + Abort)
     *
     * Needs no witness at all, where `at` needs one: membership here
     * is by CONSTRUCTION — `F + R` is built out of F — so there is
     * nothing left for a proof to establish.
     */
    inline def plus[R[+_]]: A ! (F + R) = coerce(p)
