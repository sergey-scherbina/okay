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
   * MEMBERSHIP AS SUBTYPING, for the places `In` cannot go
   * (row-membership-crash, 2026-09-17).
   *
   * `In`'s inductive given asks the compiler to solve `?G + ?H` for
   * the target, and when the target is an ABSTRACT row dotty 3.9 does
   * not fail — it CRASHES, in `orDominator`, with "Failure to join
   * alternatives F and G". That crash has decided three designs in
   * this repository (delim-safety's guard, `Replayable`'s encoding,
   * and the workflow driver's row), and `ProbeRowCrash` keeps it
   * pinned so a future Scala can be re-tested against it.
   *
   * `Sub` is the shape that does not crash: a union on the RIGHT of a
   * `<:<` needs no join, so a concrete row resolves and an abstract
   * one simply FAILS — which is what an implicit should do. It also
   * says the useful thing about `Pure`: `Nothing <:< anything`, so a
   * program with no operations rides into any row at all.
   *
   * WHAT IT IS NOT, and the difference is the whole of its honesty:
   * true membership is `∀X. F[X] <: G[X]`, and this tests it at `Any`
   * only. For the rows this library builds — unions of effect
   * signatures applied pointwise — the two coincide, and the cast it
   * licenses is exactly the one `In` licenses. It is an
   * approximation, said out loud rather than hidden behind a name.
   */
  type Sub[F[+_], G[+_]] = F[Any] <:< G[Any]

  /**
   * THE ONLY CAST. Sound by the erasure argument above; each caller
   * supplies the side condition — `at` by a witness, `plus` by
   * construction.
   *
   * `!.widen` IS this coercion since widen-split (2026-09-23). The
   * walk that used to sit under that name is `!.normalize`; the walk
   * a streaming path measurably needs is `Writer.widen`'s, over the
   * element type, and that one stays a walk.
   */
  private inline def coerce[A, F[+_], R[+_]](p: A ! F): A ! R =
    p.asInstanceOf[A ! R]

  /**
   * The same cast for the `direct` macro, whose side condition is
   * checked a different way (direct-narrow-colour, 2026-09-16): the
   * macro compares the two rows by SUBTYPING — `F <:< R`, which dotty
   * decides pointwise and which is exactly membership when R is a
   * union — because by the time it holds a row it has been beta-reduced
   * to `[A] =>> X[A] | Y[A]` and no longer matches the `F + G` shape
   * the `In` givens are written against. `summon[In[F, R]]` succeeds on
   * the ALIAS and fails on the reduced form; measured, and the reason
   * this door exists rather than the macro fabricating a witness for an
   * opaque type it cannot see.
   */
  private[okay] inline def into[A, F[+_], R[+_]](p: A ! F): A ! R = coerce(p)

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
   * Throws.scala's `abort[A].at[F]`. Everywhere else both compile
   * (checked — including inside an interpreter, where the complement
   * mentions an abstract residual row and is still nameable).
   *
   * Row ORDER is not a reason to reach for either: `+` is a union and
   * `|` commutes, so `A ! Users + Abort` and `A ! Abort + Users`
   * are the same type, assignable in both directions with no coercion
   * at all. An `.at[...]` written to reorder is noise (checked — an
   * earlier version of this comment said otherwise).
   */
  extension [A, F[+_]](p: A ! F)
    /** land in row R, which must CONTAIN this program's row */
    inline def at[R[+_]](using In[F, R]): A ! R = coerce(p)

    /** the same widening, licensed by `Sub` instead of `In` — for a
     * TARGET ROW THAT IS ABSTRACT, where `In` crashes the compiler
     * rather than resolving (row-membership-crash) */
    inline def up[R[+_]](using Sub[F, R]): A ! R = coerce(p)

    /**
     * add R to whatever row this program already has: `A ! F` becomes
     * `A ! F + R`.
     *
     * The postfix counterpart of `!.widen`, and the one to reach for
     * inside a helper: the row you are IN is already in the type, so
     * naming it again is noise — say only what you are adding.
     *
     *     Users.find(id).plus[Abort]   :  Option[String] ! Users + Abort
     *
     * Needs no witness at all, where `at` needs one: membership here
     * is by CONSTRUCTION — `F + R` is built out of F — so there is
     * nothing left for a proof to establish.
     */
    inline def plus[R[+_]]: A ! F + R = coerce(p)

    /**
     * A BIND ACROSS ROWS (bind-in-row-union, 2026-09-23): the
     * continuation may answer in ANOTHER row, and the result is the
     * union of the two. `Free` is invariant in its row by a measured
     * decision (free-row-variance, above), so `p.flatMap(f)` refuses
     * an `f` whose row differs; what this adds is not a fourth cast
     * but INFERENCE — the spelling it replaces,
     *
     *     p.plus[G].flatMap(x => f(x).plus[F])
     *
     * names both rows by hand at every call, while here `G` is read
     * off `f`'s result and `F` off `p`. Both sides ride into `F + G`
     * by construction (`plus`), so no witness is needed and no tree is
     * walked: one `Bind` node, exactly what `flatMap` builds.
     *
     * Named `bind` at the operator's word (2026-09-23) — the monad's own
     * word for the operation, on a receiver where `flatMap` cannot be
     * it. Not `flatMap` itself, on purpose: `Free.flatMap` is the hottest
     * path in the library and an overload taking a row-polymorphic
     * continuation would put every lambda's typing through overload
     * resolution; and a `for`-comprehension desugars to `flatMap` by
     * name, so mixed rows in a `for` stay `.at[R]` on each generator —
     * or a `direct` block, where marks widen with nothing written.
     */
    inline def bind[B, G[+_]](f: A => B ! G): B ! F + G =
      coerce[A, F, F + G](p).flatMap(a => coerce[B, G, F + G](f(a)))

    /** the same, the answer dropped: `p andThen q` runs p, then q */
    inline def andThen[B, G[+_]](q: => B ! G): B ! F + G =
      coerce[A, F, F + G](p).flatMap(_ => coerce[B, G, F + G](q))
