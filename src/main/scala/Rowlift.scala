package okay

/**
 * Row membership as a witness, and the one cast it licenses.
 *
 * THE PROBLEM. A constructor builds its operation at its OWN row:
 * `State.get[Int] : Int ! (State % Int)`. To use it in a program whose
 * row is wider you must move it, and the only spelling the library had
 * was `!.widen`, which asks for the COMPLEMENT — the part of the row
 * you are not talking about — and walks the tree rebuilding every
 * node. Naming the complement is what makes a helper hard to write
 * against an unknown row, and it is what leaked into the tracked
 * demo.
 *
 * WHY THE WALK IS NOT NEEDED HERE. `+` is a union, `[A] =>> F[A] |
 * G[A]`, and unions erase. A `Free[F, A]` therefore already IS a
 * `Free[R, A]` at runtime whenever F is a member of R: every operation
 * it holds is an `F[X]`, and every `F[X]` is an `R[X]`. Nothing needs
 * rebuilding. The type system cannot state this, because `Free` is
 * invariant in F, and that invariance is a measured CHOICE (see
 * Effects.scala and specs/writer-covariance.md): a covariant `Free`
 * type-checks, but deleting the walk costs 5-7% on `Source.merge`,
 * where the walk doubles as a normalisation.
 *
 * So the walk stays where it earns its keep, and `In` is the proof
 * that lets a SINGLE operation — already head-normal, nothing to
 * normalise — skip it.
 *
 * WHY THE WITNESS IS `Unit`. An earlier shape gave `In` an `inj`
 * method, so every instance had to be an object; one cached instance
 * served them all and each `given` cast it into place — three casts to
 * avoid one allocation. But `inj` is identity at every instance, which
 * is the same erasure fact stated twice. A witness that is only ever
 * summoned does not need to exist at runtime: the instances are `()`,
 * and opacity is what stops a caller conjuring one, since only this
 * scope can see that `()` is a proof.
 *
 * Opacity holds OUTSIDE this scope only. Anything written inside
 * `Rowlift` sees `In[F, R]` as literally `Unit`, implicit search looks
 * in `Unit`'s companion, and the givens below become invisible. That
 * is why the probe lives in its own file, and it is the only trap in
 * the design.
 */
object Rowlift:

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

  extension [A, F[+_]](p: A ! F)
    /**
     * THE ONLY CAST. Sound by the erasure argument above, and `In` is
     * the proof of its side condition.
     *
     * NOT a replacement for `!.widen`: on a streaming path the walk is
     * a normalisation and removing it is measurably slower.
     */
    inline def at[R[+_]](using In[F, R]): A ! R = p.asInstanceOf[A ! R]

  /**
   * THE ROUTES THAT DID NOT WORK, so the next person does not spend
   * the afternoon again. Both were tried, compiled, and measured.
   *
   * A MACRO reading the receiver and emitting the Inject at R
   * directly. `State.get[Int]` is an inline def whose body is
   * `effect(Get())`, so the macro should see the operation and skip
   * the rebuild. Two walls: Scala binds an extension's receiver to a
   * val proxy BEFORE the splice, so the macro sees `Ident("p$proxy1")`
   * and nothing else — marking the receiver `inline` fixes that and
   * the macro does then report `HIT: Free$.Inject$.apply` — but the
   * operation it extracts refers to a proxy created by `effect`'s own
   * inlining, and re-emitting it fails with "a reference to value
   * a$proxy16 was used outside the scope where it was defined".
   * Rewriting a subtree that arrived from an inline def is not
   * generally possible; that is why `direct` builds its Inject from
   * its own pieces rather than moving anyone else's term. And the
   * macro was not needed: the cast above is already at the floor.
   *
   * `<:<` INSTEAD OF `In`. The compiler proves `F[X] <:< R[X]` itself,
   * at any depth and with no instance hierarchy — but the polymorphic
   * witness `[X] => () => (F[X] <:< R[X])` is not summonable (implicit
   * search diverges), so it cannot be a context bound. Checked, not
   * assumed.
   *
   * COVARIANT `Free`, which would make all of this unnecessary: it
   * type-checks, and it is slower where it matters. See
   * specs/writer-covariance.md, free-row-variance.
   */
