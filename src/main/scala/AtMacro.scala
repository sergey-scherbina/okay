package okay

import scala.quoted.*

/**
 * PROBE (rowlift): can a macro give `.at[R]` the ergonomics of a
 * postfix and the cost of constructing at the row directly?
 *
 * The measured problem: `.at` on a program rebuilds one Inject per
 * operation (+32 B/op against the floor, and HotSpot does not
 * scalarise it). But the receiver is almost always a LITERAL
 * construction — `State.get[Int]` is an inline def whose body is
 * `effect(Get())`, i.e. `Free.inject(Get())` — so at the call site
 * the macro sees the operation itself, and rebuilding is pointless:
 * emit the Inject at R in the first place.
 *
 * The general case still has to work, so the macro falls back to the
 * runtime walk when the receiver is anything else. Zero cost where
 * the shape is known, correct everywhere.
 *
 * Lives in src/main because a macro cannot be used in the compilation
 * run that defines it, and the JMH sources are a later one.
 */
object AtMacro:

  /** the membership witness: inj is identity at every instance, so
   * this is a type-level fact with no runtime content */
  trait In[F[+_], R[+_]]:
    def inj[A](fa: F[A]): R[A]

  private object IdIn extends In[[A] =>> Any, [A] =>> Any]:
    def inj[A](fa: Any): Any = fa

  trait InLow:
    given self[F[+_]]: In[F, F] = IdIn.asInstanceOf[In[F, F]]

  object In extends InLow:
    given left[F[+_], G[+_]]: In[F, F + G] = IdIn.asInstanceOf[In[F, F + G]]
    given deeper[F[+_], G[+_], H[+_]](using i: In[F, G]): In[F, G + H] =
      IdIn.asInstanceOf[In[F, G + H]]

  /** the fallback: the tree walk, for a receiver the macro cannot see
   * through. Not inline — it recurses, and inline recursion does not
   * terminate. */
  def walk[A, F[+_], R[+_]](p: A ! F)(i: In[F, R]): A ! R =
    import okay.!.*
    (p.resume: @unchecked) match
      case Pure(a) => Free.Pure(a)
      case Effect(e) => Free.inject(i.inj(e))
      case Bind(Effect(e), k) => Free.inject(i.inj(e)).flatMap(x => walk(k(x))(i))

  // the receiver must be `inline`, or Scala binds it to a val proxy
  // before the splice and the macro sees only Ident("p$proxy1")
  /**
   * THE CHEAP ONE. `+` is a union ([A] =>> F[A] | G[A]) and unions are
   * ERASED, so a Free[F, A] already IS a Free[R, A] at runtime — every
   * operation it holds is an F[X], and In witnesses that every F[X] is
   * an R[X]. The tree walk in `widen` rebuilds a structure that was
   * already correct; the evidence is exactly the proof that makes
   * reinterpreting it sound instead of a guess.
   */
  extension [A, F[+_]](p: A ! F)
    inline def atCast[R[+_]](using In[F, R]): A ! R =
      p.asInstanceOf[A ! R]

  /**
   * THE MACRO ROUTE, REFUTED — kept as the record so the next person
   * does not spend the afternoon I did.
   *
   * The idea was sound and got most of the way: `State.get[Int]` is an
   * inline def whose body is `effect(Get())`, so a macro should see
   * the operation and emit the Inject at R directly, skipping the
   * rebuild. Two walls, in order:
   *
   *   1. Scala binds an extension's receiver to a val proxy BEFORE the
   *      splice, so the macro sees `Ident("p$proxy1")` and nothing
   *      else. Resolving it from inside is impossible — the binding
   *      lives outside the term the macro is handed. Marking the
   *      receiver `inline` DOES fix this: the macro then reports
   *      `HIT: okay.Free$.Inject$.apply`.
   *
   *   2. But the operation it extracts refers to a proxy created by
   *      `effect`'s OWN inlining, and re-emitting it fails:
   *        "a reference to value a$proxy16 was used outside the scope
   *         where it was defined"
   *      Rewriting a subtree that arrived from an inline def is not
   *      generally possible. This is why `direct` builds Free.Inject
   *      from its own pieces rather than moving anyone else's term.
   *
   * And it turned out not to be needed: `atCast` above is free without
   * any of this, because the union is erased and the evidence is the
   * proof that saying so is sound.
   */
