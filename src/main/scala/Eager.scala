package okay

/**
 * The opt-in EAGER encoding (specs/eager.md): the kyo trick as a
 * third Effects instance. A pure computation IS its value — the union
 * A | (A ! F) — so flatMap on a pure value applies at CONSTRUCTION:
 * runs of pure binds cost plain function calls, no tree and no
 * interpretation. Choose it for bind-heavy computation; choose
 * Free/Eff where the laziness contract matters. The hazards are
 * kyo's, taken knowingly: construction evaluates (a self-referential
 * program diverges before it runs), and values must not themselves be
 * effect trees (the union is discriminated by the runtime class of
 * Free — kyo's Flat constraint as a documented rule).
 *
 * The type is OPAQUE and its instance lives in the companion, behind
 * `import Eager.given`: opt-in in the type system too, so the
 * encoding cannot leak into inference or extension resolution
 * anywhere it was not asked for.
 */
opaque type Eager[F[+_], A] = A | (A ! F)

object Eager {

  /** THE dispatch of the encoding, once: an Eager[F, A] is either a
   * plain A or a tree A ! F, told apart at runtime by the one class
   * a tree has — the two casts here are the encoding's definition
   * (the opaque type is `A | (A ! F)`), and every operation below
   * goes through them. INLINE, both the function and its `value`/
   * `tree` arguments: an ordinary `fold` here built a closure for
   * BOTH branches on every call (arguments are evaluated before the
   * call, so the branch not taken still allocated) and dispatched
   * through it virtually instead of an inlined match arm — measured
   * at 3.45x on the pure-bind hot path, the whole point of Eager
   * (eager-dispatch-regression, specs/eager.md Decisions). Inlining
   * substitutes each call site's argument EXPRESSION directly into
   * the match arm, so only the arm actually taken builds anything;
   * the casts stay textually in this one function, same as before. */
  private inline def fold[F[+_], A, B](m: Eager[F, A])(inline value: A => B, inline tree: (A ! F) => B): B = m match
    case t: Free[?, ?] => tree(t.asInstanceOf[A ! F])
    case a => value(a.asInstanceOf[A])

  /** normalize into the tree world */
  def toFree[F[+_], A](m: Eager[F, A]): A ! F = fold(m)(Free.Pure(_), identity)

  // the Free instance, named: in this scope A ! F conforms to
  // Eager[F, A ! F], so unqualified extension calls would recurse
  private def FreeE: Effects[Free] = summon[Effects[Free]]

  given Effects[Eager] with
    override inline def pure[F[+_], A](a: A): Eager[F, A] = a
    override inline def perform[F[+_], A](e: F[A]): Eager[F, A] = Free.Inject(e)

    extension [F[+_], A](m: Eager[F, A])
      override def flatMap[B](f: A => Eager[F, B]): Eager[F, B] =
        fold(m)(f, t => t.flatMap(x => toFree(f(x))))

      override def foldCont[S](h: F !> S): A /> S =
        FreeE.foldCont(toFree(m))(h)

      override def foldIn[C[_, _, _] : Control, S](h: Interpr[F, C, S]): C[A, S, S] =
        FreeE.foldIn(toFree(m))(h)

      /** a pure value runs in O(1); a suspended tree runs like Free */
      override def runWith(using Handler[F]): A = fold(m)(identity, FreeE.runWith(_))
}
