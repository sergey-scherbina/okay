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

  /** normalize into the tree world */
  def toFree[F[+_], A](m: Eager[F, A]): A ! F = m match
    case t: Free[?, ?] => t.asInstanceOf[A ! F]
    case a => Free.Pure(a.asInstanceOf[A])

  // the Free instance, named: in this scope A ! F conforms to
  // Eager[F, A ! F], so unqualified extension calls would recurse
  private def FreeE: Effects[Free] = summon[Effects[Free]]

  given Effects[Eager] with
    override inline def pure[F[+_], A](a: A): Eager[F, A] = a
    override inline def perform[F[+_], A](e: F[A]): Eager[F, A] = Free.Inject(e)

    extension [F[+_], A](m: Eager[F, A])
      override def flatMap[B](f: A => Eager[F, B]): Eager[F, B] = m match
        case t: Free[?, ?] =>
          t.asInstanceOf[A ! F].flatMap(x => toFree(f(x)))
        case a => f(a.asInstanceOf[A])

      override def foldCont[S](h: F !> S): A /> S =
        FreeE.foldCont(toFree(m))(h)

      override def foldIn[C[_, _, _] : Control, S](h: Interpr[F, C, S]): C[A, S, S] =
        FreeE.foldIn(toFree(m))(h)

      /** a pure value runs in O(1); a suspended tree runs like Free */
      override def runWith(using Handler[F]): A = m match
        case t: Free[?, ?] => FreeE.runWith(t.asInstanceOf[A ! F])
        case a => a.asInstanceOf[A]
}
