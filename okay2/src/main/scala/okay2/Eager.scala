package okay2

/**
 * The opt-in EAGER encoding: the kyo trick as a second `Effects`
 * instance. A pure computation IS its value — the union `A | (A ! F)` —
 * so `flatMap` on a pure value applies at CONSTRUCTION: runs of pure
 * binds cost plain function calls, no tree and no interpretation.
 * Choose it for bind-heavy computation; choose `Free` where the laziness
 * contract matters. The hazards are kyo's, taken knowingly:
 * construction evaluates (a self-referential program diverges before it
 * runs), and a value must not itself be an effect tree (the union is
 * told apart by the runtime class of `Free` — kyo's `Flat` rule).
 *
 * The type is ABSTRACT outside the module (Scala 2's opaque type, as
 * `Cont` is), and its instance is a member: `import Eager._` opts in.
 */
sealed abstract class EagerModule {
  type Rep[F, A]

  /** normalize to the tree at any point */
  def toFree[F <: Row, A](m: Rep[F, A]): A ! F

  implicit def effects: Effects[Rep]
}

private[okay2] object EagerImpl extends EagerModule {
  /** a value, or a tree: `A | (A ! F)` */
  type Rep[F, A] = Any

  /** THE ONE DISCRIMINATION, and the kyo rule it rests on: an eager
   * program is a `Free` exactly when it is a tree, so a value that is
   * itself a `Free` would be read as one — documented, not checked */
  private def fold[F <: Row, A, B](m: Any)(value: A => B, tree: (A ! F) => B): B = m match {
    case t: Free[_, _] => tree(t.asInstanceOf[A ! F])
    case a => value(a.asInstanceOf[A])
  }

  def toFree[F <: Row, A](m: Any): A ! F = fold[F, A, A ! F](m)((a: A) => Free.Return[F, A](a), identity)

  implicit val effects: Effects[Rep] = new Effects[Rep] {
    def pure[F <: Row, A](a: A): Any = a
    def perform[F <: Row, A](e: F#Op[A]): Any = Free.Inject[F, A](e)
    def defer[F <: Row, A, B](thunk: () => Any)(f: A => Any): Any =
      Free.defer(() => toFree[F, A](thunk()))((a: A) => toFree[F, B](f(a)))
    def flatMap[F <: Row, A, B](m: Any)(f: A => Any): Any =
      fold[F, A, Any](m)(f, t => t.flatMap((x: A) => toFree[F, B](f(x))))
    def foldCont[F <: Row, A, S](m: Any)(h: F !> S): A /> S = Effects.foldContFree(toFree[F, A](m))(h)
    override def runWith[F <: Row, A](m: Any)(implicit H: Handler[F]): A =
      fold[F, A, A](m)(identity, t => Effects.runFree(t))
  }
}
