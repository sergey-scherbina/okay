package okay

/**
 * The COMPOSABLE half of provide (specs/context-functions.md, E16):
 * where `provide(a, b)(body)` is flat and capped at 22, `providing`
 * builds installers as VALUES and composes them applicatively —
 * no nesting, no cap.
 *
 * {{{
 *   val base = providing[Db](db) and providing[Log](log)
 *   (base and providing[Clock](clock)) { app }   // app(using Db, Log, Clock)
 * }}}
 *
 * The mechanism is currying as composition of type constructors:
 * one installer carries `F[X] = A ?=> X`; `and` composes to
 * `F[G[X]] = A ?=> G[X]` — the curried chain `A ?=> B ?=> X`
 * assembled by values. Type lambdas reduce where the match-type
 * route (E11/E12) stalls, so a using-method eta-expands into the
 * chain at the call site. Nesting order: the RIGHT operand of
 * `and` is the inner layer, so it wins under nearest-wins (E8) —
 * `base and providing[Log](testLog)` overrides base's Log.
 *
 * Prefer the explicit type argument, `providing[Db](db)`: inference
 * would pick the runtime refinement, and the capability you install
 * is the trait, not the anonymous class.
 */
final class Providing[F[_]](val run: [X] => F[X] => X):
  /** compose installers; the right side becomes the inner layer */
  infix def and[G[_]](that: Providing[G]): Providing[[X] =>> F[G[X]]] =
    Providing([X] => (body: F[G[X]]) => that.run(run[G[X]](body)))
  /** install everything and run the body */
  def apply[B](body: F[B]): B = run(body)

/** one installer: `providing[A](a)` holds a `given A` for later */
def providing[A](a: A): Providing[[X] =>> A ?=> X] =
  Providing([X] => (body: A ?=> X) => body(using a))
