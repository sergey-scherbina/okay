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

/**
 * The CONSUMER one-liner (E17): `wire[Db].q` pulls the ambient
 * capability by naming its type — Reader's `ask` on context
 * functions. The naive `def wire[T] = summon[T]` does not compile
 * (no given at the definition site); the `A ?=> A` result type is
 * the fix, and the eager auto-application of context functions
 * (the E10 trap) works FOR us here: in receiver position
 * `wire[Db].q` applies to the nearest given and moves on. Doors
 * write point-free: `val getQ: Db ?=> String = wire[Db].q`.
 * A missing given is a compile error — the DI claim holds.
 */
inline def wire[A]: A ?=> A = summon[A]

/**
 * The Monad instance for context functions themselves (E13/E15/E19):
 * `[X] =>> E ?=> X` is the Reader whose combinators the COMPILER
 * runs — pure is the value, flatMap is literally `f(fa)` (the
 * receiver auto-applies to the ambient E: the Reader diagonal for
 * free). Direct style needs none of it; the instance exists for the
 * GENERIC combinators written once over any F — traverse, sequence,
 * replicateA:
 *
 * {{{
 *   val xs: Seq[Env ?=> Int] = Seq(wire[Env].uid, wire[Env].uid + 1)
 *   val all: Env ?=> Seq[Int] = sequence(xs)   // F inferred
 * }}}
 *
 * Method syntax on a BARE ctx function stays out of bounds: the
 * receiver eagerly applies before extension lookup (E10), so
 * `(x: Env ?=> Int).flatMap(...)` dispatches against Int, not the
 * instance. Use the generic combinators or direct style.
 */
given ctxMonad[E]: Monad[[X] =>> E ?=> X] with
  def pure[A](a: A): E ?=> A = a
  extension [A](fa: E ?=> A)
    def flatMap[B](f: A => E ?=> B): E ?=> B = f(fa)
