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

/**
 * A module is an installer that has not been built yet (specs/di.md):
 * `Providing[F]` holds READY values, a `Module[F]` builds them in the
 * `Resource` effect — so opening a pool, starting a server, and
 * closing both at the end of the scope, in reverse order, is the
 * region's obligation and not the caller's.
 *
 * {{{
 *   val db   = module[Db](Db.open(url))(_.close)
 *   val pool = module[Pool](Pool.over(wire[Db]))(_.close)   // Db ?=> Module[...]
 *   val app  = (db and pool) { wire[Pool].borrow() }        // : Int ! Resource
 *   Resource.run(app)
 * }}}
 *
 * `and` takes its right operand INSIDE the left's context, `F[Module[G]]`:
 * a plain module coerces (a value is a context function that ignores
 * its argument), and a module whose acquisition needs an earlier one
 * reads it with `wire[Db]` — so the dependency graph is written in
 * the composition and checked by the compiler: an acquisition naming
 * a capability no module before it installs does not compile. Left
 * acquires first, so the right (inner) releases first; nesting order
 * and the override story are `Providing.and`'s, unchanged.
 *
 * A class, not an alias over the program: an extension `apply` on
 * `Providing[F] ! Resource` typed `m { wire[Db].q }` without the
 * expected type, and the body eagerly applied (the E10 trap) — the
 * same body against a class method types as it does on `Providing`.
 */
final class Module[F[_]](val build: Providing[F] ! Resource):
  /** compose; the right operand is built inside the left's context, and is the inner layer */
  infix def and[G[_]](that: F[Module[G]]): Module[[X] =>> F[G[X]]] =
    new Module(build.flatMap(p => p(that).build.map(q => p and q)))
  /** install everything and run the body inside the scope */
  def apply[B](body: F[B]): B ! Resource = build.map(p => p(body))

/** acquire one capability in the scope; the scope releases it */
def module[A](acquire: => A)(release: A => Unit): Module[[X] =>> A ?=> X] =
  new Module(Resource.acquire(acquire)(release).map(a => providing[A](a)))

/**
 * The plan is the TYPE (specs/di.md, stage 1): a module's `F` is the
 * curried chain `A ?=> B ?=> … ?=> X`, outer to inner in acquisition
 * order, so what it will install — and in what order — is read off
 * `F` at compile time, before anything is built. That is why a
 * dependent module (`Db ?=> Module[…]`), whose VALUE cannot be seen
 * without a `Db`, still has a plan: its `G` is in the type of `and`.
 * Names are the type symbols' — an opaque qualifier (`Primary`) shows
 * as itself, which the erased class could not do.
 */
extension [F[_]](m: Module[F])
  inline def plan: Vector[String] = ${ Module.planImpl[F] }

/**
 * What a module installed, for a container that wants values BY CLASS
 * (specs/di.md, stage 2): the name is the plan's, the class is the
 * erased one — an opaque qualifier exports under its underlying class
 * and keeps its role in the name — and the value is what the scope
 * built. `m.exports` is the macro twin of `plan`: it reads the same
 * chain off the type and generates the body that collects each
 * ambient value, so no cast and no reflection touch the values.
 */
final case class Installed(name: String, cls: Class[?], value: Any)

extension [F[_]](m: Module[F])
  inline def exports: Vector[Installed] ! Resource = ${ Module.exportsImpl[F]('m) }

object Module:
  /** a module with nothing to build or release — a test double, a config value */
  def ready[F[_]](p: Providing[F]): Module[F] = new Module(pure[Resource, Providing[F]](p))
  /** the same, from the bare value */
  def value[A](a: A): Module[[X] =>> A ?=> X] = ready(providing[A](a))

  import scala.quoted.*
  /** `F[Marker]` dealiased is `ContextFunction1[A, ContextFunction1[B, … Marker]]`;
   * walk it to the marker, naming each parameter */
  /** the chain `A ?=> B ?=> … ?=> End` as its parameters, outer first,
   * each with the type that remains after it */
  private def chain(using q: Quotes)(t: q.reflect.TypeRepr, end: q.reflect.TypeRepr)
      : List[(q.reflect.TypeRepr, q.reflect.TypeRepr)] =
    import q.reflect.*
    def walk(t: TypeRepr, acc: List[(TypeRepr, TypeRepr)]): List[(TypeRepr, TypeRepr)] = t.dealias match
      case AppliedType(fn, List(a, rest)) if fn.typeSymbol.name.startsWith("ContextFunction") =>
        walk(rest, (a, rest) :: acc)
      case t if t =:= end => acc.reverse
      case other => report.errorAndAbort(
        s"Module: expected a chain of context functions ending in ${end.show}, found ${other.show}")
    walk(t, Nil)

  def planImpl[F[_] : Type](using Quotes): Expr[Vector[String]] =
    import quotes.reflect.*
    val names = chain(TypeRepr.of[F[Module.Marker]], TypeRepr.of[Module.Marker]).map(_._1.typeSymbol.name)
    val list = Expr(names)
    '{ $list.toVector }

  /**
   * Generates `m.build.map(p => p((a: A) ?=> (b: B) ?=> … List(Installed(…, a), Installed(…, b)).toVector))`.
   * Each level is quoted with its own parameter type and ascribed to
   * the type the chain says remains — `asExprOf` is a CHECK at
   * expansion time, not a runtime cast, and it fails the expansion
   * if the generated body's type ever disagrees with the chain's.
   */
  def exportsImpl[F[_] : Type](m: Expr[Module[F]])(using Quotes): Expr[Vector[Installed] ! Resource] =
    import quotes.reflect.*
    val end = TypeRepr.of[Vector[Installed]]
    val levels = chain(TypeRepr.of[F[Vector[Installed]]], end)
    def body(ls: List[(TypeRepr, TypeRepr)], acc: List[Expr[Installed]]): Expr[Any] = ls match
      case Nil => '{ ${ Expr.ofList(acc.reverse) }.toVector }
      case (a, rest) :: more =>
        val name = Expr(a.typeSymbol.name)
        // the erased class: an opaque type's is its underlying's
        val cls = Literal(ClassOfConstant(a.dealias)).asExprOf[Class[?]]
        (a.asType, rest.asType) match
          case ('[at], '[rt]) =>
            '{ (x: at) ?=> ${ body(more, '{ Installed($name, $cls, x) } :: acc).asExprOf[rt] } }
    val collect = body(levels, Nil).asExprOf[F[Vector[Installed]]]
    '{ $m.build.map(p => p[Vector[Installed]]($collect)) }
  /** the end of the chain the plan walks to; never inhabited */
  sealed trait Marker
