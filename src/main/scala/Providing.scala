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
 * A KIND of fact a module may declare about itself (module-facts):
 * whoever READS the fact defines the key, and HOW TWO DECLARATIONS
 * MERGE IS A MONOID — the one this core has had in Fold.scala all
 * along, with instances for Vector, List, String and every
 * Alternative. So the usual declaration is one line and no methods:
 *
 * {{{
 *   object Routes extends Fact[Vector[Route]]
 * }}}
 *
 * A rule the givens do not have passes its own (fact-is-monoid):
 *
 * {{{
 *   object Declared extends Fact[Vector[Need]](
 *     using Monoid.of(Vector.empty[Need])((a, b) => (a ++ b).distinct))
 * }}}
 *
 * A deployment's needs are one such kind, defined in okay-deploy, and
 * the core knows no deployment word. Keys compare by identity, so a
 * `Fact` is an object, held as a val.
 */
abstract class Fact[V](using val monoid: Monoid[V]):
  private[okay] def empty: V = monoid.empty
  private[okay] def merge(a: V, b: V): V = monoid.combine(a, b)

object Fact:
  given Same[Fact] = Same.byIdentity[Fact]

/** what a module has declared about itself, by kind */
final class Facts private (private val m: TMap[Fact]):
  def get[V](k: Fact[V]): V = m.get(k).getOrElse(k.empty)
  def add[V](k: Fact[V], v: V): Facts = Facts(m.updated(k, k.merge(get(k), v)))
  /** every kind either side declared, merged by its own rule */
  def ++(that: Facts): Facts =
    var out = this
    that.m.foreach([A] => (k: Fact[A], v: A) => out = out.add(k, v))
    out
  def isEmpty: Boolean = m.isEmpty
object Facts:
  val empty: Facts = Facts(TMap.empty[Fact])

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
 * READINESS AND FACTS (module-facts). A module that has nothing to
 * acquire — `Module.value`, `Module.ready` — is `ready`: its values
 * exist before any scope opens. When the LEFT side of `and` is ready,
 * the right side is applied at once rather than inside the deferred
 * build, so the right's `facts` (and readiness) are known now. That
 * is what lets a deployment read what an application needs AFTER its
 * config and BEFORE its first acquisition: the config is a value, the
 * module that opens a log at a path from that config declares the
 * volume, and nothing opens for the reading. A module downstream of
 * an acquisition keeps its facts until the scope runs — and such
 * modules seldom need anything from a place.
 *
 * A class, not an alias over the program: an extension `apply` on
 * `Providing[F] ! Resource` typed `m { wire[Db].q }` without the
 * expected type, and the body eagerly applied (the E10 trap) — the
 * same body against a class method types as it does on `Providing`.
 */
final class Module[F[_]](val built: (Providing[F], Facts) ! Resource,
                         val ready: Option[Providing[F]] = None,
                         val facts: Facts = Facts.empty):
  /**
   * The installer alone. `built` carries the facts BESIDE it, because
   * a contribution declared below an acquisition is not known until
   * that acquisition has happened (di-multibind) — `facts` is the
   * preview a deployment reads early, `built`'s half is the complete
   * one a body can be given.
   */
  def build: Providing[F] ! Resource = built.map(_._1)

  /** compose; the right operand is built inside the left's context, and is the inner layer */
  infix def and[G[_]](that: F[Module[G]]): Module[[X] =>> F[G[X]]] =
    ready match
      case Some(p) =>
        // nothing to acquire on the left: the right exists NOW, and
        // its facts with it, so readiness and the preview travel; the
        // build still merges both halves, in acquisition order
        val inner = p(that)
        new Module(built.flatMap((_, f1) => inner.built.map((q, f2) => (p and q, f1 ++ f2))),
                   inner.ready.map(q => p and q), facts ++ inner.facts)
      case None =>
        new Module(built.flatMap((p, f1) => p(that).built.map((q, f2) => (p and q, f1 ++ f2))),
                   None, facts)

  /**
   * SEVERAL CONTRIBUTORS, ONE COLLECTION (di-multibind) — what a
   * container calls a multibinder. Each module declares its piece as
   * a FACT of kind `k`; `installing` merges every piece by that
   * kind's own rule and installs the result as a capability, so the
   * body reads it like any other:
   *
   * {{{
   *   object Routes extends Fact[Vector[Route]]:
   *     def empty = Vector.empty
   *     def merge(a: Vector[Route], b: Vector[Route]) = a ++ b
   *
   *   (admin.declare(Routes)(Vector(adminRoute)) and
   *    chat.declare(Routes)(Vector(chatRoute))).installing(Routes) { serve(wire[Vector[Route]]) }
   * }}}
   *
   * The fact's VALUE type is the capability type, so name it (an
   * opaque type, a wrapper) where a bare `Vector[X]` would collide
   * with another collection of the same element.
   */
  def installing[V](k: Fact[V]): Module[[X] =>> F[V ?=> X]] =
    new Module(built.map((p, f) => (p and providing[V](f.get(k)), f)),
               ready.map(p => p and providing[V](facts.get(k))),
               facts)
  /** install everything and run the body inside the scope */
  def apply[B](body: F[B]): B ! Resource = build.map(p => p(body))
  /**
   * The same, for a body that is ITSELF a program in the scope — a
   * server that acquires further, a stream that opens a file. An
   * application's body usually is one (di-dogfood: `Jetty.serve` is
   * `Server ! Resource`), and `apply` would answer a program inside a
   * program, which the discarded-value lint catches at every call
   * site. Flattening it belongs here, once.
   */
  def use[B](body: F[B ! Resource]): B ! Resource = build.flatMap(p => p(body))
  /**
   * Declare a fact of kind `k` about this module; a reader merges it
   * with the rest. Curried, so the value's type comes from `Fact[V]`
   * and a block needs no ascription (fact-declaring):
   *
   * {{{
   *   m.declare(Surface) { case r if r.url == "/board" => … }
   * }}}
   */
  def declare[V](k: Fact[V])(v: V): Module[F] =
    new Module(built.map((p, f) => (p, f.add(k, v))), ready, facts.add(k, v))

  /**
   * The same, for a fact computed INSIDE this module's own installer,
   * so it may read what THIS module installs (fact-declaring).
   *
   * `declare` runs outside the installer, which is why a contribution
   * sees the capabilities that came before it and not its own. That
   * forced a feature to be two pieces — a module installing `Board`
   * and another contributing the routes that use it. With this one it
   * is one:
   *
   * {{{
   *   Module.value[Board](Board(...)).declaring(Surface) {
   *     case r if r.url == "/board" => text(wire[Board].items.mkString(","))
   *   }
   * }}}
   *
   * For an acquired module the value is computed when the module
   * BUILDS (it has to be — there is nothing to read before that), so
   * it reaches the collection but not the early preview.
   */
  def declaring[V](k: Fact[V])(v: F[V]): Module[F] =
    new Module(built.map((p, f) => (p, f.add(k, p(v)))),
               ready, ready.fold(facts)(p => facts.add(k, p(v))))

/**
 * AN INSTANCE PER CONSUMER, not per scope (di-prototype).
 *
 * A `module` installs one value and everyone downstream shares it —
 * which is what most capabilities want. What a `New[A]` installs is
 * the ability to MAKE an `A`: every `fresh[A]` answers a new one.
 *
 * Its `apply` answers a PROGRAM, always, even where nothing has to be
 * closed, and that is the point rather than an oversight: a provider
 * that starts closing what it makes — a connection instead of a
 * counter — changes one line and no consumer moves. A consumer must
 * not know whether what it asks for is released, or it would have to
 * be rewritten every time the answer changes.
 *
 * The instance is released by the region its `fresh` RUNS in, so the
 * caller chooses the lifetime by choosing the region: one per
 * request (`Resource.scoped` inside the handler) or one per
 * application (the region `main` holds open — where thousands of
 * unreleased instances would pile up, which is the trade to know).
 */
@scala.annotation.implicitNotFound("no New[${A}]: nothing installed the ability to MAKE a ${A}.\n`fresh[${A}]` asks for a NEW instance per consumer. A `module[${A}](acquire)(release)` installs ONE\nfor the region, and that one is read with `wire[${A}]`.\nIf an instance per consumer is what you want, the PROVIDER says so:\n  prototype[${A}](make)                     // nothing to release\n  prototype[${A}](acquire)(release)         // released by the region each fresh runs in")
trait New[A]:
  def apply(): A ! Resource

/**
 * The consumer one-liner: a new `A`, in the region this runs in.
 *
 * Written as `wire` AT ANOTHER TYPE, which is what it is: one asks
 * for the thing, the other for the ability to make it, and there is
 * one primitive underneath. The spelling is not cosmetic — as a
 * `using` parameter the compiler prints its own "No given instance
 * … for parameter n of method fresh" and `New`'s message above never
 * reaches the call site (measured, fresh-says-why). Through the
 * context function it does.
 */
inline def fresh[A]: New[A] ?=> (A ! Resource) = wire[New[A]]()

/** an instance per consumer, with nothing to release */
def prototype[A](make: => A): Module[[X] =>> New[A] ?=> X] =
  Module.value[New[A]](new New[A]:
    def apply(): A ! Resource = pure[Resource, A](make))

/** an instance per consumer, released by the region each one runs in */
def prototype[A](acquire: => A)(release: A => Unit): Module[[X] =>> New[A] ?=> X] =
  Module.value[New[A]](new New[A]:
    def apply(): A ! Resource = Resource.acquire(acquire)(release))

/** acquire one capability in the scope; the scope releases it */
def module[A](acquire: => A)(release: A => Unit): Module[[X] =>> A ?=> X] =
  new Module(Resource.acquire(acquire)(release).map(a => (providing[A](a), Facts.empty)))

/**
 * Acquire an `R`, install it as `A`, release it as `R` — the shape an
 * application actually has (di-dogfood): a `FileStore` is opened and
 * closed, and what the program should SEE is `Store`, which has no
 * `close` and should not grow one for this. `module` alone forces
 * those to be the same type, which leaves a real app choosing between
 * installing the concrete type (every consumer over-specified) and a
 * type test in the release (a cast, which this repository refuses).
 *
 * {{{
 *   moduleAs[Store, FileStore](FileStore.open(path))(_.close())
 * }}}
 */
def moduleAs[A, R <: A](acquire: => R)(release: R => Unit): Module[[X] =>> A ?=> X] =
  new Module(Resource.acquire(acquire)(release).map(r => (providing[A](r), Facts.empty)))

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
  /**
   * Capabilities this chain installs MORE THAN ONCE — read off the
   * plan, so nothing is built to find out. The second install wins
   * and the first is acquired for nothing, which is what a test
   * double does ON PURPOSE (`base and Module.value[Db](fake)`); that
   * is why this is a report and not an error (di-multibind).
   */
  inline def shadowed: Vector[String] =
    val p = m.plan
    p.diff(p.distinct).distinct

object Module:
  /** a module with nothing to build or release — a test double, a config value */
  def ready[F[_]](p: Providing[F]): Module[F] =
    new Module(pure[Resource, (Providing[F], Facts)]((p, Facts.empty)), Some(p))
  /** the same, from the bare value */
  def value[A](a: A): Module[[X] =>> A ?=> X] = ready(providing[A](a))

  /**
   * A module that installs NOTHING — the identity installer.
   *
   * It exists for the CONTRIBUTOR (di-facts-examples): a module that
   * adds its routes, its health check, its migration to a collection
   * somebody else reads, and offers no capability of its own. The
   * alternative was installing a `Unit` nobody wants, and a fact
   * cannot be declared on the module that installs the capability it
   * reads anyway — `declare` runs OUTSIDE that installer, so a
   * contribution reading `wire[Board]` belongs to a module written
   * `Board ?=> Module[…]`, which is what this makes writable.
   */
  val nothing: Module[[X] =>> X] = ready(Providing([X] => (body: X) => body))

  /** the contributor's one-liner: install nothing, declare one fact */
  def contributing[V](k: Fact[V])(v: V): Module[[X] =>> X] = nothing.declare(k)(v)

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
    // an APPLIED capability keeps its argument: a prototype reads as
    // `New[Conn]`, not `New`, which is the difference between a plan
    // and a list of type constructors (di-prototype)
    def name(using q: Quotes)(t: q.reflect.TypeRepr): String =
      import q.reflect.*
      t.dealias match
        case AppliedType(tc, args) =>
          s"${tc.typeSymbol.name}[${args.map(a => a.typeSymbol.name).mkString(", ")}]"
        case other => other.typeSymbol.name
    val names = chain(TypeRepr.of[F[Module.Marker]], TypeRepr.of[Module.Marker]).map(t => name(t._1))
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
        a.asType match
          case '[at] => rest.asType match
            case '[rt] =>
              '{ (x: at) ?=> ${ body(more, '{ Installed($name, $cls, x) } :: acc).asExprOf[rt] } }
    val collect = body(levels, Nil).asExprOf[F[Vector[Installed]]]
    '{ $m.build.map(p => p[Vector[Installed]]($collect)) }
  /** the end of the chain the plan walks to; never inhabited */
  sealed trait Marker
