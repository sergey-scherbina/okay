package okay2

import scala.annotation.implicitNotFound

/**
 * The capability pair, in Scala 2 (the Scala 3 core's Provide.scala):
 * `provide` INSTALLS values for a block, `wire` READS one by its type,
 * and with the two the dependency-injection story is compile-time
 * resolution, implicit scopes as the object graph, zero framework.
 *
 * WHAT IS DIFFERENT, AND WHY. Scala 3 installs a given for a block
 * with a context function, `A ?=> B`; Scala 2 has no such type, and
 * an `implicit` lambda parameter may be ONE identifier. So a body that
 * takes several capabilities is CURRIED — `implicit db => implicit log
 * => app` — which is exactly the chain `Providing.and` composes, so
 * the flat form and the composable form agree on what a body is. A
 * missing capability is a compile error in both.
 *
 * Not here: `plan`/`exports`/`shadowed`, which read the chain off the
 * type with a macro (backlog `okay2-module-plan`), and the Scala 3
 * core's `ctxMonad`: okay2 has the classes since stage 11, but Scala 2
 * has no context functions for one to range over.
 */
trait Provides {
  def provide[A1, B](a1: A1)(body: A1 => B): B = body(a1)
  def provide[A1, A2, B](a1: A1, a2: A2)(body: A1 => A2 => B): B = body(a1)(a2)
  def provide[A1, A2, A3, B](a1: A1, a2: A2, a3: A3)(body: A1 => A2 => A3 => B): B = body(a1)(a2)(a3)
  def provide[A1, A2, A3, A4, B](a1: A1, a2: A2, a3: A3, a4: A4)(body: A1 => A2 => A3 => A4 => B): B = body(a1)(a2)(a3)(a4)
  def provide[A1, A2, A3, A4, A5, B](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5)(body: A1 => A2 => A3 => A4 => A5 => B): B = body(a1)(a2)(a3)(a4)(a5)
  def provide[A1, A2, A3, A4, A5, A6, B](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6)(body: A1 => A2 => A3 => A4 => A5 => A6 => B): B = body(a1)(a2)(a3)(a4)(a5)(a6)
  def provide[A1, A2, A3, A4, A5, A6, A7, B](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7)(body: A1 => A2 => A3 => A4 => A5 => A6 => A7 => B): B = body(a1)(a2)(a3)(a4)(a5)(a6)(a7)
  def provide[A1, A2, A3, A4, A5, A6, A7, A8, B](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8)(body: A1 => A2 => A3 => A4 => A5 => A6 => A7 => A8 => B): B = body(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)

  /** one installer: `providing[A](a)` holds an `A` for later. Prefer
   * the explicit type argument: inference would pick the runtime
   * refinement, and the capability you install is the trait */
  def providing[A](a: A): Providing[({ type L[X] = A => X })#L] =
    new Providing[({ type L[X] = A => X })#L](new Providing.Run[({ type L[X] = A => X })#L] {
      def apply[X](body: A => X): X = body(a)
    })

  /** the consumer one-liner: `wire[Db].q` pulls the ambient capability
   * by naming its type — Reader's `ask` on implicits; a missing one is
   * a compile error */
  def wire[A](implicit a: A): A = a

  /** a new `A`, in the region this runs in — `wire` at another type */
  def fresh[A](implicit n: New[A]): A ! Resource = n()

  /** an instance per consumer, with nothing to release */
  def prototype[A](make: => A): Module[({ type L[X] = New[A] => X })#L] =
    Module.value[New[A]](new New[A] {
      def apply(): A ! Resource = pure[Resource, A](make)
    })

  /** an instance per consumer, released by the region each one runs in */
  def prototype[A](acquire: => A, release: A => Unit): Module[({ type L[X] = New[A] => X })#L] =
    Module.value[New[A]](new New[A] {
      def apply(): A ! Resource = Resource.acquire(acquire)(release)
    })

  /** acquire one capability in the scope; the scope releases it */
  def module[A](acquire: => A)(release: A => Unit): Module[({ type L[X] = A => X })#L] =
    new Module(Resource.acquire(acquire)(release).map(a => (providing[A](a), Facts.empty)))

  /** acquire an `R`, install it as `A`, release it as `R` — a
   * `FileStore` is opened and closed, and what the program should SEE
   * is `Store` */
  def moduleAs[A, R <: A](acquire: => R)(release: R => Unit): Module[({ type L[X] = A => X })#L] =
    new Module(Resource.acquire(acquire)(release).map(r => (providing[A](r), Facts.empty)))
}

/**
 * The COMPOSABLE half of provide: installers as VALUES, composed
 * applicatively — no nesting, no cap. One installer carries
 * `F[X] = A => X`; `and` composes to `F[G[X]] = A => G[X]`, the
 * curried chain assembled by values. The RIGHT operand of `and` is the
 * inner layer, so it wins under nearest-wins: `base and
 * providing[Log](testLog)` overrides base's Log.
 */
final class Providing[F[_]](val run: Providing.Run[F]) {
  /** compose installers; the right side becomes the inner layer */
  def and[G[_]](that: Providing[G]): Providing[({ type L[X] = F[G[X]] })#L] =
    new Providing[({ type L[X] = F[G[X]] })#L](new Providing.Run[({ type L[X] = F[G[X]] })#L] {
      def apply[X](body: F[G[X]]): X = that.run(run[G[X]](body))
    })

  /** install everything and run the body */
  def apply[B](body: F[B]): B = run(body)
}

object Providing {
  /** the installer's action, ∀X — a trait, since Scala 2 has no
   * polymorphic function values */
  trait Run[F[_]] { def apply[X](body: F[X]): X }
}

/**
 * A KIND of fact a module may declare about itself: whoever READS the
 * fact defines the key, and HOW TWO DECLARATIONS MERGE is the key's
 * own rule (okay2 has no `Monoid`; the two functions are the one it
 * would carry). Keys compare by identity, so a `Fact` is an object,
 * held as a val.
 */
abstract class Fact[V](val empty: V, val merge: (V, V) => V)

object Fact {
  implicit val same: Same[Fact] = Same.byIdentity[Fact]
}

/** what a module has declared about itself, by kind */
final class Facts private (private val m: Map[Fact[_], Any]) {
  /** the one cast, isolated: a value under `k` was put there by
   * `add(k: Fact[V], v: V)`, the only writer, and `k`'s type parameter
   * IS that V. A map keyed by typed tokens is heterogeneous by
   * construction and the type system has no dependent map to say so. */
  private def stored[V](v: Any): V = v.asInstanceOf[V]

  def get[V](k: Fact[V]): V = m.get(k).fold(k.empty)(stored[V])
  def add[V](k: Fact[V], v: V): Facts = new Facts(m.updated(k, k.merge(get(k), v)))
  private def addOne[V](k: Fact[V], v: Any): Facts = add(k, stored[V](v))
  /** every kind either side declared, merged by its own rule */
  def ++(that: Facts): Facts = that.m.foldLeft(this) { case (acc, (k, v)) => acc.addOne(k, v) }
  def isEmpty: Boolean = m.isEmpty
}

object Facts {
  val empty: Facts = new Facts(Map.empty)
}

/**
 * A module is an installer that has not been built yet: `Providing[F]`
 * holds READY values, a `Module[F]` builds them in the `Resource`
 * effect — so opening a pool, starting a server, and closing both at
 * the end of the scope, in reverse order, is the region's obligation
 * and not the caller's.
 *
 * {{{
 *   val db   = module[Db](Db.open(url))(_.close)
 *   val pool = db and (implicit d => module[Pool](Pool.over(wire[Db]))(_.close))
 *   val app  = pool { implicit d => implicit p => wire[Pool].borrow() }   // : Int ! Resource
 *   Resource.scoped(app)
 * }}}
 *
 * `and` has two forms: a plain module, and one built INSIDE the left's
 * context (`F[Module[G]]`, a function of what the left installs) — so
 * an acquisition naming a capability no module before it installs
 * does not compile. Left acquires first, so the right (inner)
 * releases first. A module with nothing to acquire is `ready`, and
 * when the LEFT side of `and` is ready the right is applied at once,
 * so its `facts` are known before any scope opens.
 */
final class Module[F[_]](val built: (Providing[F], Facts) ! Resource,
                         val ready: Option[Providing[F]] = None,
                         val facts: Facts = Facts.empty) {

  /** the installer alone */
  def build: Providing[F] ! Resource = built.map(_._1)

  /** compose with a module that needs nothing from this one; the right
   * operand is the inner layer */
  def and[G[_]](that: Module[G]): Module[({ type L[X] = F[G[X]] })#L] = ready match {
    case Some(p) =>
      new Module(built.flatMap { case (_, f1) => that.built.map { case (q, f2) => (p and q, f1 ++ f2) } },
                 that.ready.map(q => p and q), facts ++ that.facts)
    case None =>
      new Module(built.flatMap { case (p, f1) => that.built.map { case (q, f2) => (p and q, f1 ++ f2) } },
                 None, facts)
  }

  /** compose with a module built INSIDE this one's context: the
   * dependency graph is written in the composition and checked by
   * the compiler */
  def and[G[_]](that: F[Module[G]]): Module[({ type L[X] = F[G[X]] })#L] = ready match {
    case Some(p) =>
      // nothing to acquire on the left: the right exists NOW, and its
      // facts with it, so readiness and the preview travel
      val inner = p(that)
      new Module(built.flatMap { case (_, f1) => inner.built.map { case (q, f2) => (p and q, f1 ++ f2) } },
                 inner.ready.map(q => p and q), facts ++ inner.facts)
    case None =>
      new Module(built.flatMap { case (p, f1) => p(that).built.map { case (q, f2) => (p and q, f1 ++ f2) } },
                 None, facts)
  }

  /** SEVERAL CONTRIBUTORS, ONE COLLECTION — a multibinder: each module
   * declares its piece as a fact of kind `k`, and this merges every
   * piece by the kind's rule and installs the result as a capability */
  def installing[V](k: Fact[V]): Module[({ type L[X] = F[V => X] })#L] =
    new Module(built.map { case (p, f) => (p and providing[V](f.get(k)), f) },
               ready.map(p => p and providing[V](facts.get(k))),
               facts)

  /** install everything and run the body inside the scope */
  def apply[B](body: F[B]): B ! Resource = build.map(p => p(body))

  /** the same, for a body that is ITSELF a program in the scope — a
   * server that acquires further */
  def use[B](body: F[B ! Resource]): B ! Resource = build.flatMap(p => p(body))

  /** declare a fact of kind `k` about this module; a reader merges it
   * with the rest */
  def declare[V](k: Fact[V])(v: V): Module[F] =
    new Module(built.map { case (p, f) => (p, f.add(k, v)) }, ready, facts.add(k, v))

  /** the same, for a fact computed INSIDE this module's own installer,
   * so it may read what THIS module installs; for an acquired module it
   * is computed when the module builds, so it reaches the collection
   * but not the early preview */
  def declaring[V](k: Fact[V])(v: F[V]): Module[F] =
    new Module(built.map { case (p, f) => (p, f.add(k, p(v))) },
               ready, ready.fold(facts)(p => facts.add(k, p(v))))
}

object Module {
  /** a module with nothing to build or release — a test double, a config value */
  def ready[F[_]](p: Providing[F]): Module[F] =
    new Module(pure[Resource, (Providing[F], Facts)]((p, Facts.empty)), Some(p))

  /** the same, from the bare value */
  def value[A](a: A): Module[({ type L[X] = A => X })#L] = ready(providing[A](a))

  /** a module that installs NOTHING — the identity installer, for the
   * CONTRIBUTOR that adds its routes or its health check to a
   * collection somebody else reads and offers no capability of its own */
  val nothing: Module[({ type L[X] = X })#L] =
    ready(new Providing[({ type L[X] = X })#L](new Providing.Run[({ type L[X] = X })#L] {
      def apply[X](body: X): X = body
    }))

  /** the contributor's one-liner: install nothing, declare one fact */
  def contributing[V](k: Fact[V])(v: V): Module[({ type L[X] = X })#L] = nothing.declare(k)(v)
}

/**
 * AN INSTANCE PER CONSUMER, not per scope. A `module` installs one
 * value and everyone downstream shares it; what a `New[A]` installs is
 * the ability to MAKE an `A`: every `fresh[A]` answers a new one. Its
 * `apply` answers a PROGRAM, always, even where nothing has to be
 * closed: a provider that starts closing what it makes changes one
 * line and no consumer moves. The instance is released by the region
 * its `fresh` RUNS in.
 */
@implicitNotFound("no New[${A}]: nothing installed the ability to MAKE a ${A}.\n`fresh[${A}]` asks for a NEW instance per consumer. A `module[${A}](acquire)(release)` installs ONE\nfor the region, and that one is read with `wire[${A}]`.\nIf an instance per consumer is what you want, the PROVIDER says so:\n  prototype[${A}](make)                     // nothing to release\n  prototype[${A}](acquire, release)         // released by the region each fresh runs in")
trait New[A] {
  def apply(): A ! Resource
}
