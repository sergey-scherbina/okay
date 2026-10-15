package okay.std

import okay.{!, Answers, Effect, Handler, Run, defer, effect, pure}
import okay.core.{Eff, Paths, Row, Split}
import okay.one.Cont
import okay.one.handler.{Deep, Recovering, Handler as OneHandler}

/**
 * How a forwarded operation reports its failure to the scope that
 * forwarded it, on okay-freer's road: a decoration of the operation
 * (`guard`), which `okay-async` supplies for `Async` (`AsyncFailing`).
 *
 * On the kernel the scope needs none of it: a handler that fails an
 * operation answers its `k` with the failure (`k.failed(e)`, okay-core),
 * which raises it where the operation was, inside the scope's guard —
 * whose finalizers run as for any other error. Kept for okay-freer's
 * modules (okay-async) until they move.
 */
trait Failing[F[+_]]:
  def guard[X](e: F[X], onFailure: () => Unit): F[X]

object Failing:
  /** okay-freer's `Pure` has no failure channel to decorate */
  given pure: Failing[okay.freer.Pure] with
    def guard[X](e: Nothing, onFailure: () => Unit): Nothing = e

/**
 * AN OPERATION THAT NEVER RESUMES — Koka's `final ctl`, OCaml 5's
 * `discontinue` seen from the operation's side (bracket-final,
 * 2026-09-26). `raise`, `abort`, a `None` of `Maybe`, `Chronicle`'s
 * `halt`, a `Choose` with no alternatives: no handler in this library
 * calls their continuation, and their meaning says none should.
 *
 * An operation is final by its VALUE where it has to be (`Maybe`,
 * `Choose`), so `isFinal` is a method, not only the type.
 *
 * On okay-freer's road `Resource.run` read this marker on every
 * operation it forwarded and released before a final one. On the
 * kernel an operation goes to its handler by its path and never
 * passes through the scope, so the scope reads nothing: its release
 * is a GUARD (okay-core's `Guard`), run when the scope is left by its
 * end, an error, an ABORT or a cancel. An abort is the kernel's
 * `Aborting` handler (`runEither`, `runOption`, `runUnsafe`): it
 * unwinds the levels it drops, so a scope inside is released. A
 * handler that drops `k` as a value instead (a `Deep` clause that
 * does not call it: `Maybe.option`'s `None`, `Chronicle`'s `halt`, a
 * `Choose` with no alternatives) unwinds nothing, and the scope's
 * release does not run. The marker stays as what these operations
 * say of themselves.
 */
trait Final:
  def isFinal: Boolean = true

/**
 * The resource effect, tied to no other effect: acquire inside a
 * scope, and the release is the SCOPE's obligation — it runs when the
 * scope ends, in reverse acquisition order, whatever else the program
 * does (region style, as in the region calculus / cats Resource).
 */
enum Resource[+A] derives Effect:
  /** acquire a resource; the scope releases it at its end */
  case Acquire[R](make: () => R, release: R => Unit) extends Resource[R]

object Resource {

  /**
   * RUN EVERY FINALIZER, WHATEVER ONE OF THEM THROWS (release-all-finalizers,
   * 2026-09-26). This used to be `fin.foreach(_())`, and a throwing
   * release then did two things wrong. It skipped every release after it,
   * so those resources leaked. And, when the release was happening
   * because the program had FAILED, its exception replaced the program's,
   * so the cause was lost. ZIO runs all of a Scope's finalizers and
   * composes their failures with the original into one `Cause`. The JVM's
   * own shape for the same thing is try-with-resources: the first error
   * wins, and the others are attached to it with `addSuppressed`.
   *
   * A loop, not a fold that could stop: every finalizer is called exactly
   * once, in list order (the lists here are newest first, so release
   * order is the reverse of acquisition).
   */
  private def runAll(fin: List[() => Unit]): List[Throwable] =
    var failed = List.empty[Throwable]
    var rest = fin
    while rest.nonEmpty do
      try rest.head()
      catch { case t: Throwable => failed = t :: failed }
      rest = rest.tail
    failed.reverse

  /** release everything; the first failure is thrown, with the later
   * ones suppressed on it */
  def releaseAll(fin: List[() => Unit]): Unit =
    runAll(fin) match
      case Nil => ()
      case first :: more =>
        more.foreach(first.addSuppressed)
        throw first

  /** release everything because `cause` is propagating; every failure of
   * a release is attached to `cause`, which the caller rethrows */
  def releaseAfter(fin: List[() => Unit], cause: Throwable): Unit =
    runAll(fin).foreach(t => if t ne cause then cause.addSuppressed(t))

  /**
   * ONE ACQUISITION'S RELEASE, RUN ONCE: the guard holding it may be left more than one way (its end, an error, an
   * abort) and, in a `k` resumed more than once, more than once — the release runs the first time only.
   */
  private final class Held(release: () => Unit) extends (() => Unit):
    private var held = true
    def apply(): Unit =
      if held then
        held = false
        release()

  /**
   * THE SCOPE'S RELEASE OF ONE ACQUISITION, A GUARD (okay-core's `Recovering`): at the end of what it holds, the
   * release, then the value; an error thrown in it, the release with its failure suppressed on the error, which
   * goes on; an abort or a cancel, the release as the guard's finalizer. One a guard, nested as the acquisitions
   * are: the newest is the innermost, released first.
   */
  private final class Releasing[T <: Row, A](held: Held) extends Recovering[T, A, A]:
    def ret(a: A): okay.one.![A, T] = Cont.defer { held(); Cont.pure(a) }
    override def finalizer: okay.one.![Unit, T] | Null = Cont.defer { held(); Cont.pure(()) }
    def recover(e: Throwable): okay.one.![A, T] | Null =
      releaseAfter(List(held), e)
      throw e

  /** `p` with `release` guarding it: run once, whichever way `p` is left */
  private[std] def releasing[T <: Row, A](p: okay.one.![A, T], release: () => Unit): okay.one.![A, T] =
    p.guard(Releasing[T, A](Held(release)))

  /** acquire inside the enclosing Resource.run scope */
  inline def acquire[R](make: => R)(release: R => Unit): R ! Resource =
    effect[Resource, R](Acquire(() => make, release))

  /**
   * Open the scope and KEEP it open: acquire everything now, hand
   * back the value and a closer that releases in reverse acquisition
   * order. For the scope whose end belongs to somebody else — a
   * Spring context's close, a main's shutdown hook — where `run`
   * cannot own the end. A throw during an acquisition releases what
   * was acquired before it and rethrows; the closer is idempotent.
   * Resource only: a row to forward has no home to forward to here.
   * Each acquisition is answered in place (the kernel's `Answering`).
   */
  def open[A](a: A ! Resource): (A, () => Unit) = {
    var fin = List.empty[() => Unit]
    def close(): Unit = { val f = fin; fin = Nil; releaseAll(f) }
    val acquiring = Handler.answering[Resource]([X] => (op: Resource[X]) => op match
      case Acquire(mk, rel) =>
        val r = mk()
        fin = (() => rel(r)) :: fin
        r)
    try (a.handle(acquiring).run, () => close())
    catch
      case t: Throwable =>
        val f = fin; fin = Nil
        releaseAfter(f, t)
        throw t
  }

  /**
   * The region as an expression: open, run, release, answer — for the
   * scope that is the whole story, which is what a per-call region
   * usually is (di-prototype). `run` forwards the rest of the
   * program's effects; this one has nothing to forward.
   *
   * {{{
   *   def handle(r: Request): New[Conn] ?=> Response =
   *     Resource.scoped(fresh[Conn].map(c => answer(r, c)))
   * }}}
   */
  def scoped[A](a: A ! Resource): A = run(a).run

  /**
   * The handler as a value: `p.handle(Resource.region)` — every acquisition released at the end, in reverse. A
   * clause given `k` (the kernel's `Deep`): the acquisition made, and the rest resumed with it under a guard that
   * releases it (`Releasing`) — at the rest's end, at an error thrown in it, at an abort or a cancel passing out of
   * it.
   */
  def region: Handler[Resource, [A] =>> A] = new Handler[Resource, [A] =>> A]:
    def kernel[T <: Row, A]: OneHandler[Resource, T, A, A] = new Deep[Resource, T, A, A]:
      def ret(a: A): okay.one.![A, T] = Cont.pure(a)
      def apply[X](op: Resource[X], k: X => okay.one.![A, T]): okay.one.![A, T] = op match
        case Acquire(mk, rel) => Cont.defer {
          val r = mk()
          releasing(k(r), () => rel(r))
        }

  /**
   * The scope: run the region, wherever `Resource` is among the
   * program's effects, forwarding the rest of them. Every acquired
   * release runs when the scope ends — at its value, at an error
   * thrown in it (a JVM exception during a step, or a failure a
   * handler outside answers an operation with: `k.failed`), at an
   * abort or a cancel passing out of it — in reverse acquisition
   * order, a release's failure suppressed on the error that is
   * propagating. Each release is a guard of okay-core's machine, around
   * the rest of the scope after its acquisition.
   *
   * What a guard sees, and so what this scope does:
   *  - an ABORT by the kernel's `Aborting` handler outside (`raise`
   *    under `runEither`, `abort` under `runOption`) unwinds the scope:
   *    released (bracket-final);
   *  - a handler outside that DROPS the continuation as a value (a
   *    `Deep` clause not calling `k`: `Maybe.option`'s `None`,
   *    `Chronicle`'s `halt`, a pruned `Choose`) unwinds nothing: the
   *    release does not run (see `Final`). `bracketNow` (a
   *    `try/finally`) is the release that holds even then;
   *  - a multi-shot handler INSIDE replays only the scope's inner part,
   *    so each acquire still releases exactly once;
   *  - a multi-shot handler OUTSIDE the scope (a `choose` passing out of
   *    it, resumed once per alternative) puts the guard back with each
   *    resumption: the release runs once, at the end of the FIRST
   *    branch to leave the scope — the later branches run on with the
   *    resource released. okay-freer's road shared it among the
   *    branches and released it after the last (logic-cut-releases,
   *    `Forked`); the kernel's guard has no hook for a resumption.
   */
  def run[A, Es[_]](a: A ! Es)(using s: Split[Es, Resource]): A ! s.Rest = a.handle(region)
}

/**
 * ACQUIRE, USE, RELEASE — with the use-program's effects FORWARDED
 * (bracket-forwards-no-effects, 2026-09-26): `use` may perform anything
 * in its effects, and an outer handler answers it, as it would anywhere
 * else. The release is a guard of okay-core's machine around `use`
 * (`Resource.run`'s, with no effect performed for the acquisition): it
 * runs when `use` finishes, when a step throws, when a handler outside
 * fails an operation of `use` (`k.failed`), and when an abort or a
 * cancel passes out of it.
 *
 * An ABORT from `use` releases too, when the handler outside is the
 * kernel's `Aborting` (`runEither`, `runOption`). What a guard cannot
 * see is a handler that drops the continuation as a value (a `Deep`
 * clause not calling `k`: `Maybe`'s `None`, `halt`, a pruned branch) —
 * see `Resource.run`. `bracketNow`'s `try/finally` is the release that
 * holds even then.
 *
 * This is the name cats (`bracket`), ZIO (`acquireReleaseWith`) and kyo
 * give the effect-polymorphic form. It belonged to `bracketNow` until
 * bracket-forwards-no-effects.
 */
def bracket[R, A, Es[_]](acquire: => R)(release: R => Unit)(use: R => A ! Es): A ! Es = new Run[Es, A]:
  // the acquire when the program RUNS, each time it does — a program
  // built once and run twice acquires twice (the test "releases after
  // them"); `use(r)` built inside the guard, so a throw building it
  // releases too
  def on[T <: Row](ps: Paths[Es, T]): Eff[T, A] = Cont.defer {
    val r = acquire
    Resource.releasing(Cont.defer(use(r).fit(ps)), () => release(r))
  }

/**
 * Bracket over any Answers-able effect F (Async, Produce, Pure, ...),
 * RUN NOW: acquire, use, release — the use-program runs to completion
 * inside one step, by `Answers[F]`, so no outer handler can skip or
 * repeat the release; a fiber's cancellation is an interrupt
 * exception, and the catch sees it. The price of that guarantee is
 * that nothing in `use` reaches an outer handler: every operation of F
 * is answered in place. For a release around effects that ARE
 * forwarded, use `bracket` above (it was this function's name until
 * bracket-forwards-no-effects, 2026-09-26).
 */
def bracketNow[R, A, F[_] : Answers](acquire: => R)(release: R => Unit)(use: R => A ! F): A ! F =
  defer:
    val r = acquire
    // not `try … finally release(r)`: a release that throws from a
    // `finally` REPLACES the exception of the `try`, and the cause is
    // lost (release-all-finalizers) — the use's error wins, the
    // release's is suppressed on it
    val a =
      try Stream.runWith(use(r))
      catch { case t: Throwable => Resource.releaseAfter(List(() => release(r)), t); throw t }
    release(r)
    pure[A, F](a)
