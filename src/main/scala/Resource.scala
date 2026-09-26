package okay

import okay.!.*
import okay.Row.plus
import scala.annotation.tailrec

/**
 * How a forwarded operation reports its failure to the scope that
 * forwarded it. Concrete failure semantics belong to the effect that
 * can fail — `okay-async` supplies the `Async` implementation — and
 * `Resource.run` below is its one consumer in the core: when a
 * forwarded operation fails out past the scope, its finalizers must
 * still run, which is what `guard` hooks into.
 */
trait Failing[F[+_]]:
  def guard[X](e: F[X], onFailure: () => Unit): F[X]

object Failing:
  /** Pure has no failure channel to decorate. `okay.Pure` because this
   * file's own `import okay.!.*` shadows it with the Free.Return case
   * (see Effects.scala's note on the same trap). */
  given pure: Failing[okay.Pure] with
    def guard[X](e: Nothing, onFailure: () => Unit): Nothing = e

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

  /** acquire inside the enclosing Resource.run scope */
  inline def acquire[R](make: => R)(release: R => Unit): R ! Resource =
    effect(Acquire(() => make, release))

  /**
   * The scope: run the region, forwarding the effects F. Every
   * acquired release runs when the scope ends — at its value, or at a
   * JVM exception thrown during a step — in reverse acquisition
   * order. Run this handler OUTERMOST: turn aborts into values inside
   * the scope (runEither before run), so no abortive handler discards
   * the finalizers; a multi-shot handler inside replays only the
   * scope's inner part, so each acquire still releases exactly once.
   * A forwarded F-operation suspends the scope with its finalizers
   * carried into the residual — they run when the residual completes
   * (abandoning the residual abandons them).
   */
  /**
   * Open the scope and KEEP it open: acquire everything now, hand
   * back the value and a closer that releases in reverse acquisition
   * order. For the scope whose end belongs to somebody else — a
   * Spring context's close, a main's shutdown hook — where `run`
   * cannot own the end. A throw during an acquisition releases what
   * was acquired before it and rethrows; the closer is idempotent.
   * Resource only: a row to forward has no home to forward to here.
   */
  def open[A](a: A ! Resource): (A, () => Unit) = {
    var fin = List.empty[() => Unit]
    def close(): Unit = { val f = fin; fin = Nil; f.foreach(_()) }
    var x = a
    try
      while true do (x.resume: @unchecked) match
        case Return(v) => return (v, () => close())
        case Inject(Acquire(mk, rel)) =>
          val r = mk()
          fin = (() => rel(r)) :: fin
          return (r, () => close())
        case Bind(Inject(Acquire(mk, rel)), k) =>
          val r = mk()
          fin = (() => rel(r)) :: fin
          x = k(r)
      throw MatchError(x)
    catch
      case t: Throwable =>
        close()
        throw t
  }

  /**
   * The region as an expression: open, run, release, answer — for the
   * scope that is the whole story, which is what a per-call region
   * usually is (di-prototype). `run` forwards a row and needs both
   * type arguments spelled; this one has nothing to forward.
   *
   * {{{
   *   def handle(r: Request): New[Conn] ?=> Response =
   *     Resource.scoped(fresh[Conn].map(c => answer(r, c)))
   * }}}
   */
  def scoped[A](a: A ! Resource): A =
    !.run(run[A, Nothing](a)(using new Failing[Nothing]:
      def guard[X](e: Nothing, onFailure: () => Unit): Nothing = e))

  def run[A, F[+_]](a: A ! Resource + F)(using failing: Failing[F]): A ! F = {
    def releaseAll(fin: List[() => Unit]): Unit = fin.foreach(_())

    /** user code under the CURRENT finalizer list: a throw releases
     * everything acquired so far and propagates. Every place the walk
     * runs code it did not write — the tree's own `resume` (Delay
     * thunks, continuations), an `Acquire`'s `mk`, a continuation
     * `k` — goes through here; the releases that END a walk do not,
     * so a throwing finalizer is not released twice. */
    def guarded[T](fin: List[() => Unit])(body: => T): T =
      try body
      catch { case t: Throwable => releaseAll(fin); throw t }

    def _loop(fin: List[() => Unit])(x: A ! Resource + F): A ! F = loop(fin)(x)

    // `split`, not `<|>` (operator-followups, 2026-09-16): no Either per
    // operation. The while-and-return shape it replaced existed so one
    // catch could see the current finalizer list; `guarded` gives each
    // throwing call that list instead, and the loop is a tail call.
    @tailrec def loop(fin: List[() => Unit])(x: A ! Resource + F): A ! F =
      (guarded(fin)(x.resume): @unchecked) match
        case Return(a) =>
          releaseAll(fin)
          Return(a)
        case Inject(e) => split[Resource, F](e) {
            case Acquire(mk, rel) =>
              val r = guarded(fin)(mk())
              releaseAll((() => rel(r)) :: fin)
              Return(r): A ! F
          } { e => Inject(failing.guard(e, () => releaseAll(fin))).map { a => releaseAll(fin); a } }
        case Bind(Inject(e), k) => split[Resource, F](e) {
            case Acquire(mk, rel) =>
              val r = guarded(fin)(mk())
              val f2 = (() => rel(r)) :: fin
              loop(f2)(guarded(f2)(k(r)))
          } { e =>
            // k(y) runs USER code (the composed continuation) at the
            // outer handler's call site — a throw there must not skip
            // the finalizers, so it is guarded like every other call
            Inject(failing.guard(e, () => releaseAll(fin))).flatMap { y => _loop(fin)(guarded(fin)(k(y))) }
          }

    loop(Nil)(a)
  }
}
/**
 * ACQUIRE, USE, RELEASE — with the use-program's effects FORWARDED
 * (bracket-forwards-no-effects, 2026-09-26): `use` may perform anything
 * in F, and an outer handler answers it, as it would anywhere else.
 * It is `Resource` in one expression: the acquire is the scope's one
 * `Resource.acquire`, and the scope is `Resource.run` around `use`, so
 * the release runs when `use` finishes, when a step throws, and when a
 * forwarded operation fails out on its handler (`Failing[F]`: `Async`'s
 * thrown `Run` or failed `Await`; rows beyond `Pure`/`Async` need
 * `import okay.AsyncFailing.anyRow`, as `Resource.run` does).
 *
 * What it inherits from `Resource.run`, said here too because a reader
 * of `bracket` expects the other kind: an ABORTIVE handler outside the
 * scope (`runEither` of a raise inside `use`, `Maybe.run` of a `None`)
 * drops the rest of the program, release included. Turn aborts into
 * values INSIDE `use` for a guaranteed release. `bracketNow` gives that
 * guarantee without the forwarding.
 *
 * This is the name cats (`bracket`), ZIO (`acquireReleaseWith`) and kyo
 * give the effect-polymorphic form. It belonged to `bracketNow` until
 * this lane.
 */
def bracket[R, A, F[+_]](acquire: => R)(release: R => Unit)(use: R => A ! F)(using Failing[F]): A ! F =
  // DELAYED: `Resource.run`, like every handler loop here, walks the
  // program up to its first forwarded operation when it is CALLED, so
  // without this the acquire would run when the program is BUILT, and
  // a program built once and run twice would acquire once (the test
  // "releases after them" caught exactly that)
  Free.delay(() => Resource.run[A, F](Resource.acquire(acquire)(release).plus[F].flatMap(r => use(r).plus[Resource])))

/**
 * Bracket over any Handler-able row F (Async, Produce, Pure, ...), RUN
 * NOW: acquire, use, release — the use-program runs to completion inside
 * one suspension, by the row's comonadic `Handler`, so no outer handler
 * can skip or repeat the release; a fiber's cancellation is an interrupt
 * exception, and the finally sees it. The price of that guarantee is
 * that nothing in `use` reaches an outer handler: every effect of F is
 * answered in place. For a release around effects that ARE forwarded,
 * use `bracket` above (it was this function's name until
 * bracket-forwards-no-effects, 2026-09-26).
 */
def bracketNow[R, A, F[+_] : Handler](acquire: => R)(release: R => Unit)(use: R => A ! F): A ! F =
  pure[F, Unit](()).flatMap: _ =>
    val r = acquire
    try pure(use(r).runWith)
    finally release(r)

/** The class IS the whole identity: Resource has no parameter but its
 * (erased) answer type, so splitting a row on it is a TOTAL test —
 * said once here, rather than as a "cannot be checked at runtime"
 * warning at every use site of a test that is in fact complete. */
