package okay2

import scala.annotation.{implicitNotFound, tailrec}
import Free.{Return, Inject, Bind}
import Split.split

/**
 * How a forwarded operation reports its failure to the scope that
 * forwarded it. `Resource.runAt` hands every non-Resource operation to
 * the outer handler and keeps the scope's finalizers in the residual;
 * when that operation fails OUT THERE the residual — finalizers
 * included — would be abandoned, so the scope hooks `guard` in first.
 * Concrete failure semantics belong to the effect that can fail:
 * okay2-async supplies the `Async` instance and the row-wide one
 * (`import okay2.async._`). The core has only `Pure`, which has no
 * failure channel to decorate. There is NO identity default for an
 * arbitrary row: a row that went silently unguarded is the defect this
 * hook exists to prevent (the Scala 3 core measured it).
 */
@implicitNotFound("no Failing[${F}]: the row a Resource scope forwards to must say how a forwarded operation reports failure.\nA Pure row needs nothing; a row with Async in it gets its instance from `import okay2.async._`;\na test effect that cannot fail says so with its own `implicit val`.")
trait Failing[F <: Row] {
  def guard[X](e: F#Op[X], onFailure: () => Unit): F#Op[X]
}

object Failing {
  /** Pure has no failure channel to decorate */
  implicit val pure: Failing[Pure] = new Failing[Pure] {
    def guard[X](e: Nothing, onFailure: () => Unit): Nothing = e
  }

  /** an effect whose operations cannot fail on the outer handler —
   * said EXPLICITLY by whoever knows (a test effect, a pure interpreter),
   * never inferred: `implicit val f: Failing[Produce] = Failing.never` */
  def never[F <: Row]: Failing[F] = new Failing[F] {
    def guard[X](e: F#Op[X], onFailure: () => Unit): F#Op[X] = e
  }
}

/**
 * The resource effect, tied to no other effect: acquire inside a
 * scope, and the release is the SCOPE's obligation — it runs when the
 * scope ends, in reverse acquisition order, whatever else the program
 * does (region style, as in the region calculus / cats Resource). The
 * class IS the whole identity: `Resource` has no parameter but its
 * erased answer, so splitting a row on it is a TOTAL test.
 */
sealed trait Resource extends Row { type Op[+A] = Resource.Op[A] }

object Resource {
  sealed trait Op[+A]
  /** acquire a resource; the scope releases it at its end */
  final case class Acquire[R](make: () => R, release: R => Unit) extends Op[R]

  implicit val effect: Effect[Resource] = Effect.of[Resource]

  /** acquire inside the enclosing Resource scope */
  def acquire[R](make: => R)(release: R => Unit): R ! Resource =
    Free.inject[Resource, R](Acquire(() => make, release))

  /**
   * Open the scope and KEEP it open: acquire everything now, hand back
   * the value and a closer that releases in reverse acquisition order.
   * For the scope whose end belongs to somebody else — a container's
   * close, a main's shutdown hook — where `run` cannot own the end. A
   * throw during an acquisition releases what was acquired before it
   * and rethrows; the closer is idempotent. Resource only: a row to
   * forward has no home to forward to here.
   */
  def open[A](a: A ! Resource): (A, () => Unit) = {
    var fin = List.empty[() => Unit]
    def close(): Unit = { val f = fin; fin = Nil; f.foreach(_()) }
    var x = a
    try {
      while (true) Free.resume(x) match {
        case Return(v) => return (v, () => close())
        // a lone operation is a Bind with a pure continuation (package.scala)
        case Inject(e) => x = Bind(Inject[Resource, A](e), (v: A) => Return[Resource, A](v))
        case Bind(Inject(acq: Acquire[r]), k) =>
          val res = acq.make()
          fin = (() => acq.release(res)) :: fin
          x = k(res)
        case other => throw new IllegalStateException("resume left a non-head form: " + other)
      }
      throw new MatchError(x)
    } catch {
      case t: Throwable =>
        close()
        throw t
    }
  }

  /** the region as an expression: open, run, release, answer — for the
   * scope that is the whole story, which is what a per-call region
   * usually is */
  def scoped[A](a: A ! Resource): A = Effects.run(runAt[A, Pure](a.plus[Pure]))

  /**
   * The scope, for a program whose row mentions `Resource` anywhere:
   * `Remove` finds it and names the residual row G, whose `Failing`
   * says how a forwarded operation reports failure. Two type-level
   * facts in one implicit section, so G is a type parameter rather
   * than `rm.Out` (Scala 2 refuses a dependent implicit beside the one
   * it depends on).
   */
  def run[A, R <: Row, G <: Row](a: A ! R)(implicit rm: Remove.Aux[Resource, R, G], failing: Failing[G]): A ! G =
    runAt[A, G](rm.split(a))

  /**
   * The scope at its own shape: run the region, forwarding the effects
   * F. Every acquired release runs when the scope ends — at its value,
   * or at a JVM exception thrown during a step — in reverse acquisition
   * order. Run this handler OUTERMOST: turn aborts into values inside
   * the scope (runEither before run), so no abortive handler discards
   * the finalizers; a multi-shot handler inside replays only the
   * scope's inner part, so each acquire still releases exactly once. A
   * forwarded F-operation suspends the scope with its finalizers
   * carried into the residual — they run when the residual completes,
   * and `failing.guard` runs them when it fails out there.
   */
  def runAt[A, F <: Row](a: A ! (Resource + F))(implicit failing: Failing[F]): A ! F = {
    def releaseAll(fin: List[() => Unit]): Unit = fin.foreach(_())

    /** user code under the CURRENT finalizer list: a throw releases
     * everything acquired so far and propagates. Every place the walk
     * runs code it did not write — the tree's own `resume` (Delay
     * thunks, continuations), an `Acquire`'s `make`, a continuation
     * `k` — goes through here; the releases that END a walk do not,
     * so a throwing finalizer is not released twice. */
    def guarded[T](fin: List[() => Unit])(body: => T): T =
      try body
      catch { case t: Throwable => releaseAll(fin); throw t }

    def _loop(fin: List[() => Unit])(x: A ! (Resource + F)): A ! F = loop(fin)(x)

    @tailrec def loop(fin: List[() => Unit])(x: A ! (Resource + F)): A ! F =
      guarded(fin)(Free.resume(x)) match {
        case Return(v) =>
          releaseAll(fin)
          Return(v)
        case Inject(e) => loop(fin)(Bind(Inject[Resource + F, A](e), (v: A) => Return[Resource + F, A](v)))
        case Bind(Inject(e), k) =>
          split[Resource, F, Any, Either[(List[() => Unit], A ! (Resource + F)), A ! F]](e) {
            case acq: Acquire[r] =>
              val res = guarded(fin)(acq.make())
              val f2 = (() => acq.release(res)) :: fin
              Left((f2, guarded(f2)(k(res))))
          } { g =>
            // k(y) runs USER code (the composed continuation) at the
            // outer handler's call site — a throw there must not skip
            // the finalizers, so it is guarded like every other call
            Right(Inject[F, Any](failing.guard(g, () => releaseAll(fin))).flatMap(y => _loop(fin)(guarded(fin)(k(y)))))
          } match {
            case Left((f2, next)) => loop(f2)(next)
            case Right(done) => done
          }
        case other => throw new IllegalStateException("resume left a non-head form: " + other)
      }

    loop(Nil)(a)
  }
}
