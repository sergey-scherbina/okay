package okay


import okay.freer.*


import okay.std.*
/**
 * How a FORWARDED operation reports its failure to the scope that
 * forwarded it (resource-guard, 2026-09-09).
 *
 * `Resource.run` hands every non-Resource operation to the outer
 * handler and keeps the scope's finalizers in the residual; when that
 * operation fails OUT THERE, the residual — finalizers included — is
 * abandoned, and a transaction region's brake never runs (found
 * through pgjdbc: a throwing statement left the connection in the
 * transaction). Only `Async` operations can fail on the outer handler
 * that way — a `Run` thunk throws, an `Await` callback answers Left —
 * so this typeclass says, per row, how to attach a hook that runs
 * first.
 *
 * TWO INSTANCES: the typed one for `Async` alone, and the default for
 * any row, which is the typed one LIFTED over the row by the kernel's
 * prism (`over`, beside `split` in Effects.scala). A row is a type
 * LAMBDA, `A + B + C` nests to the left, and an instance can only be
 * written for a shape the compiler can pin — so the default reads the
 * OPERATION's own class instead, which is total over every nesting,
 * and the one cast a row costs is made in the kernel, where `split`
 * already makes the same claim, not here. The alternative — typed
 * instances for the shapes plus an identity default — was built,
 * measured and REFUTED: `(Async + S) + P` matched no instance, the
 * identity answered, and the row went silently unguarded (the defect
 * this hook exists to prevent). An identity default is the one thing
 * to refuse.
 *
 * Also refuted, in the same lane: an UNANCHORED
 * `given [F[+_], G[+_]]: Failing[F + G]` is selected by dotty and then
 * cannot pin `F` (ambiguous `TypeableK[F]`); and instances ANCHORED on
 * the concrete effect (`Failing[Async + G]`, `Failing[F + Async]`,
 * splitting through the kernel `<|>` with no cast) do work — they were
 * written, and then deleted as decoration, because a total default
 * already answers those shapes identically and at the same cost. What
 * survives of that road is `Failing[Async]` below: a single effect IS
 * a shape the compiler pins, it is what most `Resource.run` call sites
 * pass, and it needs no cast at all. And refuted last (failing-over):
 * a `Row.In[Async, F]` witness with a `NotGiven` identity — `In`
 * walks the left spine only, so `(S + P) + Async` and a right-nested
 * row resolve nothing and would take the identity, and on an abstract
 * `F` `NotGiven` reads "unknown" as "absent". The probe's table is in
 * specs/sql.md (resource-async-failure).
 *
 * The whole story, with the measurements: docs/typepedia.md, "The
 * row-typeclass recipe"; the shapes are walked in TestFailing.
 */
trait FailingLow:
  /**
   * ANY row, by the operation's own class — total over every nesting:
   * the typed instance for `Async` alone, lifted over the row by the
   * kernel's prism. The class test proves the operation is an Async,
   * `Failing.async` rebuilds it as one under its GADT match, and
   * `over` puts it back under the row's type — the one cast a row
   * costs, made in the kernel beside `split`'s, not here. A row with
   * no Async operation in it never reaches it: the test fails and the
   * operation is returned untouched.
   */
  given anyRow[F[+_]]: okay.std.Failing[F] = new:
    def guard[X](e: F[X], onFailure: () => Unit): F[X] =
      over[Async, F](e)(AsyncFailing.async.guard(_, onFailure))

object AsyncFailing extends FailingLow:
  /** the typed road, for the row every second `Resource.run` passes:
   * one effect is a shape the compiler pins, so the operation is
   * rebuilt under a GADT match and no cast is needed. Higher priority
   * than `anyRow`, which would answer the same way through the cast. */
  given async: okay.std.Failing[Async] = new:
    def guard[X](e: Async[X], onFailure: () => Unit): Async[X] =
      val release = Release(onFailure)
      e match
        case Async.Run(f) => Async.Run(GuardedRun(f, release))
        case Async.Await(reg, _) => Async.Await(GuardedAwait(reg, release))

/**
 * A GUARDED OPERATION'S RELEASE, REACHABLE WITHOUT RUNNING IT
 * (drive-discontinue, 2026-09-26). The callback drive, cancelled between
 * two operations, stops before the next one, and the scope that forwarded
 * that operation would never continue. The drive therefore asks the next
 * operation to `discontinue`: release the scopes it was guarded by,
 * innermost first, and run NO user code. This is OCaml 5's
 * `discontinue`, done by the runner rather than by a handler's author.
 */
trait Discontinue:
  def discontinue(): Unit

/** a scope's release, ONCE, across every door an operation has: its
 * failure, its cancellation, its discontinuation */
private final class Release(onFailure: () => Unit):
  private val done = java.util.concurrent.atomic.AtomicBoolean(false)
  /** a failure is propagating: the release runs, and a release that
   * throws must not REPLACE that failure, so it is suppressed on it
   * (release-all-finalizers) */
  def failed(cause: Throwable): Unit =
    if !done.getAndSet(true) then
      try onFailure()
      catch { case t: Throwable => if t ne cause then cause.addSuppressed(t) }
  /** the operation will never answer: release now */
  def now(): Unit = if !done.getAndSet(true) then onFailure()

/** a guarded Run: a throw releases; `discontinue` releases without
 * running `f` */
private final class GuardedRun[X](f: () => X, release: Release) extends (() => X), Discontinue:
  def apply(): X =
    try f()
    catch { case t: Throwable => release.failed(t); throw t }
  def discontinue(): Unit =
    f match
      case d: Discontinue => d.discontinue()   // an inner scope first
      case _ => ()
    release.now()

/**
 * A guarded Await. A Left answer releases. Cancelling an OPEN wait
 * releases too (cancel-releases-resource): both schedulers cancel a
 * parked Await through the canceller its registration answered with
 * (`CanBlock.block` on an interrupt, the callback drive's `unregister`),
 * and a scope waiting there will never continue, as with ZIO's
 * interruption. Nothing is released after the wait has ANSWERED: the
 * callback drive keeps that canceller and calls it on a later cancel,
 * when the scope is still running.
 */
private final class GuardedAwait[X](reg: (Either[Throwable, X] => Unit) => (() => Unit), release: Release)
    extends ((Either[Throwable, X] => Unit) => (() => Unit)), Discontinue:
  def apply(k: Either[Throwable, X] => Unit): () => Unit =
    val answered = java.util.concurrent.atomic.AtomicBoolean(false)
    val cancel = reg { r =>
      answered.set(true)
      r.left.foreach(release.failed)
      k(r)
    }
    () =>
      cancel()
      if !answered.get then release.now()
  def discontinue(): Unit =
    reg match
      case d: Discontinue => d.discontinue()
      case _ => ()
    release.now()
