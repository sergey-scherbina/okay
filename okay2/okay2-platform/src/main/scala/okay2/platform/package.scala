package okay2

import okay2.async.{Async, CanBlock, Fiber, PlatformDefaults, Scheduler, Timer}

/**
 * okay2-platform — the JVM under okay2-async (specs/okay2.md,
 * stage 4): `import okay2.platform._` installs the capabilities every
 * blocking door asks for — `CanBlock` (park a virtual thread),
 * `Timer` (one scheduled thread holds every pending delay), the
 * default `Scheduler` (Loom where this JVM has it, the owned workers
 * where it does not) — plus `Net`, and the fiber combinators that
 * need a blocking join (`parAll`, `parTraverse`, `retry`,
 * `supervised`). Where the Scala 3 core's givens are top-level in
 * package `okay`, Scala 2's live here in the package object, so one
 * import brings them all.
 */
package object platform {

  /** the JVM's three capabilities as ONE implicit: `CanBlock` (park a
   * virtual thread), `Timer` (one scheduled thread for every delay),
   * the default `Scheduler` (Loom where this JVM has it, `own` watched
   * where it does not; `-Dokay.scheduler=own|adaptive|drive|threads|loom`
   * selects another). The companions derive each from this, so a local
   * `implicit val S: Scheduler = ...` overrides without ambiguity. */
  implicit val jvm: PlatformDefaults = new PlatformDefaults {
    def canBlock: CanBlock = Platform.canBlock
    def timer: Timer = Platform.timer
    def scheduler: Scheduler = Platform.scheduler
  }

  /** the three, by name, for a caller that wants one explicitly */
  def canBlock: CanBlock = Platform.canBlock
  def timer: Timer = Platform.timer
  def scheduler: Scheduler = Platform.scheduler

  /** the blocking socket behind Async.Run */
  implicit val net: Net = Platform.net

  /** a fiber per program, all joined, order preserved */
  def parAll[A](progs: Seq[A ! Async])(implicit S: Scheduler): Seq[A] ! Async =
    Async(progs.map(p => Async.spawn(p)(S)).map(_.join()))

  /** a fiber per element */
  def parTraverse[A, B](xs: Seq[A])(f: A => B ! Async)(implicit S: Scheduler): Seq[B] ! Async =
    parAll(xs.map(f))(S)

  /** run, retrying per the policy on any exception; delays park the
   * current (virtual) thread; a policy exhausted rethrows. The program
   * reruns FROM ITS BEGINNING */
  def retry[A](policy: LazyList[Long])(prog: => Free[Async, A]): A ! Async =
    Async {
      def go(delays: LazyList[Long]): A =
        try Effects.runFree(prog)
        catch {
          case e: Throwable => delays match {
            case d #:: rest =>
              if (d > 0) Thread.sleep(d)
              go(rest)
            case _ => throw e
          }
        }
      go(policy)
    }

  /** a fiber that restarts its program per the policy on failure */
  def supervised[A](policy: LazyList[Long])(prog: => Free[Async, A])(implicit S: Scheduler): Fiber[A] =
    Async.spawn(retry(policy)(prog))(S)
}
