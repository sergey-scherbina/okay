package okay2.platform

import okay2._
import okay2.async._

/** the fiber combinators that need a BLOCKING join — the JVM's and
 * Native's, mixed into their package objects; Scala.js has none, as
 * the Scala 3 core keeps `Parallel.scala` in `scala-jvm-native` */
trait BlockingOps {
  /** a fiber per program, all joined, order preserved */
  def parAll[A](progs: Seq[A ! Async])(implicit S: Scheduler, cb: CanBlock): Seq[A] ! Async =
    Async(progs.map(p => Async.spawn(p)(S)).map(_.join()(cb)))

  /** a fiber per element */
  def parTraverse[A, B](xs: Seq[A])(f: A => B ! Async)(implicit S: Scheduler, cb: CanBlock): Seq[B] ! Async =
    parAll(xs.map(f))(S, cb)

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
