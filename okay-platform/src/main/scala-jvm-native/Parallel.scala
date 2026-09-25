package okay

import scala.annotation.tailrec

/**
 * Parallelism and resilience over fibers (specs/parallel-resilience.md):
 * the half that needs nothing but Async, a Scheduler and a retry
 * policy. The chunked half — parMap and retryChunks, where the CHUNK
 * is the unit of parallelism and of recompute — moved to okay-stream
 * with the streams themselves (core-modules stage 1); it is
 * `ParallelChunks.scala` there.
 */

/** a fiber per program, all joined, order preserved */
def parAll[A](progs: Seq[A ! Async])(using Scheduler): Seq[A] ! Async =
  async(progs.map(p => Async.spawn(p)).map(_.join()))

/** a fiber per element */
def parTraverse[A, B](xs: Seq[A])(f: A => B ! Async)(using Scheduler): Seq[B] ! Async =
  parAll(xs.map(f))

/**
 * Run, retrying per the policy on any exception; delays park the
 * current (virtual) thread; a policy exhausted rethrows. The program
 * reruns FROM ITS BEGINNING — at-least-once, for replayable work.
 */
def retry[A](policy: LazyList[Long])(prog: => A ! Async): A ! Async =
  async {
    @tailrec def go(delays: LazyList[Long]): A =
      try prog.runWith
      catch
        case e: Throwable => delays match
          case d #:: rest =>
            if d > 0 then Thread.sleep(d)
            go(rest)
          case _ => throw e

    go(policy)
  }

/** a fiber that restarts its program per the policy on failure */
def supervised[A](policy: LazyList[Long])(prog: => A ! Async)(using Scheduler): Fiber[A] =
  Async.spawn(retry(policy)(prog))
