package okay

import scala.annotation.tailrec

/**
 * Parallelism and resilience over fibers (specs/parallel-resilience.md).
 * The organizing fact: our streams are pure programs and
 * re-observation recomputes — that is Spark's lineage, so the CHUNK is
 * both the unit of parallelism (parMap) and the unit of failure and
 * recompute (retryChunks).
 */

/**
 * Map a chunked stream with a FIBER PER CHUNK: up to parallelism
 * chunks are in flight ahead of the consumer (the prefetch window —
 * pulling one output chunk may start several input chunks), order is
 * preserved by joining in sequence.
 */
def parMap[A, B](p: Chunks[A], parallelism: Int = Runtime.getRuntime.availableProcessors())
                (f: A => B)(using S: Scheduler): Chunks[B] =
  def go(inflight: Vector[Fiber[Chunk[B]]], rest: Chunks[A]): Chunks[B] = Chunks.defer:
    @tailrec def fill(q: Vector[Fiber[Chunk[B]]], r: Chunks[A]): (Vector[Fiber[Chunk[B]]], Chunks[A]) =
      if q.length >= parallelism then (q, r)
      else Chunks.pull(r) match
        case Some((c, r2)) => fill(q :+ S.fork(() => async(Chunks.mapChunk(c)(f))), r2)
        case None => (q, Chunks.end)

    val (q, r) = fill(inflight, rest)
    q match
      case h +: t => produce(h.join()).flatMap(_ => go(t, r))
      case _ => Chunks.end

  go(Vector.empty, p)

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
    def go(delays: LazyList[Long]): A =
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

/**
 * Per-chunk fault tolerance, Spark-style: a failed chunk PULL is
 * recomputed from the stream's own program — the value IS the
 * lineage — after the policy's delay; the policy is fresh for every
 * chunk. Only replayable work belongs under this (a pure generator, a
 * rewindable source); a non-replayable effect would be repeated.
 */
def retryChunks[A](p: Chunks[A], policy: LazyList[Long] = Retry.immediate(3)): Chunks[A] =
  def attempt(rest: Chunks[A], delays: LazyList[Long]): Option[(Chunk[A], Chunks[A])] =
    try Chunks.pull(rest)
    catch
      case e: Throwable => delays match
        case d #:: t =>
          if d > 0 then Thread.sleep(d)
          attempt(rest, t)
        case _ => throw e

  def go(rest: Chunks[A]): Chunks[A] = Chunks.defer:
    attempt(rest, policy) match
      case Some((c, r)) => produce(c).flatMap(_ => go(r))
      case None => Chunks.end

  go(p)
