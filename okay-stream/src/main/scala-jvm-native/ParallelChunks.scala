package okay

import scala.annotation.tailrec

/**
 * Parallelism and resilience over fibers, the CHUNKED half
 * (specs/parallel-resilience.md). It left `Parallel.scala` in the
 * core when the streams did (core-modules stage 1): the organizing
 * fact here is that our streams are pure programs and re-observation
 * recomputes — that is Spark's lineage, so the CHUNK is both the unit
 * of parallelism (parMap) and the unit of failure and recompute
 * (retryChunks). The fiber half — parAll, parTraverse, retry,
 * supervised — stayed, because it needs only Async and a Scheduler.
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
      case h +: t => Writer.tell(h.join()).flatMap(_ => go(t, r))
      case _ => Chunks.end

  go(Vector.empty, p)

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
      case Some((c, r)) => Writer.tell(c).flatMap(_ => go(r))
      case None => Chunks.end

  go(p)
