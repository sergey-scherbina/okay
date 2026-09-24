package okay2.stream

import scala.annotation.tailrec
import okay2._
import okay2.async._

/**
 * Parallelism and resilience over CHUNKS — the Scala 3 core's
 * okay-stream `ParallelChunks` (jvm-native, since it joins a fiber by
 * parking). Our streams are pure programs and re-observation
 * recomputes: that is Spark's lineage, so the chunk is both the unit of
 * parallelism (`parMap`) and the unit of recompute (`retryChunks`).
 */
object ParallelChunks {

  /** map a chunked stream with a FIBER PER CHUNK: up to `parallelism`
   * chunks in flight ahead of the consumer, order kept by joining in
   * sequence */
  def parMap[A, B](p: Chunks[A], parallelism: Int = Runtime.getRuntime.availableProcessors())(f: A => B)
                  (implicit S: Scheduler, cb: CanBlock): Chunks[B] = {
    def go(inflight: Vector[Fiber[Chunk[B]]], rest: Chunks[A]): Chunks[B] = Chunks.defer {
      @tailrec def fill(q: Vector[Fiber[Chunk[B]]], r: Chunks[A]): (Vector[Fiber[Chunk[B]]], Chunks[A]) =
        if (q.length >= parallelism) (q, r)
        else Chunks.pull(r) match {
          case Some((c, r2)) => fill(q :+ S.fork(() => Async(c.map(f))), r2)
          case None => (q, Chunks.end[A])
        }
      val (q, r) = fill(inflight, rest)
      q match {
        case h +: t => Writer.tell(h.join()).flatMap(_ => go(t, r))
        case _ => Chunks.end[B]
      }
    }
    go(Vector.empty, p)
  }

  /** per-chunk fault tolerance: a failed chunk PULL is recomputed from
   * the stream's own program — the value IS the lineage — after the
   * policy's delay, the policy fresh for every chunk. Replayable work
   * only: typed on pure `Chunks`, so an effectful source does not fit */
  def retryChunks[A](p: Chunks[A], policy: LazyList[Long] = Retry.immediate(3)): Chunks[A] = {
    @tailrec def attempt(rest: Chunks[A], delays: LazyList[Long]): Option[(Chunk[A], Chunks[A])] = {
      val tried = try Right(Chunks.pull(rest)) catch { case e: Throwable => Left(e) }
      tried match {
        case Right(r) => r
        case Left(e) => delays match {
          case d #:: t =>
            if (d > 0) Thread.sleep(d)
            attempt(rest, t)
          case _ => throw e
        }
      }
    }
    def go(rest: Chunks[A]): Chunks[A] = Chunks.defer {
      attempt(rest, policy) match {
        case Some((c, r)) => Writer.tell(c).flatMap(_ => go(r))
        case None => Chunks.end[A]
      }
    }
    go(p)
  }
}
