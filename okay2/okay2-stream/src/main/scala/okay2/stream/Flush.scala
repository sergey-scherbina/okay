package okay2.stream

import okay2._
import okay2.Free.{Inject, Bind, Return}
import okay2.async._

/**
 * A PRODUCER'S OWN CHUNK BOUNDARY — the Scala 3 core's okay-stream
 * `Flush`. Chunking by size or by a timer puts a boundary wherever the
 * count or the clock happened to fall; a producer that KNOWS where its
 * natural boundary is (the end of a model's turn, of a frame) says so
 * with `Flush.now`, and a flushing merge emits what it holds at exactly
 * that point. An operation rather than a distinguished element, because
 * a boundary is not data.
 */
sealed trait Flush extends Row { type Op[+A] = Flush.Op[A] }

object Flush {
  sealed trait Op[+A]
  case object Now extends Op[Unit]
  implicit val effect: Effect[Flush] = Effect.of[Flush]

  /** emit whatever the chunker holds, full or not */
  def now: Unit ! Flush = Free.inject[Flush, Unit](Now)

  /** map the elements of a flushing stream, leaving its `Flush.now`
   * marks exactly where the producer put them: the row split spelled
   * out, as the core does (`Writer.map`'s rest needs one TypeableK) */
  def map[A, B](a: Flushing[A])(f: A => B): Flushing[B] = {
    // the two splits as patterns, made once per map (okay2-split-at-rest)
    val Flushed = Split.at[Flush]
    val Told = Split.at[Writer[A]]
    def step(e: Any, k: Any => Flushing[B]): Flushing[B] = e match {
      case Flushed(fl) => Inject[Flush, Any](fl).flatMap(k)
      case Told(Writer.Say(w)) => Writer.tell(f(w)).flatMap(x => k(x))
      case g => Inject[Async, Any](g).flatMap(k)
    }
    def go(a: Flushing[A]): Flushing[B] = Free.resume(a) match {
      case Return(x) => Return(x)
      case Inject(e) => step(e, (_: Any) => pure[Flush + (Writer[B] + Async), Unit](()))
      case Bind(Inject(e), k) => step(e, (x: Any) => go(k(x)))
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }
    go(a)
  }

  implicit final class FlushingOps[A](private val s: Flushing[A]) extends AnyVal {
    /**
     * Merge two sources that mark their own chunk boundaries: the chunked
     * merge, except that `Flush.now` in either one emits what that side
     * holds at exactly that point. Always chunked — an unchunked merge
     * has nothing to flush. `flushAfter` stays the backstop for a
     * producer that goes quiet WITHOUT marking a boundary.
     */
    def mergeFlushing[B >: A](t: Flushing[B], capacity: Int = 64, flushAfter: Option[Long] = None)
                             (implicit sch: Scheduler, timer: Timer): Source[B] = {
      val slots = math.max(1, capacity / Source.ChunkSize)
      // `Say[A]` would be a `Say[B]`, but the row is invariant in W
      val sw: Flushing[B] = Flush.map[A, B](s)(a => a)
      pure[Writer[B] + Async, Unit](()).flatMap(_ =>
        Writer.expand[Chunk[B], B, Unit, Async](
          Writer.of[Drain, Async, Chunk[B]](Drain(Channel.mergeFlushing[B](sw, t, slots, Source.ChunkSize, flushAfter))))(c => c))
    }

    /** `mergeFlushing`, keeping which side each element came from */
    def eitherFlushing[B](t: Flushing[B], capacity: Int = 64, flushAfter: Option[Long] = None)
                         (implicit sch: Scheduler, timer: Timer): Source[Either[A, B]] =
      new FlushingOps(Flush.map[A, Either[A, B]](s)(a => Left(a)))
        .mergeFlushing[Either[A, B]](Flush.map[B, Either[A, B]](t)(b => Right(b)), capacity, flushAfter)
  }
}
