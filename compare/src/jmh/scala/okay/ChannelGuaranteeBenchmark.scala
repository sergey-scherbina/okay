package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * What a GUARANTEE costs, measured on both sides of the fence, at
 * both granularities.
 *
 * The earlier tables compared our channel with `zio.Queue` and read
 * the gap as a mechanism gap. It is not only that. `StmChannel`
 * promises that an accepted element is delivered and that close ends
 * the channel only after the buffer is spent; `zio.Queue.shutdown`
 * promises neither. So the two rows were never the same row.
 *
 *   strong   drains on close, termination detected
 *   weak     close discards, no detection at all
 *   layered  a weak channel plus a sentinel riding the buffer in FIFO
 *            order: the STRONG contract, assembled as a layer instead
 *            of as an invariant of the mechanism
 *
 * GRANULARITY, and why every lane now names it. The first version of
 * this file had exactly the fault `ChannelGranularityBenchmark` was
 * written to expose: its okay lanes were elementwise and its zio
 * lanes went through `ZStream`, which takes up to `maxChunkSize`
 * (4096) elements per queue operation. `zioWeak` read 114.8 against
 * `zioChunked`'s 114.0, and `okayWeak` read 203.7 against
 * `okayElementwise`'s 197.1 -- the two libraries were on different
 * axes and no row could be read across. Only lanes sharing a suffix
 * compare.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 4, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 6, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ChannelGuaranteeBenchmark {

  final val N = 4000
  final val Cap = 1024

  private def produce(send: Long => Unit, after: => Unit): Thread =
    Thread.ofVirtual().start { () =>
      var i = 0L
      while i < N do { send(i); i += 1 }
      after
    }

  /** read one element per channel operation */
  private def sumElem(c: Channel[Long], counted: Boolean): Long =
    var sum = 0L
    var got = 0
    var go = true
    while go do
      c.receiveBlocking() match
        case Some(v) => sum += v; got += 1; if counted && got == N then go = false
        case None => go = false
    sum

  /** read whatever is buffered per channel operation */
  private def sumChunk(c: Channel[Long], counted: Boolean): Long =
    val cb = summon[CanBlock]
    var sum = 0L
    var got = 0
    var go = true
    while go do
      val chunk = cb.block[Either[Throwable, Chunk[Long]]] { k =>
        c.receiveManyAsync(4096)(k); () => ()
      }.fold(throw _, identity)
      var i = 0
      while i < chunk.length do { sum = sum + chunk(i); i += 1 }
      got += chunk.length
      if chunk.length == 0 || (counted && got >= N) then go = false
    sum

  // ── strong: StmChannel, drain-on-close as an invariant ───────────

  @Benchmark def okayStrongElem(): Long =
    val c = StmChannel[Long](Cap)
    val t = produce(c.sendBlocking(_): Unit, c.close())
    val s = sumElem(c, counted = false); t.join(); s

  @Benchmark def okayStrongChunk(): Long =
    val c = StmChannel[Long](Cap)
    val t = produce(c.sendBlocking(_): Unit, c.close())
    val s = sumChunk(c, counted = false); t.join(); s

  // ── strong, the other way round: a mutable ring plus a mark in
  //    the FIFO stream. The same contract as okayStrong, assembled
  //    as okayLayered was, but as a real channel rather than a
  //    benchmark's Option wrapper ─────────────────────────────────

  @Benchmark def okaySentinelElem(): Long =
    val c = SentinelChannel[Long](Cap)
    val t = produce(c.sendBlocking(_): Unit, c.close())
    val s = sumElem(c, counted = false); t.join(); s

  @Benchmark def okaySentinelChunk(): Long =
    val c = SentinelChannel[Long](Cap)
    val t = produce(c.sendBlocking(_): Unit, c.close())
    val s = sumChunk(c, counted = false); t.join(); s

  // ── the UNBOUNDED default: the same channel over Segments ───────
  //    This is the lane the work was for. Channel.apply defaults to
  //    unbounded, that case used to be StmChannel, and it was the one
  //    place zio.Queue was still ahead.

  @Benchmark def okayUnboundedElem(): Long =
    val c = SentinelChannel[Long](Segments[Long | Mark]())
    val t = produce(c.sendBlocking(_): Unit, c.close())
    val s = sumElem(c, counted = false); t.join(); s

  @Benchmark def okayUnboundedChunk(): Long =
    val c = SentinelChannel[Long](Segments[Long | Mark]())
    val t = produce(c.sendBlocking(_): Unit, c.close())
    val s = sumChunk(c, counted = false); t.join(); s

  // ── weak: AbruptChannel, close discards, so the consumer counts ──

  @Benchmark def okayWeakElem(): Long =
    val c = AbruptChannel[Long](Cap)
    val t = produce(c.sendBlocking(_): Unit, ())
    val s = sumElem(c, counted = true); t.join(); c.close(); s

  @Benchmark def okayWeakChunk(): Long =
    val c = AbruptChannel[Long](Cap)
    val t = produce(c.sendBlocking(_): Unit, ())
    val s = sumChunk(c, counted = true); t.join(); c.close(); s

  // ── layered: the strong contract bought as a sentinel ────────────

  @Benchmark def okayLayeredElem(): Long =
    val c = AbruptChannel[Option[Long]](Cap)
    val t = Thread.ofVirtual().start { () =>
      var i = 0L
      while i < N do { val _ = c.sendBlocking(Some(i)); i += 1 }
      val _ = c.sendBlocking(None)
    }
    var sum = 0L
    var go = true
    while go do c.receiveBlocking() match
      case Some(Some(v)) => sum += v
      case _ => go = false
    t.join(); c.close(); sum

  @Benchmark def okayLayeredChunk(): Long =
    val c = AbruptChannel[Option[Long]](Cap)
    val t = Thread.ofVirtual().start { () =>
      var i = 0L
      while i < N do { val _ = c.sendBlocking(Some(i)); i += 1 }
      val _ = c.sendBlocking(None)
    }
    val cb = summon[CanBlock]
    var sum = 0L
    var go = true
    while go do
      val chunk = cb.block[Either[Throwable, Chunk[Option[Long]]]] { k =>
        c.receiveManyAsync(4096)(k); () => ()
      }.fold(throw _, identity)
      if chunk.length == 0 then go = false
      var i = 0
      while go && i < chunk.length do
        chunk(i) match
          case Some(v) => sum += v; i += 1
          case None => go = false
    t.join(); c.close(); sum

  // ── zio, weak: shutdown promises no drain ────────────────────────

  @Benchmark def zioWeakElem(): Long =
    import _root_.zio.*
    _root_.zio.Unsafe.unsafe(implicit u =>
      _root_.zio.Runtime.default.unsafe.run(for
        q <- Queue.bounded[Long](Cap)
        _ <- ZIO.foreachDiscard(0L until N.toLong)(q.offer).fork
        r <- Ref.make(0L)
        _ <- ZIO.foreachDiscard(0 until N)(_ => q.take.flatMap(x => r.update(_ + x)))
        s <- r.get
      yield s).getOrThrowFiberFailure())

  @Benchmark def zioWeakChunk(): Long =
    import _root_.zio.*
    _root_.zio.Unsafe.unsafe(implicit u =>
      _root_.zio.Runtime.default.unsafe.run(for
        q <- Queue.bounded[Long](Cap)
        _ <- ZIO.foreachDiscard(0L until N.toLong)(q.offer).fork
        r <- Ref.make(0L)
        _ <- zio.stream.ZStream.fromQueue(q).take(N.toLong).runForeach(x => r.update(_ + x))
        s <- r.get
      yield s).getOrThrowFiberFailure())

  // ── zio unbounded: the like-for-like for okayUnbounded ──────────
  //    A channel that never makes its producer wait is not the same
  //    thing as one that does, so the comparison has to be against
  //    Queue.unbounded, not against the bounded one.

  @Benchmark
  def zioUnboundedElem(): Long =
    import _root_.zio.*
    _root_.zio.Unsafe.unsafe(implicit u =>
      _root_.zio.Runtime.default.unsafe.run(for
        q <- Queue.unbounded[Option[Long]]
        _ <- (ZIO.foreachDiscard(0L until N.toLong)(i => q.offer(Some(i))) *> q.offer(None)).fork
        r <- Ref.make(0L)
        _ <- ZIO.foreachDiscard(0 until N)(_ => q.take.flatMap {
               case Some(x) => r.update(_ + x)
               case None => ZIO.unit
             })
        s <- r.get
      yield s).getOrThrowFiberFailure())

  @Benchmark
  def zioUnboundedChunk(): Long =
    import _root_.zio.*
    _root_.zio.Unsafe.unsafe(implicit u =>
      _root_.zio.Runtime.default.unsafe.run(for
        q <- Queue.unbounded[Option[Long]]
        _ <- (ZIO.foreachDiscard(0L until N.toLong)(i => q.offer(Some(i))) *> q.offer(None)).fork
        r <- Ref.make(0L)
        _ <- zio.stream.ZStream.fromQueue(q).collectWhileSome.runForeach(x => r.update(_ + x))
        s <- r.get
      yield s).getOrThrowFiberFailure())

  // ── zio, strong: OUR contract on THEIR queue, via a sentinel ─────

  @Benchmark def zioStrongElem(): Long =
    import _root_.zio.*
    _root_.zio.Unsafe.unsafe(implicit u =>
      _root_.zio.Runtime.default.unsafe.run(for
        q <- Queue.bounded[Option[Long]](Cap)
        _ <- (ZIO.foreachDiscard(0L until N.toLong)(i => q.offer(Some(i))) *> q.offer(None)).fork
        r <- Ref.make(0L)
        _ <- ZIO.foreachDiscard(0 until N)(_ => q.take.flatMap {
               case Some(x) => r.update(_ + x)
               case None => ZIO.unit
             })
        s <- r.get
      yield s).getOrThrowFiberFailure())

  @Benchmark def zioStrongChunk(): Long =
    import _root_.zio.*
    _root_.zio.Unsafe.unsafe(implicit u =>
      _root_.zio.Runtime.default.unsafe.run(for
        q <- Queue.bounded[Option[Long]](Cap)
        _ <- (ZIO.foreachDiscard(0L until N.toLong)(i => q.offer(Some(i))) *> q.offer(None)).fork
        r <- Ref.make(0L)
        _ <- zio.stream.ZStream.fromQueue(q).collectWhileSome.runForeach(x => r.update(_ + x))
        s <- r.get
      yield s).getOrThrowFiberFailure())
}
