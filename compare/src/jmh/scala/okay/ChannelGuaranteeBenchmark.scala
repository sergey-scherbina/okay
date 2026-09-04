package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * What a GUARANTEE costs, measured on both sides of the fence.
 *
 * The earlier tables compared our channel with `zio.Queue` and read
 * the gap as a mechanism gap. It is not only that. `StmChannel`
 * promises that an accepted element is delivered and that close ends
 * the channel only after the buffer is spent; `zio.Queue.shutdown`
 * promises neither. So the two rows were never the same row.
 *
 * Four lanes make the trade legible: each library at ITS OWN promise,
 * and each carrying THE OTHER'S.
 *
 *   okayStrong  StmChannel        drains on close, termination detected
 *   okayWeak    AbruptChannel     discards on close, no detection
 *   zioWeak     Queue + take(N)   the count is known UP FRONT, so the
 *                                 consumer never asks "is it over?" --
 *                                 the weakest of the four, and the
 *                                 shape the previous table quoted
 *   zioStrong   Queue[Option]     the sentinel rides the queue behind
 *                                 the buffered elements, so nothing
 *                                 accepted is lost and termination
 *                                 arrives in FIFO order. This IS our
 *                                 contract, expressed on their queue
 *
 * `zioStrong` is the honest question: the guarantee is expressible
 * over `zio.Queue` (their own `zio.stream.Take` is this idea, chunked)
 * -- but it is a layer ABOVE the queue, and the layer is not free.
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

  private def drain(c: Channel[Long]): Long =
    val t = Thread.ofVirtual().start { () =>
      var i = 0L
      while i < N do { val _ = c.sendBlocking(i); i += 1 }
      c.close()
    }
    var sum = 0L
    var go = true
    while go do c.receiveBlocking() match
      case Some(v) => sum += v
      case None => go = false
    t.join()
    sum

  @Benchmark def okayStrong(): Long = drain(StmChannel[Long](Cap))

  @Benchmark def okayWeak(): Long =
    val c = AbruptChannel[Long](Cap)
    // the weak channel cannot be drained by closing -- the consumer
    // must know when to stop, which is the guarantee it does not give
    val t = Thread.ofVirtual().start { () =>
      var i = 0L
      while i < N do { val _ = c.sendBlocking(i); i += 1 }
    }
    var sum = 0L
    var got = 0
    while got < N do
      c.receiveBlocking() match
        case Some(v) => sum += v; got += 1
        case None => got = N
    t.join(); c.close()
    sum

  /**
   * OUR guarantee, bought THEIR way: a weak channel plus a sentinel
   * that rides the buffer in FIFO order. Nothing accepted is lost and
   * termination is detected -- the same contract `okayStrong` gives,
   * assembled as a LAYER instead of an invariant of the mechanism.
   */
  @Benchmark
  def okayLayered(): Long =
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
    t.join(); c.close()
    sum

  @Benchmark
  def zioWeak(): Long =
    import _root_.zio.*
    _root_.zio.Unsafe.unsafe(implicit u =>
      _root_.zio.Runtime.default.unsafe.run(for
        q <- Queue.bounded[Long](Cap)
        _ <- ZIO.foreachDiscard(0L until N.toLong)(q.offer).fork
        r <- Ref.make(0L)
        _ <- zio.stream.ZStream.fromQueue(q).take(N.toLong).runForeach(x => r.update(_ + x))
        s <- r.get
      yield s).getOrThrowFiberFailure())

  @Benchmark
  def zioStrong(): Long =
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
