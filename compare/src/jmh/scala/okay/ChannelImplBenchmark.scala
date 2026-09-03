package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * The harness the channel seam exists for: the same work through
 * different mechanisms, plus `zio.Queue` as the outside reference.
 * One producer fiber and one consumer, bounded at 1024, which is the
 * shape `Channel.buffer` and `Source.merge` actually run.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 4, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 6, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ChannelImplBenchmark {

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

  @Benchmark
  def stmChannel(): Long = drain(StmChannel[Long](Cap))

  @Benchmark
  def ringChannel(): Long = drain(RingChannel[Long](Cap))

  /** the unbounded MS queue -- the only candidate for the capacity
   * Channel.merge actually defaults to, since a ring cannot be
   * unbounded. Bounded at nothing, so its sends never park. */
  @Benchmark
  def casChannel(): Long = drain(CasChannel[Long]())

  /** and the default at ITS unbounded setting, which is the honest
   * comparison for casChannel */
  @Benchmark
  def stmChannelUnbounded(): Long = drain(StmChannel[Long](Int.MaxValue))

  @Benchmark
  def zioQueue(): Long =
    import _root_.zio.*
    _root_.zio.Unsafe.unsafe(implicit u =>
      _root_.zio.Runtime.default.unsafe.run(for
        q <- Queue.bounded[Long](Cap)
        _ <- ZIO.foreachDiscard(0L until N.toLong)(q.offer).fork
        r <- Ref.make(0L)
        _ <- zio.stream.ZStream.fromQueue(q).take(N.toLong).runForeach(x => r.update(_ + x))
        s <- r.get
      yield s).getOrThrowFiberFailure())
}
