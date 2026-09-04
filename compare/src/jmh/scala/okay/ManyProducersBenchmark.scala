package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * The shape every earlier table in this repo left out: MANY
 * producers.
 *
 * Everything measured so far was one producer and one consumer, and
 * at that width a single ring's tail CAS is uncontended and a relaxed
 * buffer can only lose — it adds a part to pick and gains nothing.
 * The reason `MultiFifo` exists is the other end of the curve, where
 * every producer fights for the same counter, and the only honest way
 * to say whether it earns its keep is to sweep the width.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 4, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 6, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(3)
class ManyProducersBenchmark {

  @Param(Array("1", "4", "16"))
  var producers: Int = 1

  final val Total = 8000
  final val Cap = 1024

  private def run(c: Channel[Long]): Long =
    val per = Total / producers
    val ps = (0 until producers).map(w => Thread.ofVirtual().start { () =>
      var i = 0L
      while i < per do { val _ = c.sendBlocking(w.toLong * per + i); i += 1 }
    })
    val done = Thread.ofVirtual().start { () =>
      ps.foreach(_.join()); c.close()
    }
    var sum = 0L
    var go = true
    while go do
      c.receiveBlocking() match
        case Some(v) => sum += v
        case None => go = false
    done.join()
    sum

  /** one ring: every producer contends for one tail */
  @Benchmark def oneRing(): Long = run(Queues.strong[Long].bounded(Cap).build)

  /** parts as wide as the producers: contention only when two land on
   * the same part */
  @Benchmark def relaxed(): Long =
    run(Queues.strong[Long].relaxed(parts = producers, each = Cap / producers max 8).build)

  /**
   * The same relaxation over parts that GROW. The hypothesis this
   * lane exists to test: what breaks `relaxed` above is not the
   * relaxation but its combination with boundedness. The channel's
   * queue of waiting senders is GLOBAL while the resource is PER
   * PART, so a freed slot in one part wakes an arbitrary sender, who
   * usually finds its own part still full and parks again. With parts
   * that never fill, no sender ever parks and the mismatch cannot
   * arise.
   */
  @Benchmark def relaxedUnbounded(): Long =
    run(Queues.strong[Long].relaxedUnbounded(parts = producers).build)

  /** and one growable buffer, so the comparison is like for like */
  @Benchmark def oneUnbounded(): Long = run(Queues.strong[Long].unbounded.build)

  /** their queue, for scale */
  @Benchmark
  def zioQueue(): Long =
    import _root_.zio.*
    val per = Total / producers
    _root_.zio.Unsafe.unsafe(implicit u =>
      _root_.zio.Runtime.default.unsafe.run(for
        q <- Queue.bounded[Option[Long]](Cap)
        fs <- ZIO.foreach(0 until producers)(w =>
                ZIO.foreachDiscard(0L until per.toLong)(i => q.offer(Some(w.toLong * per + i))).fork)
        _ <- ZIO.foreachDiscard(fs)(_.join).flatMap(_ => q.offer(None)).fork
        r <- Ref.make(0L)
        _ <- zio.stream.ZStream.fromQueue(q).collectWhileSome.runForeach(x => r.update(_ + x))
        s <- r.get
      yield s).getOrThrowFiberFailure())
}
