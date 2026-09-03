package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import scala.collection.immutable.Queue

/**
 * channel-ring: does the ring actually earn its way in, measured
 * BEFORE it is wired into Channel?
 *
 * The comparison is the mechanism, isolated from everything else: one
 * push and one pop per element, through the ring, against the same
 * two operations expressed the way `Channel.State` expresses them
 * today — rebuild an immutable state (a persistent `Queue` inside a
 * case class) and CAS it into an `AtomicReference`. Nothing else
 * about the channel is involved on either side.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 4, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 6, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class RingBenchmark {

  final val N = 4000
  final val Cap = 1024

  /** the shape Channel.State has today, reduced to its buffer */
  final case class St(buf: Queue[Long], size: Int) extends TRef.Stamped[St]:
    def value: St = this

  @Benchmark
  def rebuildImmutableState(): Long =
    val cell = TRef.bare(St(Queue.empty[Long], 0))
    var sum = 0L
    var i = 0L
    while i < N do
      // push: rebuild the whole state and CAS it
      cell.modify(s => (St(s.buf.enqueue(i), s.size + 1), ()))
      // pop: rebuild the whole state and CAS it
      sum += cell.modify: s =>
        val (a, rest) = s.buf.dequeue
        (St(rest, s.size - 1), a)
      i += 1
    sum

  @Benchmark
  def ring(): Long =
    val r = Ring[Long](Cap)
    var sum = 0L
    var i = 0L
    while i < N do
      val _ = r.push(i)
      val v = r.pop()
      if v != null then sum += v.nn
      i += 1
    sum
}
