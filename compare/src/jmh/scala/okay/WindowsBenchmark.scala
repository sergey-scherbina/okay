package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * THE EVENT-TIME WINDOW OPERATOR, on the instrument that can actually
 * see a ten-percent change.
 *
 * docs/benchmarks.md §20 prices `okay.Windows` inside a whole
 * benchmark — a 2.4M-event replay whose wall clock moves 10–30% with
 * whatever else the box is doing, which is fine for a 1.4x and
 * useless for the 8–11% the pane store is worth. This is the same
 * operator with warmup, forks and error bars, so a change to `add`
 * can be judged the day it is written.
 *
 * THE SHAPE is §20's, scaled down and made synthetic: elements
 * arriving in event-time order with a small jitter, spread over
 * `keys` keys, folded by `Aggregator.summary` — the flat accumulator,
 * so what is left in the measurement is the operator and not the
 * arithmetic (aggregator-zip-allocates measured the arithmetic
 * separately and it dominated everything else).
 *
 * The two lanes are the two window shapes with different pane
 * traffic: tumbling puts each element in ONE pane, sliding in three,
 * so a change to the per-pane path shows up three times as strongly in
 * the second — which is how a suspicious number is caught.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class WindowsBenchmark {

  /** one event: a key, a time, a measure */
  final class Ev(val key: Int, val ts: Long, val v: Long)

  final val N = 20000
  final val keys = 500
  final val WindowMs = 300000L
  final val SlideMs = 100000L
  final val Lateness = 30000L

  /** the elements, built once: event time climbs with a bounded
   * backwards jitter, exactly as a real out-of-order source does, and
   * deterministically so every fork sees the same stream */
  private var events: Array[Ev] = null

  @Setup(Level.Trial)
  def setup(): Unit =
    val xs = new Array[Ev](N)
    var i = 0
    var t = 1700000000000L
    var h = 12345L
    while i < N do
      h = h * 6364136223846793005L + 1442695040888963407L
      val jitter = ((h >>> 33) % 25000L)
      xs(i) = new Ev(((h >>> 17) % keys).toInt, t - jitter, (h >>> 40) % 600L)
      t += 40L
      i += 1
    events = xs

  private val agg: Aggregator[Ev, Aggregator.Summary, Aggregator.Summary] =
    Aggregator.summary[Ev](_.v)

  /** each element in ONE pane */
  @Benchmark
  def tumbling(): Long =
    var wins = 0L
    val w = Windows.tumbling[Int, Ev, Aggregator.Summary, Aggregator.Summary](
      WindowMs, Lateness)(_.key)(_.ts)(agg)
    val emit: Pane[Int, Aggregator.Summary] => Unit = p => wins += p.value.count
    var i = 0
    while i < events.length do
      w.add(events(i))(emit)
      i += 1
    w.close()(emit)
    wins

  /** each element in THREE panes — the per-pane path, three times over */
  @Benchmark
  def sliding(): Long =
    var wins = 0L
    val w = Windows.sliding[Int, Ev, Aggregator.Summary, Aggregator.Summary](
      WindowMs, SlideMs, Lateness)(_.key)(_.ts)(agg)
    val emit: Pane[Int, Aggregator.Summary] => Unit = p => wins += p.value.count
    var i = 0
    while i < events.length do
      w.add(events(i))(emit)
      i += 1
    w.close()(emit)
    wins
}
