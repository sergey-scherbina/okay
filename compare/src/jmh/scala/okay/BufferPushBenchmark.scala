package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * THE INSTRUMENT the channel lanes are too noisy to be (queue-swap,
 * 2026-09-09).
 *
 * `ManyProducersBenchmark` at one producer asks a 150 us question
 * whose answer moves by 20% between rounds — in the first round of
 * this lane's own A/B, `forwarded_chunk` (a buffer that does nothing
 * but forward) read WORSE than `growing_chunk` (a buffer that
 * forwards AND samples), which is impossible as a cost and marks the
 * column as noise rather than measurement.
 *
 * So this asks the same question with the threads taken out: one
 * thread fills a 1 024 buffer and drains it, on the same pre-boxed
 * element so nothing is allocated. Everything the lanes share —
 * boxing, the ring's own arithmetic, the loop — is common to all four
 * rows, so the DIFFERENCE between them is the wrapper and its
 * trigger, and only that.
 *
 * What it cannot say: anything about contention, growth, or the
 * channel. That is what the channel lanes are for. This one prices
 * the per-push tax a single producer pays for the ABILITY to grow.
 *
 * WHAT IT SAID, two independent runs of three rounds, us per 1 024
 * push+pop, minimum per lane, bars under 1%:
 *
 * {{{
 * ring           9.036   1.000x   -
 * forwarding     9.057   1.002x   + a wrapper layer, plain final ref
 * noSample       9.053   1.002x   + a @volatile buffer field
 * growing        9.913   1.097x   + the counting trigger
 * adaptiveLazy  10.058   1.113x   routing by thread from the first push
 * growingCheap  10.479   1.160x   + an identity-compare trigger
 * }}}
 *
 * The layer is free and so is the volatile field. Every point
 * `Growing` costs at one producer is the trigger, and that is not a
 * structure that can be rearranged away: it is the price of asking
 * WHO IS PUSHING on every push. `adaptiveLazy` asks the same
 * question by routing and pays 1.5% more; `growingCheap` asks it
 * more directly and pays 6% more.
 *
 * `queue-swap` was filed to remove the layer by moving the swap into
 * the channel. These rows closed it: the plan removes the
 * `forwarding` row and adds the `noSample` row, both zero.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 4, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 6, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(3)
class BufferPushBenchmark {

  final val Cap = 1024

  /** pre-boxed, so a lane measures push and pop and not the allocator */
  private val one: java.lang.Long = java.lang.Long.valueOf(1L)

  private var ring: Buffer[java.lang.Long] = null
  private var forwarding: Buffer[java.lang.Long] = null
  private var growing: Buffer[java.lang.Long] = null
  private var growingCheap: Buffer[java.lang.Long] = null
  private var noSample: Buffer[java.lang.Long] = null
  private var adaptiveLazy: Buffer[java.lang.Long] = null

  private final class Fwd[A](to: Buffer[A]) extends Buffer[A]:
    def capacity: Int = to.capacity
    def push(a: A): Boolean = to.push(a)
    def pushDeciding(a: A, unless: java.util.concurrent.atomic.AtomicBoolean, orElse: A): A | Null =
      to.pushDeciding(a, unless, orElse)
    def pop(): A | Null = to.pop()
    def popMany(max: Int)(sink: A => Unit): Int = to.popMany(max)(sink)
    override def pushMany(n: Int)(src: Int => A): Int = to.pushMany(n)(src)
    def size: Int = to.size
    def isEmpty: Boolean = to.isEmpty
    def hasReady: Boolean = to.hasReady
    override def hasRoom: Boolean = to.hasRoom

  @Setup(Level.Trial) def setup(): Unit =
    ring = Ring[java.lang.Long](Cap)
    forwarding = Fwd[java.lang.Long](Ring[java.lang.Long](Cap))
    growing = Growing[java.lang.Long](Ring[java.lang.Long](Cap), 16, () => Ring[java.lang.Long](Cap))
    growingCheap = GrowingCheap[java.lang.Long](Ring[java.lang.Long](Cap), 16, () => Ring[java.lang.Long](Cap))
    noSample = GrowingNoSample[java.lang.Long](Ring[java.lang.Long](Cap))
    adaptiveLazy = AdaptiveFifo[java.lang.Long](16, () => Ring[java.lang.Long](Cap), eager = false)

  /** fill it, drain it, leave it empty for the next op */
  private def drive(b: Buffer[java.lang.Long]): Int =
    var pushed = 0
    while pushed < Cap && b.push(one) do pushed += 1
    var popped = 0
    while popped < pushed do { val _ = b.pop(); popped += 1 }
    popped

  @Benchmark def ring_fillDrain(): Int = drive(ring)
  @Benchmark def forwarding_fillDrain(): Int = drive(forwarding)
  @Benchmark def growing_fillDrain(): Int = drive(growing)
  @Benchmark def growingCheap_fillDrain(): Int = drive(growingCheap)

  /** the volatile field and the forwarding, with the trigger taken
   * out: the half of `growing_fillDrain` that `queue-swap` would move
   * into the channel rather than remove */
  @Benchmark def noSample_fillDrain(): Int = drive(noSample)

  /** the candidate `Growing` may be redundant with: a partitioned
   * buffer that opens its parts LAZILY, so at one producer it holds
   * one part of `Cap` — the same memory the ring holds — and detects
   * the second producer for free, because it routes by thread
   * identity on every push anyway. If this reads at the ring, the
   * 9.9% `Growing` pays for the SAME behaviour is a tax with nothing
   * bought. */
  @Benchmark def adaptiveLazy_fillDrain(): Int = drive(adaptiveLazy)
}
