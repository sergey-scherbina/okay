package okay




import okay.std.*
import okay.std.given
import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * DIAGNOSTIC (adaptive-chunked-merge-cost, 2026-09-29): the chunked
 * merge of `ChunkFlushBenchmark.okayChunked` with the element count as
 * an axis, run under each scheduler by `-Dokay.scheduler`. The default
 * `adaptive` reads the chunked merge 1.5-1.9x slower than Loom while
 * doing the same CPU work per op; a gap that stays CONSTANT as `n`
 * grows is a per-merge price (forking the feeds, waking a worker), a
 * gap that GROWS with `n` is a per-element or per-chunk handoff.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 4, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 6, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class ChunkMergeScaleBenchmark {

  @Param(Array("250", "2000", "16000"))
  var n: Int = 2000

  @Benchmark
  def chunked(): Long =
    val l = Source.of(LazyList.range(0L, n.toLong))
    val r = Source.of(LazyList.range(n.toLong, 2L * n))
    l.merge(r, capacity = 1024, chunked = true).toLazyList.foldLeft(0L)(_ + _)

  /** the same merge on `adaptive` told to spread at once
   * (`forLongTasks`: helpAfter 0, spreadAbove 0) — if the default's
   * gap is its helper rule keeping both feeds on one worker, this arm
   * closes it */
  private val spreading: Scheduler = Schedulers.adaptive.forLongTasks.build

  @Benchmark
  def chunkedSpreading(): Long =
    given Scheduler = spreading
    val l = Source.of(LazyList.range(0L, n.toLong))
    val r = Source.of(LazyList.range(n.toLong, 2L * n))
    l.merge(r, capacity = 1024, chunked = true).toLazyList.foldLeft(0L)(_ + _)

  @Benchmark
  def elementsSpreading(): Long =
    given Scheduler = spreading
    val l = Source.of(LazyList.range(0L, n.toLong))
    val r = Source.of(LazyList.range(n.toLong, 2L * n))
    (l merge r).toLazyList.foldLeft(0L)(_ + _)

  /** `adaptive` waking a sleeper on EVERY outside submission
   * (`wakeAbove(0)`): the merge forks its two feeds from the consumer's
   * thread, outside the pool, and by default the second fork wakes
   * nobody because the first one's worker is awake */
  private val wakeEvery: Scheduler = Schedulers.adaptive.wakeAbove(0).build
  /** `adaptive` whose monitor looks every 10 us instead of 100 */
  private val monitorFast: Scheduler = Schedulers.adaptive.monitorEvery(10000L).build

  @Benchmark
  def chunkedWakeEvery(): Long =
    given Scheduler = wakeEvery
    val l = Source.of(LazyList.range(0L, n.toLong))
    val r = Source.of(LazyList.range(n.toLong, 2L * n))
    l.merge(r, capacity = 1024, chunked = true).toLazyList.foldLeft(0L)(_ + _)

  @Benchmark
  def chunkedMonitorFast(): Long =
    given Scheduler = monitorFast
    val l = Source.of(LazyList.range(0L, n.toLong))
    val r = Source.of(LazyList.range(n.toLong, 2L * n))
    l.merge(r, capacity = 1024, chunked = true).toLazyList.foldLeft(0L)(_ + _)

  @Benchmark
  def elements(): Long =
    val l = Source.of(LazyList.range(0L, n.toLong))
    val r = Source.of(LazyList.range(n.toLong, 2L * n))
    (l merge r).toLazyList.foldLeft(0L)(_ + _)
}
