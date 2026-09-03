package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * free-row-variance: what is the PRIZE, before paying for it?
 *
 * `Source.merge` calls `Writer.widen` once per source, and widen
 * rebuilds every Free node of the walk — solely because `Free` is
 * invariant in its row (Effects.scala says so in as many words). A
 * spike confirmed `enum Free[+F[+_], A]` passes the variance check
 * and that the row subtyping then holds at concrete rows, which
 * would make those two calls disappear. It also confirmed the cost:
 * every tree walker that matches `Bind(Inject(e), k)` captures a
 * fresh row and needs re-typing by hand.
 *
 * So: measure the prize first. `widened` is the plain drain with ONE
 * widen pass over it; `plain` is the same drain without. The
 * difference per element is what covariance would remove per source.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class WidenBenchmark {

  @Param(Array("500", "2000"))
  var n: Int = 500

  @Benchmark
  def plain(): Long =
    Source.of(LazyList.range(0L, n.toLong)).toLazyList.foldLeft(0L)(_ + _)

  /** the same source with one widen pass — exactly what Source.merge
   * pays per source, and exactly what a covariant row would delete */
  @Benchmark
  def widened(): Long =
    val s: Unit ! (Writer % Long + Async) = Source.of(LazyList.range(0L, n.toLong))
    Writer.widen[Long, Long | String, Unit, Async](s)
      .toLazyList.foldLeft(0L)((acc, x) => acc + (x match { case l: Long => l; case _ => 0L }))

  // ── the decisive pair: the same merge, with and without widen ──
  // Source.merge's body, at ONE element type so the union collapses
  // and the two widen calls are the ONLY difference between them.

  private type S[W] = Unit ! (Writer % W + Async)

  @Benchmark
  def mergeWithWiden(): Long =
    (Source.of(LazyList.range(0L, n.toLong)) merge Source.of(LazyList.range(n.toLong, 2L * n)))
      .toLazyList.foldLeft(0L)(_ + _)

  /** identical, minus the two Writer.widen passes */
  @Benchmark
  def mergeNoWiden(): Long =
    val a: S[Long] = Source.of(LazyList.range(0L, n.toLong))
    val b: S[Long] = Source.of(LazyList.range(n.toLong, 2L * n))
    val merged: Unit ! (Writer % Long + Async) =
      okay.pure[Writer % Long + Async, Unit](()).flatMap: _ =>
        Writer.of(Channel.merge[Long, S, Async, S, Async](a, b, 64))
    merged.toLazyList.foldLeft(0L)(_ + _)
}
