package okaybench

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

import okay.*
import okay.given
import okay.Direct.{direct, reflect}

/**
 * The direct syntax, priced (specs/direct-macro.md): the same 10k
 * sequential binds written as a while loop in a direct block, next
 * to the hand-written flatMap chain — one pair per ecosystem that
 * HAS a first-party direct form (okay `direct`, kyo-direct's
 * `defer`/`.now`, zio-direct's `defer`/`.run`; cats has none). The
 * pair's delta is the price of the syntax; the flatMap baselines
 * double as the same-run tie to the bind-chain table.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class DirectBenchmark {

  final val N = 10000

  // ── okay ──────────────────────────────────────────────────────

  def step(x: Int): Int ! Nothing = pure(x + 1)

  @Benchmark
  def okayFlatMap(): Int =
    val chain = (1 to N).foldLeft(pure[Nothing, Int](0))((m, _) => m.flatMap(step))
    okay.!.run(chain)

  /** the macro rewrites the block into Monadic's Cont binds — the
   * delta over okayFlatMap is the whole price of direct style */
  @Benchmark
  def okayDirect(): Int =
    okay.!.run(direct[[A] =>> A ! Nothing] {
      var x = 0
      var i = 0
      while i < N do
        x = step(x).reflect
        i += 1
      x
    })

  /** the recursion spelling, apples-to-apples with kyo and zio
   * below (both forbid `var` in their blocks; okay allows either) */
  @Benchmark
  def okayDirectRec(): Int =
    def loop(n: Int, acc: Int): Int ! Nothing = direct[[A] =>> A ! Nothing] {
      if n == 0 then acc
      else
        val x = step(acc).reflect
        loop(n - 1, x).reflect
    }
    okay.!.run(loop(N, 0))

  // ── kyo ───────────────────────────────────────────────────────

  @Benchmark
  def kyoFlatMap(): Int =
    import _root_.kyo.*
    val chain = (1 to N).foldLeft(0: Int < Any)((m, _) => m.flatMap((x: Int) => x + 1))
    chain.eval

  /** kyo-direct FORBIDS `var` inside `defer` (a design stance,
   * stated in its error), so kyo's natural direct form is
   * recursion — the difference is part of the measurement */
  @Benchmark
  def kyoDirect(): Int =
    import _root_.kyo.*
    def kstep(x: Int): Int < Any = x + 1
    def kloop(n: Int, acc: Int): Int < Any = defer {
      if n == 0 then acc
      else
        val x = kstep(acc).now
        kloop(n - 1, x).now
    }
    kloop(N, 0).eval

  // ── zio ───────────────────────────────────────────────────────

  private def runZio[A](z: _root_.zio.ZIO[Any, Nothing, A]): A =
    _root_.zio.Unsafe.unsafe(implicit u =>
      _root_.zio.Runtime.default.unsafe.run(z).getOrThrowFiberFailure())

  @Benchmark
  def zioFlatMap(): Int =
    import _root_.zio.*
    val chain = (1 to N).foldLeft(ZIO.succeed(0): ZIO[Any, Nothing, Int])(
      (m, _) => m.flatMap(x => ZIO.succeed(x + 1)))
    runZio(chain)

  /** zio-direct forbids `var` inside `defer` too — recursion, the
   * same shape as kyo's */
  @Benchmark
  def zioDirect(): Int =
    import _root_.zio.*
    import _root_.zio.direct.*
    def zstep(x: Int): ZIO[Any, Nothing, Int] = ZIO.succeed(x + 1)
    def zloop(n: Int, acc: Int): ZIO[Any, Nothing, Int] = defer {
      if (n == 0) acc
      else {
        val x = zstep(acc).run
        zloop(n - 1, x).run
      }
    }
    runZio(zloop(N, 0))
}
