package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * Fork/join across the ecosystem: K trivial tasks forked in parallel,
 * all joined, results summed. okay forks Loom virtual threads (one
 * per task, no runtime of its own); rawLoom is the same without the
 * effect layer — the floor. cats-effect and ZIO fork fibers on their
 * schedulers; kyo forks on its own scheduler. This measures
 * fork/join overhead, not throughput under load.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class AsyncBenchmark {

  final val K = 100

  @Benchmark
  def rawLoom(): Int =
    import java.util.concurrent.CompletableFuture
    val fs = (1 to K).map: _ =>
      val f = CompletableFuture[Int]()
      Thread.startVirtualThread(() => f.complete(1): Unit)
      f
    fs.map(_.join()).sum

  @Benchmark
  def okaySpawn(): Int =
    (1 to K).map(_ => Async.spawn(async(1))).map(_.join()).sum

  @Benchmark
  def catsIOPar(): Int =
    import cats.effect.IO
    import cats.effect.unsafe.implicits.global
    import cats.syntax.parallel.*
    (1 to K).toList.parTraverse(_ => IO(1)).unsafeRunSync().sum

  @Benchmark
  def zioForkJoin(): Int =
    import _root_.zio.*
    val z = ZIO.foreachPar(1 to K)(_ => ZIO.succeed(1)).map(_.sum)
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(z).getOrThrowFiberFailure())

  @Benchmark
  def kyoAsync(): Int =
    import _root_.kyo.*
    import AllowUnsafe.embrace.danger
    val seq: Seq[Int < (Abort[Nothing] & Async)] = (1 to K).map(_ => (1: Int < Any))
    KyoApp.Unsafe.runAndBlock(Duration.Infinity)(
      Async.parallelUnbounded(seq).flatMap((c: Seq[Int]) => (c.sum: Int < Any)))
      .getOrThrow
}
