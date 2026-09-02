package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

import !.*

/**
 * Resource safety across the ecosystem: N sequential bracketed
 * acquire/use/release steps, plus okay's region form (one scope, N
 * acquires, all released at the end). cats-effect brackets on IO,
 * ZIO uses acquireReleaseWith, kyo its Resource + Async runtime.
 *
 * The foldLeft lanes are LEFT-nested, which is O(N²) in kyo (see
 * ReaderBenchmark's note) — kyo's foldLeft number is that trap, not
 * its runtime; the `*Rec` lanes are the right-nested shape real
 * code builds, for both libraries.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ResourceBenchmark {

  final val N = 1000

  @Benchmark
  def okayBracket(): Int =
    var c = 0
    val r = (1 to N).foldLeft(pure[Produce, Int](0)): (m, _) =>
      m.flatMap(x => bracket(x)(_ => c += 1)(r => produce(r + 1)))
    r.runWith + c

  @Benchmark
  def okayResource(): Int =
    var c = 0
    val prog = (1 to N).foldLeft(pure[Resource, Int](0)): (m, _) =>
      m.flatMap(x => Resource.acquire(x + 1)(_ => c += 1))
    !.run(Resource.run[Int, Nothing](prog)) + c

  /** the region form, RIGHT-nested by recursion */
  @Benchmark
  def okayResourceRec(): Int =
    var c = 0
    def go(i: Int, x: Int): Int ! Resource =
      if i == 0 then pure(x) else Resource.acquire(x + 1)(_ => c += 1).flatMap(y => go(i - 1, y))
    !.run(Resource.run[Int, Nothing](go(N, 0))) + c

  @Benchmark
  def catsIOBracket(): Int =
    import cats.effect.IO
    import cats.effect.unsafe.implicits.global
    var c = 0
    (1 to N).foldLeft(IO.pure(0)): (m, _) =>
      m.flatMap(x => IO(x).bracket(r => IO(r + 1))(_ => IO(c += 1)))
    .unsafeRunSync() + c

  @Benchmark
  def zioAcquireRelease(): Int =
    import _root_.zio.*
    var c = 0
    val z = (1 to N).foldLeft(ZIO.succeed(0): UIO[Int]): (m, _) =>
      m.flatMap(x => ZIO.acquireReleaseWith(ZIO.succeed(x))(_ => ZIO.succeed(c += 1))(r => ZIO.succeed(r + 1)))
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(z).getOrThrowFiberFailure()) + c

  @Benchmark
  def kyoResource(): Int =
    import _root_.kyo.*
    import AllowUnsafe.embrace.danger
    var c = 0
    val k = (1 to N).foldLeft(0: Int < (Resource & IO)): (m, _) =>
      m.flatMap((x: Int) => Resource.acquireRelease(x + 1)(_ => c += 1))
    KyoApp.Unsafe.runAndBlock(Duration.Infinity)(Resource.run(k)).getOrThrow + c

  /** kyo Resource in its natural, right-nested shape — linear */
  @Benchmark
  def kyoResourceRec(): Int =
    import _root_.kyo.*
    import AllowUnsafe.embrace.danger
    var c = 0
    def go(i: Int, x: Int): Int < (Resource & IO) =
      if i == 0 then (x: Int < (Resource & IO))
      else Resource.acquireRelease(x + 1)(_ => c += 1).flatMap((y: Int) => go(i - 1, y))
    KyoApp.Unsafe.runAndBlock(Duration.Infinity)(Resource.run(go(N, 0))).getOrThrow + c
}
