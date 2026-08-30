package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

/**
 * The classic effect-library microbenchmark: a chain of N binds built
 * by foldLeft, then run. Lanes are grouped by kind — pure trampolines
 * (okayCont, catsEval), free/extensible effects (okayFree, catsFree),
 * production runtimes (catsIO, zioChain) and fused pending-effects
 * (kyoChain). Construction is included in every lane.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class CompareBenchmark {

  final val N = 10000

  @Benchmark
  def okayCont(): Int =
    reset((1 to N).foldLeft(Cont.Pure(0): Int /> Int)((m, _) => m.flatMap(x => Cont.Pure(x + 1))))

  @Benchmark
  def okayFree(): Int =
    (1 to N).foldLeft(pure[Produce, Int](0))((m, _) => m.flatMap(x => produce(x + 1))).runWith

  @Benchmark
  def catsEval(): Int =
    (1 to N).foldLeft(cats.Eval.now(0))((m, _) => m.flatMap(x => cats.Eval.now(x + 1))).value

  @Benchmark
  def catsFree(): Int =
    import cats.catsInstancesForId
    (1 to N).foldLeft(cats.free.Free.pure[cats.Id, Int](0)): (m, _) =>
      m.flatMap(x => cats.free.Free.liftF[cats.Id, Int](x + 1))
    .foldMap(cats.arrow.FunctionK.id[cats.Id])

  @Benchmark
  def catsIO(): Int =
    import cats.effect.unsafe.implicits.global
    (1 to N).foldLeft(cats.effect.IO.pure(0))((m, _) => m.flatMap(x => cats.effect.IO(x + 1)))
      .unsafeRunSync()

  @Benchmark
  def zioChain(): Int =
    import _root_.zio.*
    val z = (1 to N).foldLeft(ZIO.succeed(0): UIO[Int])((m, _) => m.flatMap(x => ZIO.succeed(x + 1)))
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(z).getOrThrowFiberFailure())

  @Benchmark
  def atnosEff(): Int =
    import org.atnos.eff.*
    Eff.run((1 to N).foldLeft(Eff.pure[NoFx, Int](0))((m, _) => m.flatMap(x => Eff.pure(x + 1))))

  @Benchmark
  def okayEager(): Int =
    import Eager.given
    val E = Effects[Eager]
    (1 to N).foldLeft(E.pure[Produce, Int](0))((m, _) => E.flatMap(m)(x => E.pure(x + 1)))
      .runWith

  @Benchmark
  def kyoChain(): Int =
    import _root_.kyo.*
    val k = (1 to N).foldLeft(0: Int < Any)((m, _) => m.flatMap((x: Int) => x + 1))
    k.eval
}
