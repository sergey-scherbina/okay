package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

import !.*

/**
 * The Reader effect across the ecosystem: a chain of N asks
 * (m.flatMap(_ => ask)), then run with the environment 42.
 * okay handles it with the tail-resumptive relay; cats is Kleisli
 * over Eval (Id would build an N-deep stack); ZIO reads its
 * environment; kyo uses Env; atnos-eff its ReaderEffect.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ReaderBenchmark {

  final val N = 10000

  @Benchmark
  def okayReader(): Int =
    val prog = (1 to N).foldLeft(Reader.ask[Int]): (m, _) =>
      m.flatMap(_ => Reader.ask[Int])
    !.run(Reader.run[Int, Int, Nothing](42)(prog))

  @Benchmark
  def catsKleisli(): Int =
    import cats.data.Kleisli
    val ask: Kleisli[cats.Eval, Int, Int] = Kleisli(e => cats.Eval.now(e))
    (1 to N).foldLeft(ask)((m, _) => m.flatMap(_ => ask)).run(42).value

  @Benchmark
  def zioService(): Int =
    import _root_.zio.*
    val ask = ZIO.service[Int]
    val z = (1 to N).foldLeft(ask)((m, _) => m.flatMap(_ => ask))
      .provideEnvironment(ZEnvironment(42))
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(z).getOrThrowFiberFailure())

  @Benchmark
  def kyoEnv(): Int =
    import _root_.kyo.*
    val k = (1 to N).foldLeft(Env.get[Int]: Int < Env[Int]): (m, _) =>
      m.flatMap((_: Int) => Env.get[Int])
    Env.run(42)(k).eval

  @Benchmark
  def atnosReader(): Int =
    import org.atnos.eff.*
    import org.atnos.eff.all.*
    import org.atnos.eff.syntax.all.*
    type S = Fx.fx1[[X] =>> cats.data.Reader[Int, X]]
    val a = ask[S, Int]
    (1 to N).foldLeft(a)((m, _) => m.flatMap(_ => a)).runReader(42).run
}
