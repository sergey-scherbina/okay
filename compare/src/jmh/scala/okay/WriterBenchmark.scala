package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

import !.*

/**
 * The Writer effect across the ecosystem: tell N values, collect them
 * in order, answer with how many were collected. okay tells raw
 * values (the opaque identity signature — no wrapper) into a Vector
 * fold; cats is WriterT over Eval with a Chain log; kyo's Emit is its
 * writer/stream primitive; atnos-eff has a WriterEffect. ZIO has no
 * native writer and is omitted.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class WriterBenchmark {

  final val N = 10000

  @Benchmark
  def okayWriter(): Int =
    val prog = (1 to N).foldLeft(Writer.tell(0)): (m, i) =>
      m.flatMap(_ => Writer.tell(i))
    !.run(Writer.run[Int, Unit, Nothing](prog))._1.length

  @Benchmark
  def catsWriterT(): Int =
    import cats.data.{Chain, WriterT}
    type W[A] = WriterT[cats.Eval, Chain[Int], A]
    val z: W[Int] = WriterT.value(0)
    (1 to N).foldLeft(z): (m, i) =>
      m.flatMap(_ => WriterT.tell[cats.Eval, Chain[Int]](Chain.one(i)).map(_ => i))
    .run.value._1.length.toInt

  @Benchmark
  def kyoEmit(): Int =
    import _root_.kyo.*
    val k = (1 to N).foldLeft(0: Int < Emit[Int]): (m, i) =>
      m.flatMap((_: Int) => Emit.valueWith(i)(i))
    Emit.run[Int](k).eval._1.length

  @Benchmark
  def atnosWriter(): Int =
    import org.atnos.eff.*
    import org.atnos.eff.all.*
    import org.atnos.eff.syntax.all.*
    type S = Fx.fx1[[X] =>> cats.data.Writer[Int, X]]
    val z = Eff.pure[S, Int](0)
    (1 to N).foldLeft(z)((m, i) => m.flatMap(_ => tell[S, Int](i).map(_ => i)))
      .runWriter.run._2.length
}
