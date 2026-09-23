package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

/**
 * What `!.widen`'s walk cost a stage (widen-split): `through` over a
 * pure doubling stage joined to an `Async` row three ways — the stage
 * at the row directly (the floor), by the coercion `!.widen` is now,
 * and by `!.normalize`, the walk it used to be, which rebuilds one
 * node per operation as the stage runs. 10 000 elements, drained.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class WidenBenchmark {

  val n = 10000L
  type Row = Take % Long + (Writer % Long + Async)

  /** the pure stage, TestPipe's shape */
  def double: Stage[Long, Long, Unit] =
    Stage.await[Long, Long].flatMap {
      case Some(x) => Stage.tell[Long, Long](x * 2).flatMap(_ => double)
      case None => pure(())
    }

  /** the same stage written at the Async row — the floor */
  def doubleAt: Unit ! Row =
    effect[Row, Option[Long]](Take.Await[Long]()).flatMap {
      case Some(x) => effect[Row, Unit](Writer(x * 2)).flatMap(_ => doubleAt)
      case None => pure(())
    }

  def src: Source[Long] = Source.range(0L, n)

  @Benchmark
  def throughAtRow: Long =
    Writer.foldUntil[Long, Long, Unit, Long, Async](through(src)(doubleAt))(using summon, FoldUntil.long[Long, Long](0L)(_ + _)(_ => false)(identity)).runWith

  @Benchmark
  def throughWidened: Long =
    Writer.foldUntil[Long, Long, Unit, Long, Async](through(src)(!.widen[Unit, Take % Long + Writer % Long, Async](double)))(using summon, FoldUntil.long[Long, Long](0L)(_ + _)(_ => false)(identity)).runWith

  @Benchmark
  def throughNormalized: Long =
    Writer.foldUntil[Long, Long, Unit, Long, Async](through(src)(!.normalize[Unit, Take % Long + Writer % Long, Async](double)))(using summon, FoldUntil.long[Long, Long](0L)(_ + _)(_ => false)(identity)).runWith
}
