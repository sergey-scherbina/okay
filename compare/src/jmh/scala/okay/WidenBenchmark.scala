package okay

import okay.FoldUntil

import okay.std.*
import org.openjdk.jmh.annotations.{State, *}
import java.util.concurrent.TimeUnit

/**
 * What `!.widen`'s walk cost a stage (widen-split): `through` over a
 * pure doubling stage joined to an `Async` row two ways — the stage
 * at the row directly (the floor), and widened to it (`.at[Row]`, the
 * kernel's word for what `!.widen` was). 10 000 elements, drained.
 * (The third lane, okay-freer's `!.normalize` walk, has no
 * counterpart on the kernel and is gone.)
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
    Take.await[Long].at[Row].flatMap {
      case Some(x) => Writer.tell(x * 2).at[Row].flatMap(_ => doubleAt)
      case None => pure(())
    }

  def src: Source[Long] = Source.range(0L, n)

  @Benchmark
  def throughAtRow: Long =
    Writer.foldUntil[Long, Long, Unit, Long, Writer % Long + Async](through(src)(doubleAt))(using summon, FoldUntil.long[Long, Long](0L)(_ + _)(_ => false)(identity)).runWith

  @Benchmark
  def throughWidened: Long =
    Writer.foldUntil[Long, Long, Unit, Long, Writer % Long + Async](through(src)(double.at[Row]))(using summon, FoldUntil.long[Long, Long](0L)(_ + _)(_ => false)(identity)).runWith
}
