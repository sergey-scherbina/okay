package okay.freer

import okay.std.*
import okay.std.given
import okay.given
import okay.freer.Row.at
import org.openjdk.jmh.annotations.{State, *}
import java.util.concurrent.TimeUnit

/** the per-bind cost on the classic road: the same loop as okay-core's ProgValueBenchmark (ask, tell, n times),
 * okay-freer's words, Reader and Writer handled */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class ClassicLoopBenchmark {
  @Param(Array("1000")) var n: Int = 0
  type R = Reader % Int + Writer % String

  def loop(i: Int, acc: Int): Int ! R =
    if i == 0 then pure(acc)
    else Reader.ask[Int].at[R].flatMap(x => Writer.tell("x").at[R].flatMap(_ => loop(i - 1, acc + x)))

  @Benchmark def classic(): Any = Writer.run[String, Int, Pure](Reader.run[Int, Int, Writer % String](1)(loop(n, 0))).run
}
