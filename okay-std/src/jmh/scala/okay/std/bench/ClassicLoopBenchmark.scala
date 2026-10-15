package okay.std.bench

import okay.{!, +, %, pure}
import okay.std.*
import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/** the per-bind cost on the new road: the same loop as okay-core's ProgValueBenchmark (ask, tell, n times), the
 * root's words (`A ! Es` a value), Reader and Writer handled — `okay.freer.ClassicLoopBenchmark` at the tag
 * `classic-freer` is the same program on okay-freer's words */
@JmhState(Scope.Thread)
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

  @Benchmark def classic(): Any = Writer.run[String, Int, Writer % String](Reader.run[Int, Int, R](1)(loop(n, 0))).run
}
