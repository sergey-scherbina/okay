package okay.std.bench

import okay.{!, %, pure}
import okay.std.*

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * The lanes of the classic's DelimBenchmark that are programs of the std's effects: the native `Writer` producing
 * N values, the State handler over N get/set pairs, and the plain-List floor (no effects; the same on both roads).
 * Every other classic lane is okay-freer's `Shift`/`Delimited` machinery (`delim*`, `layered*`, `stateDeep`,
 * `stateForeign`, `writerTellUnderDelim`) or its `Lexical` instances (`stateLex*`), and has no counterpart here.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class DelimBenchmark {

  val N = 1000

  // ---- the native effect

  @Benchmark
  def writerTell(): Int =
    def go(i: Int): Unit ! Writer % Int =
      if i >= N then pure(())
      else Writer.tell(i).flatMap(_ => go(i + 1))
    Writer.run[Int, Unit, Writer % Int](go(0)).run._1.length

  // ---- the State handler over N get/set pairs

  type SRow = State % Int
  def stateProg(n: Int): Int ! SRow =
    if n == 0 then State.get[Int]
    else State.get[Int].flatMap(s => State.set(s + 1)).flatMap(_ => stateProg(n - 1))

  @Benchmark
  def stateHandle(): Int =
    State.run(0)(stateProg(N))._2

  // ---- the floor

  @Benchmark
  def plainList(): Int =
    var xs = List.empty[Int]
    var i = N - 1
    while i >= 0 do { xs = i :: xs; i -= 1 }
    xs.length
}
