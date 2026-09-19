package okay

import org.openjdk.jmh.annotations.*
import okay.Direct.*
import java.util.concurrent.TimeUnit

/**
 * What `import Direct.parallelBinds.given` costs and buys
 * (specs/applicative-static.md, stage 3). Eight trivial independent
 * leaves, and two questions kept apart:
 *
 *  - DOES THE PARALLEL BLOCK EMIT THE FLAT SHAPE? `parallel8` against
 *    `parAllFlat8`, the same eight leaves through the hand door. The
 *    macro emits N spawns then N joins, which is what `parAll` does,
 *    so the pair should be close — the spec predicted within 20%
 *    before measuring. If it is not, the macro is building something
 *    else.
 *  - DOES THE SEQUENTIAL ROAD STILL COST WHAT IT COST? `sequential8`
 *    (the same block, no import) against `handChain8`, the flatMap
 *    chain a person would write. They were equal before this lane
 *    and must still be, to the byte — that is the "nothing changed
 *    without the import" claim, measured rather than asserted.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class DirectParallelBenchmark {

  private def leaf(i: Int): Int ! Async = async(i)

  @Benchmark
  def parallel8(): Int =
    import Direct.parallelBinds.given
    val prog: Int ! Async = Direct.direct:
      val a = leaf(1).reflect
      val b = leaf(2).reflect
      val c = leaf(3).reflect
      val d = leaf(4).reflect
      val e = leaf(5).reflect
      val f = leaf(6).reflect
      val g = leaf(7).reflect
      val h = leaf(8).reflect
      a + b + c + d + e + f + g + h
    prog.runWith

  @Benchmark
  def parAllFlat8(): Int = parAll((1 to 8).map(leaf)).runWith.sum

  @Benchmark
  def sequential8(): Int =
    val prog: Int ! Async = Direct.direct:
      val a = leaf(1).reflect
      val b = leaf(2).reflect
      val c = leaf(3).reflect
      val d = leaf(4).reflect
      val e = leaf(5).reflect
      val f = leaf(6).reflect
      val g = leaf(7).reflect
      val h = leaf(8).reflect
      a + b + c + d + e + f + g + h
    prog.runWith

  @Benchmark
  def handChain8(): Int =
    val prog: Int ! Async =
      leaf(1).flatMap(a => leaf(2).flatMap(b => leaf(3).flatMap(c =>
      leaf(4).flatMap(d => leaf(5).flatMap(e => leaf(6).flatMap(f =>
      leaf(7).flatMap(g => leaf(8).map(h => a + b + c + d + e + f + g + h))))))))
    prog.runWith
}
