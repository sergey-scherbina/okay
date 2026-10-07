package okay.freer

import okay.*
import okay.given

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

/**
 * contAnswer's body, `k(x + 1) + 1`, at depth (cont-leaf-by-platform): the macro's LAZY leaf (a program over
 * the lazy `k`, nothing on the host stack) against the STRICT leaf (`Cont.shiftLeaf`, `k` a nested run per
 * level, the body's `+ 1` waiting on the host stack: rooms, then StackSwitch past them). At 1000 levels the
 * strict leaf is 0.85x the time (cont-leaf-forms); this asks whether that holds where the strict one switches.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class ContDepthBenchmark {

  @Param(Array("1000", "100000", "1000000"))
  var depth: Int = 0

  @Benchmark
  def lazyLeaf(): Int =
    Cont.reset((1 to depth).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) => m.flatMap(x => Cont.shift[Int, Int, Int](k => k(x + 1) + 1))))

  @Benchmark
  def strictLeaf(): Int =
    Cont.reset((1 to depth).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) => m.flatMap(x => Cont.shiftLeaf[Int, Int, Int](k => k(x + 1) + 1))))
}
