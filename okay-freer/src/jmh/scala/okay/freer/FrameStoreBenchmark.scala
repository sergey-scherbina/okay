package okay.freer

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

/**
 * THE PREMISE OF AN ARRAY STACK, priced before any machine is built
 * (frames-loop-shape, 2026-10-01). The segmented frame machine keeps a
 * run's frames as a linked list: a `Frame(f, rest)` node per bind,
 * allocated in the TLAB, no GC barrier (a fresh object's stores need
 * none). The alternative is a mutable array per run: `f` stored into a
 * cell — no allocation, but a G1 post-write barrier per store into a heap
 * array. `linked` and `array` push N continuations and pop them, calling
 * each; the functions are N distinct objects so nothing folds. If `array`
 * does not win clearly, the array-stack machine is refuted here.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class FrameStoreBenchmark {

  val N = 1000

  /** N distinct continuations: each adds its own index */
  val fs: Array[Int => Int] = Array.tabulate(N)(i => (x: Int) => x + i)

  final class Node(val f: Int => Int, val rest: Node | Null)

  @Benchmark
  def linked(): Int =
    var top: Node | Null = null
    var i = 0
    while i < N do
      top = Node(fs(i), top)
      i += 1
    var acc = 0
    var cur = top
    while cur ne null do
      val n = cur.nn
      acc = n.f(acc)
      cur = n.rest
    acc

  /** one array per run, as a machine would keep it */
  @Benchmark
  def array(): Int =
    val stack = new Array[AnyRef](N)
    var sp = 0
    while sp < N do
      stack(sp) = fs(sp)
      sp += 1
    var acc = 0
    while sp > 0 do
      sp -= 1
      acc = stack(sp).asInstanceOf[Int => Int](acc)
    acc

  /** the same array reused across runs (a machine keeping its stack):
   * stores into an OLD array, where G1's barrier has work to do */
  val kept = new Array[AnyRef](N)

  @Benchmark
  def arrayReused(): Int =
    var sp = 0
    while sp < N do
      kept(sp) = fs(sp)
      sp += 1
    var acc = 0
    while sp > 0 do
      sp -= 1
      acc = kept(sp).asInstanceOf[Int => Int](acc)
    acc
}
