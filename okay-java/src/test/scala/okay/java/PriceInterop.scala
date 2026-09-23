package okay.java

import okay.Stage
import scala.jdk.CollectionConverters.*

/**
 * PRICE lines for the bridge drivers (interop-shared): a gatherer over a
 * million elements, the minimum of seven timed runs after a warm-up.
 * Printed, not asserted — they are what interop-shared compared before
 * and after moving the push driver into okay-stream.
 */
class PriceInterop extends munit.FunSuite {

  def minMillis(runs: Int)(f: () => Unit): Double =
    f(); f()
    (1 to runs).map { _ => val t0 = System.nanoTime(); f(); (System.nanoTime() - t0) / 1e6 }.min

  test("PRICE: a Stage as a JDK gatherer over 1e6 elements") {
    val runningSum = Stage.mapAccumulate[Int, Int, Int](0)((s, i) => (s + i, s + i))
    val xs = (0 until 1000000).toList.asJava
    val ms = minMillis(7)(() => { val _ = xs.stream().gather(Gather.gatherer(runningSum)).count() })
    println(f"PRICE interop gatherer 1e6: $ms%.1f ms (min of 7)")
  }
}
