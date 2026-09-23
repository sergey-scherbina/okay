package okay.clojure

import okay.Stage
import clojure.lang.PersistentVector

/**
 * PRICE lines for the bridge drivers (interop-shared): a Stage as a
 * Clojure transducer over a million elements, and a Clojure okay.core
 * program of 200 000 steps — each the minimum of seven timed runs after a
 * warm-up. Printed, not asserted.
 */
class PriceInterop extends munit.FunSuite {

  def minMillis(runs: Int)(f: () => Unit): Double =
    f(); f()
    (1 to runs).map { _ => val t0 = System.nanoTime(); f(); (System.nanoTime() - t0) / 1e6 }.min

  test("PRICE: a Stage as a Clojure transducer over 1e6, and an okay.core program of 2e5 steps") {
    val runningSum = Stage.mapAccumulate[Long, Long, Long](0L)((s, i) => (s + i, s + i))
    val into = Clj.fn("clojure.core", "into").fold(e => fail(e), identity)
    val range = Clj.eval("(range 1000000)").fold(e => fail(e), identity)
    val xf = Transducers.of(runningSum)
    val t = minMillis(7)(() => { val _ = into.invoke(PersistentVector.EMPTY, xf, range) })
    println(f"PRICE interop transducer 1e6: $t%.1f ms (min of 7)")
    val countTo = Clj.fn("okay.clojure.programs", "count-to").fold(e => fail(e), identity)
    val p = minMillis(7)(() => {
      val stage = Program.stage[Long, java.lang.Long](countTo.invoke(Long.box(0L), Long.box(200000L)))
      val _ = okay.!.run(okay.Writer.run(okay.through(okay.pure[okay.%[okay.Writer, Long], Unit](()))(stage)))
    })
    println(f"PRICE interop clojure program 2e5 steps: $p%.1f ms (min of 7)")
  }
}
