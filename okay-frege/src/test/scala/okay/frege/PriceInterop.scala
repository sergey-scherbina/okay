package okay.frege

import okay.{!, %, Writer, pure, through}
import okay.frege.{Programs as P}

/**
 * PRICE line for the bridge drivers (interop-shared): a Frege Prog of
 * 200 000 tells through the driver, the minimum of seven timed runs after
 * a warm-up. Printed, not asserted.
 */
class PriceInterop extends munit.FunSuite {

  def minMillis(runs: Int)(f: () => Unit): Double =
    f(); f()
    (1 to runs).map { _ => val t0 = System.nanoTime(); f(); (System.nanoTime() - t0) / 1e6 }.min

  test("PRICE: a Frege program of 2e5 steps through the driver") {
    val ms = minMillis(7)(() => {
      val stage = Frege.stage[Long, java.lang.Long](P.countTo(0L, 200000L).call())
      val _ = !.run(Writer.run(through(pure[Writer % Long, Unit](()))(stage)))
    })
    println(f"PRICE interop frege program 2e5 steps: $ms%.1f ms (min of 7)")
  }
}
