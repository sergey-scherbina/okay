package okay


import okay.freer.*


import okay.std.*
import okay.freer.given
import okay.std.given
/**
 * docs/effects/async.md, VERBATIM (doc-snippet-debt): the page's example
 * lines as it prints them, answer comment included, then asserted. The
 * other effect pages are the core's TestDocExamplesEffects.
 */
class TestDocExamplesAsync extends munit.FunSuite:

  test("async.md") {
    val prog: Int ! Async = async(20).flatMap(x => async(x + 22))
    val answer = prog.runWith   // 42

    val a = Async.spawn(async(20))
    val b = Async.spawn(async(22))
    val sum = a.join() + b.join()   // 42

    val both = Async.par(async(1), async("one")).runWith   // (1, one)
    val first = Async.race(Async.sleep(5_000).map(_ => "slow"), async("fast")).runWith   // fast
    val late = Async.timeout(10)(Async.sleep(5_000).map(_ => 1)).runWith   // None
    assertEquals(answer, 42)
    assertEquals(sum, 42)
    assertEquals(both, (1, "one"))
    assertEquals(first, "fast")
    assertEquals(late, None)
  }
