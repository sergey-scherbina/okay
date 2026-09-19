package okay

import okay.Direct.*
import scala.language.implicitConversions

/**
 * THE BOOK'S CHAPTER 8, COMPILED (docs/continuations/08-on-the-way-back.md).
 *
 * Two of these tests exist to PIN BEHAVIOUR THE CHAPTER MUST STATE
 * rather than imply: whether the hook runs when the block leaves
 * early, and what happens when the block throws. Both were written as
 * questions and answered by running them.
 */
class TestBookOnTheWayBack extends munit.FunSuite {

  final case class Outcome(status: Int, took: Long)

  // ---- chapter 1's timing problem, on both paths

  def handle(fail: Boolean, clock: () => Long): Outcome ! Pure =
    Delim.delimited[Outcome, Pure]:
      direct:
        val started = clock()
        !Delim.onReturn(o => o.copy(took = clock() - started))
        if fail then Outcome(500, -1) else Outcome(200, -1)

  test("the hook sees the value coming back, on either path") {
    var t = 0L
    val tick = () => { t += 5; t }
    assertEquals(!.run(handle(fail = false, tick)).status, 200)
    t = 0
    val bad = !.run(handle(fail = true, tick))
    assertEquals(bad.status, 500)
    assert(bad.took > 0, "the failing path was not timed")
  }

  test("it runs LAST, on whatever the block produced") {
    val r = !.run(Delim.delimited[Int, Pure]:
      direct:
        !Delim.onReturn(n => n * 10)
        1 + 2)
    assertEquals(r, 30)
  }

  // ---- QUESTION ONE: does it run when the block leaves early?

  test("an early exit still passes through the hook") {
    val r = !.run(Delim.delimited[Int, Pure]:
      direct:
        !Delim.onReturn(n => n + 100)
        !Delim.exit(7)
        1)
    assertEquals(r, 107, "the hook did not see an early exit's value")
  }

  // ---- QUESTION TWO: what about a thrown exception?

  test("a THROWN exception does not pass through the hook") {
    var hookRan = false
    val thrown = intercept[RuntimeException](
      !.run(Delim.delimited[Int, Pure]:
        direct:
          !Delim.onReturn(n => { hookRan = true; n })
          throw new RuntimeException("boom")))
    assertEquals(thrown.getMessage, "boom")
    assert(!hookRan,
      "the hook ran on a thrown exception — the chapter says it does not")
  }

  // ---- two hooks nest: the inner one is applied first

  test("two hooks: the one registered last is applied first") {
    val r = !.run(Delim.delimited[String, Pure]:
      direct:
        !Delim.onReturn(s => s"outer($s)")
        !Delim.onReturn(s => s"inner($s)")
        "x")
    assertEquals(r, "outer(inner(x))")
  }

  // ---- the compensation shape, which is what people reach for it for

  def charge(amount: Int, ok: Boolean): String ! Pure =
    Delim.delimited[String, Pure]:
      direct:
        !Delim.onReturn(s => if s.startsWith("failed") then s"$s; refunded $amount" else s)
        if ok then s"charged $amount" else "failed: card declined"

  test("a compensation folded into the answer, registered from the middle") {
    assertEquals(!.run(charge(90, true)), "charged 90")
    assertEquals(!.run(charge(90, false)), "failed: card declined; refunded 90")
  }
}
