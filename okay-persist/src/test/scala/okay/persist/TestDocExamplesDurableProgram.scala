package okay.persist

import munit.FunSuite
import okay.{!, +, Delim, Pure, Wf}
import okay.Direct.*
import okay.codec.Schema
import scala.language.implicitConversions

/**
 * docs/continuations-in-practice.md, "A durable program, as it
 * actually reads" — the block VERBATIM (doc-snippets-pin-all), then
 * run: two questions answered by an oracle, the clock read once, and
 * `patch` true for a run that began after the branch existed.
 */
class TestDocExamplesDurableProgram extends FunSuite {

  given Schema[Wf.SysA] = Schema.derived
  given Schema[Wf.Ans[String]] = Schema.derived
  given Wf.Runtime = Wf.Runtime.scripted(millis = 1_700_000_000_000L,
                                         id = "id-1", dice = 0.25)

  def booking(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure = direct:
    val city = !w.pause("city?")          // the world answers
    val when = !w.now                     // the RUNTIME answers, once, and it is journalled
    val n    = !w.pause("nights?")
    if !w.patch("promo") then s"$city/$n/promo at $when" else s"$city/$n at $when"

  test("the durable program on the page runs as the page says") {
    val topic = MemoryStore().topic("bookings")
    val id = "booking-1"
    val oracle: String => Dialogue.Attempt ?=> String ! Pure =
      q => okay.pure(if q == "city?" then "Kyiv" else "3")
    val answer = !.run(
      Dialogue.workflow[String, String, String, Pure](topic, id, "booking/1")(booking)
        .runWorkflow(oracle)
    )
    assertEquals(answer, Right("Kyiv/3/promo at 1700000000000"))
  }
}
