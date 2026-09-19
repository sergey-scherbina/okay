package okay

import okay.Direct.*
import scala.language.implicitConversions

/**
 * A PAUSED DIALOGUE THAT OUTLIVES THE PROCESS (paused-persist,
 * 2026-09-17). The continuation is a closure and cannot be written
 * down; the JOURNAL can, and the paused state is re-derived from the
 * program plus the journal. These tests include the limit, measured:
 * what replay re-runs, and the discipline under which it re-runs
 * nothing.
 */
class TestDelimPersist extends munit.FunSuite {

  type Row = Delim + okay.Pure

  def booking(using Delim.Asking[String, String, String, Row]): String ! Row = direct:
    val city = !Delim.pause("Which city?")
    val nights = !Delim.pause(s"How many nights in $city?")
    val pay = !Delim.pause(s"Pay ${nights.toInt * 90} for $city?")
    if pay == "yes" then s"Booked $city for $nights nights" else "Cancelled"

  test("replay: a dialogue survives a restart, because its journal does") {
    // ---- the process that starts the booking
    val p0 = !.run(Delim.resumable[String, String, String, okay.Pure](booking))
    assertEquals(p0.asking, Some("Which city?"))
    val (p1, j1) = !.run(Delim.answer(p0, List.empty[String])("Kyiv"))
    val (p2, j2) = !.run(Delim.answer(p1, j1)("3"))
    assertEquals(j2, List("Kyiv", "3"))
    assertEquals(p2.asking, Some("Pay 270 for Kyiv?"))

    // ---- the process dies. p0, p1, p2 are gone with it; the only
    //      thing that was written down is j2, a list of two strings.

    val back = !.run(Delim.replay[String, String, String, okay.Pure](booking)(j2))
    assertEquals(back.asking, Some("Pay 270 for Kyiv?"))   // the same place
    val (end, j3) = !.run(Delim.answer(back, j2)("yes"))
    assertEquals(end.finished, Some("Booked Kyiv for 3 nights"))
    assertEquals(j3, List("Kyiv", "3", "yes"))
  }

  test("replay: the empty journal is where it started") {
    val fresh = !.run(Delim.replay[String, String, String, okay.Pure](booking)(Nil))
    assertEquals(fresh.asking, Some("Which city?"))
  }

  test("replay: a full journal comes back finished") {
    val done = !.run(Delim.replay[String, String, String, okay.Pure](booking)(
      List("Lviv", "2", "no")))
    assertEquals(done.finished, Some("Cancelled"))
    // more answers than questions is not an error: the extra are ignored
    val over = !.run(Delim.replay[String, String, String, okay.Pure](booking)(
      List("Lviv", "2", "no", "stray")))
    assertEquals(over.finished, Some("Cancelled"))
  }

  // ---- the limit, measured

  type Log = Writer % String + okay.Pure
  type Logged = Delim + Log
  type Where = Delim.Dialogue[String, String, String, Log]

  /** an effect OUTSIDE pause: this is what replay re-runs */
  def chatty(using Delim.Asking[String, String, String, Logged]): String ! Logged = direct:
    "asking for the city".tell
    val city = !Delim.pause("Which city?")
    s"got $city".tell
    val nights = !Delim.pause("How many nights?")
    s"booking $city for $nights".tell
    s"$city/$nights"

  test("the limit: replay re-runs what did not come through pause") {
    // THE ESCAPE HATCH, and this test is the reason it exists: since
    // dialogue-replay-discipline a row with a `Writer` in it does not
    // typecheck as replayable, because replay tells the log again —
    // which is precisely what this test measures. Saying
    // `Replayable.unchecked` is how a deliberate breach is written
    // down where a reviewer sees it.
    given Replayable[Logged] = Replayable.unchecked

    def go(j: List[String]) = Writer.run[String, Where, okay.Pure](
      Delim.replay[String, String, String, Log](chatty)(j))

    val (log1, p1) = !.run(go(List("Kyiv")))
    assertEquals(log1.toList, List("asking for the city", "got Kyiv"))
    assertEquals(p1.asking, Some("How many nights?"))

    // the SECOND process replays from the journal — and says the same
    // two things again on the way back to where it stood
    val (log2, p2) = !.run(go(List("Kyiv", "3")))
    assertEquals(log2.toList,
      List("asking for the city", "got Kyiv", "booking Kyiv for 3"))
    assertEquals(p2.finished, Some("Kyiv/3"))
    // "asking for the city" has now been told twice across the two runs
    val said = log1.count(_ == "asking for the city") +
      log2.count(_ == "asking for the city")
    assertEquals(said, 2)
  }

  test("the discipline: what goes through pause is performed once") {
    // the program asks for the outside world instead of reaching for
    // it, so the driver is the only thing that performs anything
    var performed = List.empty[String]
    def perform(q: String): String =
      performed = performed :+ q
      q match
        case "rate:EUR" => "42"
        case other => s"?$other"

    def priced(using Delim.Asking[String, String, String, Row]): String ! Row = direct:
      val rate = !Delim.pause("rate:EUR")
      val fee = !Delim.pause("fee:standard")
      s"$rate/$fee"

    // leg 1: one question performed, the answer journalled
    val p0 = !.run(Delim.resumable[String, String, String, okay.Pure](priced))
    val (p1, j1) = !.run(Delim.answer(p0, List.empty[String])(perform(p0.asking.get)))
    assertEquals(performed, List("rate:EUR"))

    // the process dies; a new one replays from j1 and carries on
    val back = !.run(Delim.replay[String, String, String, okay.Pure](priced)(j1))
    assertEquals(back.asking, Some("fee:standard"))
    val (end, _) = !.run(Delim.answer(back, j1)(perform(back.asking.get)))
    assertEquals(end.finished, Some("42/?fee:standard"))

    // the replayed leg did NOT ask the outside world again
    assertEquals(performed, List("rate:EUR", "fee:standard"))
    val _ = p1
  }
}
