package okay.persist

import munit.FunSuite
import okay.{!, +, Delim, Pure}
import okay.Direct.*
import okay.codec.Schema
import scala.language.implicitConversions

/**
 * THE DIALOGUE WHOSE JOURNAL IS A TOPIC (durable-dialogue,
 * 2026-09-17). The property under test is the event-sourcing one: a
 * new process over the same log stands exactly where the old one
 * stood, and asks the outside world nothing it has already been told.
 */
class TestDialogue extends FunSuite {

  type Row = Delim + Pure

  /** the program: straight-line code that happens to pause */
  def booking(using Delim.Asking[String, String, String, Row]): String ! Row = direct:
    val city = !Delim.pause("Which city?")
    val nights = !Delim.pause(s"How many nights in $city?")
    val pay = !Delim.pause(s"Pay ${nights.toInt * 90} for $city?")
    if pay == "yes" then s"Booked $city for $nights nights" else "Cancelled"

  def dialogue(t: Topic, id: String = "b-1") =
    Dialogue[String, String, String, Pure](t, id)(booking)

  test("a new process stands where the old one stood") {
    val t = MemoryStore().topic("bookings")

    // ---- process 1
    val d1 = dialogue(t)
    assertEquals((!.run(d1.at)).asking, Some("Which city?"))
    assertEquals((!.run(d1.answer("Kyiv"))).asking, Some("How many nights in Kyiv?"))
    assertEquals((!.run(d1.answer("3"))).asking, Some("Pay 270 for Kyiv?"))
    assertEquals(d1.journal, List("Kyiv", "3"))

    // ---- process 1 dies. d1 and every continuation it held go with
    //      it; the topic is all that is left.
    val d2 = dialogue(t)
    assertEquals(d2.journal, List("Kyiv", "3"))
    assertEquals((!.run(d2.at)).asking, Some("Pay 270 for Kyiv?"))
    assertEquals((!.run(d2.answer("yes"))).finished, Some("Booked Kyiv for 3 nights"))

    // ---- and a third process reads the finished dialogue as finished
    assertEquals((!.run(dialogue(t).at)).finished, Some("Booked Kyiv for 3 nights"))
  }

  test("the oracle is never asked what the journal already knows") {
    val t = MemoryStore().topic("bookings")
    var asked = List.empty[String]
    def oracle(q: String): String ! Pure =
      asked = asked :+ q
      okay.pure(if q.startsWith("Which") then "Lviv"
                else if q.startsWith("How") then "2" else "yes")

    assertEquals(!.run(dialogue(t).run(oracle)), "Booked Lviv for 2 nights")
    assertEquals(asked.size, 3)

    // a second process over the same log: the whole dialogue is
    // already journaled, so the outside world is not touched again
    asked = Nil
    assertEquals(!.run(dialogue(t).run(oracle)), "Booked Lviv for 2 nights")
    assertEquals(asked, Nil)
  }

  test("two dialogues in one topic do not see each other") {
    // ONE partition on purpose: with two, routing would separate them
    // and the key filter — the thing under test — would never run
    val t = MemoryStore().topic("bookings", partitions = 1)
    val a = dialogue(t, "a")
    val b = dialogue(t, "b")
    val _ = !.run(a.answer("Kyiv"))
    assertEquals(a.journal, List("Kyiv"))
    assertEquals(b.journal, Nil)
    assertEquals((!.run(b.at)).asking, Some("Which city?"))
  }

  test("a record that does not decode stops the journal and names itself") {
    val t = MemoryStore().topic("bookings")
    val d = dialogue(t)
    val _ = !.run(d.answer("Kyiv"))
    // something else wrote to this partition — no envelope, no CBOR
    val _ = t.append(0, "b-1".getBytes("UTF-8"), Array[Byte](1, 2), Ack.Durable)
    val r = d.recovered
    assertEquals(r.answers, List("Kyiv"))
    assert(!r.intact, "damage went unnoticed")
    assertEquals(r.damage.map(_.offset), Some(1L))
    // and the program is where the intact prefix puts it, not further
    assertEquals((!.run(d.at)).asking, Some("How many nights in Kyiv?"))
  }
}
