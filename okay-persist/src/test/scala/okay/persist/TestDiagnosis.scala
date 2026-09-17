package okay.persist

import munit.FunSuite
import okay.{!, +, Delim, Pure}
import okay.Direct.*
import scala.language.implicitConversions

/**
 * A STOPPED FOLD THAT POINTS AT CODE (delim-diagnostics-position,
 * 2026-09-17).
 *
 * `Stopped` names an offset, which says where in the LOG the trouble
 * is and nothing about the program. The line an operator needs is in
 * the code that cannot read the journal — and the reader holds that
 * code, so it replays the part it DID accept and reports where it is
 * standing. Nothing had to travel in the journal for this, which is
 * why no record gained a field and no journal needed an upcast.
 */
class TestDiagnosis extends FunSuite {

  type Row = Delim + Pure

  def booking(using Delim.Asking[String, String, String, Row]): String ! Row = direct:
    val city = !Delim.pause("city?")
    val nights = !Delim.pause("nights?")            // THE LINE the second test wants
    s"$city/$nights"

  def dialogue(t: Topic, program: String = "book/1") =
    Dialogue[String, String, String, Pure](t, "b-1", program)(booking)

  /**
   * A RECORD FROM ANOTHER DEPLOY, written straight into the topic.
   * It has to be staged this way and the reason is itself a property
   * worth knowing: a `Dialogue` for "book/2" REFUSES to append after
   * it folds a "book/1" record — it stops, exactly as designed — so
   * two Dialogue objects cannot produce a mixed journal between them.
   * The situation this tests is a v2 process that wrote while a v1
   * record was not yet visible to it.
   */
  def foreign(t: Topic, program: String, expect: Int, a: String): Unit =
    val typed = Typed[Dialogue.Entry[String]](t, 1, Map.empty)
    val _ = typed.append(0, "b-1".getBytes("UTF-8"),
      Dialogue.Entry.Answered(program, expect, a), Ack.Durable)

  test("nothing wrong: no diagnosis") {
    val t = MemoryStore().topic("bookings")
    val d = dialogue(t)
    assertEquals(!.run(d.diagnosis), None)
    val _ = !.run(d.answer("Kyiv"))
    assertEquals(!.run(d.diagnosis), None)
  }

  test("THE POINT: a foreign record stops the fold, and the reader names its OWN line") {
    val t = MemoryStore().topic("bookings")
    val mine = dialogue(t)
    val _ = !.run(mine.answer("Kyiv"))          // one good answer, accepted

    // a different deploy appends to the same journal
    foreign(t, "book/2", 1, "2")

    val found = !.run(mine.diagnosis).getOrElse(fail("the fold did not stop"))
    found.why match
      case Dialogue.Stopped.Mismatch(_, wrote, expected) =>
        assertEquals(wrote, "book/2")
        assertEquals(expected, "book/1")
      case other => fail(s"expected a mismatch, got $other")

    // ...and here is the half that did not exist before: where THIS
    // program is standing, in this file
    assertEquals(found.accepted, 1)
    assertEquals(found.asking, Some("nights?"))
    assert(found.where.exists(_.contains("TestDiagnosis.scala")),
      s"the diagnosis does not name the reader's own file: ${found.where}")
    // NOT a hardcoded line number — that rots on the next edit above
    // it. The property is that the line TRACKS THE POSITION, so a run
    // standing at the first question must name a different line from
    // one standing at the second.
    val atFirst = {
      val fresh = MemoryStore().topic("bookings")
      foreign(fresh, "book/2", 0, "x")
      !.run(dialogue(fresh).diagnosis).getOrElse(fail("the fold did not stop"))
    }
    assertEquals(atFirst.accepted, 0)
    assertEquals(atFirst.asking, Some("city?"))
    assertNotEquals(atFirst.where, found.where,
      s"the diagnosis names the same line wherever the program stands: ${found.where}")
  }

  test("damage too: an unreadable record is diagnosed the same way") {
    val t = MemoryStore().topic("bookings")
    val d = dialogue(t)
    val _ = t.append(0, "b-1".getBytes("UTF-8"), Array[Byte](1, 2, 3), Ack.Durable)

    val found = !.run(d.diagnosis).getOrElse(fail("the fold did not stop"))
    assert(found.why.isInstanceOf[Dialogue.Stopped.Damage], s"got ${found.why}")
    assertEquals(found.accepted, 0)
    assertEquals(found.asking, Some("city?"), "a run with nothing accepted is at its first question")
    assert(found.where.isDefined)
  }

  test("it reads as one line, because that is where it will be read") {
    val t = MemoryStore().topic("bookings")
    val mine = dialogue(t)
    val _ = !.run(mine.answer("Kyiv"))
    foreign(t, "book/2", 1, "2")

    val line = !.run(mine.diagnosis).get.toString
    assert(line.contains("after 1 answer(s)"), line)
    assert(line.contains("this program is at"), line)
    assert(line.contains("asking nights?"), line)
  }
}
