package okay2.persist

import munit.FunSuite
import okay2.{!, Shift}

/**
 * A STOPPED FOLD THAT POINTS AT CODE (okay-persist's TestDiagnosis;
 * delim-diagnostics-position): the reader replays the part of the
 * journal it DID accept and reports where it is standing — nothing had
 * to travel in the journal for this.
 */
class TestDiagnosis extends FunSuite {
  import DialogueFixtures._

  def booking(s: Shift.Asking.Aux[String, String, String, P]): String ! Rw = for {
    city <- Shift.pause(s)("city?")
    nights <- Shift.pause(s)("nights?")            // THE LINE the second test wants
  } yield s"$city/$nights"

  def dialogue(t: Topic, program: String = "book/1"): Dialogue[String, String, String, P] =
    Dialogue[String, String, String, P](t, "b-1", program)(booking)

  /** A RECORD FROM ANOTHER DEPLOY, written straight into the topic: a
   * `Dialogue` for "book/2" refuses to append after folding a "book/1"
   * record, so two Dialogue objects cannot produce a mixed journal */
  def foreign(t: Topic, program: String, expect: Int, a: String): Unit = {
    val typed = Typed[Dialogue.Entry[String]](t, 1, Map.empty)
    val _ = typed.append(0, "b-1".getBytes("UTF-8"), Dialogue.Entry.Answered(program, expect, a): Dialogue.Entry[String], Ack.Durable)
  }

  test("nothing wrong: no diagnosis") {
    val t = new MemoryStore().topic("bookings")
    val d = dialogue(t)
    assertEquals(!.run(d.diagnosis), None)
    val _ = !.run(d.answer("Kyiv"))
    assertEquals(!.run(d.diagnosis), None)
  }

  test("THE POINT: a foreign record stops the fold, and the reader names its OWN line") {
    val t = new MemoryStore().topic("bookings")
    val mine = dialogue(t)
    val _ = !.run(mine.answer("Kyiv"))          // one good answer, accepted

    // a different deploy appends to the same journal
    foreign(t, "book/2", 1, "2")

    val found = !.run(mine.diagnosis).getOrElse(fail("the fold did not stop"))
    found.why match {
      case Dialogue.Stopped.Mismatch(_, wrote, expected) =>
        assertEquals(wrote, "book/2")
        assertEquals(expected, "book/1")
      case other => fail(s"expected a mismatch, got $other")
    }

    // where THIS program is standing, in this file
    assertEquals(found.accepted, 1)
    assertEquals(found.asking, Some("nights?"))
    assert(found.where.exists(_.contains("TestDiagnosis.scala")), s"the diagnosis does not name the reader's own file: ${found.where}")
    // the line TRACKS THE POSITION: a run at the first question names a
    // different line from one at the second
    val atFirst = {
      val fresh = new MemoryStore().topic("bookings")
      foreign(fresh, "book/2", 0, "x")
      !.run(dialogue(fresh).diagnosis).getOrElse(fail("the fold did not stop"))
    }
    assertEquals(atFirst.accepted, 0)
    assertEquals(atFirst.asking, Some("city?"))
    assertNotEquals(atFirst.where, found.where, s"the diagnosis names the same line wherever the program stands: ${found.where}")
  }

  test("damage too: an unreadable record is diagnosed the same way") {
    val t = new MemoryStore().topic("bookings")
    val d = dialogue(t)
    val _ = t.append(0, "b-1".getBytes("UTF-8"), Array[Byte](1, 2, 3), Ack.Durable)

    val found = !.run(d.diagnosis).getOrElse(fail("the fold did not stop"))
    assert(found.why.isInstanceOf[Dialogue.Stopped.Damage], s"got ${found.why}")
    assertEquals(found.accepted, 0)
    assertEquals(found.asking, Some("city?"), "a run with nothing accepted is at its first question")
    assert(found.where.isDefined)
  }

  test("it reads as one line, because that is where it will be read") {
    val t = new MemoryStore().topic("bookings")
    val mine = dialogue(t)
    val _ = !.run(mine.answer("Kyiv"))
    foreign(t, "book/2", 1, "2")

    val line = !.run(mine.diagnosis).get.toString
    assert(line.contains("after 1 answer(s)"), line)
    assert(line.contains("this program is at"), line)
    assert(line.contains("asking nights?"), line)
  }
}
