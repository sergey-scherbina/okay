package okay.persist

import munit.FunSuite
import okay.{!, +, Delim, Pure, Wf}
import okay.Direct.*
import okay.codec.Schema
import scala.language.implicitConversions

/**
 * EVIDENCE FOR DELETING CODE (workflow-retire, 2026-09-17).
 *
 * The last test is the one that pays for the replay. A journal holds
 * ANSWERS and a `patch` id lives in the QUESTION, so no reader of
 * records can say which branch a `Flag(true)` belongs to — only
 * running the program pairs them up. Everything above it is cheaper
 * and answers a coarser question.
 */
class TestRetire extends FunSuite {

  type Row = Delim + Pure

  def three(using Delim.Asking[String, String, String, Row]): String ! Row = direct:
    val a = !Delim.pause("1?")
    val b = !Delim.pause("2?")
    val c = !Delim.pause("3?")
    s"$a$b$c"

  def dialogue(t: Topic, id: String, program: String = "three/1") =
    Dialogue[String, String, String, Pure](t, id, program)(three)

  test("the census: which programs wrote here, for which runs") {
    val t = MemoryStore().topic("runs")
    val _ = !.run(dialogue(t, "a-1").answer("x"))
    val _ = !.run(dialogue(t, "a-2").answer("y"))
    val _ = !.run(dialogue(t, "b-1", "three/2").answer("z"))

    val c = Retire.census[String](t)
    assertEquals(c.programs.keySet, Set("three/1", "three/2"))
    assertEquals(c.programs("three/1").ids, Set("a-1", "a-2"))
    assertEquals(c.programs("three/1").records, 2)
    assertEquals(c.programs("three/2").ids, Set("b-1"))

    // the one-line answer a deletion actually wants
    assert(c.gone("three/3"), "a program that never wrote here is not reported gone")
    assert(!c.gone("three/1"))
  }

  test("a record it cannot read is NAMED, not counted as absence") {
    val t = MemoryStore().topic("runs")
    val _ = !.run(dialogue(t, "a-1").answer("x"))
    // something else wrote into this topic, or a record rotted
    val _ = t.append(0, "a-1".getBytes("UTF-8"), Array[Byte](1, 2, 3), Ack.Durable)

    val c = Retire.census[String](t)
    assertEquals(c.programs("three/1").records, 1)
    assertEquals(c.unreadable.size, 1, s"the unreadable record was swallowed: $c")
  }

  test("the states: who is still asking, who finished, who is stuck") {
    val t = MemoryStore().topic("runs")
    val _ = !.run(dialogue(t, "asking").answer("x"))

    val done = dialogue(t, "done")
    val _ = !.run(done.answer("x"))
    val _ = !.run(done.answer("y"))
    val _ = !.run(done.answer("z"))

    // a run whose journal was written by another program
    val _ = !.run(dialogue(t, "stuck", "three/2").answer("x"))

    val states = !.run(Retire.states[String, String, String, Pure](
      List("asking", "done", "stuck"))(dialogue(t, _)))

    states("asking") match
      case Retire.State.Asking(q, where) =>
        assertEquals(q, "2?")
        assert(where.isDefined, "a live run does not say WHERE it is standing")
      case other => fail(s"a run with one answer of three is not asking: $other")
    assertEquals(states("done"), Retire.State.Finished)
    assert(states("stuck").isInstanceOf[Retire.State.Stopped], s"got ${states("stuck")}")
  }

  // ==== the branch census ==========================================

  given Schema[Wf.SysA] = Schema.derived
  given Wf.Runtime = Wf.Runtime.scripted(millis = 1L, id = "id", dice = 0.5)

  def v2(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure =
    direct:
      val city = !w.pause("city?")
      val promo = !w.patch("promo")
      val nights = !w.pause("nights?")
      if promo then s"$city/$nights/promo" else s"$city/$nights"

  test("THE POINT: which patch branches are still live, which needs the BODY") {
    // a journal written BEFORE the branch existed: no decision for it,
    // and the next record answers the question after it
    val old = List(Right("Kyiv"), Right("2"))
    // and one written after: the decision is in the log
    val fresh = List(Right("Lviv"), Left(Wf.SysA.Flag(true)), Right("3"))

    val branches = !.run(Retire.patches[String, String, String, Pure](
      List("old-1" -> old, "new-1" -> fresh))(v2))

    val promo = branches("promo")
    assertEquals(promo.taken, Set("new-1"))
    assertEquals(promo.skipped, Set("old-1"),
      "the run that predates the branch was not reported as still on the old half")
    assert(!promo.oldHalfDead, "the else-branch was declared dead with a run still on it")
  }

  test("once the old runs are gone, the old half is dead and can be deleted") {
    val fresh = List(Right("Lviv"), Left(Wf.SysA.Flag(true)), Right("3"))
    val branches = !.run(Retire.patches[String, String, String, Pure](
      List("new-1" -> fresh, "new-2" -> fresh))(v2))

    assertEquals(branches("promo").skipped, Set.empty[String])
    assert(branches("promo").oldHalfDead)
  }
}
