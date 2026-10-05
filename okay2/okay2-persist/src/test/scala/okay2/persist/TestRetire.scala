package okay2.persist

import munit.FunSuite
import okay2.{!, Shift}
import okay2.workflow.Wf

/**
 * EVIDENCE FOR DELETING CODE (okay-persist's TestRetire;
 * workflow-retire). A journal holds ANSWERS and a `patch` id lives in
 * the QUESTION, so only running the program pairs a `Flag(true)` with
 * its branch.
 */
class TestRetire extends FunSuite {
  import DialogueFixtures._
  import WorkflowFixtures._

  def three(s: Shift.Asking.Aux[String, String, String, P]): String ! Rw = for {
    a <- Shift.pause(s)("1?")
    b <- Shift.pause(s)("2?")
    c <- Shift.pause(s)("3?")
  } yield s"$a$b$c"

  def dialogue(t: Topic, id: String, program: String = "three/1"): Dialogue[String, String, String, P] =
    Dialogue[String, String, String, P](t, id, program)(three)

  test("the census: which programs wrote here, for which runs") {
    val t = new MemoryStore().topic("runs")
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
    val t = new MemoryStore().topic("runs")
    val _ = !.run(dialogue(t, "a-1").answer("x"))
    val _ = t.append(0, "a-1".getBytes("UTF-8"), Array[Byte](1, 2, 3), Ack.Durable)

    val c = Retire.census[String](t)
    assertEquals(c.programs("three/1").records, 1)
    assertEquals(c.unreadable.size, 1, s"the unreadable record was swallowed: $c")
  }

  test("the states: who is still asking, who finished, who is stuck") {
    val t = new MemoryStore().topic("runs")
    val _ = !.run(dialogue(t, "asking").answer("x"))

    val finished = dialogue(t, "done")
    val _ = !.run(finished.answer("x"))
    val _ = !.run(finished.answer("y"))
    val _ = !.run(finished.answer("z"))

    // a run whose journal was written by another program
    val _ = !.run(dialogue(t, "stuck", "three/2").answer("x"))

    val states = !.run(Retire.states[String, String, String, P](List("asking", "done", "stuck"))(dialogue(t, _)))

    states("asking") match {
      case Retire.State.Asking(q, where) =>
        assertEquals(q, "2?")
        assert(where.isDefined, "a live run does not say WHERE it is standing")
      case other => fail(s"a run with one answer of three is not asking: $other")
    }
    assertEquals(states("done"), Retire.State.Finished: Retire.State)
    assert(states("stuck").isInstanceOf[Retire.State.Stopped], s"got ${states("stuck")}")
  }

  // ==== the branch census ==========================================

  implicit val rt: Wf.Runtime = Wf.Runtime.scripted(millis = 1L, id = "id", dice = 0.5)

  def v2(w: W): String ! Rw = for {
    city <- w.pause("city?")
    promo <- w.patch("promo")
    nights <- w.pause("nights?")
  } yield if (promo) s"$city/$nights/promo" else s"$city/$nights"

  test("THE POINT: which patch branches are still live, which needs the BODY") {
    // a journal written BEFORE the branch existed, and one written after
    val old: List[Wf.Ans[String]] = List(Right("Kyiv"), Right("2"))
    val fresh: List[Wf.Ans[String]] = List(Right("Lviv"), Left(Wf.SysA.Flag(true)), Right("3"))

    val branches = !.run(Retire.patches[String, String, String, P](List("old-1" -> old, "new-1" -> fresh))(v2))

    val promo = branches("promo")
    assertEquals(promo.taken, Set("new-1"))
    assertEquals(promo.skipped, Set("old-1"), "the run that predates the branch was not reported as still on the old half")
    assert(!promo.oldHalfDead, "the else-branch was declared dead with a run still on it")
  }

  test("once the old runs are gone, the old half is dead and can be deleted") {
    val fresh: List[Wf.Ans[String]] = List(Right("Lviv"), Left(Wf.SysA.Flag(true)), Right("3"))
    val branches = !.run(Retire.patches[String, String, String, P](List("new-1" -> fresh, "new-2" -> fresh))(v2))

    assertEquals(branches("promo").skipped, Set.empty[String])
    assert(branches("promo").oldHalfDead)
  }
}
