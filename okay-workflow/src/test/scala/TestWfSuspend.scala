package okay

import okay.Direct.*
import scala.language.implicitConversions

/**
 * A DRIVER THAT CAN STOP (workflow-suspended-driver, 2026-09-17).
 *
 * Durable timers, signals and child workflows look like three
 * features and are one: each is a question the driver CANNOT ANSWER
 * WHEN IT IS ASKED. The runtime declines, the drive returns `Waiting`
 * with what it is waiting ON, and somebody else — a scheduler, an API
 * call, the child's own completion — appends the answer later.
 *
 * Nothing about the journal changes, which is the point: these are
 * ordinary `Sys` questions, so replay, `patch` and the race check
 * carry over untouched.
 */
class TestWfSuspend extends munit.FunSuite {

  type P = okay.Pure
  type Row = Delim + P

  given Wf.Runtime = Wf.Runtime.scripted(millis = 1_000L, id = "id", dice = 0.5)

  /** approve, then wait a day, then finish */
  def overnight(using w: Wf.Asks[String, String, String, P]): String ! Row = direct:
    val who = !w.pause("who?")
    !w.sleep(86_400_000L)
    s"$who slept"

  test("a sleep STOPS the drive, and says when to come back") {
    val start = !.run(Wf.resumable[String, String, String, P](overnight))
    val (st, j) = !.run(Wf.drive(start)(_ => okay.pure("ada")))

    // the clock reading is journalled; the timer's answer is not,
    // because nobody could give it
    assertEquals(j, List(Right("ada"), Left(Wf.SysA.Millis(1_000L))))
    st match
      case Wf.Step.Waiting(Wf.Wait.Until(t)) => assertEquals(t, 1_000L + 86_400_000L)
      case other => fail(s"expected a wait until the deadline, got $other")
  }

  test("the deadline is JOURNALLED, so a replay does not move it") {
    val (st1, j) = !.run(Wf.drive(
      !.run(Wf.resumable[String, String, String, P](overnight)))(_ => okay.pure("ada")))

    // a runtime whose clock says something else entirely, handed to
    // the second drive explicitly (a `given` here would shadow the
    // first drive's too, and Scala refuses the forward reference)
    val later: Wf.Runtime = Wf.Runtime.scripted(millis = 9_999_999L, id = "x", dice = 0.1)
    val back = !.run(Wf.replay[String, String, String, P](overnight)(j))
    val (st2, _) = !.run(Wf.drive(back)(_ => fail("the oracle was asked again"))(
      using later, summon[Delim.OneMachine[P]]))
    assertEquals(st2, st1, "the replayed run chose a different deadline")
  }

  test("feed the elapsed answer and the run carries on to the end") {
    val (_, j) = !.run(Wf.drive(
      !.run(Wf.resumable[String, String, String, P](overnight)))(_ => okay.pure("ada")))

    // the scheduler appends this when the instant passes
    val woken = j :+ Left(Wf.SysA.Elapsed)
    val back = !.run(Wf.replay[String, String, String, P](overnight)(woken))
    assertEquals(back.finished, Some("ada slept"))
  }

  // ---- signals

  def approved(using w: Wf.Asks[String, String, String, P]): String ! Row = direct:
    val what = !w.pause("what?")
    val by = !w.awaitSignal("approval")
    s"$what approved by $by"

  test("a signal stops the drive, naming the channel it waits on") {
    val (st, j) = !.run(Wf.drive(
      !.run(Wf.resumable[String, String, String, P](approved)))(_ => okay.pure("the budget")))
    assertEquals(j, List(Right("the budget")))
    assertEquals(st, Wf.Step.Waiting(Wf.Wait.Signal("approval")))
  }

  test("the signal's payload arrives as an ordinary journal entry") {
    val woken = List(Right("the budget"), Left(Wf.SysA.Got("ada")))
    val back = !.run(Wf.replay[String, String, String, P](approved)(woken))
    assertEquals(back.finished, Some("the budget approved by ada"))
  }

  // ---- the worker's primitive

  test("advance: with no oracle, the AUTHOR's question comes back to be answered") {
    val (st, j) = !.run(Wf.advance[String, String, String, P](
      !.run(Wf.resumable[String, String, String, P](approved))))
    assertEquals(st, Wf.Step.Asking("what?"))
    assertEquals(j, Nil, "nothing should have been journalled")
  }

  test("advance runs the runtime's questions and stops at the first it cannot answer") {
    // the clock is answerable, the timer is not
    val (st, j) = !.run(Wf.advance[String, String, String, P](
      !.run(Wf.replay[String, String, String, P](overnight)(List(Right("ada"))))))
    assertEquals(j, List(Left(Wf.SysA.Millis(1_000L))), "the clock was not read and journalled")
    st match
      case Wf.Step.Waiting(Wf.Wait.Until(_)) => ()
      case other => fail(s"expected a wait, got $other")
  }

  test("a child is the same shape: wait on its id, take its answer as a payload") {
    def parent(using w: Wf.Asks[String, String, String, P]): String ! Row = direct:
      val r = !w.awaitChild("child-1")
      s"child said $r"
    val (st, _) = !.run(Wf.drive(
      !.run(Wf.resumable[String, String, String, P](parent)))(_ => okay.pure("")))
    assertEquals(st, Wf.Step.Waiting(Wf.Wait.Child("child-1")))

    val back = !.run(Wf.replay[String, String, String, P](parent)(
      List(Left(Wf.SysA.Got("42")))))
    assertEquals(back.finished, Some("child said 42"))
  }
}
