package okay2

import WfFixtures._

/** A DRIVER THAT CAN STOP — the Scala 3 core's TestWfSuspend: timers,
 * signals and child workflows are one feature, a question the driver
 * cannot answer when it is asked */
class TestWfSuspend extends munit.FunSuite {

  implicit val rt: Wf.Runtime = Wf.Runtime.scripted(millis = 1000L, id = "id", dice = 0.5)

  /** approve, then wait a day, then finish */
  def overnight(w: Wf.Asks[String, String, String, P]): String ! Rw =
    for { who <- w.pause("who?"); _ <- w.sleep(86400000L) } yield s"$who slept"

  def ada(q: String): String ! P = pure[P, String]("ada")

  test("a sleep STOPS the drive, and says when to come back") {
    val (st, j) = !.run(Wf.drive(!.run(Wf.resumable[String, String, String, P](overnight)))(ada))
    assertEquals(j, List(Right("ada"), Left(Wf.SysA.Millis(1000L))))
    assertEquals(st, Wf.Step.Waiting[String, String](Wf.Wait.Until(1000L + 86400000L)))
  }

  test("the deadline is JOURNALLED, so a replay does not move it") {
    val (st1, j) = !.run(Wf.drive(!.run(Wf.resumable[String, String, String, P](overnight)))(ada))
    val later: Wf.Runtime = Wf.Runtime.scripted(millis = 9999999L, id = "x", dice = 0.1)
    val back = !.run(Wf.replay[String, String, String, P](overnight)(j))
    val (st2, _) = !.run(Wf.drive(back)(_ => fail("the oracle was asked again"))(later, implicitly))
    assertEquals(st2, st1, "the replayed run chose a different deadline")
  }

  test("feed the elapsed answer and the run carries on to the end") {
    val (_, j) = !.run(Wf.drive(!.run(Wf.resumable[String, String, String, P](overnight)))(ada))
    val back = !.run(Wf.replay[String, String, String, P](overnight)(j :+ Left(Wf.SysA.Elapsed)))
    assertEquals(back.finished, Some("ada slept"))
  }

  def approved(w: Wf.Asks[String, String, String, P]): String ! Rw =
    for { what <- w.pause("what?"); by <- w.awaitSignal("approval") } yield s"$what approved by $by"

  test("a signal stops the drive, naming the channel; its payload arrives as a journal entry") {
    val (st, j) = !.run(Wf.drive(!.run(Wf.resumable[String, String, String, P](approved)))(_ => pure[P, String]("the budget")))
    assertEquals(j, List(Right("the budget")))
    assertEquals(st, Wf.Step.Waiting[String, String](Wf.Wait.Signal("approval")))
    val back = !.run(Wf.replay[String, String, String, P](approved)(List(Right("the budget"), Left(Wf.SysA.Got("ada")))))
    assertEquals(back.finished, Some("the budget approved by ada"))
  }

  test("advance: with no oracle, the AUTHOR's question comes back to be answered") {
    val (st, j) = !.run(Wf.advance[String, String, String, P](!.run(Wf.resumable[String, String, String, P](approved))))
    assertEquals(st, Wf.Step.Asking[String, String]("what?"))
    assertEquals(j, Nil, "nothing should have been journalled")
  }

  test("advance runs the runtime's questions and stops at the first it cannot answer") {
    val (st, j) = !.run(Wf.advance[String, String, String, P](!.run(Wf.replay[String, String, String, P](overnight)(List(Right("ada"))))))
    assertEquals(j, List(Left(Wf.SysA.Millis(1000L))), "the clock was not read and journalled")
    assert(st.isInstanceOf[Wf.Step.Waiting[_, _]], s"expected a wait, got $st")
  }

  test("a child is the same shape: wait on its id, take its answer as a payload") {
    def parent(w: Wf.Asks[String, String, String, P]): String ! Rw = w.awaitChild("child-1").map(r => s"child said $r")
    val (st, _) = !.run(Wf.drive(!.run(Wf.resumable[String, String, String, P](parent)))(_ => pure[P, String]("")))
    assertEquals(st, Wf.Step.Waiting[String, String](Wf.Wait.Child("child-1")))
    assertEquals(!.run(Wf.replay[String, String, String, P](parent)(List(Left(Wf.SysA.Got("42"))))).finished, Some("child said 42"))
  }
}
