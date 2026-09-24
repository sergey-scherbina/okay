package okay2

import WfFixtures._

/** TWO INDEPENDENT BRANCHES, and what is parallel about them: the
 * WAITING — both questions known at once, answers still recorded left
 * then right. The Scala 3 core's TestProcPar */
class TestProcPar extends munit.FunSuite {

  implicit val rt: Wf.Runtime = Wf.Runtime.scripted(millis = 1700000000000L, id = "id-1", dice = 0.25)

  val approvals: Wf.Proc[String, String, Unit, (String, String)] =
    Proc.par(Wf.Proc.ask[String, String, Unit](_ => "finance?"), Wf.Proc.ask[String, String, Unit](_ => "legal?"))

  def program(w: Wf.Asks[String, String, (String, String), P]): (String, String) ! Rw = Wf.Proc.program(approvals)(())(w, implicitly)

  /** the author's question, read through its journal spelling */
  def asked(q: Wf.Question[String, String, _]): String = Wf.Proc.tag(q) match {
    case Right(a) => a
    case other => fail(s"expected an Ask, got $other")
  }

  test("a Par with nothing answered is waiting on BOTH questions, in term order") {
    Wf.Proc.walk(approvals)((), Nil) match {
      case Right(Wf.Proc.Standing.Waiting(on, accepted)) =>
        assertEquals(on.map(_._1.show), Vector("par0", "par1"))
        assertEquals(on.map(p => asked(p._2)), Vector("finance?", "legal?"))
        assertEquals(accepted, 0)
      case other => fail(s"expected a Waiting on two questions, got $other")
    }
  }

  test("THE JOURNAL IS STILL ORDERED: one answer leaves one question, and it is the right one") {
    Wf.Proc.walk(approvals)((), List(Right("yes"))) match {
      case Right(Wf.Proc.Standing.Asking(at, q, accepted)) =>
        assertEquals(at.show, "par1")
        assertEquals(asked(q), "legal?")
        assertEquals(accepted, 1)
      case other => fail(s"expected one question outstanding, got $other")
    }
    assertEquals(Wf.Proc.walk(approvals)((), List(Right("yes"), Right("ok"))), Right(Wf.Proc.Standing.Done[String, String, (String, String)](("yes", "ok"))))
  }

  test("`pending` reads a Waiting and an Asking alike") {
    assertEquals(Wf.Proc.walk(approvals)((), Nil).toOption.get.pending.length, 2)
    assertEquals(Wf.Proc.walk(approvals)((), List(Right("yes"))).toOption.get.pending.length, 1)
    assertEquals(Wf.Proc.walk(approvals)((), List(Right("y"), Right("o"))).toOption.get.pending.length, 0)
  }

  test("a Par whose other branch asks nothing is an ordinary wait") {
    val lopsided: Wf.Proc[String, String, Unit, (String, Int)] =
      Proc.par(Wf.Proc.ask[String, String, Unit](_ => "finance?"), Proc.arr[Wf.Asked[String, String], Unit, Int](_ => 42))
    Wf.Proc.walk(lopsided)((), Nil) match {
      case Right(Wf.Proc.Standing.Asking(at, q, _)) =>
        assertEquals(at.show, "par0")
        assertEquals(asked(q), "finance?")
      case other => fail(s"expected a plain Asking, got $other")
    }
  }

  test("the run answers the pair on the engine, and walk agrees with replay on every prefix") {
    val (st, j) = !.run(Wf.drive(!.run(Wf.resumable[String, String, (String, String), P](program)))(q => pure[P, String](if (q == "finance?") "yes" else "ok")))
    assertEquals(done(st), ("yes", "ok"))
    assertEquals(j, List(Right("yes"), Right("ok")))
    for (n <- 0 to j.length) {
      val prefix = j.take(n)
      val byTerm = Wf.Proc.walk(approvals)((), prefix) match {
        case Right(Wf.Proc.Standing.Done(y)) => Right(Left(y.toString))
        case Right(s) => Right(Right(Wf.Proc.tag(s.pending.head._2)))
        case Left(bad) => Left(bad.toString)
      }
      val paused = !.run(Wf.replay[String, String, (String, String), P](program)(prefix))
      val byReplay = paused.finished match {
        case Some(y) => Right(Left(y.toString))
        case None => Right(Right(paused.asking.get))
      }
      assertEquals(byTerm, byReplay, s"the two readings disagree at $n answers")
    }
  }

  test("leaves, render and the picture: both branches, a fork and ONE join") {
    assertEquals(approvals.leaves.map(_.at.show), Vector("par0", "par1"))
    assertEquals(approvals.render(), "par\n  ask\n  ask\n")
    val m = approvals.mermaid()
    assert(m.contains("""{{"both"}}"""), m)
    assertEquals(m.linesIterator.count(_.contains("--> q")), 2, m)
    val at = Wf.Proc.walk(approvals)((), List(Right("yes"))) match {
      case Right(Wf.Proc.Standing.Asking(a, _, _)) => a
      case other => fail(s"expected an Asking, got $other")
    }
    assert(approvals.render(Some(at)).contains("  ask  <-- here"), approvals.render(Some(at)))
  }

  test("foldMap runs both branches, left first, and pairs what they answer") {
    val add = Proc.op[Tick, Int, Int]("add")(x => Tick.Add(x))
    assertEquals(Proc.par(add, add).foldMap(Tick.run)(Tick.counting)(5)(0), (10, (5, 10)))
  }
}
