package okay2

import okay2.Optic.arrows._
import WfFixtures._

/** COMPENSATION AS STRUCTURE — the Scala 3 core's TestProcUndo: the
 * undos are FOUND at the paths where their steps ran, and what comes
 * back is a term the engine runs */
class TestProcUndo extends munit.FunSuite {

  type Sig = Wf.Asked[String, String]
  implicit val A: Optic.Arrow[Proc.Of[Sig]#L] with Optic.Choice[Proc.Of[Sig]#L] = Proc.procArrow[Sig]
  implicit val rt: Wf.Runtime = Wf.Runtime.scripted(millis = 1700000000000L, id = "id-1", dice = 0.25)

  /** ask something, and say how to take it back */
  def step(q: String, undo: String): Wf.Proc[String, String, Unit, String] =
    Proc.undoable(Wf.Proc.ask[String, String, Unit](_ => q))(
      Wf.Proc.ask[String, String, (Unit, String)](p => s"$undo ${p._2}") >>> A.arr((_: String) => ()))

  def step2(q: String, undo: String): Wf.Proc[String, String, List[String], String] =
    Proc.undoable(Wf.Proc.ask[String, String, List[String]](got => s"$q ${got.length + 1}?"))(
      Wf.Proc.ask[String, String, (List[String], String)](p => s"$undo ${p._2}") >>> A.arr((_: String) => ()))

  /** reserve, then charge — both compensable */
  val booking: Wf.Proc[String, String, Unit, (String, String)] =
    A.arr((_: Unit) => ((), ())) >>> A.first[Unit, String, Unit](step("reserve?", "release")) >>> A.second[Unit, String, String](step("charge?", "refund"))

  /** drive a compensation term and collect what it asked, in order */
  def questions(p: Wf.Proc[String, String, Unit, Unit]): List[String] = {
    val asked = List.newBuilder[String]
    def body(w: Wf.Asks[String, String, Unit, P]): Unit ! Rw = Wf.Proc.program(p)(())(w, implicitly)
    val _ = !.run(Wf.drive(!.run(Wf.resumable[String, String, Unit, P](body))) { q => asked += q; pure[P, String]("ok") })
    asked.result()
  }

  test("an Undo runs its STEP and nothing else") {
    def body(w: Wf.Asks[String, String, (String, String), P]): (String, String) ! Rw = Wf.Proc.program(booking)(())(w, implicitly)
    val (st, j) = !.run(Wf.drive(!.run(Wf.resumable[String, String, (String, String), P](body)))(_ => pure[P, String]("yes")))
    assertEquals(done(st), ("yes", "yes"))
    assertEquals(j.length, 2, "two steps, two questions — no compensation ran")
  }

  test("nothing done, nothing to undo; one done, its own; both done, the last first") {
    assertEquals(questions(Wf.Proc.compensating(booking)((), Nil)), Nil)
    assertEquals(questions(Wf.Proc.compensating(booking)((), List(Right("r-7")))), List("release r-7"))
    assertEquals(questions(Wf.Proc.compensating(booking)((), List(Right("r-7"), Right("c-9")))), List("refund c-9", "release r-7"))
    val c = Wf.Proc.compensating(booking)((), List(Right("r-7"), Right("c-9")))
    assertEquals(c.leaves.map(_.name), Vector("ask", "ask"))
  }

  test("A LOOP'S ROUNDS ARE EACH UNDONE, newest first") {
    val body: Wf.Proc[String, String, List[String], Either[List[String], List[String]]] =
      Proc.alongside(step2("room", "cancel")) >>> A.arr { (p: (List[String], String)) =>
        val next = p._1 :+ p._2
        if (next.length < 3) Left(next) else Right(next)
      }
    val nights: Wf.Proc[String, String, List[String], List[String]] = Proc.iter(body)
    assertEquals(questions(Wf.Proc.compensating(nights)(Nil, List(Right("a"), Right("b"), Right("c")))), List("cancel c", "cancel b", "cancel a"))
    assertEquals(questions(Wf.Proc.compensating(nights)(Nil, List(Right("a"), Right("b")))), List("cancel b", "cancel a"))
  }

  test("a branch not taken leaves nothing to undo; a Par's UNANSWERED branch is not compensated") {
    val either: Wf.Proc[String, String, Either[String, Unit], Either[String, String]] = Proc.onRight(step("charge?", "refund"))
    assertEquals(questions(Wf.Proc.compensating(either)(Left("skipped"), Nil)), Nil)
    assertEquals(questions(Wf.Proc.compensating(either)(Right(()), List(Right("c-9")))), List("refund c-9"))
    val slot: Wf.Proc[String, String, Unit, String] =
      Proc.undoable(A.arr((_: Unit) => "slot-1"))(Wf.Proc.ask[String, String, (Unit, String)](p => s"free ${p._2}") >>> A.arr((_: String) => ())) >>>
        A.arr((_: String) => ()) >>> Wf.Proc.ask[String, String, Unit](_ => "charge?")
    val pair: Wf.Proc[String, String, Unit, (String, String)] = Proc.par(step("reserve?", "release"), slot)
    assertEquals(questions(Wf.Proc.compensating(pair)((), Nil)), Nil)
    assertEquals(questions(Wf.Proc.compensating(pair)((), List(Right("r-7")))), List("free slot-1", "release r-7"))
  }

  test("the undos are IN the term, and the picture puts the inverse off the path") {
    val ls = booking.leaves
    assertEquals(ls.length, 4, "two steps and two compensations")
    assertEquals(ls.count(_.at.show.split("/").contains("undo")), 2)
    assertEquals(ls.count(_.at.show.split("/").contains("do")), 2)
    assert(step("charge?", "refund").render().contains("undoable"))
    val m = step("charge?", "refund").mermaid()
    assert(m.contains("-.->") && m.contains("""{"on failure"}"""), m)
  }
}
