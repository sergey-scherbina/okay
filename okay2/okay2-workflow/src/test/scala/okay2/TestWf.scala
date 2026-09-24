package okay2

import WfFixtures._

/** THE LIBRARY'S OWN QUESTIONS — the Scala 3 core's okay-workflow
 * TestWf: the clock, the id and the die are questions too, remembered
 * in the journal; the TAG makes a program changeable */
class TestWf extends munit.FunSuite {

  implicit val rt: Wf.Runtime = Wf.Runtime.scripted(millis = 1700000000000L, id = "id-1", dice = 0.25)

  /** a booking that asks the world for a city and the RUNTIME for the
   * time and an id; the four types are named once, in the signature */
  def booking(w: Wf.Asks[String, String, String, P]): String ! Rw =
    for { city <- w.pause("city?"); t <- w.now; id <- w.uuid } yield s"$city/$t/$id"

  test("the runtime answers its own questions; the oracle answers the author's") {
    var asked = List.empty[String]
    val start = !.run(Wf.resumable[String, String, String, P](booking))
    val (st, j) = !.run(Wf.drive(start) { q => asked = asked :+ q; pure[P, String]("Kyiv") })
    assertEquals(done(st), "Kyiv/1700000000000/id-1")
    assertEquals(asked, List("city?"), "the oracle was asked the runtime's questions too")
    assertEquals(j, List(Right("Kyiv"), Left(Wf.SysA.Millis(1700000000000L)), Left(Wf.SysA.Text("id-1"))))
  }

  test("replay gives the same values — the clock is read from the journal, not the wall") {
    val (st1, j) = !.run(Wf.drive(!.run(Wf.resumable[String, String, String, P](booking)))(_ => pure[P, String]("Kyiv")))
    val back = !.run(Wf.replay[String, String, String, P](booking)(j))
    assertEquals(back.finished, Some(done(st1)), "replay asked the runtime again")
  }

  test("a die and a clock are each read ONCE, however often the program is replayed") {
    var reads = 0
    val counting: Wf.Runtime = new Wf.Runtime {
      def answer(q: Wf.Sys): Either[Wf.Wait, Wf.SysA] = {
        reads += 1
        q match {
          case Wf.Sys.Random => Right(Wf.SysA.Dice(0.5))
          case Wf.Sys.Now => Right(Wf.SysA.Millis(7L))
          case Wf.Sys.Uuid => Right(Wf.SysA.Text("x"))
          case _ => Right(Wf.SysA.Flag(true))
        }
      }
    }
    def dicey(w: Wf.Asks[String, Double, Double, P]): Double ! Rw = w.random
    val (stv, j) = !.run(Wf.drive(!.run(Wf.resumable[String, Double, Double, P](dicey)))(_ => pure[P, Double](0.0))(counting, implicitly))
    val v = done(stv)
    assertEquals(reads, 1)
    assertEquals(!.run(Wf.replay[String, Double, Double, P](dicey)(j)).finished, Some(v))
    assertEquals(reads, 1, "replay rolled the die again")
  }

  /** v1: ask the city, then the nights */
  def v1(w: Wf.Asks[String, String, String, P]): String ! Rw =
    for { city <- w.pause("city?"); n <- w.pause("nights?") } yield s"$city/$n"

  /** v2: the same, with a branch added BETWEEN the two questions */
  def v2(w: Wf.Asks[String, String, String, P]): String ! Rw =
    for { city <- w.pause("city?"); on <- w.patch("promo"); n <- w.pause("nights?") } yield
      if (on) s"$city/$n/promo" else s"$city/$n"

  def cityThenNights(q: String): String ! P = pure[P, String](if (q == "city?") "Kyiv" else "3")

  test("patch: a journal written BEFORE the branch existed takes the old path") {
    val (stOld, j) = !.run(Wf.drive(!.run(Wf.resumable[String, String, String, P](v1)))(cityThenNights))
    assertEquals(done(stOld), "Kyiv/3")
    assertEquals(j, List(Right("Kyiv"), Right("3")))
    val back = !.run(Wf.replay[String, String, String, P](v2)(j))
    assertEquals(back.finished, Some("Kyiv/3"), "the patch ate the answer that followed it, or took the new branch")
  }

  test("patch: a run that STARTS under v2 takes the new path, and it is journalled") {
    val (stFresh, j) = !.run(Wf.drive(!.run(Wf.resumable[String, String, String, P](v2)))(q => pure[P, String](if (q == "city?") "Lviv" else "2")))
    assertEquals(done(stFresh), "Lviv/2/promo")
    assertEquals(j, List(Right("Lviv"), Left(Wf.SysA.Flag(true)), Right("2")))
    assertEquals(!.run(Wf.replay[String, String, String, P](v2)(j)).finished, Some("Lviv/2/promo"))
  }

  test("patch: the old journal replays under v2 and can still be FINISHED live") {
    val at = !.run(Wf.replay[String, String, String, P](v2)(List(Right("Kyiv"))))
    val (st, more) = !.run(Wf.drive(at)(_ => pure[P, String]("4")))
    assertEquals(done(st), "Kyiv/4/promo")
    assertEquals(more, List(Left(Wf.SysA.Flag(true)), Right("4")))
  }

  test("replaying pairs every answer with the question it answered") {
    val j: Wf.Journal[String] = List(Right("Kyiv"), Right("3"))
    val (_, seen) = !.run(Wf.replaying[String, String, String, P](v2)(j))
    assertEquals(seen, List((Right("city?"), Right("Kyiv")), (Left(Wf.Sys.Patch("promo")), Left(Wf.SysA.Flag(false))), (Right("nights?"), Right("3"))))
  }

  test("cancellation is a question: the runtime answers it when asked, and the answer is journalled") {
    var why: Option[String] = None
    val rt2 = Wf.Runtime.cancellable(rt)(why)
    def guarded(w: Wf.Asks[String, String, String, P]): String ! Rw =
      for { a <- w.pause("first?"); c <- w.cancelled } yield c.fold(s"$a done")(r => s"cancelled: $r")
    val (st, j) = !.run(Wf.drive(!.run(Wf.resumable[String, String, String, P](guarded)))(_ => { why = Some("operator"); pure[P, String]("x") })(rt2, implicitly))
    assertEquals(done(st), "cancelled: operator")
    assertEquals(j.last, Left(Wf.SysA.Text("operator")))
    assertEquals(Wf.Next.seed(Wf.Next.Continue(5)), Some(5))
  }
}
