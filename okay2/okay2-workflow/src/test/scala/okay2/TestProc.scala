package okay2

import okay2.Optic.arrows._
import WfFixtures._

/** THE DURABLE SPINE, WRITTEN AS A TERM — the Scala 3 core's TestProc:
 * the static and the monadic booking write the same journal, and a term
 * can name its leaves, draw itself and say where it stands without
 * running. (`Proc.direct` is the core's macro and is not here.) */
class TestProc extends munit.FunSuite {

  type Sig = Wf.Asked[String, String]
  implicit val A: Optic.Arrow[Proc.Of[Sig]#L] with Optic.Choice[Proc.Of[Sig]#L] = Proc.procArrow[Sig]
  implicit val rt: Wf.Runtime = Wf.Runtime.scripted(millis = 1700000000000L, id = "id-1", dice = 0.25)

  /** run a step and KEEP what went in */
  def keep[X, Y](p: Wf.Proc[String, String, X, Y]): Wf.Proc[String, String, X, (X, Y)] =
    A.arr((x: X) => (x, x)) >>> A.second[X, Y, X](p)

  val bookingTerm: Wf.Proc[String, String, Unit, String] =
    Wf.Proc.ask[String, String, Unit](_ => "city?") >>>
      keep(Wf.Proc.now[String, String, String]) >>>
      A.arr((p: (String, Long)) => p._1) >>>
      keep(Wf.Proc.uuid[String, String, String]) >>>
      A.arr((p: (String, String)) => s"${p._1}/${p._2}")

  def bookingStatic(w: Wf.Asks[String, String, String, P]): String ! Rw = Wf.Proc.program(bookingTerm)(())(w, implicitly)

  def bookingMonadic(w: Wf.Asks[String, String, String, P]): String ! Rw =
    for { city <- w.pause("city?"); _ <- w.now; id <- w.uuid } yield s"$city/$id"

  def kyiv(q: String): String ! P = pure[P, String]("Kyiv")

  def journalOf(body: Wf.Asks[String, String, String, P] => String ! Rw): Wf.Journal[String] =
    !.run(Wf.drive(!.run(Wf.resumable[String, String, String, P](body)))(kyiv))._2

  test("the static booking runs on the engine, and answers") {
    val (st, j) = !.run(Wf.drive(!.run(Wf.resumable[String, String, String, P](bookingStatic)))(kyiv))
    assertEquals(done(st), "Kyiv/id-1")
    assertEquals(j, List(Right("Kyiv"), Left(Wf.SysA.Millis(1700000000000L)), Left(Wf.SysA.Text("id-1"))))
  }

  test("ONE JOURNAL: the static and the monadic booking write the same records") {
    assertEquals(journalOf(bookingStatic), journalOf(bookingMonadic))
  }

  test("an existing monadic journal is accepted by the static term") {
    val monadic = journalOf(bookingMonadic)
    assert(Wf.Proc.accepts(bookingTerm)((), monadic))
    assertEquals(Wf.Proc.walk(bookingTerm)((), monadic), Right(Wf.Proc.Standing.Done[String, String, String]("Kyiv/id-1")))
  }

  test("leaves: every operation the term MAY reach — both sides of a choice, an Iter's body once") {
    assertEquals(bookingTerm.leaves.map(_.name), Vector("ask", "now", "uuid"))
    val branch: Wf.Proc[String, String, Either[Unit, Unit], Either[String, String]] =
      A.right[Unit, String, Unit](Wf.Proc.ask[String, String, Unit](_ => "right?")) >>>
        A.arr((e: Either[Unit, String]) => e.swap) >>>
        A.right[Unit, String, String](Wf.Proc.ask[String, String, Unit](_ => "left?")) >>>
        A.arr((e: Either[String, String]) => e.swap)
    assertEquals(branch.leaves.map(_.name), Vector("ask", "ask"))
    val loop: Wf.Proc[String, String, Int, String] =
      Proc.iter(Wf.Proc.ask[String, String, Int](i => s"room $i?") >>> A.arr((r: String) => if (r == "done") Right(r) else Left(0)))
    assertEquals(loop.leaves.map(_.name), Vector("ask"))
  }

  test("render draws the term, and marks a position") {
    val drawn = bookingTerm.render()
    assert(drawn.contains("ask") && drawn.contains("now") && drawn.contains("uuid"), drawn)
    val at = Wf.Proc.walk(bookingTerm)((), Nil) match {
      case Right(Wf.Proc.Standing.Asking(p, _, _)) => p
      case other => fail(s"expected a standing question, got $other")
    }
    assert(bookingTerm.render(Some(at)).contains("<-- here"))
  }

  /** the two readings of a position — by the term, and by replaying the program */
  def agreeOnEveryPrefix[Y](term: Wf.Proc[String, String, Unit, Y], body: Wf.Asks[String, String, Y, P] => Y ! Rw, full: Wf.Journal[String]): Unit =
    for (n <- 0 to full.length) {
      val prefix = full.take(n)
      val byTerm = Wf.Proc.walk(term)((), prefix) match {
        case Right(Wf.Proc.Standing.Done(y)) => Right(Left(y))
        case Right(st) => Right(Right(Wf.Proc.tag(st.pending.head._2)))
        case Left(bad) => Left(bad.toString)
      }
      val paused = !.run(Wf.replay[String, String, Y, P](body)(prefix))
      val byReplay = paused.finished match {
        case Some(y) => Right(Left(y))
        case None => Right(Right(paused.asking.getOrElse(fail("neither done nor asking"))))
      }
      assertEquals(byTerm, byReplay, s"the two readings of a position disagree at $n answers")
    }

  test("the keystone: walk and Wf.replay agree on EVERY prefix of the journal") {
    agreeOnEveryPrefix[String](bookingTerm, bookingStatic, journalOf(bookingStatic))
  }

  test("walk counts the records it accepted, and stops where the journal runs out") {
    Wf.Proc.walk(bookingTerm)((), journalOf(bookingStatic).take(2)) match {
      case Right(Wf.Proc.Standing.Asking(_, q, accepted)) =>
        assertEquals(Wf.Proc.tag(q), Left(Wf.Sys.Uuid): Wf.Ask[String])
        assertEquals(accepted, 2)
      case other => fail(s"expected to stand at uuid, got $other")
    }
  }

  /** ask how many nights, then one room per night: a shape that
   * depends on an answer, which is what `Iter` is for */
  val rooms: Wf.Proc[String, String, Unit, List[String]] = {
    val askNights = Wf.Proc.ask[String, String, Unit](_ => "nights?") >>> A.arr((n: String) => (n.toInt, List.empty[String]))
    val body: Wf.Proc[String, String, (Int, List[String]), Either[(Int, List[String]), List[String]]] =
      keep(Wf.Proc.ask[String, String, (Int, List[String])](s => s"room ${s._2.length + 1}?")) >>>
        A.arr { (p: ((Int, List[String]), String)) =>
          val (n, got) = p._1
          val next = got :+ p._2
          if (next.length >= n) Right(next) else Left((n, next))
        }
    askNights >>> Proc.iter(body)
  }

  def roomsProgram(w: Wf.Asks[String, String, List[String], P]): List[String] ! Rw = Wf.Proc.program(rooms)(())(w, implicitly)

  test("a loop whose trip count is an ANSWER, on the engine and in the walk") {
    var asked = List.empty[String]
    val (st, j) = !.run(Wf.drive(!.run(Wf.resumable[String, String, List[String], P](roomsProgram))) { q =>
      asked = asked :+ q
      pure[P, String](if (q == "nights?") "3" else s"r${asked.length - 1}")
    })
    assertEquals(done(st), List("r1", "r2", "r3"))
    assertEquals(asked, List("nights?", "room 1?", "room 2?", "room 3?"))
    assertEquals(Wf.Proc.walk(rooms)((), j), Right(Wf.Proc.Standing.Done[String, String, List[String]](List("r1", "r2", "r3"))))
  }

  test("the position after two rooms names the ROUND it is on; walk and replay agree over the loop") {
    Wf.Proc.walk(rooms)((), List(Right("3"), Right("a"), Right("b"))) match {
      case Right(Wf.Proc.Standing.Asking(at, q, accepted)) =>
        assertEquals(Wf.Proc.tag(q), Right("room 3?"): Wf.Ask[String])
        assertEquals(accepted, 3)
        assert(at.show.contains("round2"), s"the path must say which turn of the loop it is on, got ${at.show}")
      case other => fail(s"expected to stand inside the loop, got $other")
    }
    agreeOnEveryPrefix[List[String]](rooms, roomsProgram, List(Right("2"), Right("a"), Right("b")))
  }

  /** v2: a patch BETWEEN the two questions v1 asked */
  val v2: Wf.Proc[String, String, Unit, String] =
    Wf.Proc.ask[String, String, Unit](_ => "city?") >>>
      keep(Wf.Proc.patch[String, String, String]("promo")) >>>
      keep(Wf.Proc.ask[String, String, (String, Boolean)](_ => "nights?")) >>>
      A.arr((p: ((String, Boolean), String)) => s"${p._1._1}/${if (p._1._2) "promo" else "plain"}/${p._2}")

  def v2Program(w: Wf.Asks[String, String, String, P]): String ! Rw = Wf.Proc.program(v2)(())(w, implicitly)

  test("a v1 journal takes the OLD branch, and its next answer is not eaten; walk and replay agree on it") {
    val v1: Wf.Journal[String] = List(Right("Kyiv"), Right("3"))
    assertEquals(Wf.Proc.walk(v2)((), v1), Right(Wf.Proc.Standing.Done[String, String, String]("Kyiv/plain/3")))
    agreeOnEveryPrefix[String](v2, v2Program, v1)
  }

  test("a fresh run stands AT the patch, and the engine decides it") {
    Wf.Proc.walk(v2)((), List(Right("Kyiv"))) match {
      case Right(Wf.Proc.Standing.Asking(_, q, _)) => assertEquals(Wf.Proc.tag(q), Left(Wf.Sys.Patch("promo")): Wf.Ask[String])
      case other => fail(s"expected to stand at the patch, got $other")
    }
    val (st, j) = !.run(Wf.drive(!.run(Wf.resumable[String, String, String, P](v2Program)))(q => pure[P, String](if (q == "city?") "Kyiv" else "3")))
    assertEquals(done(st), "Kyiv/promo/3")
    assertEquals(j, List(Right("Kyiv"), Left(Wf.SysA.Flag(true)), Right("3")))
  }

  test("a journal the term cannot take is DATA, not a throw; strands names the runs it would strand") {
    val wrong: Wf.Journal[String] = List(Left(Wf.SysA.Millis(1L)))
    Wf.Proc.walk(bookingTerm)((), wrong) match {
      case Left(bad) =>
        assertEquals(bad.record, 0)
        assert(bad.why.contains("cannot take"), bad.why)
      case other => fail(s"expected a Stranded, got $other")
    }
    assert(!Wf.Proc.accepts(bookingTerm)((), wrong))
    assertEquals(Wf.Proc.strands(bookingTerm)(())(List("ok" -> journalOf(bookingStatic), "bad" -> wrong)).keySet, Set("bad"))
  }

  test("foldMap into a counting monad, and the Mermaid picture of a loop") {
    val add = Proc.op[Tick, Int, Int]("add")(x => Tick.Add(x))
    // add 5 from 0 is (5, 5); add the answer 5 again from 5 is (10, 10)
    assertEquals(Proc.andThen(add, add).foldMap(Tick.run)(Tick.counting)(5)(0), (10, 10))
    val m = rooms.mermaid()
    assert(m.contains("flowchart TD") && m.contains("|again|"), m)
  }
}
