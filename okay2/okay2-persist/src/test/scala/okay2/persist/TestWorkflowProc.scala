package okay2.persist

import munit.FunSuite
import okay2.{!}
import okay2.optics.Optic
import okay2.workflow.{Proc, Wf}
import okay2.optics.Optic.arrows._

/** a term's helpers, shared by the static-workflow suites */
object ProcFixtures {
  import DialogueFixtures._

  type Sig = Wf.Asked[String, String]
  implicit val A: Optic.Arrow[Proc.Of[Sig]#L] with Optic.Choice[Proc.Of[Sig]#L] = Proc.procArrow[Sig]

  /** run a step and KEEP what went in */
  def keep[X, Y](p: Wf.Proc[String, String, X, Y]): Wf.Proc[String, String, X, (X, Y)] =
    A.arr((x: X) => (x, x)) >>> A.second[X, Y, X](p)

  /** a term as an ordinary durable program */
  def term(p: Wf.Proc[String, String, Unit, String])(w: WorkflowFixtures.W): String ! Rw =
    Wf.Proc.program(p)(())(w, implicitly)
}

/**
 * A STATIC SPINE ON THE LANDED ENGINE (okay-persist's TestWorkflowProc;
 * specs/static-workflow.md stage 1): `Wf.Proc.program` produces an
 * ordinary durable program, so `Dialogue.workflow`, the envelope, the
 * races, the snapshots and `patch` need no change — and the two front
 * ends share ONE topic.
 */
class TestWorkflowProc extends FunSuite {
  import DialogueFixtures._
  import WorkflowFixtures._
  import ProcFixtures._

  implicit val rt: Wf.Runtime = Wf.Runtime.scripted(millis = 1700000000000L, id = "id-1", dice = 0.25)

  /** v1 as a TERM: a city, then the nights */
  val v1Term: Wf.Proc[String, String, Unit, String] =
    Wf.Proc.ask[String, String, Unit](_ => "city?") >>>
      keep(Wf.Proc.ask[String, String, String](_ => "nights?")) >>>
      A.arr((p: (String, String)) => s"${p._1}/${p._2}")

  /** v2 as a TERM: the same, with a branch added BETWEEN the two */
  val v2Term: Wf.Proc[String, String, Unit, String] =
    Wf.Proc.ask[String, String, Unit](_ => "city?") >>>
      keep(Wf.Proc.patch[String, String, String]("promo")) >>>
      keep(Wf.Proc.ask[String, String, (String, Boolean)](_ => "nights?")) >>>
      A.arr((p: ((String, Boolean), String)) => if (p._1._2) s"${p._1._1}/${p._2}/promo" else s"${p._1._1}/${p._2}")

  /** the MONADIC v1, asking the same two questions */
  def v1Monadic(w: W): String ! Rw = for {
    city <- w.pause("city?")
    n <- w.pause("nights?")
  } yield s"$city/$n"

  val oracle: String => String = q => if (q == "city?") "Kyiv" else "3"

  test("a term runs through Dialogue.workflow with nothing in okay2-persist changed") {
    val t = new MemoryStore().topic("proc-1")
    assertEquals(done(!.run(wf(t, "b-1", "booking/1")(term(v1Term)).runWorkflow(answering(oracle)))), "Kyiv/3")
  }

  test("ONE TOPIC, TWO FRONT ENDS: the journals are equal record for record") {
    def journalOf(body: W => String ! Rw): List[Wf.Ans[String]] = {
      val t = new MemoryStore().topic("j")
      val _ = done(!.run(wf(t, "b", "booking/1")(body).runWorkflow(answering(oracle))))
      wf(t, "b", "booking/1")(body).recovered.answers
    }
    assertEquals(journalOf(term(v1Term)), journalOf(v1Monadic),
      "a static and a monadic booking asking the same questions must be interchangeable")
  }

  test("a run STARTED monadically is carried on by the term, from the same topic") {
    val t = new MemoryStore().topic("mixed")
    // the first process runs the monadic booking and answers one question
    val half = !.run(wf(t, "m-1", "booking/1")(v1Monadic).answer(Right("Kyiv")))
    assert(half.isInstanceOf[Dialogue.Answered.Advanced[_, _, _, _]], half.toString)
    // the deploy: the same journal, the same questions, a TERM
    val got = done(!.run(wf(t, "m-1", "booking/1")(term(v1Term)).runWorkflow(answering(oracle))))
    assertEquals(got, "Kyiv/3", "the term did not pick up where the monadic run left off")
  }

  test("patch through the durable journal, at a term: the old run keeps the old path") {
    val t = new MemoryStore().topic("proc-patch")
    val old = done(!.run(wf(t, "p-1", "booking/1")(term(v1Term)).runWorkflow(answering(oracle))))
    assertEquals(old, "Kyiv/3")
    // the deploy: the same journal, the term that gained a patch
    val after = !.run(wf(t, "p-1", "booking/1")(term(v2Term)).at)
    assertEquals(after.map(_.finished), Right(Some("Kyiv/3")), "the patch ate the answer that followed it, or took the new branch")
    // and `walk` says the same thing WITHOUT running anything
    val j = wf(t, "p-1", "booking/1")(term(v2Term)).recovered.answers
    assertEquals(Wf.Proc.walk(v2Term)((), j), Right(Wf.Proc.Standing.Done[String, String, String]("Kyiv/3")),
      "the structural fold and the engine's replay disagree over a real topic")
  }

  test("a fresh run at the term takes the new branch, durably") {
    val t = new MemoryStore().topic("proc-patch2")
    val fresh = done(!.run(wf(t, "p-2", "booking/1")(term(v2Term)).runWorkflow(answering(q => if (q == "city?") "Lviv" else "2"))))
    assertEquals(fresh, "Lviv/2/promo")
    val j = wf(t, "p-2", "booking/1")(term(v2Term)).recovered.answers
    assertEquals(Wf.Proc.walk(v2Term)((), j), Right(Wf.Proc.Standing.Done[String, String, String]("Lviv/2/promo")))
  }

  /** a term that STOPS: the run ends at the sleep */
  val sleepy: Wf.Proc[String, String, Unit, String] =
    Wf.Proc.ask[String, String, Unit](_ => "city?") >>>
      keep(Wf.Proc.sleep[String, String, String](24 * 3600 * 1000L)) >>>
      A.arr((p: (String, Unit)) => p._1)

  /** a term that stamps itself with the time */
  val stamped: Wf.Proc[String, String, Unit, String] =
    Wf.Proc.ask[String, String, Unit](_ => "who?") >>>
      keep(Wf.Proc.now[String, String, String]) >>>
      A.arr((p: (String, Long)) => s"${p._1}@${p._2}")

  test("a term SUSPENDS: the drive ends on Waiting(Until), with the deadline journalled") {
    val t = new MemoryStore().topic("proc-sleep")
    val stopped = !.run(wf(t, "s-1", "booking/1")(term(sleepy)).runWorkflow(answering(_ => "Kyiv")))
    assertEquals(stopped, Left(Wf.Wait.Until(1700000000000L + 24 * 3600 * 1000L)), "the run must stop at the sleep rather than block or finish")
    // a second process with a different clock waits until the SAME instant
    val other: Wf.Runtime = Wf.Runtime.scripted(millis = 9999999L, id = "id-2", dice = 0.5)
    val again = !.run(wf(t, "s-1", "booking/1")(term(sleepy)).runWorkflow(never)(other))
    assertEquals(again, Left(Wf.Wait.Until(1700000000000L + 24 * 3600 * 1000L)), "a restart slid the deadline forward")
  }

  test("the clock is read ONCE at a term, and the reading outlives the process") {
    val t = new MemoryStore().topic("proc-stamp")
    var reads = 0
    val counting: Wf.Runtime = new Wf.Runtime {
      def answer(q: Wf.Sys): Either[Wf.Wait, Wf.SysA] = {
        reads += 1
        Right(Wf.SysA.Millis(4242L))
      }
    }
    val first = done(!.run(wf(t, "st-1", "stamp/1")(term(stamped)).runWorkflow(answering(_ => "ada"))(counting)))
    assertEquals(first, "ada@4242")
    assertEquals(reads, 1)
    val second = done(!.run(wf(t, "st-1", "stamp/1")(term(stamped)).runWorkflow(never)(counting)))
    assertEquals(second, "ada@4242")
    assertEquals(reads, 1, "the runtime was asked again after a restart")
  }
}
