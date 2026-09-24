package okay.persist

import munit.FunSuite
import okay.{!, +, Delim, Optic, Proc, Pure, Wf}
import okay.codec.Schema
import okay.Direct.*
import scala.language.implicitConversions
import okay.Optic.arrows.*

/**
 * A STATIC SPINE ON THE LANDED ENGINE (specs/static-workflow.md
 * stage 1). The bet the design rests on is that `Wf.Proc.program`
 * produces an ordinary durable program, so `Dialogue.workflow`, the
 * envelope, the races, the snapshots and `patch` need no change at
 * all — and the way to test a bet like that is to make the two front
 * ends share ONE topic.
 */
class TestWorkflowProc extends FunSuite {

  given Schema[Wf.SysA] = Schema.derived
  given Schema[Wf.Ans[String]] = Schema.derived
  given Wf.Runtime = Wf.Runtime.scripted(millis = 1_700_000_000_000L,
                                         id = "id-1", dice = 0.25)

  type Sig = Wf.Asked[String, String]
  val A: Optic.Arrow[[X, Y] =>> Proc[Sig, X, Y]] & Optic.Choice[[X, Y] =>> Proc[Sig, X, Y]] =
    Proc.procArrow[Sig]

  // the local `>>>` that stood here is gone: `Optic.arrows` composes
  // any `Arrow`, and `Proc` is one (arrow-glyphs, 2026-09-18). It only
  // existed because `Monad.scala` held the name for Kleisli.

  def keep[X, Y](p: Wf.Proc[String, String, X, Y]): Wf.Proc[String, String, X, (X, Y)] =
    A.arr((x: X) => (x, x)) >>> A.second(p)

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
      A.arr: (p: ((String, Boolean), String)) =>
        if p._1._2 then s"${p._1._1}/${p._2}/promo" else s"${p._1._1}/${p._2}"

  /** the MONADIC v1, asking the same two questions */
  def v1Monadic(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure =
    direct:
      val city = !w.pause("city?")
      val n = !w.pause("nights?")
      s"$city/$n"

  def done[R](e: Either[Wf.Wait, R]): R = e match
    case Right(r) => r
    case Left(w) => fail(s"expected the workflow to finish, it is waiting on $w")

  def wf(t: Topic, id: String, program: String)
        (body: Wf.Asks[String, String, String, Pure] ?=> String ! Delim + Pure) =
    Dialogue.workflow[String, String, String, Pure](t, id, program)(body)

  def term(p: Wf.Proc[String, String, Unit, String]):
      Wf.Asks[String, String, String, Pure] ?=> String ! Delim + Pure =
    Wf.Proc.program(p)(())

  val oracle: String => String = q => if q == "city?" then "Kyiv" else "3"

  test("a term runs through Dialogue.workflow with nothing in okay-persist changed") {
    val t = MemoryStore().topic("proc-1")
    val got = done(!.run(wf(t, "b-1", "booking/1")(term(v1Term))
      .runWorkflow(q => okay.pure(oracle(q)))))
    assertEquals(got, "Kyiv/3")
  }

  test("ONE TOPIC, TWO FRONT ENDS: the journals are equal record for record") {
    def journalOf(body: Wf.Asks[String, String, String, Pure] ?=> String ! Delim + Pure) =
      val t = MemoryStore().topic("j")
      val _ = done(!.run(wf(t, "b", "booking/1")(body).runWorkflow(q => okay.pure(oracle(q)))))
      wf(t, "b", "booking/1")(body).recovered.answers
    assertEquals(journalOf(term(v1Term)), journalOf(v1Monadic),
      "a static and a monadic booking asking the same questions must be interchangeable")
  }

  test("a run STARTED monadically is carried on by the term, from the same topic") {
    val t = MemoryStore().topic("mixed")
    // the first process runs the monadic booking and answers one question
    val half = !.run(wf(t, "m-1", "booking/1")(v1Monadic).answer(Right("Kyiv")))
    assert(half.isInstanceOf[Dialogue.Answered.Advanced[?, ?, ?, ?]], half.toString)
    // the deploy: the same journal, the same questions, a TERM
    val got = done(!.run(wf(t, "m-1", "booking/1")(term(v1Term))
      .runWorkflow(q => okay.pure(oracle(q)))))
    assertEquals(got, "Kyiv/3", "the term did not pick up where the monadic run left off")
  }

  test("patch through the durable journal, at a term: the old run keeps the old path") {
    val t = MemoryStore().topic("proc-patch")
    val old = done(!.run(wf(t, "p-1", "booking/1")(term(v1Term))
      .runWorkflow(q => okay.pure(oracle(q)))))
    assertEquals(old, "Kyiv/3")
    // the deploy: the same journal, the term that gained a patch
    val after = !.run(wf(t, "p-1", "booking/1")(term(v2Term)).at)
    assertEquals(after.map(_.finished), Right(Some("Kyiv/3")),
      "the patch ate the answer that followed it, or took the new branch")
    // and `walk` says the same thing WITHOUT running anything
    val j = wf(t, "p-1", "booking/1")(term(v2Term)).recovered.answers
    assertEquals(Wf.Proc.walk(v2Term)((), j), Right(Wf.Proc.Standing.Done("Kyiv/3")),
      "the structural fold and the engine's replay disagree over a real topic")
  }

  test("a fresh run at the term takes the new branch, durably") {
    val t = MemoryStore().topic("proc-patch2")
    val fresh = done(!.run(wf(t, "p-2", "booking/1")(term(v2Term))
      .runWorkflow(q => okay.pure(if q == "city?" then "Lviv" else "2"))))
    assertEquals(fresh, "Lviv/2/promo")
    val j = wf(t, "p-2", "booking/1")(term(v2Term)).recovered.answers
    assertEquals(Wf.Proc.walk(v2Term)((), j), Right(Wf.Proc.Standing.Done("Lviv/2/promo")))
  }

  /** a term that STOPS: the run ends at the sleep and something else
   * appends the answer later */
  val sleepy: Wf.Proc[String, String, Unit, String] =
    Wf.Proc.ask[String, String, Unit](_ => "city?") >>>
      keep(Wf.Proc.sleep[String, String, String](24 * 3600 * 1000L)) >>>
      A.arr((p: (String, Unit)) => p._1)

  /** a term that stamps itself with the time */
  val stamped: Wf.Proc[String, String, Unit, String] =
    Wf.Proc.ask[String, String, Unit](_ => "who?") >>>
      keep(Wf.Proc.now) >>>
      A.arr((p: (String, Long)) => s"${p._1}@${p._2}")

  test("a term SUSPENDS: the drive ends on Waiting(Until), with the deadline journalled") {
    val t = MemoryStore().topic("proc-sleep")
    val stopped = !.run(wf(t, "s-1", "booking/1")(term(sleepy))
      .runWorkflow(_ => okay.pure("Kyiv")))
    assertEquals(stopped, Left(Wf.Wait.Until(1_700_000_000_000L + 24 * 3600 * 1000L)),
      "the run must stop at the sleep rather than block or finish")
    // the deadline was computed from a JOURNALLED `now`, so a second
    // process with a different clock waits until the SAME instant —
    // the durable-timer bug every engine has had once
    val again =
      given other: Wf.Runtime = Wf.Runtime.scripted(millis = 9_999_999L, id = "id-2", dice = 0.5)
      !.run(wf(t, "s-1", "booking/1")(term(sleepy))
        .runWorkflow(_ => fail("the oracle was asked again")))
    assertEquals(again, Left(Wf.Wait.Until(1_700_000_000_000L + 24 * 3600 * 1000L)),
      "a restart slid the deadline forward")
  }

  test("the clock is read ONCE at a term, and the reading outlives the process") {
    val t = MemoryStore().topic("proc-stamp")
    var reads = 0
    given counting: Wf.Runtime = new Wf.Runtime:
      def answer(q: Wf.Sys): Either[Wf.Wait, Wf.SysA] =
        reads += 1
        Right(Wf.SysA.Millis(4242L))
    val first = done(!.run(wf(t, "st-1", "stamp/1")(term(stamped))
      .runWorkflow(_ => okay.pure("ada"))))
    assertEquals(first, "ada@4242")
    assertEquals(reads, 1)
    val second = done(!.run(wf(t, "st-1", "stamp/1")(term(stamped))
      .runWorkflow(_ => fail("the oracle was asked again"))))
    assertEquals(second, "ada@4242")
    assertEquals(reads, 1, "the runtime was asked again after a restart")
  }
}
