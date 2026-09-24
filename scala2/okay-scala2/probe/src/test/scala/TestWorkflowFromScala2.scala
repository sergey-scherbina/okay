package scala2probe

import okay.Wf
import okay.scala2._

/** okay-workflow from Scala 2.13 (specs/scala2-facade.md, stage 15.7) */
class TestWorkflowFromScala2 extends munit.FunSuite {

  val wf = Workflow[String, String]
  val runtime = Wf.Runtime.scripted(1700000000000L, "bk-1", 0.25)

  val booking: String ! Workflow[String, String] = for {
    city <- wf.ask("which city?")
    when <- wf.now
    ref <- wf.uuid
    nights <- wf.ask(s"how many nights in $city?")
  } yield s"$city/$nights/$when/$ref"

  def oracle(q: String): String = if (q.startsWith("which")) "Kyiv" else "3"

  test("a drive asks the world once per question and journals the runtime's answers too") {
    var asked = List.empty[String]
    val (step, journal) = Workflows.drive(booking, Nil, runtime) { q => asked = asked :+ q; oracle(q) }
    assertEquals(step, Wf.Step.Done[String, String]("Kyiv/3/1700000000000/bk-1"))
    assertEquals(asked, List("which city?", "how many nights in Kyiv?"))
    assert(journal.contains(Right("Kyiv")) && journal.contains(Left(Wf.SysA.Millis(1700000000000L))), journal)
  }

  test("a new process over the same journal lands where the first run did, asking nobody") {
    val (_, journal) = Workflows.drive(booking, Nil, runtime)(oracle)
    assertEquals(Workflows.replay(booking, journal), Some("Kyiv/3/1700000000000/bk-1"))
  }

  test("a worker: advance to the author's question, append the answer, advance again") {
    val (first, entries1) = Workflows.advance(booking, Nil, runtime)
    assertEquals(first, Wf.Step.Asking[String, String]("which city?"))
    val journal1 = entries1 :+ Right("Lviv")
    val (second, entries2) = Workflows.advance(booking, journal1, runtime)
    assertEquals(second, Wf.Step.Asking[String, String]("how many nights in Lviv?"))
    val journal2 = journal1 ++ entries2 :+ Right("2")
    val (last, _) = Workflows.advance(booking, journal2, runtime)
    assertEquals(last, Wf.Step.Done[String, String]("Lviv/2/1700000000000/bk-1"))
  }

  test("a durable sleep and a signal stop the run, and an appended entry wakes it") {
    val approval: String ! Workflow[String, String] = for {
      _ <- wf.sleep(86400000L)
      verdict <- wf.awaitSignal("approve")
    } yield "approved: " + verdict
    val (slept, e1) = Workflows.advance(approval, Nil, runtime)
    assertEquals(slept, Wf.Step.Waiting[String, String](Wf.Wait.Until(1700000000000L + 86400000L)))
    val j1 = e1 :+ Workflows.elapsed
    val (waiting, e2) = Workflows.advance(approval, j1, runtime)
    assertEquals(waiting, Wf.Step.Waiting[String, String](Wf.Wait.Signal("approve")))
    val (done, _) = Workflows.advance(approval, j1 ++ e2 :+ Workflows.got("yes"), runtime)
    assertEquals(done, Wf.Step.Done[String, String]("approved: yes"))
  }

  test("patch: a branch added later is on for new runs and off for a journal that predates it") {
    val before: String ! Workflow[String, String] = wf.ask("name?").map(n => "hello " + n)
    val after: String ! Workflow[String, String] = for {
      promo <- wf.patch("greeting-v2")
      n <- wf.ask("name?")
    } yield (if (promo) "welcome, " else "hello ") + n
    val (_, oldJournal) = Workflows.drive(before, Nil, runtime)(_ => "Ada")
    assertEquals(Workflows.replay(after, oldJournal), Some("hello Ada"))
    val (fresh, _) = Workflows.drive(after, Nil, runtime)(_ => "Ada")
    assertEquals(fresh, Wf.Step.Done[String, String]("welcome, Ada"))
  }
}
