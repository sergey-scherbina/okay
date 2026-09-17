package okay.persist

import munit.FunSuite
import okay.{!, +, Delim, Pure, Wf}
import okay.Direct.*
import okay.codec.Schema
import scala.language.implicitConversions

/**
 * WHICH RUNS EXIST AND WHAT THEY ARE DOING (workflow-visibility,
 * 2026-09-17). The index is written by the WORKER, because the model
 * can only answer the question by running every program over every
 * journal — right, and the wrong thing to pay on a dashboard refresh.
 *
 * The last test is the one that says what kind of thing this is:
 * losing the whole index loses no correctness, only the view.
 */
class TestStatuses extends FunSuite {

  given Schema[Wf.SysA] = Schema.derived
  given Schema[Wf.Ans[String]] = Schema.derived
  given Wf.Runtime = Wf.Runtime.scripted(millis = 1_000L, id = "id", dice = 0.5)

  def nap(using w: Wf.Asks[String, String, String, Pure]): String ! (Delim + Pure) = direct:
    val who = !w.pause("who?")
    !w.sleep(60_000L)
    s"$who woke"

  def approval(using w: Wf.Asks[String, String, String, Pure]): String ! (Delim + Pure) =
    direct:
      val what = !w.pause("what?")
      val by = !w.awaitSignal("approved")
      s"$what by $by"

  test("a worker writes what it learned, and the index reads back without running anything") {
    val store = MemoryStore()
    val ix = Statuses.over(store)
    val w = Worker[String, String, String, Pure](
      store.topic("naps"), "nap/1", Timers.over(store),
      _ => okay.pure("ada"), statuses = Some(ix))(nap)

    val _ = !.run(w.start("n-1"))
    val s = ix.get("n-1").getOrElse(fail("the worker wrote no status"))
    assertEquals(s.id, "n-1")
    assertEquals(s.program, "nap/1")
    assertEquals(s.state, Statuses.State.Sleeping(61_000L))

    val _ = !.run(w.tick(61_000L))
    assertEquals(ix.get("n-1").map(_.state), Some(Statuses.State.Finished("ada woke")))
  }

  test("who is blocked on approval — one read, no programs run") {
    val store = MemoryStore()
    val ix = Statuses.over(store)
    val w = Worker[String, String, String, Pure](
      store.topic("approvals"), "approve/1", Timers.over(store),
      _ => okay.pure("the budget"), statuses = Some(ix))(approval)

    val _ = !.run(w.start("a-1"))
    val _ = !.run(w.start("a-2"))
    assertEquals(ix.waitingOn("approved").map(_.id).sorted, List("a-1", "a-2"))
    assertEquals(ix.waitingOn("something-else"), Nil)
  }

  test("the index carries the QUESTION and the line it is waiting on") {
    val store = MemoryStore()
    val ix = Statuses.over(store)
    // an oracle that refuses to answer leaves the run at its question
    val w = Worker[String, String, String, Pure](
      store.topic("approvals"), "approve/1", Timers.over(store),
      _ => okay.pure("the budget"), statuses = Some(ix))(approval)
    val _ = !.run(w.start("a-1"))

    val s = ix.get("a-1").get
    assertEquals(s.asking, Some("Left(Signal(approved))"))
    assert(s.where.exists(_.startsWith("TestStatuses.scala:")), s"where=${s.where}")
  }

  test("idleSince finds what has not moved, and never the finished") {
    val store = MemoryStore()
    val ix = Statuses.over(store)
    val w = Worker[String, String, String, Pure](
      store.topic("naps"), "nap/1", Timers.over(store),
      _ => okay.pure("ada"), statuses = Some(ix))(nap)
    val _ = !.run(w.start("n-1"))

    // written just now, so nothing is idle as of an hour ago...
    assertEquals(ix.idleSince(System.currentTimeMillis() - 3_600_000L), Nil)
    // ...and everything unfinished is idle as of the future
    assertEquals(ix.idleSince(System.currentTimeMillis() + 1_000L).map(_.id), List("n-1"))

    val _ = !.run(w.tick(61_000L))
    assertEquals(ix.idleSince(System.currentTimeMillis() + 1_000L), Nil,
      "a finished run was reported as idle")
  }

  test("LOSING THE INDEX LOSES NO CORRECTNESS: the run is still where its journal says") {
    val store = MemoryStore()
    val t = store.topic("naps")
    val ix = Statuses.over(store)
    val w = Worker[String, String, String, Pure](
      t, "nap/1", Timers.over(store), _ => okay.pure("ada"), statuses = Some(ix))(nap)
    val _ = !.run(w.start("n-1"))

    // a worker with NO index at all, over the same journal
    val blind = Worker[String, String, String, Pure](
      t, "nap/1", Timers.over(store), _ => fail("the oracle was asked again"))(nap)
    assertEquals(!.run(blind.tick(61_000L)),
      List("n-1" -> Worker.Progress.Finished("ada woke")))
  }
}
