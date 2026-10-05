package okay2.persist

import munit.FunSuite
import okay2.{!}
import okay2.workflow.Wf
import okay2.async.Async

/**
 * WHICH RUNS EXIST AND WHAT THEY ARE DOING (okay-persist's TestStatuses;
 * workflow-visibility): the index is written by the WORKER, because the
 * model could only answer by running every program over every journal.
 * Losing the whole index loses no correctness, only the view.
 */
class TestStatuses extends FunSuite {
  import DialogueFixtures._
  import WorkflowFixtures._
  import WorkerFixtures._

  implicit val rt: Wf.Runtime = Wf.Runtime.scripted(millis = 1000L, id = "id", dice = 0.5)

  def approval(w: W): String ! Rw = for {
    what <- w.pause("what?")
    by <- w.awaitSignal("approved")
  } yield s"$what by $by"

  def napper(store: MemoryStore, t: Topic, ix: Statuses): Worker[String, String, String, P, Async] =
    new Worker[String, String, String, P, Async](t, "nap/1", Timers.over(store), say("ada"), statuses = Some(ix))(nap)

  def approver(store: MemoryStore, ix: Statuses): Worker[String, String, String, P, Async] =
    new Worker[String, String, String, P, Async](store.topic("approvals"), "approve/1", Timers.over(store), say("the budget"), statuses = Some(ix))(approval)

  test("a worker writes what it learned, and the index reads back without running anything") {
    val store = new MemoryStore
    val ix = Statuses.over(store)
    val w = napper(store, store.topic("naps"), ix)

    val _ = drive(w.start("n-1"))
    val s = ix.get("n-1").getOrElse(fail("the worker wrote no status"))
    assertEquals(s.id, "n-1")
    assertEquals(s.program, "nap/1")
    assertEquals(s.state, Statuses.State.Sleeping(61000L): Statuses.State)

    val _ = drive(w.tick(61000L))
    assertEquals(ix.get("n-1").map(_.state), Some(Statuses.State.Finished("ada woke")))
  }

  test("who is blocked on approval — one read, no programs run") {
    val store = new MemoryStore
    val ix = Statuses.over(store)
    val w = approver(store, ix)

    val _ = drive(w.start("a-1"))
    val _ = drive(w.start("a-2"))
    assertEquals(ix.waitingOn("approved").map(_.id).sorted, List("a-1", "a-2"))
    assertEquals(ix.waitingOn("something-else"), Nil)
  }

  test("the index carries the QUESTION and the line it is waiting on") {
    val store = new MemoryStore
    val ix = Statuses.over(store)
    val _ = drive(approver(store, ix).start("a-1"))

    val s = ix.get("a-1").get
    assertEquals(s.asking, Some("Left(Signal(approved))"))
    assert(s.where.exists(_.startsWith("TestStatuses.scala:")), s"where=${s.where}")
  }

  test("idleSince finds what has not moved, and never the finished") {
    val store = new MemoryStore
    val ix = Statuses.over(store)
    val w = napper(store, store.topic("naps"), ix)
    val _ = drive(w.start("n-1"))

    assertEquals(ix.idleSince(System.currentTimeMillis() - 3600000L), Nil)
    assertEquals(ix.idleSince(System.currentTimeMillis() + 1000L).map(_.id), List("n-1"))

    val _ = drive(w.tick(61000L))
    assertEquals(ix.idleSince(System.currentTimeMillis() + 1000L), Nil, "a finished run was reported as idle")
  }

  test("LOSING THE INDEX LOSES NO CORRECTNESS: the run is still where its journal says") {
    val store = new MemoryStore
    val t = store.topic("naps")
    val _ = drive(napper(store, t, Statuses.over(store)).start("n-1"))

    val blind = new Worker[String, String, String, P, Async](t, "nap/1", Timers.over(store),
      (q: String, _: Dialogue.Attempt) => Async[String](fail(s"the oracle was asked again: $q")))(nap)
    assertEquals(drive(blind.tick(61000L)), List("n-1" -> (Worker.Progress.Finished("ada woke"): Worker.Progress[String])))
  }
}
