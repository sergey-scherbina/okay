package okay2.persist

import munit.FunSuite
import okay2.{!}
import okay2.workflow.Wf
import okay2.async.Async
import okay2.platform._

/**
 * THE DASHBOARD MUST KEEP THE DISTINCTION THAT MATTERS (okay-persist's
 * TestStatusVerdicts; statuses-verdicts): `Failed` is the worker's own
 * business (it retries), `Incompatible` needs a person with the code,
 * `Broken` a person with the data — and "what needs me?" is a query, not
 * a string match.
 */
class TestStatusVerdicts extends FunSuite {
  import DialogueFixtures._
  import WorkflowFixtures._
  import WorkerFixtures._

  implicit val rt: Wf.Runtime = Wf.Runtime.scripted(millis = 1000L, id = "id", dice = 0.5)

  def put(ix: Statuses, id: String, s: Statuses.State): Unit = ix.put(Statuses.Status(id, "p/1", s, None, None, 1L))

  test("the index keeps the three apart") {
    val ix = Statuses.over(new MemoryStore)
    put(ix, "a", Statuses.State.Failed("the service is down"))
    put(ix, "b", Statuses.State.Incompatible("cannot read v1 data"))
    put(ix, "c", Statuses.State.Broken("Damage(3,bad cbor)"))
    put(ix, "d", Statuses.State.Sleeping(99L))

    assertEquals(ix.get("a").map(_.state), Some(Statuses.State.Failed("the service is down")))
    assertEquals(ix.get("b").map(_.state), Some(Statuses.State.Incompatible("cannot read v1 data")))
  }

  test("THE POINT: 'what needs me' is a query, not a string match") {
    val ix = Statuses.over(new MemoryStore)
    put(ix, "retrying", Statuses.State.Failed("the service is down"))
    put(ix, "bad-deploy", Statuses.State.Incompatible("cannot read v1 data"))
    put(ix, "damaged", Statuses.State.Broken("Damage(3,bad cbor)"))
    put(ix, "asleep", Statuses.State.Sleeping(99L))
    put(ix, "done", Statuses.State.Finished("ok"))

    assertEquals(ix.needsAttention.map(_.id).sorted, List("bad-deploy", "damaged"))
  }

  test("a worker reports the verdicts it learned, not a prefix of them") {
    val store = new MemoryStore
    val t = store.topic("runs")
    val ix = Statuses.over(store)

    def v1(w: W): String ! Rw = for {
      a <- w.pause("1?")
      _ <- w.sleep(60000L)
    } yield s"ok $a"
    def v1prime(w: W): String ! Rw = for {
      a <- w.pause("1?")
      _ = if (a == "old") throw new IllegalStateException("cannot read v1 data")
      _ <- w.sleep(60000L)
    } yield s"ok $a"

    val old = new Worker[String, String, String, P, Async](t, "job/1", Timers.over(store), say("old"), statuses = Some(ix))(v1)
    val _ = drive(old.start("r-1"))

    val fresh = new Worker[String, String, String, P, Async](t, "job/1", Timers.over(store), say("new"),
      statuses = Some(ix), isolate = Some(Worker.isolating))(v1prime)
    val _ = drive(fresh.advance("r-1"))

    ix.get("r-1").map(_.state) match {
      case Some(Statuses.State.Incompatible(why)) => assert(why.contains("cannot read v1 data"), why)
      case other => fail(s"the index lost the verdict: $other")
    }
    assertEquals(ix.needsAttention.map(_.id), List("r-1"))
  }
}
