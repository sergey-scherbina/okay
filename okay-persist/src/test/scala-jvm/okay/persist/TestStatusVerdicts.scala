package okay.persist

import munit.FunSuite
import okay.{!, +, Async, CanBlock, Delim, Pure, Wf}
import okay.given_CanBlock
import okay.given_Scheduler
import okay.Direct.*
import okay.codec.Schema
import scala.language.implicitConversions

/**
 * THE DASHBOARD MUST KEEP THE DISTINCTION THAT MATTERS
 * (statuses-verdicts, 2026-09-17).
 *
 * A worker now tells three kinds of bad news apart, and the
 * difference between them is WHO FIXES IT: `Failed` is the worker's
 * own business (it retries), `Incompatible` needs a person with the
 * code, `Broken` a person with the data. The index collapsed all
 * three into `Broken(String)` with a prefix, so the one question an
 * operator actually asks — "what needs me?" — could only be answered
 * by matching text.
 */
class TestStatusVerdicts extends FunSuite {

  given Schema[Wf.SysA] = Schema.derived
  given Schema[Wf.Ans[String]] = Schema.derived
  given Wf.Runtime = Wf.Runtime.scripted(millis = 1_000L, id = "id", dice = 0.5)

  def drive[A](p: A ! Pure + Async)(using CanBlock): A =
    !.run(Async.run[A, Pure](p))

  test("the index keeps the three apart") {
    val ix = Statuses.over(MemoryStore())
    def put(id: String, s: Statuses.State) =
      ix.put(Statuses.Status(id, "p/1", s, None, None, 1L))

    put("a", Statuses.State.Failed("the service is down"))
    put("b", Statuses.State.Incompatible("cannot read v1 data"))
    put("c", Statuses.State.Broken("Damage(3,bad cbor)"))
    put("d", Statuses.State.Sleeping(99L))

    assertEquals(ix.get("a").map(_.state), Some(Statuses.State.Failed("the service is down")))
    assertEquals(ix.get("b").map(_.state),
      Some(Statuses.State.Incompatible("cannot read v1 data")))
  }

  test("THE POINT: 'what needs me' is a query, not a string match") {
    val ix = Statuses.over(MemoryStore())
    def put(id: String, s: Statuses.State) =
      ix.put(Statuses.Status(id, "p/1", s, None, None, 1L))

    put("retrying", Statuses.State.Failed("the service is down"))
    put("bad-deploy", Statuses.State.Incompatible("cannot read v1 data"))
    put("damaged", Statuses.State.Broken("Damage(3,bad cbor)"))
    put("asleep", Statuses.State.Sleeping(99L))
    put("done", Statuses.State.Finished("ok"))

    // Failed is NOT here on purpose: the worker retries it by itself,
    // and waking somebody for it is how a dashboard trains people to
    // ignore dashboards
    assertEquals(ix.needsAttention.map(_.id).sorted, List("bad-deploy", "damaged"))
  }

  test("a worker reports the verdicts it learned, not a prefix of them") {
    val store = MemoryStore()
    val t = store.topic("runs")
    val ix = Statuses.over(store)

    def v1(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure =
      direct:
        val a = !w.pause("1?")
        !w.sleep(60_000L)
        s"ok $a"
    def v1prime(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure =
      direct:
        val a = !w.pause("1?")
        if a == "old" then throw new IllegalStateException("cannot read v1 data")
        !w.sleep(60_000L)
        s"ok $a"

    val old = Worker[String, String, String, Pure, Async](
      t, "job/1", Timers.over(store), _ => okay.async("old"), statuses = Some(ix))(v1)
    val _ = drive(old.start("r-1"))

    val fresh = Worker[String, String, String, Pure, Async](
      t, "job/1", Timers.over(store), _ => okay.async("new"),
      statuses = Some(ix), isolate = Some(Worker.isolating))(v1prime)
    val _ = drive(fresh.advance("r-1"))

    ix.get("r-1").map(_.state) match
      case Some(Statuses.State.Incompatible(why)) =>
        assert(why.contains("cannot read v1 data"), why)
      case other => fail(s"the index lost the verdict: $other")
    assertEquals(ix.needsAttention.map(_.id), List("r-1"))
  }
}
