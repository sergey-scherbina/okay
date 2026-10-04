package okay2.persist

import okay2.{!, Pure}
import okay2.async.Async
import okay2.codec.Schema
import okay2.platform._

final case class Order(id: String, total: Int, log: Vector[String])
object Order {
  implicit val schema: Schema[Order] = Schema.derived
}

/** specs/persist.md "The saga" (okay-persist's TestSaga): intent-first
 * steps with compensations over a keyed topic, recovery by policy. The
 * "world" is a Set, so a re-run step is idempotent at its far end — the
 * rule the saga states. */
class TestSaga extends munit.FunSuite {

  def run[A](prog: A ! Async): A = !.run(Async.run[A, Pure](prog))

  final class World {
    val facts = scala.collection.mutable.Set.empty[String]
    var haltOn: Option[String] = None
    var failOn: Option[String] = None
    var runs = 0
    def step(name: String): Saga.Step[Order] = Saga.Step[Order](name,
      forward = o => Async {
        runs += 1
        if (failOn.contains(name)) throw new RuntimeException(s"$name refused")
        facts += name
        if (haltOn.contains(name)) throw new Saga.Halt
        o.copy(log = o.log :+ name)
      },
      compensate = o => Async {
        facts -= name
        o.copy(log = o.log :+ s"undo $name")
      })
  }

  def saga(world: World, topic: Topic, id: String, policy: Saga.Policy = Saga.Policy.Forward): Saga[Order] =
    Saga[Order](topic, id, policy)(world.step("reserve"), world.step("charge"), world.step("ship"))

  def finished(o: Order): Saga.Outcome[Order] = Saga.Outcome.Finished(o)

  test("the happy path: every step forward, Finished with the final state, status says so") {
    val w = new World
    val t = new MemoryStore().topic("sagas")
    val s = saga(w, t, "o1")
    assertEquals(run(s.run(Order("o1", 10, Vector.empty))), finished(Order("o1", 10, Vector("reserve", "charge", "ship"))))
    assertEquals(w.facts.toSet, Set("reserve", "charge", "ship"))
    assertEquals(s.status, Saga.Status("o1", "finished", Vector("reserve", "charge", "ship"), Vector.empty, None, None))
    // a finished saga recovers to its outcome without running anything
    val before = w.runs
    assertEquals(run(s.recover()), finished(Order("o1", 10, Vector("reserve", "charge", "ship"))))
    assertEquals(w.runs, before)
  }

  test("a step fails: the steps before it are compensated in reverse, Aborted names the step and the error") {
    val w = new World
    w.failOn = Some("ship")
    val s = saga(w, new MemoryStore().topic("sagas"), "o2")
    val out = run(s.run(Order("o2", 5, Vector.empty)))
    assertEquals(out, Saga.Outcome.Aborted(Order("o2", 5, Vector("reserve", "charge", "undo charge", "undo reserve")), Some("ship"), "ship refused"): Saga.Outcome[Order])
    assertEquals(w.facts.toSet, Set.empty[String])
    assertEquals(s.status.phase, "aborted")
    assertEquals(s.status.undone, Vector("charge", "reserve"))
  }

  test("the crash window, Forward: the intent stands without an answer; recover re-runs that step and finishes") {
    val w = new World
    w.haltOn = Some("charge")
    val t = new MemoryStore().topic("sagas")
    val s = saga(w, t, "o3")
    intercept[Saga.Halt](run(s.run(Order("o3", 7, Vector.empty))))
    assert(w.facts.contains("charge"), "the effect happened before the process died")
    assertEquals(s.status, Saga.Status("o3", "running", Vector("reserve"), Vector.empty, Some("charge"), None))
    w.haltOn = None
    val out = run(saga(w, t, "o3").recover())
    assertEquals(out, finished(Order("o3", 7, Vector("reserve", "charge", "ship"))))
    assertEquals(w.facts.toSet, Set("reserve", "charge", "ship"))
  }

  test("the crash window, Backward: recover compensates the uncertain step and everything before it, then Aborted") {
    val w = new World
    w.haltOn = Some("charge")
    val t = new MemoryStore().topic("sagas")
    intercept[Saga.Halt](run(saga(w, t, "o4", Saga.Policy.Backward).run(Order("o4", 3, Vector.empty))))
    w.haltOn = None
    val out = run(saga(w, t, "o4", Saga.Policy.Backward).recover())
    assertEquals(out, Saga.Outcome.Aborted(Order("o4", 3, Vector("reserve", "undo charge", "undo reserve")), None, "recovered backward by policy"): Saga.Outcome[Order])
    assertEquals(w.facts.toSet, Set.empty[String])
  }

  test("a crash mid-compensation: recover resumes the compensations where they stopped") {
    val w = new World
    w.failOn = Some("ship")
    val t = new MemoryStore().topic("sagas")
    val dying = Saga[Order](t, "o5")(w.step("reserve"),
      Saga.Step[Order]("charge", w.step("charge").forward, _ => Async[Order] { w.facts -= "charge"; throw new Saga.Halt }),
      w.step("ship"))
    intercept[Saga.Halt](run(dying.run(Order("o5", 1, Vector.empty))))
    assertEquals(saga(w, t, "o5").status.phase, "compensating")
    val out = run(saga(w, t, "o5").recover())
    assertEquals(out, Saga.Outcome.Aborted(Order("o5", 1, Vector("reserve", "charge", "undo charge", "undo reserve")), Some("ship"), "ship refused"): Saga.Outcome[Order])
    assertEquals(w.facts.toSet, Set.empty[String])
  }

  test("two sagas on one topic keep their own records; an unknown id refuses") {
    val w = new World
    val t = new MemoryStore().topic("sagas")
    run(saga(w, t, "a").run(Order("a", 1, Vector.empty)))
    run(saga(w, t, "b").run(Order("b", 2, Vector.empty)))
    assertEquals(saga(w, t, "a").status.done, Vector("reserve", "charge", "ship"))
    assertEquals(saga(w, t, "b").status.phase, "finished")
    assertEquals(saga(w, t, "nope").status.phase, "unknown")
    intercept[IllegalStateException](run(saga(w, t, "nope").recover()))
  }
}
