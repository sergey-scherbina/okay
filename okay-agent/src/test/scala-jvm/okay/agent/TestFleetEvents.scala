package okay.agent

import okay.*
import okay.given
import okay.codec.Json
import okay.persist.MemoryStore

/** specs/agent-fleet.md, "Events": what a screen folds instead of asking */
class TestFleetEvents extends munit.FunSuite:
  def go[A](p: A ! Async): A = Async.run[A, Pure](p).runWith
  def take[A](s: Source[A], n: Int): List[A] =
    summon[Stream[[W] =>> Unit ! Writer % W + Async, Async]].iterator(s).take(n).toList

  object Instant extends Runner:
    def run(id: AgentId, spec: Spec, ctx: Fleet.Ctx): Outcome ! Async =
      ctx.turned(Turn.Assistant("hi", Nil))
      ctx.checkpoint(1, "read_file").map(_ => Outcome("done", Some(Json.JObj(Vector("ok" -> Json.JBool(true)))), Phase.Done))

  test("in-process: a listener sees spawned, turned, stepped, finished — in the order they were written") {
    val f = go(Fleet.open(MemoryStore(), Instant, () => 7L))
    val feed = f.events()
    val id = go(f.spawn(Spec("t", "/w", Budget(3, 1000))))
    go(f.await(id)): Unit
    val seen = take(feed, 4)
    assertEquals(seen.map(_.productPrefix), List("Spawned", "Turned", "Stepped", "Finished"))
    assertEquals(seen.head, Fleet.Event.Spawned(id, Spec("t", "/w", Budget(3, 1000), None, None), None, 7L))
    assertEquals(seen(1), Fleet.Event.Turned(id, Turn.Assistant("hi", Nil)))
    assertEquals(seen(2), Fleet.Event.Stepped(id, 1, "read_file", 7L))
    assertEquals(seen(3), Fleet.Event.Finished(id, Phase.Done, "done", Some(Json.JObj(Vector("ok" -> Json.JBool(true)))), 7L))
  }

  test("from the topic: Fleet.events tails the log, from the start and from an offset, and folds to the same status") {
    val store = MemoryStore()
    val f = go(Fleet.open(store, Instant, () => 9L))
    val a = go(f.spawn(Spec("a", "/w", Budget(3, 1000))))
    go(f.await(a)): Unit
    val all = take(Fleet.events(store.topic("agents")), 4)
    assertEquals(all.map(_.productPrefix), List("Spawned", "Turned", "Stepped", "Finished"))
    // an offset skips what was already folded
    val later = take(Fleet.events(store.topic("agents"), from = 3), 1)
    assertEquals(later.head.productPrefix, "Finished")
    // a second fleet over the topic agrees with what the feed said
    val g = go(Fleet.open(store, Instant))
    assertEquals(g.status(a).map(s => (s.phase, s.step, s.result)), Some((Phase.Done, 1, Some("done"))))
    assertEquals(g.transcript(a).toList, List(Turn.Assistant("hi", Nil)))
  }

  test("event: every kind decodes; an unknown kind is None, not a throw") {
    import Json.*
    def rec(kind: String, fs: (String, Json)*) = JObj(Vector("kind" -> JStr(kind), "id" -> JNum(4)) ++ fs.toVector :+ ("at" -> JNum(1)))
    assertEquals(Fleet.event(rec("phased", "phase" -> JStr("Paused"))), Some(Fleet.Event.Phased(AgentId(4), Phase.Paused, 1L)))
    assertEquals(Fleet.event(rec("stepped", "step" -> JNum(2), "tool" -> JStr("bash"))), Some(Fleet.Event.Stepped(AgentId(4), 2, "bash", 1L)))
    assertEquals(Fleet.event(rec("turned", "turn" -> JObj(Vector("t" -> JStr("user"), "text" -> JStr("go"))))), Some(Fleet.Event.Turned(AgentId(4), Turn.User("go"))))
    assertEquals(Fleet.event(rec("finished", "phase" -> JStr("Killed"), "text" -> JStr(""), "report" -> JNull)), Some(Fleet.Event.Finished(AgentId(4), Phase.Killed, "", None, 1L)))
    assertEquals(Fleet.event(rec("sang")), None)
    assertEquals(Fleet.event(JStr("not a record")), None)
  }
