package okay.agent

import okay.*
import okay.given
import okay.codec.Json
import okay.persist.MemoryStore
import Fleet.{Ask, Control, Event}

/** specs/agent-fleet.md, "Approvals": the runner asks, the record shows the
 * ask, an answer from any host lets it through — or a stop says no */
class TestFleetApprovals extends munit.FunSuite:
  def go[A](p: A ! Async): A = Async.run[A, Pure](p).runWith
  def take[A](s: Source[A], n: Int): List[A] =
    summon[Stream[[W] =>> Unit ! Writer % W + Async, Async]].iterator(s).take(n).toList
  def until(what: => Boolean, ms: Int = 5000): Unit =
    val end = System.currentTimeMillis + ms
    while !what && System.currentTimeMillis < end do Thread.sleep(5)
    assert(what, s"waited ${ms}ms")

  val rm = ToolCall("c1", "bash", Json.JObj(Vector("command" -> Json.JStr("rm -rf build"))))

  /** asks before its one call; reports what it was told */
  object Asking extends Runner:
    def run(id: AgentId, spec: Spec, ctx: Fleet.Ctx): Outcome ! Async =
      ctx.ask(1, rm).flatMap { yes =>
        ctx.checkpoint(1, "bash").map(_ => Outcome(if yes then "ran it" else "did not run it", None, Phase.Done))
      }

  test("an ask goes on the record and parks the runner; Approve(seq, yes) lets it through; the answer is recorded") {
    val f = go(Fleet.open(MemoryStore(), Asking, () => 3L))
    val feed = f.events()
    val id = go(f.spawn(Spec("clean", "/w", Budget(3, 1000))))
    until(f.status(id).exists(_.asking.isDefined))
    assertEquals(f.status(id).flatMap(_.asking), Some(Ask(1, "bash", rm.args)))
    assertEquals(f.status(id).map(_.phase), Some(Phase.Running))
    assert(go(f.send(id, Control.Approve(7, true))) , "a wrong seq is accepted by the mailbox…")
    Thread.sleep(50)
    assertEquals(f.status(id).flatMap(_.asking), Some(Ask(1, "bash", rm.args)), "…and changes nothing")
    assert(go(f.send(id, Control.Approve(1, true))))
    until(f.status(id).exists(_.phase == Phase.Done))
    assertEquals(f.status(id).map(_.result), Some(Some("ran it")))
    assertEquals(f.status(id).flatMap(_.asking), None)
    val seen = take(feed, 4)
    assertEquals(seen(1), Event.Asked(id, Ask(1, "bash", rm.args), 3L))
    assertEquals(seen(2), Event.Answered(id, 1, true, 3L))
  }

  test("a no is a no; a Stop while asking answers no and the runner is not left parked") {
    val f = go(Fleet.open(MemoryStore(), Asking))
    val a = go(f.spawn(Spec("a", "/w", Budget(3, 1000))))
    val b = go(f.spawn(Spec("b", "/w", Budget(3, 1000))))
    until(f.status(a).exists(_.asking.isDefined) && f.status(b).exists(_.asking.isDefined))
    assert(go(f.send(a, Control.Approve(1, false))))
    until(f.status(a).exists(_.phase == Phase.Done))
    assertEquals(f.status(a).map(_.result), Some(Some("did not run it")))
    assert(go(f.send(b, Control.Stop)))
    until(f.status(b).exists(s => !Phase.live(s.phase)))
    assertEquals(f.status(b).flatMap(_.asking), None)
    assertEquals(f.status(b).map(_.result), Some(Some("did not run it")))
  }

  test("the approve command reaches the ask through the commands topic, and a restored fleet remembers an unanswered ask only as Interrupted") {
    val store = MemoryStore()
    val f = go(Fleet.open(store, Asking))
    val id = go(f.spawn(Spec("x", "/w", Budget(3, 1000))))
    until(f.status(id).exists(_.asking.isDefined))
    val cmds = store.topic("commands")
    val service = Async.spawn(f.commands(cmds, (_, _) => Right(())))
    cmds.append(Array.empty, Json.print(Fleet.commandJson(Fleet.Command.Send(id, Control.Approve(1, true), "ada"))).getBytes("UTF-8"), okay.persist.Ack.Received): Unit
    until(f.status(id).exists(_.phase == Phase.Done))
    service.cancel()
    // another agent left asking when the process died
    val g = go(Fleet.open(store, Asking))
    val y = go(g.spawn(Spec("y", "/w", Budget(3, 1000))))
    until(g.status(y).exists(_.asking.isDefined))
    val h = go(Fleet.open(store, Asking))
    assertEquals(h.status(y).map(s => (s.phase, s.asking)), Some((Phase.Interrupted, Some(Ask(1, "bash", rm.args)))))
  }
