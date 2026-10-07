package okay.agent
import okay.freer.*

import okay.{! as _, Pure as _, pure as _, effect as _, *}
import okay.given
import okay.freer.given
import okay.codec.Json
import okay.persist.MemoryStore
import Fleet.Control

/**
 * specs/agent-fleet.md — driven by a SCRIPTED runner: no model, no
 * gateway, no filesystem. The runner takes one step per tick the test
 * sends it, so pausing, stopping and killing happen at known points.
 */
class TestFleet extends munit.FunSuite:
  def go[A](p: A ! Async): A = Async.run[A, Pure](p).runWith
  def until(what: => Boolean, ms: Int = 5000): Unit =
    val end = System.currentTimeMillis + ms
    while !what && System.currentTimeMillis < end do Thread.sleep(5)
    assert(what, s"waited ${ms}ms")

  /** every step waits for a tick ON THE AGENT'S OWN CHANNEL — two agents
   * over one channel race for the ticks, and the loser never reaches its
   * first step (seen as a 5 s timeout under a loaded gate); a Stop finishes
   * the step and returns */
  final class Ticking(ticksFor: Spec => Channel[Unit], steps: Int, done: String = "done") extends Runner:
    def run(id: AgentId, spec: Spec, ctx: Fleet.Ctx): Outcome ! Async =
      val ticks = ticksFor(spec)
      def step(n: Int): Outcome ! Async =
        if n > steps then pure(Outcome(done, Some(Json.JObj(Vector("ok" -> Json.JBool(true)))), Phase.Done))
        else ticks.receive.flatMap {
          case None => pure(Outcome("ticks closed", None, Phase.Interrupted))
          case Some(_) =>
            ctx.inbox().foreach(m => ctx.turned(Turn.User(m)))
            ctx.checkpoint(n, s"tool$n").flatMap {
              case Some(Control.Stop) => pure(Outcome(s"stopped at $n", None, Phase.Interrupted))
              case Some(_) => pure(Outcome("over", None, Phase.Killed))
              case None =>
                ctx.turned(Turn.Assistant(s"step $n", Nil))
                step(n + 1)
            }
        }
      step(1)

  def spec(task: String, steps: Int = 10, wall: Long = 60_000, parent: Option[AgentId] = None) =
    Spec(task, "/w", Budget(steps, wall), parent)

  /** a channel per task, so each agent is ticked by name */
  final class Ticks:
    private val chans = scala.collection.concurrent.TrieMap.empty[String, Channel[Unit]]
    def apply(task: String): Channel[Unit] = chans.getOrElseUpdate(task, Channel[Unit](64))
    def by: Spec => Channel[Unit] = s => apply(s.task)
    def tick(task: String): Unit = go(apply(task).send(())): Unit

  test("spawn returns at once; the run steps as ticked; Done with its report; the record folds back") {
    val t = Ticks()
    val store = MemoryStore()
    val f = go(Fleet.open(store, Ticking(t.by, 2)))
    val id = go(f.spawn(spec("a")))
    assertEquals(f.status(id).map(s => (s.phase, s.step)), Some((Phase.Running, 0)))
    t.tick("a")
    until(f.status(id).exists(_.step == 1))
    assertEquals(f.status(id).map(_.lastTool), Some(Some("tool1")))
    t.tick("a"); t.tick("a")
    until(f.status(id).exists(_.phase == Phase.Done))
    val s = f.status(id).get
    assertEquals(s.result, Some("done"))
    assertEquals(s.report, Some(Json.JObj(Vector("ok" -> Json.JBool(true)))))
    assertEquals(f.transcript(id).toList, List(Turn.Assistant("step 1", Nil), Turn.Assistant("step 2", Nil)))
    // a second fleet over the same store sees the same agent
    val g = go(Fleet.open(store, Ticking(t.by, 0)))
    assertEquals(g.status(id).map(s => (s.phase, s.step, s.result)), Some((Phase.Done, 2, Some("done"))))
    assertEquals(g.transcript(id), f.transcript(id))
  }

  test("Pause holds the next tool call, Resume continues; elapsed still grows; Tell is the next user turn") {
    val t = Ticks()
    var clock = 1_000L
    val f = go(Fleet.open(MemoryStore(), Ticking(t.by, 3), () => clock))
    val id = go(f.spawn(spec("b")))
    t.tick("b")
    until(f.status(id).exists(_.step == 1))
    assert(go(f.send(id, Control.Pause)))
    until(f.status(id).exists(_.phase == Phase.Paused))
    t.tick("b")                                       // the runner reaches checkpoint 2 and waits there
    until(f.status(id).exists(_.step == 2))
    clock += 500
    assertEquals(f.status(id).map(_.elapsedMs), Some(500L))
    assert(go(f.send(id, Control.Tell("also check the tests"))))
    assert(f.transcript(id).forall(_ != Turn.User("also check the tests")), "not delivered while paused")
    assert(go(f.send(id, Control.Resume)))
    t.tick("b"); t.tick("b")
    until(f.status(id).exists(_.phase == Phase.Done))
    assert(f.transcript(id).contains(Turn.User("also check the tests")), f.transcript(id).toString)
  }

  test("Stop ends at the current tool with Interrupted; Kill ends now with Killed and nothing more is recorded") {
    val t = Ticks()
    val store = MemoryStore()
    val f = go(Fleet.open(store, Ticking(t.by, 10)))
    val a = go(f.spawn(spec("a"))); val b = go(f.spawn(spec("b")))
    t.tick("a"); t.tick("b")
    until(f.status(a).exists(_.step == 1) && f.status(b).exists(_.step == 1))
    assert(go(f.send(a, Control.Stop)))
    // `send` answers that the mailbox TOOK the Stop, not that the actor has
    // applied it (fleet-stop-race, 2026-09-29): tick before it has, and the
    // runner's checkpoint 2 can read "go on", park for a third tick nobody
    // sends, and never end -- a whole gate and a lone run each met it once,
    // at this line. The Pause case above waits for its phase for the same
    // reason; "at the current tool" means the next checkpoint AFTER it lands
    until(f.status(a).exists(_.phase == Phase.Stopping))
    t.tick("a")
    until(f.status(a).exists(_.phase == Phase.Interrupted))
    assertEquals(f.status(a).map(_.result), Some(Some("stopped at 2")))
    assert(go(f.send(b, Control.Kill)))
    until(f.status(b).exists(_.phase == Phase.Killed))
    val before = store.topic("agents").end(0)
    t.tick("b")
    Thread.sleep(50)
    assertEquals(store.topic("agents").end(0), before, "a killed agent writes nothing")
    assert(!go(f.send(b, Control.Tell("x"))), "an ended agent takes no message")
  }

  test("budgets: a step past the budget, or a wall clock past it, ends the run Interrupted") {
    val t = Ticks()
    var clock = 0L
    val f = go(Fleet.open(MemoryStore(), Ticking(t.by, 10), () => clock))
    val short = go(f.spawn(spec("short", steps = 2)))
    for _ <- 1 to 3 do t.tick("short")
    until(f.status(short).exists(_.phase == Phase.Interrupted))
    assertEquals(f.status(short).map(_.step), Some(3))
    val slow = go(f.spawn(spec("slow", wall = 100)))
    t.tick("slow")
    until(f.status(slow).exists(_.step == 1))
    clock = 200
    t.tick("slow")
    until(f.status(slow).exists(_.phase == Phase.Interrupted))
  }

  test("delegate: two children, steps deducted, results carried; too big a child refused naming both numbers") {
    val t = Ticks()
    var fleet: Fleet = null
    // the parent: delegates twice through the tool, then finishes
    val parent = new Runner:
      def run(id: AgentId, spec: Spec, ctx: Fleet.Ctx): Outcome ! Async =
        if spec.parent.isDefined then Ticking(t.by, 2).run(id, spec, ctx)
        else
          val tool = Fleet.delegate(fleet, id)
          def call(task: String, budget: Option[Int]) =
            tool.call(ToolCall("c", "delegate", Json.JObj(Vector("task" -> Json.JStr(task)) ++
              budget.map(b => "budget" -> Json.JNum(b.toDouble))))).get
          call("part one", Some(4)).flatMap { r1 => ctx.turned(Turn.Result("c", r1))
            call("part two", Some(4)).flatMap { r2 => ctx.turned(Turn.Result("c", r2))
              call("part three", Some(50)).map { r3 => ctx.turned(Turn.Result("c", r3))
                Outcome(s"$r1 | $r2", None, Phase.Done) } } }
    fleet = go(Fleet.open(MemoryStore(), parent))
    val p = go(fleet.spawn(spec("whole", steps = 10)))
    t.tick("part one"); t.tick("part one"); t.tick("part two"); t.tick("part two")
    until(fleet.status(p).exists(_.phase == Phase.Done))
    val ps = fleet.status(p).get
    assertEquals(ps.children.size, 2)
    assertEquals(fleet.stepsLeft(p), 10 - 2 - 2, "each child used two steps of the parent's ten")
    val kids = ps.children.map(fleet.status(_).get)
    assert(kids.forall(k => k.parent == Some(p) && k.phase == Phase.Done && k.workspace == "/w"))
    assert(ps.result.get.contains("\"result\":\"done\""), ps.result.get)
    val third = fleet.transcript(p).collect { case Turn.Result(_, c) => c }.last
    assert(third.contains("asked for 50 steps, only 6 left"), third)
    assertEquals(fleet.all.map(_.id), Vector(p) ++ ps.children)
  }

  test("a child whose runner throws is Failed; the parent reads a tool error and continues") {
    var fleet: Fleet = null
    val runner = new Runner:
      def run(id: AgentId, spec: Spec, ctx: Fleet.Ctx): Outcome ! Async =
        if spec.parent.isDefined then async { throw new RuntimeException("boom") }
        else Fleet.delegate(fleet, id).call(ToolCall("c", "delegate", Json.JObj(Vector("task" -> Json.JStr("x"))))).get
          .map(r => Outcome(r, None, Phase.Done))
    fleet = go(Fleet.open(MemoryStore(), runner))
    val p = go(fleet.spawn(spec("whole")))
    until(fleet.status(p).exists(_.phase == Phase.Done))
    val child = fleet.status(p).get.children.head
    assertEquals(fleet.status(child).map(_.phase), Some(Phase.Failed))
    assert(fleet.status(p).get.result.get.contains("failed: boom"), fleet.status(p).get.result)
  }

  test("restore: two running agents and a new process — both Interrupted, transcripts intact, ids continue") {
    val t = Ticks()
    val store = MemoryStore()
    val f = go(Fleet.open(store, Ticking(t.by, 10)))
    val a = go(f.spawn(spec("a"))); val b = go(f.spawn(spec("b")))
    t.tick("a"); t.tick("b")
    until(f.status(a).exists(_.step == 1) && f.status(b).exists(_.step == 1))
    // "the process ends": a fresh fleet over the same store
    val g = go(Fleet.open(store, Ticking(t.by, 0)))
    assertEquals(g.all.map(s => (s.id, s.phase)), Vector((a, Phase.Interrupted), (b, Phase.Interrupted)))
    assertEquals(g.transcript(a).toList, List(Turn.Assistant("step 1", Nil)))
    val c = go(g.spawn(spec("c")))
    assert(c.n > b.n, s"$c after $b")
    until(g.status(c).exists(_.phase == Phase.Done))   // zero steps: done at once
    // a third fold agrees with the second: Interrupted was written once, and c is Done
    val h = go(Fleet.open(store, Ticking(t.by, 0)))
    assertEquals(h.all.map(_.phase), Vector(Phase.Interrupted, Phase.Interrupted, Phase.Done))
  }
