package okay.agent
import okay.freer.*
import okay.std.*
import okay.{! as _, + as _, % as _, Pure as _, *}
import okay.given
import okay.freer.given
import okay.std.given
import okay.codec.Json
import okay.persist.{Ack, MemoryStore}
import Fleet.{Command, Control, Event}

/** specs/agent-fleet.md, "Commands": the control plane as a topic the
 * service folds, with a principal the service checks */
class TestFleetCommands extends munit.FunSuite:
  def go[A](p: A ! Async): A = Async.run[A, Pure](p).runWith
  def take[A](s: Source[A], n: Int): List[A] =
    summon[Stream[[W] =>> Unit ! Writer % W + Async, Async]].iterator(s).take(n).toList
  def until(what: => Boolean, ms: Int = 5000): Unit =
    val end = System.currentTimeMillis + ms
    while !what && System.currentTimeMillis < end do Thread.sleep(5)
    assert(what, s"waited ${ms}ms")

  /** waits for a tick on its own channel, then finishes */
  final class Waiting(ticks: Channel[Unit]) extends Runner:
    def run(id: AgentId, spec: Spec, ctx: Fleet.Ctx): Outcome ! Async =
      ticks.receive.flatMap(_ => ctx.checkpoint(1, "bash").map(v => Outcome(if v.isDefined then "stopped" else "done", None,
        if v.isDefined then Phase.Interrupted else Phase.Done)))

  test("command JSON round-trips for every kind; what is not a command is None") {
    val spawn = Command.Spawn(Spec("t", "/w", Budget(3, 1000), Some(AgentId(2)), Some("m")), "ada")
    assertEquals(Fleet.command(Fleet.commandJson(spawn)), Some(spawn))
    for c <- List(Control.Tell("go"), Control.Pause, Control.Resume, Control.Stop, Control.Kill, Control.Approve(3, true), Control.Approve(4, false)) do
      val cmd = Command.Send(AgentId(5), c, "bob")
      assertEquals(Fleet.command(Fleet.commandJson(cmd)), Some(cmd))
    assertEquals(Fleet.command(Json.parse("""{"c":"dance","by":"x"}""")), None)
    assertEquals(Fleet.command(Json.parse("""{"c":"tell","by":"x"}""")), None, "a send without an id")
  }

  test("the service folds the topic: an allowed spawn runs, a denied tell is Refused with why, a stop to nobody is Refused, offsets are reported in order") {
    val store = MemoryStore()
    val ticks = Channel[Unit](4)
    val fleet = go(Fleet.open(store, Waiting(ticks), () => 5L))
    val feed = fleet.events()
    val cmds = store.topic("commands")
    val applied = scala.collection.mutable.ListBuffer.empty[Long]
    val allow: (String, Command) => Either[String, Unit] = (by, c) => (by, c) match
      case ("ada", _) => Right(())
      case ("bob", Command.Send(_, Control.Tell(_), _)) => Left("bob may only look")
      case ("bob", _) => Right(())
      case _ => Left(s"unknown principal '$by'")
    val service = Async.spawn(fleet.commands(cmds, allow, applied = o => applied.synchronized { applied += o; () }))
    def put(c: Command) = cmds.append(Array.empty, Json.print(Fleet.commandJson(c)).getBytes("UTF-8"), Ack.Received): Unit
    put(Command.Spawn(Spec("t", "/w", Budget(3, 1000)), "ada"))
    until(fleet.all.nonEmpty)
    val id = fleet.all.head.id
    assertEquals(fleet.status(id).map(_.phase), Some(Phase.Running))
    put(Command.Send(id, Control.Tell("hurry"), "bob"))
    put(Command.Send(AgentId(99), Control.Stop, "ada"))
    cmds.append(Array.empty, "{\"c\":\"dance\"}".getBytes("UTF-8"), Ack.Received): Unit
    put(Command.Send(id, Control.Stop, "bob"))
    until(fleet.status(id).exists(_.phase == Phase.Stopping))   // the fold applied the stop…
    go(ticks.send(())): Unit                                    // …before the runner reaches its checkpoint
    until(fleet.status(id).exists(_.phase == Phase.Interrupted))
    val seen = take(feed, 6)
    assertEquals(seen.head, Event.Spawned(id, Spec("t", "/w", Budget(3, 1000)), Some("ada"), 5L))
    assertEquals(seen(1), Event.Refused(1, "bob", "bob may only look", 5L))
    assertEquals(seen(2), Event.Refused(2, "ada", "no live agent 99", 5L))
    assertEquals(seen(3), Event.Refused(3, "", "not a command", 5L))
    assert(seen.drop(4).exists { case Event.Phased(`id`, Phase.Stopping, _) => true; case _ => false }, seen.toString)
    until(applied.synchronized(applied.toList) == List(0L, 1L, 2L, 3L, 4L))
    // a fresh fold of the agents record is not confused by refusals
    val again = go(Fleet.open(store, Waiting(ticks)))
    assertEquals(again.all.map(_.id), Vector(id))
    service.cancel()
  }
