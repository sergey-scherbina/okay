package okay.agent

import okay.*
import okay.freer.*

import okay.given
import okay.actor.{Actor, ActorRef, Behavior}
import Fleet.Control
import okay.codec.{Json, Schema}
import okay.codec.Json.*
import okay.persist.{Ack, Record, Store, Streams, Topic}

/**
 * AGENTS AS SUPERVISED ACTORS, A HIERARCHY THE PARENT GROWS
 * (specs/agent-fleet.md).
 *
 * okay-agent had the loop and no notion of a second agent. Here an agent
 * is a child of the fleet's root actor: its mailbox takes `Control`
 * (tell, pause, resume, stop, kill), its run is a fiber the consumer's
 * `Runner` drives, its status is a VALUE, and everything the fleet must
 * remember is appended to a topic and folded back by `restore()`.
 * Delegation is a tool in the parent's toolbox, so a hierarchy costs the
 * parent nothing it does not already know how to do — and the child's
 * steps come out of the parent's budget.
 *
 * The fleet never reads or writes a workspace: the tools, the prompt,
 * the gate and the sandbox are the `Runner`'s, which is what keeps a
 * product's six tools out of this module and lets the suite run on a
 * scripted runner with no model and no filesystem.
 */

final case class Budget(steps: Int, wallMs: Long)

/** what an agent is asked to do, and within what */
final case class Spec(task: String, workspace: String, budget: Budget,
                      parent: Option[AgentId] = None, model: Option[String] = None)

opaque type AgentId = Long
object AgentId:
  def apply(n: Long): AgentId = n
  extension (id: AgentId) def n: Long = id
  given Ordering[AgentId] = Ordering.Long

enum Phase:
  case Running, Paused, Stopping, Done, Failed, Killed, Interrupted
object Phase:
  def live(p: Phase): Boolean = p == Running || p == Paused || p == Stopping
  def parse(s: String): Phase = values.find(_.toString.equalsIgnoreCase(s)).getOrElse(Interrupted)

/** one agent, as the operator and a screen see it */
final case class Status(id: AgentId, parent: Option[AgentId], task: String, workspace: String,
                        phase: Phase, step: Int, lastTool: Option[String], elapsedMs: Long,
                        children: Vector[AgentId], result: Option[String], report: Option[Json],
                        asking: Option[Fleet.Ask] = None)

/** how a run ended: the runner says Done when the model had a final
 * answer, Interrupted when it was stopped or ran out */
final case class Outcome(text: String, report: Option[Json], phase: Phase)

/** what runs ONE agent — the consumer's loop, tools and gate. Between
 * tool calls it asks `ctx.checkpoint`; what it learns there is the only
 * control it needs to honour. */
trait Runner:
  def run(id: AgentId, spec: Spec, ctx: Fleet.Ctx): Outcome ! Async

final class Fleet private (store: Store, runner: Runner, now: () => Long, root: ActorRef[Unit])
                          (using Scheduler):
  import Fleet.*

  private val topic: Topic = store.topic("agents")
  private val lock = new Object
  private var entries = Map.empty[Long, Entry]
  private var nextId = 1L
  private var listeners = Vector.empty[Channel[Event]]

  // ---- the operator's side ------------------------------------------------

  /** returns at once with an id; the run is a fiber under an actor of its own */
  def spawn(spec: Spec, by: Option[String] = None): AgentId ! Async =
    val e = lock.synchronized {
      val e = new Entry(nextId, spec, now())
      nextId += 1
      entries += e.id -> e
      spec.parent.flatMap(p => entries.get(p.n)).foreach(pe => pe.children :+= e.id)
      write(e.id, rec("spawned", "task" -> JStr(spec.task), "workspace" -> JStr(spec.workspace),
        "steps" -> JNum(spec.budget.steps.toDouble), "wallMs" -> JNum(spec.budget.wallMs.toDouble),
        "parent" -> spec.parent.fold[Json](JNull)(p => JNum(p.n.toDouble)),
        "model" -> spec.model.fold[Json](JNull)(JStr(_)), "by" -> by.fold[Json](JNull)(JStr(_))))
      e
    }
    root.spawnChild[Entry, Control](e)(behaviour).map { ref =>
      e.actor = Some(ref)
      start(e)
      AgentId(e.id)
    }

  /** false: no such agent, or one that has ended */
  def send(id: AgentId, c: Control): Boolean ! Async =
    lock.synchronized(entries.get(id.n).filter(e => Phase.live(e.phase)).flatMap(_.actor)) match
      case Some(ref) => ref.tell(c)
      case None => pure(false)

  def status(id: AgentId): Option[Status] = lock.synchronized(entries.get(id.n).map(statusOf))

  def all: Vector[Status] = lock.synchronized(entries.values.toVector.sortBy(_.id).map(statusOf))

  def transcript(id: AgentId): Seq[Turn] = lock.synchronized(entries.get(id.n).fold(Vector.empty)(_.transcript))

  /** the steps an agent may still spend: its budget less its own and its children's */
  def stepsLeft(id: AgentId): Int = lock.synchronized(entries.get(id.n).fold(0)(_.left))

  def specOf(id: AgentId): Option[Spec] = lock.synchronized(entries.get(id.n).map(_.spec))

  /** the agent's end, as its status — for a parent waiting on a child */
  def await(id: AgentId): Option[Status] ! Async =
    lock.synchronized(entries.get(id.n).flatMap(_.fiber)) match
      case Some(f) => Async.attempt(f.joinAsync).map(_ => status(id))
      case None => pure(status(id))

  /** stop every agent (children first, the actor tree's order) and the root */
  def close(): Unit ! Async =
    lock.synchronized { listeners.foreach(_.close()); listeners = Vector.empty }
    root.stop()

  /**
   * THE SERVICE'S SIDE OF THE CONTROL PLANE: follow the `commands` topic as
   * it grows and apply each command — after `allow(by, command)` said so —
   * or put a `Refused(seq, by, why)` on the agents record, where the screen
   * that appended the command is already listening. `applied` hears every
   * offset once it is settled either way; the caller keeps it, and starts
   * from it next time. Never ends; the caller's fiber is the lifetime.
   */
  def commands(topic: Topic, allow: (String, Command) => Either[String, Unit], from: Long = 0L,
               pollMillis: Long = 25, applied: Long => Unit = _ => ())(using Timer): Unit ! Async =
    def refuse(seq: Long, by: String, why: String): Unit = lock.synchronized {
      write(-1L, rec("refused", "seq" -> JNum(seq.toDouble), "by" -> JStr(by), "why" -> JStr(why)))
    }
    def one(r: Record): Unit ! Async =
      val seq = r.offset
      Fleet.command(Json.parse(new String(r.value, "UTF-8"))) match
        case None => async(refuse(seq, "", "not a command"))
        case Some(c) => allow(c.principal, c) match
          case Left(why) => async(refuse(seq, c.principal, why))
          case Right(()) => c match
            case Command.Spawn(spec, by) => spawn(spec, Some(by)).map(_ => ())
            case Command.Send(id, control, by) =>
              send(id, control).map(ok => if !ok then refuse(seq, by, s"no live agent ${id.n}"))
    def go(src: Source[Chunk[Record]]): Unit ! Async =
      Writer.uncons[Chunk[Record], Unit, Async](src).flatMap {
        case Left(_) => pure(())
        case Right((chunk, more)) =>
          okay.freer.!.each(chunk.toVector)(r => one(r).map(_ => applied(r.offset))).flatMap(_ => go(more))
      }
    go(Streams.tail(topic, 0, from, pollMillis = pollMillis))

  /** every record this fleet writes from now on, as it is written — a
   * screen in the same process folds these instead of asking; a slow
   * listener is dropped after `capacity` unread, never the fleet held */
  def events(capacity: Int = 1024): Source[Event] =
    val ch = Channel[Event](capacity)
    lock.synchronized { listeners :+= ch }
    Writer.of(ch)

  /**
   * The log's projection: fold the topic from the start. An agent that was
   * live when the process ended comes back `Interrupted` — written once,
   * so a second fold agrees — with its transcript; ids continue from the
   * last. Called by `open`; calling it on a fleet with live agents would
   * replace what it knows, so it is for an empty one.
   */
  def restore(): Int =
    var n = 0
    lock.synchronized {
      entries = Map.empty
      for part <- 0 until topic.partitions do
        var from = topic.begin(part)
        var going = true
        while going do
          topic.read(part, from, 256) match
            case Topic.Read.TooEarly(b) => from = b
            case Topic.Read.Records(rs) if rs.isEmpty => going = false
            case Topic.Read.Records(rs) =>
              rs.foreach { r => if fold(Json.parse(new String(r.value, "UTF-8"))) then n += 1 }
              from = rs.last.offset + 1
      nextId = entries.keys.maxOption.fold(1L)(_ + 1)
      entries.values.filter(e => Phase.live(e.phase)).foreach { e =>
        e.phase = Phase.Interrupted
        write(e.id, rec("phased", "phase" -> JStr(e.phase.toString)))
      }
    }
    n

  // ---- the agent's side (through Ctx) --------------------------------------

  /** the actor: control messages, serialized per agent */
  private def behaviour: Behavior[Entry, Control] = (e, c) => async {
    lock.synchronized {
      c match
        case Control.Tell(m) => e.inbox :+= m
        case Control.Pause =>
          if e.phase == Phase.Running then
            e.phase = Phase.Paused; e.resume = Some(Channel[Unit](1)); phased(e)
        case Control.Resume =>
          if e.phase == Phase.Paused then
            e.phase = Phase.Running; phased(e); wake(e)
        case Control.Stop =>
          if Phase.live(e.phase) then
            e.stopping = true
            if e.phase == Phase.Paused then wake(e)
            e.phase = Phase.Stopping; phased(e)
            decline(e)
        case Control.Kill =>
          if Phase.live(e.phase) then
            wake(e)
            decline(e)
            finish(e, Outcome("", None, Phase.Killed))
            e.fiber.foreach(_.cancel())
        case Control.Approve(seq, yes) =>
          e.asking.filter(_.seq == seq).foreach { _ =>
            write(e.id, rec("answered", "seq" -> JNum(seq.toDouble), "yes" -> JBool(yes)))
            e.asking = None
            e.answer.foreach(ch => { ch.offer(yes): Unit; ch.close() }); e.answer = None
          }
    }
    e
  }

  private def wake(e: Entry): Unit =
    e.resume.foreach(_.close()); e.resume = None

  /** an open ask, answered no by a stop or a kill: the runner is not left parked */
  private def decline(e: Entry): Unit =
    e.asking.foreach { a => write(e.id, rec("answered", "seq" -> JNum(a.seq.toDouble), "yes" -> JBool(false))) }
    e.asking = None
    e.answer.foreach(_.close()); e.answer = None

  private def phased(e: Entry): Unit = write(e.id, rec("phased", "phase" -> JStr(e.phase.toString)))

  /** the run, as ONE fiber whose completion means the record is written:
   * a parent awaiting a child joins this fiber, so it must not see the
   * child before `finish` has run — which an `onComplete` callback would
   * not promise. A runner that throws ends the agent `Failed`. */
  private def start(e: Entry): Unit =
    val f = Async.spawn(Async.attempt(runner.run(AgentId(e.id), e.spec, new Ctx(e, this))).map {
      case Right(o) => lock.synchronized(finish(e, o)); o
      case Left(t) =>
        val o = Outcome(s"failed: ${t.getMessage}", None, Phase.Failed)
        lock.synchronized(finish(e, o)); o
    })
    lock.synchronized { e.fiber = Some(f) }

  /** the end, once: a later outcome for an agent already ended (a killed
   * fiber's cancellation, say) changes nothing */
  private def finish(e: Entry, o: Outcome): Unit =
    if Phase.live(e.phase) then
      e.phase = if Phase.live(o.phase) then Phase.Interrupted else o.phase
      e.result = Some(o.text); e.report = o.report; e.endedAt = Some(now()); e.asking = None
      e.spec.parent.flatMap(p => entries.get(p.n)).foreach(pe => pe.childSteps += e.step)
      write(e.id, rec("finished", "phase" -> JStr(e.phase.toString), "text" -> JStr(o.text),
        "report" -> o.report.getOrElse(JNull)))

  private[agent] def checkpoint(e: Entry, step: Int, tool: String): Option[Control] ! Async =
    val gate = lock.synchronized {
      e.step = step; e.lastTool = Some(tool)
      write(e.id, rec("stepped", "step" -> JNum(step.toDouble), "tool" -> JStr(tool)))
      e.resume
    }
    gate match
      case Some(ch) => ch.receive.map(_ => lock.synchronized(verdict(e)))
      case None => pure(lock.synchronized(verdict(e)))

  private def verdict(e: Entry): Option[Control] =
    if !Phase.live(e.phase) then Some(Control.Kill)
    else if e.stopping then Some(Control.Stop)
    else if e.left < 0 || now() - e.startedAt > e.spec.budget.wallMs then Some(Control.Stop)   // the step past the budget
    else None

  /** the runner asks before a call: on the record at once, parked until an
   * answer — `Approve(seq, yes)` through the mailbox, or no from a stop/kill */
  private[agent] def ask(e: Entry, step: Int, call: ToolCall): Boolean ! Async =
    val ch = lock.synchronized {
      if !Phase.live(e.phase) then None
      else
        val a = Ask(step.toLong, call.name, call.args)
        e.asking = Some(a)
        val ch = Channel[Boolean](1)
        e.answer = Some(ch)
        write(e.id, rec("asked", "seq" -> JNum(a.seq.toDouble), "tool" -> JStr(a.tool), "args" -> a.args))
        Some(ch)
    }
    ch match
      case None => pure(false)
      case Some(c) => c.receive.map(_.getOrElse(false))

  private[agent] def turned(e: Entry, t: Turn): Unit = lock.synchronized {
    if Phase.live(e.phase) then
      e.transcript :+= t
      write(e.id, JObj(Vector("kind" -> JStr("turned"), "id" -> JNum(e.id.toDouble), "turn" -> turnJson(t))))
  }

  private[agent] def drain(e: Entry): Vector[String] = lock.synchronized {
    val m = e.inbox; e.inbox = Vector.empty; m
  }

  // ---- the record -----------------------------------------------------------

  private def rec(kind: String, fields: (String, Json)*): Json =
    JObj(Vector("kind" -> JStr(kind)) ++ fields.toVector :+ ("at" -> JNum(now().toDouble)))

  private def write(id: Long, j: Json): Unit =
    val withId = j match
      case JObj(fs) if !fs.exists(_._1 == "id") => JObj(fs.take(1) ++ Vector("id" -> JNum(id.toDouble)) ++ fs.drop(1))
      case other => other
    topic.append(id.toString.getBytes("UTF-8"), Json.print(withId).getBytes("UTF-8"), Ack.Durable): Unit
    Fleet.event(withId).foreach { e =>
      // offer, never park: the fleet's lock is held; a full listener loses this one
      listeners = listeners.filter(ch => { ch.offer(e): Unit; !ch.finished })
    }

  private def fold(j: Json): Boolean = Fleet.event(j).exists(apply)

  /** one event into the maps — restore's step, and what a screen does with `events` */
  private def apply(ev: Event): Boolean = ev match
    case Event.Refused(_, _, _, _) => false   // on the record for the screen, not state
    case Event.Asked(id, ask, _) => entries.get(id.n).exists { e => e.asking = Some(ask); true }
    case Event.Answered(id, seq, _, _) => entries.get(id.n).exists { e => if e.asking.exists(_.seq == seq) then e.asking = None; true }
    case Event.Spawned(id, spec, _, at) =>
      val e = new Entry(id.n, spec, at)
      entries += id.n -> e
      spec.parent.flatMap(p => entries.get(p.n)).foreach(pe => pe.children :+= id.n)
      true
    case Event.Phased(id, phase, at) => entries.get(id.n).exists { e => e.phase = phase; e.endedAt = Some(at); true }
    case Event.Stepped(id, step, tool, at) => entries.get(id.n).exists { e => e.step = step; e.lastTool = Some(tool); e.endedAt = Some(at); true }
    case Event.Turned(id, turn) => entries.get(id.n).exists { e => e.transcript :+= turn; true }
    case Event.Finished(id, phase, text, report, at) => entries.get(id.n).exists { e =>
      e.phase = phase; e.result = Some(text); e.report = report; e.endedAt = Some(at)
      e.spec.parent.flatMap(p => entries.get(p.n)).foreach(pe => pe.childSteps += e.step)
      true }

  private def statusOf(e: Entry): Status =
    Status(AgentId(e.id), e.spec.parent, e.spec.task, e.spec.workspace, e.phase, e.step, e.lastTool,
      e.endedAt.filter(_ => !Phase.live(e.phase)).getOrElse(now()) - e.startedAt,
      e.children.map(AgentId(_)), e.result, e.report, e.asking)

object Fleet:

  /** what an agent can be told. Nested here, not at the package level:
   * okay's core exports a `Control` too, and in a file that imports
   * `okay.*` that one would win over a package member from this file. */
  enum Control:
    case Tell(message: String)
    case Pause, Resume
    case Stop     // finish the current tool, then halt
    case Kill     // now
    case Approve(seq: Long, yes: Boolean)   // the answer to an Ask

  /** what the fleet writes, typed — the record of specs/agent-fleet.md as
   * values, so a feed folds these and not JSON */
  enum Event:
    case Spawned(id: AgentId, spec: Spec, by: Option[String], at: Long)
    case Phased(id: AgentId, phase: Phase, at: Long)
    case Stepped(id: AgentId, step: Int, tool: String, at: Long)
    case Turned(id: AgentId, turn: Turn)
    case Finished(id: AgentId, phase: Phase, text: String, report: Option[Json], at: Long)
    /** a command the service would not apply — on the record, so the screen
     * that sent it sees why (`seq` is the command's offset) */
    case Refused(seq: Long, by: String, why: String, at: Long)
    /** the agent wants a yes before a tool call; a screen draws it and answers with `approve` */
    case Asked(id: AgentId, ask: Ask, at: Long)
    case Answered(id: AgentId, seq: Long, yes: Boolean, at: Long)

  /** what an agent is waiting on: the call, and the seq an answer must name */
  final case class Ask(seq: Long, tool: String, args: Json)

  /** THE CONTROL PLANE AS DATA (nadia NAD-14/NAD-20): what a screen appends to
   * a `commands` topic — through a RemoteStore from another process — and the
   * service folds with `fleet.commands`. `by` is the principal; the service's
   * `allow` decides. */
  enum Command:
    case Spawn(spec: Spec, by: String)
    case Send(id: AgentId, control: Control, by: String)
    /** the principal who appended it */
    def principal: String = this match
      case Spawn(_, b) => b
      case Send(_, _, b) => b

  def commandJson(c: Command): Json = c match
    case Command.Spawn(spec, by) => JObj(Vector("c" -> JStr("spawn"), "task" -> JStr(spec.task), "workspace" -> JStr(spec.workspace),
      "steps" -> JNum(spec.budget.steps.toDouble), "wallMs" -> JNum(spec.budget.wallMs.toDouble),
      "parent" -> spec.parent.fold[Json](JNull)(p => JNum(p.n.toDouble)), "model" -> spec.model.fold[Json](JNull)(JStr(_)), "by" -> JStr(by)))
    case Command.Send(id, control, by) =>
      val (name, extra) = control match
        case Control.Tell(m) => ("tell", Vector("message" -> JStr(m)))
        case Control.Pause => ("pause", Vector.empty)
        case Control.Resume => ("resume", Vector.empty)
        case Control.Stop => ("stop", Vector.empty)
        case Control.Kill => ("kill", Vector.empty)
        case Control.Approve(seq, yes) => ("approve", Vector("seq" -> JNum(seq.toDouble), "yes" -> JBool(yes)))
      JObj(Vector("c" -> JStr(name), "id" -> JNum(id.n.toDouble)) ++ extra :+ ("by" -> JStr(by)))

  /** total: what is not a command is `None`, and the service refuses it by offset */
  def command(j: Json): Option[Command] =
    val by = J.str(j, "by").getOrElse("")
    J.str(j, "c").flatMap {
      case "spawn" => Some(Command.Spawn(Spec(J.str(j, "task").getOrElse(""), J.str(j, "workspace").getOrElse(""),
        Budget(J.long(j, "steps").getOrElse(0L).toInt, J.long(j, "wallMs").getOrElse(0L)),
        J.long(j, "parent").map(AgentId(_)), J.str(j, "model")), by))
      case "tell" => J.long(j, "id").map(id => Command.Send(AgentId(id), Control.Tell(J.str(j, "message").getOrElse("")), by))
      case "pause" => J.long(j, "id").map(id => Command.Send(AgentId(id), Control.Pause, by))
      case "resume" => J.long(j, "id").map(id => Command.Send(AgentId(id), Control.Resume, by))
      case "stop" => J.long(j, "id").map(id => Command.Send(AgentId(id), Control.Stop, by))
      case "kill" => J.long(j, "id").map(id => Command.Send(AgentId(id), Control.Kill, by))
      case "approve" => for id <- J.long(j, "id"); seq <- J.long(j, "seq") yield
        Command.Send(AgentId(id), Control.Approve(seq, J.bool(j, "yes")), by)
      case _ => None
    }

  /** THE decoder of a record: `restore` folds through it, `events` tells
   * through it; `None` for a kind this version does not read */
  def event(j: Json): Option[Event] =
    val id = AgentId(J.long(j, "id").getOrElse(-1L))
    val at = J.long(j, "at").getOrElse(0L)
    J.str(j, "kind").flatMap {
      case "spawned" => Some(Event.Spawned(id, Spec(J.str(j, "task").getOrElse(""), J.str(j, "workspace").getOrElse(""),
        Budget(J.long(j, "steps").getOrElse(0L).toInt, J.long(j, "wallMs").getOrElse(0L)),
        J.long(j, "parent").map(AgentId(_)), J.str(j, "model")), J.str(j, "by"), at))
      case "refused" => Some(Event.Refused(J.long(j, "seq").getOrElse(-1L), J.str(j, "by").getOrElse(""), J.str(j, "why").getOrElse(""), at))
      case "asked" => Some(Event.Asked(id, Ask(J.long(j, "seq").getOrElse(0L), J.str(j, "tool").getOrElse(""), J.field(j, "args").getOrElse(JNull)), at))
      case "answered" => Some(Event.Answered(id, J.long(j, "seq").getOrElse(0L), J.bool(j, "yes"), at))
      case "phased" => Some(Event.Phased(id, Phase.parse(J.str(j, "phase").getOrElse("")), at))
      case "stepped" => Some(Event.Stepped(id, J.long(j, "step").getOrElse(0L).toInt, J.str(j, "tool").getOrElse(""), at))
      case "turned" => J.field(j, "turn").flatMap(turnOf).map(Event.Turned(id, _))
      case "finished" => Some(Event.Finished(id, Phase.parse(J.str(j, "phase").getOrElse("")), J.str(j, "text").getOrElse(""),
        J.field(j, "report").filter(_ != JNull), at))
      case _ => None
    }

  /** the log followed as events, from an offset, in ANY process that can
   * read the topic (a `RemoteStore` over okay-persist's wire included):
   * a feed that never asks the agent. It does not end; the consumer stops
   * pulling. */
  def events(topic: Topic, from: Long = 0L, pollMillis: Long = 25)(using Timer): Source[Event] =
    type F = Writer % Event + Async
    def go(src: Source[Chunk[Record]]): Unit ! F =
      okay.freer.!.widen[Either[Unit, (Chunk[Record], Source[Chunk[Record]])], Async, Writer % Event](
        Writer.uncons[Chunk[Record], Unit, Async](src)).flatMap {
        case Left(_) => pure(())
        case Right((chunk, more)) =>
          val evs = chunk.iterator.flatMap(r => event(Json.parse(new String(r.value, "UTF-8")))).toList
          okay.freer.!.each(evs)(e => effect[F, Unit](Writer(e))).flatMap(_ => go(more))
      }
    go(Streams.tail(topic, 0, from, pollMillis = pollMillis))

  /** the fleet: its root actor, and the log folded */
  def open(store: Store, runner: Runner, now: () => Long = () => System.currentTimeMillis())
          (using Scheduler): Fleet ! Async =
    Actor.spawn[Unit, Unit](())((s, _) => pure(s)).map { root =>
      val f = new Fleet(store, runner, now, root)
      f.restore(): Unit
      f
    }

  /** what a runner sees of the fleet: its inbox, the checkpoint it asks
   * between tool calls, the transcript it appends to, its budget */
  final class Ctx private[agent] (e: Entry, fleet: Fleet):
    def spec: Spec = e.spec
    /** the tells since the last call, in order */
    def inbox(): Vector[String] = fleet.drain(e)
    /** between tool calls: records the step; awaits while paused; then
     * `Some(Stop)` to finish this tool and return, `Some(Kill)` when the
     * agent is already over, `None` to go on. Budget exhaustion — steps
     * or wall clock — answers `Stop` too. */
    def checkpoint(step: Int, tool: String): Option[Control] ! Async = fleet.checkpoint(e, step, tool)
    /** one turn, appended as it happens — a kill loses at most the one in flight */
    def turned(t: Turn): Unit = fleet.turned(e, t)
    /** before a call the policy gates (SPEC §3.3): the ask goes on the record and
     * this parks until `Control.Approve(step, yes)` arrives — false when the agent
     * is stopped or killed meanwhile. A runner that never asks is auto-approve. */
    def ask(step: Int, call: ToolCall): Boolean ! Async = fleet.ask(e, step, call)
    def stepsLeft: Int = fleet.stepsLeft(AgentId(e.id))

  /** the arguments of the parent's tool */
  final case class Delegate(task: String, subdir: Option[String] = None, budget: Option[Int] = None)
  given Schema[Delegate] = Schema.derived

  /**
   * `delegate(task, subdir?, budget?)` for a parent's toolbox: runs a child
   * to completion in the parent's workspace (or under it) and answers its
   * text and report. The child's steps are deducted from the parent's; a
   * child asked for more than the parent has is refused naming both
   * numbers; a child that crashes is a tool error the parent reads, not
   * the parent's end.
   */
  def delegate(fleet: Fleet, parent: AgentId): Toolbox.In[Async] =
    Toolbox.In.empty[Async].on[Delegate]("delegate",
      "Run one independent part of your task as a subagent, to completion, and get back its final answer " +
      "and its check report. It works in your workspace (or a subdirectory of it) and its steps are taken " +
      "out of your own budget.") { d =>
      fleet.status(parent) match
        case None => pure(Toolbox.failed("delegate", s"no such parent agent ${parent.n}"))
        case Some(ps) =>
          val left = fleet.stepsLeft(parent)
          val asked = d.budget.getOrElse(left)
          if left <= 0 then pure(Toolbox.failed("delegate", "no steps left to delegate"))
          else if asked > left then pure(Toolbox.failed("delegate", s"asked for $asked steps, only $left left"))
          else
            val where = d.subdir.filter(_.nonEmpty).fold(ps.workspace)(s => ps.workspace.stripSuffix("/") + "/" + s.stripPrefix("/"))
            val spec = Spec(d.task, where, Budget(asked, wallLeft(fleet, parent)), Some(parent), fleet.modelOf(parent))
            fleet.spawn(spec).flatMap(cid => fleet.await(cid).map {
              case Some(st) if st.phase == Phase.Done || st.phase == Phase.Interrupted =>
                Json.print(JObj(Vector("agent" -> JNum(st.id.n.toDouble), "phase" -> JStr(st.phase.toString),
                  "result" -> JStr(st.result.getOrElse("")), "report" -> st.report.getOrElse(JNull))))
              case Some(st) => Toolbox.failed("delegate", s"subagent ${st.id.n} ${st.phase.toString.toLowerCase}: ${st.result.getOrElse("")}")
              case None => Toolbox.failed("delegate", "the subagent vanished")
            })
    }

  private def wallLeft(fleet: Fleet, id: AgentId): Long =
    fleet.status(id).fold(0L)(s => math.max(0L, fleet.budgetOf(id).wallMs - s.elapsedMs))

  extension (f: Fleet)
    private def budgetOf(id: AgentId): Budget = f.specOf(id).fold(Budget(0, 0L))(_.budget)
    private def modelOf(id: AgentId): Option[String] = f.specOf(id).flatMap(_.model)

  // ---- turns on the wire ------------------------------------------------------

  private[agent] def turnJson(t: Turn): Json = t match
    case Turn.System(s) => JObj(Vector("t" -> JStr("system"), "text" -> JStr(s)))
    case Turn.User(s) => JObj(Vector("t" -> JStr("user"), "text" -> JStr(s)))
    case Turn.Assistant(s, calls) => JObj(Vector("t" -> JStr("assistant"), "text" -> JStr(s),
      "calls" -> JArr(calls.toVector.map(c => JObj(Vector("id" -> JStr(c.id), "name" -> JStr(c.name), "args" -> c.args))))))
    case Turn.Result(call, content) => JObj(Vector("t" -> JStr("result"), "call" -> JStr(call), "content" -> JStr(content)))
    case Turn.Summary(s, covers) => JObj(Vector("t" -> JStr("summary"), "text" -> JStr(s), "covers" -> JNum(covers.toDouble)))
    case Turn.StatePatch(p) => JObj(Vector("t" -> JStr("patch"), "patch" -> p))

  private[agent] def turnOf(j: Json): Option[Turn] = J.str(j, "t").flatMap {
    case "system" => Some(Turn.System(J.str(j, "text").getOrElse("")))
    case "user" => Some(Turn.User(J.str(j, "text").getOrElse("")))
    case "assistant" => Some(Turn.Assistant(J.str(j, "text").getOrElse(""), J.arr(j, "calls").map(c =>
      ToolCall(J.str(c, "id").getOrElse(""), J.str(c, "name").getOrElse(""), J.field(c, "args").getOrElse(JNull)))))
    case "result" => Some(Turn.Result(J.str(j, "call").getOrElse(""), J.str(j, "content").getOrElse("")))
    case "summary" => Some(Turn.Summary(J.str(j, "text").getOrElse(""), J.long(j, "covers").getOrElse(0L).toInt))
    case "patch" => J.field(j, "patch").map(Turn.StatePatch(_))
    case _ => None
  }

  private[agent] object J:
    def field(j: Json, k: String): Option[Json] = j match
      case JObj(fs) => fs.collectFirst { case (`k`, v) => v }
      case _ => None
    def str(j: Json, k: String): Option[String] = field(j, k).collect { case JStr(s) => s }
    def long(j: Json, k: String): Option[Long] = field(j, k).collect { case JNum(n) => n.toLong }
    def bool(j: Json, k: String): Boolean = field(j, k).contains(JBool(true))
    def arr(j: Json, k: String): Vector[Json] = field(j, k) match
      case Some(JArr(vs)) => vs
      case _ => Vector.empty

  /** one agent's mutable record; every field is read and written under the fleet's lock */
  private[agent] final class Entry(val id: Long, val spec: Spec, val startedAt: Long):
    var phase: Phase = Phase.Running
    var step = 0
    var lastTool: Option[String] = None
    var children = Vector.empty[Long]
    var childSteps = 0
    var result: Option[String] = None
    var report: Option[Json] = None
    var endedAt: Option[Long] = None
    var inbox = Vector.empty[String]
    var stopping = false
    var resume: Option[Channel[Unit]] = None
    var asking: Option[Ask] = None
    var answer: Option[Channel[Boolean]] = None
    var fiber: Option[Fiber[Outcome]] = None
    var transcript = Vector.empty[Turn]
    var actor: Option[ActorRef[Control]] = None
    def left: Int = spec.budget.steps - step - childSteps
