package okay.demo

import okay.codec.Json
import okay.codec.Json.*
import okay.persist.{Ack, Policy, Store, Topic}

/**
 * A shared task board, log-first — the demo's own small domain.
 *
 * It exists so the chat demo has something to be ABOUT. What the demo
 * is for is the mechanism around it: an agent calling tools, a
 * projection rebuilt from a durable log, live notifications, an MCP
 * front door, a page that streams. A domain is needed to show any of
 * that, and this is the smallest one that shows all of it.
 *
 * DELIBERATELY NOT A TWO-SIDED MARKET. The board has an owner and an
 * assignee, not a buyer and a seller, and nothing here searches for a
 * counterpart. That is not an accident of taste: matching is somebody
 * else's product, and a demo that re-implemented it would be
 * demonstrating the thing this library does not carry.
 *
 * THE LOG IS THE TRUTH and the board is a projection. Every change is
 * an appended record; `Board.replay` drops the projection and rebuilds
 * it by folding the same records through the same operations the live
 * calls use. That is one claim the whole demo rests on, and it is
 * checkable in one line: replay twice, get the same board.
 */
final case class Task(id: Long, text: String, owner: String,
                      assignee: Option[String], done: Boolean)
                      derives okay.codec.Schema

/**
 * What `/board.json` answers (openapi-media).
 *
 * A wrapper and not a bare array, because that is the shape the page
 * already reads (`d.tasks`) and because an object leaves room to add
 * a field without breaking a client. The `Schema` is what puts the
 * board's rows into the published document: the handler answers this
 * value and the router encodes it, so the document and the wire are
 * one derivation. `assignee: None` encodes as `null`, which is what
 * the hand-built object it replaced sent.
 */
final case class BoardView(tasks: Vector[Task]) derives okay.codec.Schema

object Board:

  def store(path: String): Store =
    if path == ":memory:" then okay.persist.MemoryStore()
    else okay.persist.FileStore.open(java.nio.file.Path.of(path))

  def topicOf(s: Store): Topic = s.topic("board", 1, Policy.default)

/**
 * The board itself: a projection in memory, and the log beside it.
 *
 * Everything that changes the board goes through `append` first and
 * then through `apply`, so the live path and the replay path are the
 * SAME code — a projection that can drift from its log is a projection
 * nobody can trust after a restart.
 */
final class Board(topic: Topic):

  private var tasks = Vector.empty[Task]
  private var next = 0L

  /** rung whenever the board moves, so an open page can be told */
  private var listeners = Vector.empty[String => Unit]
  def onChange(f: String => Unit): Unit = synchronized { listeners = listeners :+ f }
  private def rang(what: String): Unit = listeners.foreach(_(what))

  def all: Vector[Task] = synchronized(tasks)
  def of(person: String): Vector[Task] =
    synchronized(tasks.filter(t => t.owner == person || t.assignee.contains(person)))

  /** the projection step, and the only place the board changes */
  private def apply(op: String, f: String => Option[String]): Option[Task] = synchronized:
    op match
      case "add" =>
        for text <- f("text"); owner <- f("owner") yield
          next += 1
          val t = Task(next, text, owner, None, done = false)
          tasks = tasks :+ t
          t
      // a change to a task that is not there answers None. An earlier
      // cut returned an EMPTY task with the asked-for id, which is a
      // fabricated fact wearing the shape of a real one — the caller
      // could not tell "assigned" from "there is nothing to assign",
      // and a replay would have invented rows for records naming
      // tasks the log never created
      case "assign" =>
        for
          id <- f("id").flatMap(_.toLongOption)
          who <- f("who")
          t <- tasks.find(_.id == id).map(_.copy(assignee = Some(who)))
        yield
          tasks = tasks.map(x => if x.id == id then t else x)
          t
      case "done" =>
        for
          id <- f("id").flatMap(_.toLongOption)
          t <- tasks.find(_.id == id).map(_.copy(done = true))
        yield
          tasks = tasks.map(x => if x.id == id then t else x)
          t
      case _ => None

  private def write(op: String, fields: Vector[(String, Json)], key: String): Unit =
    topic.append(key.getBytes("UTF-8"),
      Json.print(JObj(("op" -> JStr(op)) +: fields)).getBytes("UTF-8"), Ack.Durable): Unit

  def add(text: String, owner: String): Option[Task] =
    write("add", Vector("text" -> JStr(text), "owner" -> JStr(owner)), owner)
    val t = apply("add", Map("text" -> text, "owner" -> owner).get)
    t.foreach(_ => rang("add"))
    t

  def assign(id: Long, who: String): Option[Task] =
    write("assign", Vector("id" -> JNum(id.toDouble), "who" -> JStr(who)), who)
    val t = apply("assign", Map("id" -> id.toString, "who" -> who).get)
    t.foreach(_ => rang("assign"))
    t

  def complete(id: Long): Option[Task] =
    write("done", Vector("id" -> JNum(id.toDouble)), "board")
    val t = apply("done", Map("id" -> id.toString).get)
    t.foreach(_ => rang("done"))
    t

  /**
   * Drop the projection and rebuild it from the log.
   *
   * The claim the whole demo rests on, made good in one method: the
   * board is derived, the log is the truth, and a process that lost
   * its memory has lost nothing. Nothing is rung while walking — a
   * restore rebuilds what happened, it does not announce it again.
   */
  def replay(): Long = replayFrom(topic)

  /**
   * Rebuild from ANOTHER handle on the same log.
   *
   * A follower polls a log another process is writing, and a handle
   * opened once does not see what arrived after it — so the two-node
   * lane opens a fresh one per tick and replays through this. Same
   * projection code either way; only the reader differs.
   */
  def replayFrom(t: Topic): Long = synchronized:
    tasks = Vector.empty
    next = 0L
    var n = 0L
    for part <- 0 until t.partitions do
      var from = t.begin(part)
      var going = true
      while going do
        t.read(part, from, 256) match
          case Topic.Read.TooEarly(begin) => from = begin
          case Topic.Read.Records(rs) if rs.isEmpty => going = false
          case Topic.Read.Records(rs) =>
            rs.foreach { r =>
              Json.parse(new String(r.value, "UTF-8")) match
                case JObj(fs) =>
                  def field(k: String) = fs.collectFirst {
                    case (`k`, JStr(v)) => v
                    case (`k`, JNum(v)) => v.toLong.toString
                  }
                  // a record that does not parse is skipped and not
                  // thrown: a log outlives the code that wrote it
                  field("op").foreach { op =>
                    // the applied task is not wanted here, only the
                    // count — said with a type rather than dropped
                    apply(op, field): Unit
                    n += 1
                  }
                case _ => ()
            }
            from = rs.last.offset + 1
    n

/**
 * The board as TOOLS, which is the only way an agent can touch it.
 *
 * The same table serves the model lane, the deterministic offline
 * agent and the MCP front door — one set of operations, three callers,
 * which is the property worth demonstrating. A tool answers with JSON
 * and never throws: a model that gets an exception learns nothing,
 * while a model that gets `{"error": ...}` can say what went wrong.
 */
object BoardTools:

  import okay.agent.{ToolCall, ToolSpec, Toolbox}
  import okay.codec.Schema

  /**
   * The arguments, which ARE the declaration
   * (specs/optics-outside.md, stage 2).
   *
   * What stood here was a hand-written `schema("text" -> "string", ...)`
   * beside a dispatch table that re-read those same field names as
   * string literals — two places to change and no compiler between
   * them. These four case classes are the single place now: the JSON
   * Schema the model is told, and the decode of a real call, are two
   * interpretations of one `Schema[A]`.
   *
   * The declaration got MORE accurate in the conversion, which is the
   * finding worth keeping: the hand-written schemas never said which
   * fields were required, so the model was never told `board_add`
   * needs both of them; and `id` is an integer, which is what a `Long`
   * is, not the "number" that was declared.
   */
  final case class Add(text: String, owner: String)
  final case class Listing(who: Option[String])
  final case class Assign(id: Long, who: String)
  final case class Done(id: Long)

  private given Schema[Add] = Schema.derived
  private given Schema[Listing] = Schema.derived
  private given Schema[Assign] = Schema.derived
  private given Schema[Done] = Schema.derived

  private def obj(fs: (String, Json)*): Json = JObj(fs.toVector)

  private def taskJson(t: Task): Json = obj(
    "id" -> JNum(t.id.toDouble), "text" -> JStr(t.text), "owner" -> JStr(t.owner),
    "assignee" -> t.assignee.map(JStr(_)).getOrElse(JNull),
    "done" -> JBool(t.done))

  private def err(m: String): String = Json.print(obj("error" -> JStr(m)))

  /** a tool that cannot do the thing answers with DATA */
  private def one(t: Option[Task], missing: => String): String =
    t.map(x => Json.print(taskJson(x))).getOrElse(err(missing))

  /** the four tools: name, purpose, argument type and answer, each
   * written once. `specs` and `table` below both come from here, so
   * a tool cannot be declared and undispatched or the other way. */
  def of(board: Board): Toolbox = Toolbox.empty
    .on[Add]("board_add",
      "Add a task to the shared board. The owner is whoever asked for it.")(a =>
      one(board.add(a.text, a.owner), "could not add"))
    .on[Listing]("board_list",
      "List the board. With `who`, only the tasks that person owns or was assigned.")(l =>
      Json.print(JArr(l.who.map(board.of).getOrElse(board.all).map(taskJson))))
    .on[Assign]("board_assign",
      "Assign an existing task, by number, to somebody.")(a =>
      one(board.assign(a.id, a.who), s"no task ${a.id}"))
    .on[Done]("board_done",
      "Mark a task finished, by number.")(d =>
      one(board.complete(d.id), s"no task ${d.id}"))

  /** the two interpreters the callers ask for, both out of `of` —
   * DECLARE for the model, DISPATCH for the wire. There is no
   * board-free `specs`: every caller has a board, and a fake one to
   * render a declaration would be machinery nobody needs. */
  def specs(board: Board): Seq[ToolSpec] = of(board).specs

  def table(board: Board): Map[String, ToolCall => String] = of(board).table
