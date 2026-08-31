package okay.mcp

import okay.*
import okay.given
import okay.agent.{Model, Reply, Tool, ToolCall, ToolSpec, Turn}
import okay.codec.Json

/**
 * The transport seam, and the only thing either end needs from one:
 * a way to put a line out and a stream of the lines coming back. A
 * subprocess's pipes, a socket, a pair of Channels in a test — the
 * protocol above cannot tell which, which is why the whole of it is
 * testable with no process and no clock.
 *
 * A Link is LINEAR, like a Channel: its lines are consumed as they
 * are read, and re-observing the source does not replay them.
 */
trait Link:
  /** put one line on the wire */
  def send(line: String): Unit ! Async

  /** the lines coming back, as they come */
  def lines: Source[String]

/**
 * An MCP server, in our own vocabulary — and DUPLEX, because the
 * protocol is: a server asks the client for its roots, or for a
 * completion from the client's model, and tells it when a resource
 * changed. So a session is not a request/answer loop with a filter on
 * it; it is a reader fiber and three destinations.
 *
 *   - an ANSWER completes the request that is waiting for it (each
 *     one waits on an `Async.await`, so nothing parks a thread and
 *     the whole thing works where nothing may park)
 *   - a NOTIFICATION goes to a Channel — something arriving when it
 *     arrives is what a channel is for, and the caller consumes it as
 *     the async stream it already is
 *   - a REQUEST is answered by the `Peer`, on its own fiber so that a
 *     slow sampling call cannot stop the reader
 */
final class Session private[mcp] (link: Link, peer: Duplex.Peer)(using Scheduler) {

  import java.util.concurrent.ConcurrentHashMap
  import java.util.concurrent.atomic.AtomicInteger

  private val counter = AtomicInteger(0)
  private val pending = ConcurrentHashMap[String, Json => Unit]()
  private var info: Option[Mcp.Info] = None
  private var caps: Set[String] = Set.empty

  /**
   * Everything the server said without being asked: resource updates,
   * list-changed, progress, cancellation. A channel, so a consumer
   * reads it as the async stream it is — `session.notifications
   * .toLazyList.foreach(...)` on its own fiber is the whole pattern.
   */
  val notifications: Channel[Rpc.Notify] = Channel[Rpc.Notify]()

  /** who answered the handshake, if it has happened */
  def server: Option[Mcp.Info] = info

  /** what the handshake said this server has — ask before asking */
  def has(capability: String): Boolean = caps.contains(capability)

  private[mcp] def opened(i: Option[Mcp.Info], c: Set[String]): Session =
    info = i; caps = c; this

  // ---------------------------------------------------------------- reading

  /** the reader: one fiber, for the life of the session */
  private[mcp] def start(): Fiber[Unit] = Async.spawn(read(link.lines))

  private def read(rest: Source[String]): Unit ! Async =
    Writer.uncons[String, Unit, Async](rest).flatMap {
      case Left(_) => async(ended())
      case Right((line, more)) => dispatch(Rpc.decode(line)).flatMap(_ => read(more))
    }

  /** the link is gone: every waiting request answers, nobody hangs */
  private def ended(): Unit =
    notifications.close()
    pending.values().forEach(k => k(Json.JErr("the MCP link ended")))
    pending.clear()

  private def dispatch(m: Rpc): Unit ! Async = m match
    case Rpc.Answer(id, result) => async(complete(id, result))
    case Rpc.Failed(id, code, msg) if id != Json.JNull =>
      async(complete(id, Json.JErr(s"$code $msg")))
    // a damaged line from the server answers nothing and ends nothing
    case Rpc.Failed(_, _, _) => pure(())
    case n: Rpc.Notify => async(notifications.send(n))
    case Rpc.Request(id, method, params) =>
      // on its own fiber: a sampling call may take a second, and the
      // reader must keep reading while it does
      async(Async.spawn(serve(id, method, params)): Unit)

  private def complete(id: Json, result: Json): Unit =
    val k = pending.remove(Json.print(id))
    if k != null then k(result)

  // ---------------------------------------------------------------- being asked

  /** what this client answers when the server asks it something */
  private def serve(id: Json, method: String, params: Json): Unit ! Async = method match
    case Mcp.Ping => reply(id, Rpc.obj())

    case Mcp.RootsList =>
      if peer.roots.isEmpty then refuse(id, Mcp.RootsList)
      else reply(id, Duplex.rootsResult(peer.roots))

    /**
     * The sentence this half exists for: a server asking for a
     * completion is `Model.Complete`, and the handler that answers it
     * is the one an agent in this process is already using. The
     * server borrows YOUR model, and nothing new interprets it.
     */
    case Mcp.SamplingCreate => peer.sample match
      case None => refuse(id, Mcp.SamplingCreate)
      case Some(model) => async {
        val turns = Duplex.samplingTurns(params)
        model.handle(Model.Complete(turns, Nil))
      }.flatMap(r => reply(id, Duplex.samplingResult(r)))

    case other => refuse(id, other)

  private def reply(id: Json, result: Json): Unit ! Async =
    link.send(Rpc.encode(Rpc.Answer(id, result)))

  private def refuse(id: Json, method: String): Unit ! Async =
    link.send(Rpc.encode(Rpc.Failed(id, Rpc.MethodNotFound, method)))

  // ---------------------------------------------------------------- asking

  /**
   * One request, and the answer to it: register the slot, send, wait
   * on the slot. The waiting is an `Async.await` — a callback the
   * reader fires — so no thread parks here.
   *
   * The send is part of the PROGRAM, not spawned onto a fiber, and
   * that is a bug this file already had once. Spawning it makes the
   * writing thread ephemeral, and a `PipedOutputStream` remembers
   * which thread wrote last: when that fiber has died and the reader
   * finds the buffer empty, the pipe declares the write end dead and
   * the session hangs. Sending in the caller's own thread of control
   * also keeps the lines in the order the calls were made, which a
   * race between spawned writers would not.
   */
  def request(method: String, params: Json): Json ! Async =
    val id = Json.JNum(counter.incrementAndGet().toDouble)
    val key = Json.print(id)
    val slot = Slot()
    pending.put(key, j => slot.complete(j))
    link.send(Rpc.encode(Rpc.Request(id, method, params)))
      .flatMap(_ => okay.await[Json](k => slot.onComplete(k)))

  /** one notification — nothing comes back, so nothing is waited for */
  def notify(method: String, params: Json): Unit ! Async =
    link.send(Rpc.encode(Rpc.Notify(method, params)))

  // ---------------------------------------------------------------- tools

  /**
   * Everything the server serves, pages followed. `nextCursor` is the
   * protocol's pagination and it is the client's job, not the
   * caller's: a caller asking "what tools are there" wants the tools.
   */
  def tools: Seq[ToolSpec] ! Async =
    def page(cursor: Option[String], acc: Seq[ToolSpec]): Seq[ToolSpec] ! Async =
      val params = cursor.fold(Rpc.obj())(c => Rpc.obj("cursor" -> Json.JStr(c)))
      request(Mcp.ToolsList, params).flatMap { result =>
        val (ts, next) = Mcp.toolsOf(result)
        next match
          case Some(c) if ts.nonEmpty || acc.isEmpty => page(Some(c), acc ++ ts)
          case _ => pure(acc ++ ts)
      }

    page(None, Nil)

  /** call one, and answer what it said */
  def call(c: ToolCall): String ! Async =
    request(Mcp.ToolsCall, Mcp.callParams(c)).map {
      case Json.JErr(m) => s"error: $m"
      case result => Mcp.textOf(result)
    }

  /**
   * The server AS a handler for the Tool effect — the sentence this
   * module exists for. An agent program does not change by one
   * character when its tools come from here.
   *
   * Two forms, and the split is the library's usual one. `interpret`
   * answers with a PROGRAM, so it forwards into Async and works where
   * nothing may park (use it with `translate`). `handler` answers
   * with a VALUE, which means blocking, which is why it asks for the
   * evidence that this platform can.
   */
  def interpret: Tool ==> ([X] =>> X ! Async) =
    [X] => (t: Tool[X]) => t match
      // Tool is covariant, so matching `Call` gives String <: X — an
      // upcast, not an assertion
      case Tool.Call(c) => call(c).map(s => (s: X))

  def handler(using CanBlock): Handler[Tool] = new:
    def handle[A](e: Tool[A]): A = e match
      case Tool.Call(c) => call(c).runWith

  // ---------------------------------------------------------------- documents

  /**
   * The documents a server offers. Pages are followed, like tools —
   * `nextCursor` is the client's business, not the caller's.
   */
  def resources: Seq[Mcp.Resource] ! Async =
    def page(cursor: Option[String], acc: Seq[Mcp.Resource]): Seq[Mcp.Resource] ! Async =
      val params = cursor.fold(Rpc.obj())(c => Rpc.obj("cursor" -> Json.JStr(c)))
      request(Mcp.ResourcesList, params).flatMap { result =>
        val (rs, next) = McpDocs.resourcesOf(result)
        next match
          case Some(c) if rs.nonEmpty || acc.isEmpty => page(Some(c), acc ++ rs)
          case _ => pure(acc ++ rs)
      }

    page(None, Nil)

  /** the text of one, or None — the uri is unknown or the link ended */
  def read(uri: String): Option[String] ! Async =
    request(Mcp.ResourcesRead, Rpc.obj("uri" -> Json.JStr(uri))).map {
      case Json.JErr(_) => None
      case result => McpDocs.contentsOf(result)
    }

  /**
   * Every resource the server has, as documents — the bridge that
   * makes a remote server's files something the RETRIEVER can index.
   * An agent then searches them exactly as it searches local ones,
   * which is the resource half of this module's thesis: nothing in
   * the agent knows where a document came from.
   */
  def corpus: okay.rag.Corpus ! Async =
    resources.flatMap { rs =>
      def go(rest: Seq[Mcp.Resource], acc: Seq[okay.rag.Source])
      : Seq[okay.rag.Source] ! Async = rest match
        case Nil => pure(acc)
        case r +: more => read(r.uri).flatMap {
          case Some(text) => go(more, acc :+ okay.rag.Source(r.uri, text))
          case None => go(more, acc)   // a resource that will not read is not a document
        }

      go(rs, Nil).map(okay.rag.Corpus.of)
    }

  /** watch one resource: its updates arrive on `notifications` */
  def subscribe(uri: String): Boolean ! Async =
    request(Mcp.ResourcesSubscribe, Rpc.obj("uri" -> Json.JStr(uri))).map(ok)

  /** stop watching it */
  def unsubscribe(uri: String): Boolean ! Async =
    request(Mcp.ResourcesUnsubscribe, Rpc.obj("uri" -> Json.JStr(uri))).map(ok)

  private def ok(j: Json): Boolean = j match
    case Json.JErr(_) => false
    case _ => true

  /** the conversation openings a server offers */
  def prompts: Seq[Mcp.Prompt] ! Async =
    request(Mcp.PromptsList, Rpc.obj()).map(McpDocs.promptsOf)

  /**
   * One of them, arguments substituted by the server, as the turns an
   * agent's context is made of. Empty if the prompt is unknown — the
   * caller asked for a name it got from `prompts`.
   */
  def prompt(name: String, args: Map[String, String] = Map.empty): Seq[Turn] ! Async =
    request(Mcp.PromptsGet, Rpc.obj(
      "name" -> Json.JStr(name),
      "arguments" -> Json.JObj(args.toVector.map((k, v) => (k, Json.JStr(v))))))
      .map {
        case Json.JErr(_) => Nil
        case result => McpDocs.turnsOf(result)
      }

  /** our roots changed; a server that cares will ask again */
  def rootsChanged: Unit ! Async = notify(Mcp.RootsChanged, Rpc.obj())
}

/**
 * A one-shot cell: the answer arrives from the reader, the waiter
 * arrives from the caller, and whichever is second finds the first.
 * `CompletableFuture` would do it on the JVM and not on JS, and this
 * is nine lines.
 */
private final class Slot {
  private var value: Option[Json] = None
  private var waiter: Option[Json => Unit] = None

  def complete(j: Json): Unit =
    val k = synchronized {
      if value.isEmpty then value = Some(j)
      val w = waiter
      waiter = None
      w
    }
    k.foreach(_(j))

  def onComplete(k: Json => Unit): Unit =
    val ready = synchronized {
      if value.isDefined then value else { waiter = Some(k); None }
    }
    ready.foreach(k)
}

object Client {

  /**
   * Open a session: start the reader, then the `initialize`
   * handshake and the `notifications/initialized` the protocol
   * requires before anything else may be asked. The client's own
   * capabilities go out with it — in a duplex protocol a server may
   * only ask for what the client said it has.
   */
  def connect(link: Link, client: Mcp.Info,
              peer: Duplex.Peer = Duplex.Peer())(using Scheduler): Session ! Async =
    val s = Session(link, peer)
    s.start(): Unit
    s.request(Mcp.Initialize, Mcp.initializeParams(client,
      roots = peer.roots.nonEmpty, sampling = peer.sample.isDefined)).flatMap { result =>
      s.notify(Mcp.Initialized, Rpc.obj())
        .map(_ => s.opened(Rpc.field(result, "serverInfo").flatMap(Mcp.infoOf),
          Set("tools", "resources", "prompts").filter(Mcp.capability(result, _))))
    }
}
