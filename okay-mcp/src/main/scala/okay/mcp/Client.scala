package okay.mcp

import okay.*
import okay.given
import okay.agent.{Tool, ToolCall, ToolSpec, Turn}
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
 * An MCP server, in our own vocabulary. The session owns two pieces
 * of state and no more: which request id comes next, and how much of
 * the incoming line stream is left — both mutable, both for the same
 * reason a Channel is (the wire is linear; reading it twice does not
 * read the same bytes twice).
 *
 * Everything else is a program: `tools` and `call` answer in Async
 * and perform nothing until something runs them.
 */
final class Session private[mcp] (link: Link, private var rest: Source[String]) {

  private var counter = 0
  private var info: Option[Mcp.Info] = None

  /** who answered the handshake, if it has happened */
  def server: Option[Mcp.Info] = info

  /** what the handshake said this server has — ask before asking */
  def has(capability: String): Boolean = caps.contains(capability)

  private var caps: Set[String] = Set.empty

  private[mcp] def opened(i: Option[Mcp.Info], c: Set[String]): Session =
    info = i; caps = c; this

  /** one request, and the answer to it */
  def request(method: String, params: Json): Json ! Async =
    counter += 1
    val id = Json.JNum(counter.toDouble)
    link.send(Rpc.encode(Rpc.Request(id, method, params))).flatMap(_ => answerTo(id))

  /** one notification — nothing comes back, so nothing is waited for */
  def notify(method: String, params: Json): Unit ! Async =
    link.send(Rpc.encode(Rpc.Notify(method, params)))

  /**
   * Read until the answer to THIS id arrives. Notifications and
   * messages for other ids are skipped rather than an error: a server
   * is allowed to talk while we wait, and a session that failed on
   * that would work only against servers that stay silent.
   *
   * The link ending is an answer too — `JErr`, which every reader
   * below turns into the `error: ...` a tool call is allowed to
   * answer. A server that dies mid-call does not take the agent with
   * it.
   */
  private def answerTo(id: Json): Json ! Async =
    Writer.uncons[String, Unit, Async](rest).flatMap {
      case Left(_) => pure(Json.JErr("the MCP link ended before the answer"))
      case Right((line, more)) =>
        rest = more
        Rpc.decode(line) match
          case Rpc.Answer(i, r) if i == id => pure(r)
          case Rpc.Failed(i, c, m) if i == id => pure(Json.JErr(s"$c $m"))
          case _ => answerTo(id)
    }

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
}

object Client {

  /**
   * Open a session: the `initialize` handshake, then the
   * `notifications/initialized` the protocol requires before anything
   * else may be asked. The server's own info comes back with it.
   */
  def connect(link: Link, client: Mcp.Info): Session ! Async =
    val s = Session(link, link.lines)
    s.request(Mcp.Initialize, Mcp.initializeParams(client)).flatMap { result =>
      s.notify(Mcp.Initialized, Rpc.obj())
        .map(_ => s.opened(Rpc.field(result, "serverInfo").flatMap(Mcp.infoOf),
          Set("tools", "resources", "prompts").filter(Mcp.capability(result, _))))
    }
}
