package okay.http

import okay.*
import okay.given
import okay.codec.Json
import okay.mcp.{Mcp, Rpc, Server as McpServer}

/**
 * MCP's streamable HTTP transport, both ends — and the point is how
 * little of it is new. A `Link` is `send(line)` plus
 * `lines: Source[String]`, so okay-mcp's session and server are
 * untouched; what changes is where the lines come from.
 *
 * One endpoint answers three ways, and each is something this module
 * already returns: a POST answered by one JSON-RPC message
 * (`application/json`), a POST answered by a STREAM of them
 * (`text/event-stream`, which is `Http.sse` verbatim), and a GET that
 * opens a stream for what the server says unasked. Plus one piece of
 * state: the `Mcp-Session-Id` a server may issue on initialize, which
 * every later request must carry.
 *
 * It lives here rather than in okay-mcp because the layering is that
 * way round — a transport depends on the protocol, like `Ws.link`
 * above it.
 */
object McpHttp {

  val SessionHeader = "mcp-session-id"
  val VersionHeader = "mcp-protocol-version"

  /**
   * An endpoint AS a Link. Everything a POST answers with — one
   * message or a stream of them — lands on the same inbound channel,
   * which is what `lines` reads, so the session above cannot tell
   * which shape the server chose.
   */
  final class McpLink private[http] (http: Http, url: String)(using Scheduler)
    extends okay.mcp.Link {

    private val inbound = Channel[String]()
    @volatile private var session: Option[String] = None

    /** what the server issued at initialize, if it issued one */
    def sessionId: Option[String] = session

    private def headers: Seq[(String, String)] = Seq(
      ("content-type", "application/json"),
      ("accept", "application/json, text/event-stream"),
      (VersionHeader, Mcp.Version)) ++
      session.map(id => (SessionHeader, id))

    def send(line: String): Unit ! Async =
      http.send(Request.post(url, Body.Text(line), headers))
        .flatMap(receive(_, awaiting(line)))

    /** the id this line is waiting for an answer to, if it is a request */
    private def awaiting(line: String): Option[Json] = Rpc.decode(line) match
      case Rpc.Request(id, _, _) => Some(id)
      case _ => None

    /**
     * What came back. A 202 is the protocol saying "there was nothing
     * to answer"; a JSON body is one message; an event stream is many,
     * drained on a fiber so a long stream does not hold up the send.
     */
    private def receive(r: Response, awaited: Option[Json]): Unit ! Async =
      r.header(SessionHeader).foreach(id => session = Some(id))
      if r.status == 404 then
        // the session expired: the protocol's own "reinitialize" —
        // said as a message, because a Link may not throw
        fail(awaited, Rpc.InvalidRequest, "the MCP session is gone")
      else if r.status == 202 || r.status == 204 then
        // 202 to a NOTIFICATION is right and there is nothing to do.
        // 202 to a REQUEST is a server saying it will never answer,
        // and the caller is waiting: it hears that, rather than
        // waiting for ever.
        fail(awaited, Rpc.InternalError, s"the server answered ${r.status} to a request")
      else if r.status >= 400 then
        fail(awaited, Rpc.InternalError, s"HTTP ${r.status}")
      else if r.header("content-type").exists(_.contains("text/event-stream")) then
        async(Async.spawn(drain(Http.sse(r))): Unit)
      else Http.text(r).flatMap(t =>
        if t.trim.nonEmpty then async(inbound.send(t))
        else fail(awaited, Rpc.InternalError, "the server answered nothing"))

    /** tell the waiting request that no answer is coming (a
     * notification waits for nothing, so it hears nothing) */
    private def fail(awaited: Option[Json], code: Int, message: String): Unit ! Async =
      awaited match
        case None => pure(())
        case Some(id) => async(inbound.send(Rpc.encode(Rpc.Failed(id, code, message))))

    private def drain(s: Source[String]): Unit ! Async =
      Writer.uncons[String, Unit, Async](s).flatMap {
        case Left(_) => pure(())
        case Right((line, rest)) =>
          if line.trim.isEmpty then drain(rest)
          else async(inbound.send(line)).flatMap(_ => drain(rest))
      }

    /**
     * The GET stream: what the server says unasked. Optional by the
     * protocol (a server may answer 405), and optional here — a
     * client that never opens it simply never hears a notification,
     * which over HTTP is a choice rather than a fault.
     */
    def open(): Fiber[Unit] =
      Async.spawn(http.send(Request.get(url,
        Seq(("accept", "text/event-stream"), (VersionHeader, Mcp.Version)) ++
          session.map(id => (SessionHeader, id)))).flatMap { r =>
        if r.status >= 400 then pure(())      // 405: this server does not push
        else drain(Http.sse(r))
      })

    def lines: Source[String] = Writer.of(inbound)

    /** no more will arrive */
    def close(): Unit = inbound.close()
  }

  /** an endpoint, as a link */
  def link(http: Http, url: String)(using Scheduler): McpLink = McpLink(http, url)

  // ---------------------------------------------------------------- serving

  /**
   * A `Serving` AS a route.
   *
   * Each session is a LINK made of two channels — the same in-memory
   * link the protocol's own tests use — with okay-mcp's server
   * running over it on a fiber. So the route does no protocol work at
   * all: it puts the posted line in, takes the answer out, and the
   * stage that produced it is the one stdio uses.
   *
   * A POST carrying a request answers with its one message; a POST
   * carrying a notification answers 202, because there is nothing to
   * answer. A GET with `accept: text/event-stream` opens the stream a
   * server PUSHES on — which needs a server that can write a body
   * incrementally (okay-jetty can; okay-http's own buffers, and there
   * the GET simply never delivers anything).
   */
  def route(serving: McpServer.Serving)
           (using Scheduler, CanBlock): Request => Response ! Async =
    routed(serving)._1

  /**
   * The route, and the handle for what the server says unasked. Every
   * session gets the same `Pushes`, because a `Serving` is one server
   * — its subscriptions are its own, and a push goes to the sessions
   * that have a stream open.
   */
  def routed(serving: McpServer.Serving)
            (using Scheduler, CanBlock): (Request => Response ! Async, McpServer.Pushes) =
    val sessions = java.util.concurrent.ConcurrentHashMap[String, Wire]()
    val pushes = Channel[Rpc]()
    val handle = McpServer.pushesTo(pushes, serving.subscriptions)

    // one fan-out fiber: a push goes to every session with a stream
    Async.spawn(fanOut(pushes, sessions)): Unit

    val route: Request => Response ! Async = request =>
      if request.method == Method.Get then
        val id = header(request, SessionHeader).getOrElse("")
        Option(sessions.get(id)) match
          case None => pure(Response(404, Nil, Http.one(Array.empty)))
          case Some(wire) =>
            // the body is that session's stream, as events; it ends
            // when the session does
            pure(Response(200, Seq(("content-type", "text/event-stream")),
              events(wire.stream)))
      else
        val body = request.body match
          case Body.Text(s) => s
          case Body.Bytes(b) => String(b.toArray, java.nio.charset.StandardCharsets.UTF_8)
          case Body.Empty => ""
        val id = header(request, SessionHeader)
        val message = Rpc.decode(body)
        val isInit = message match
          case Rpc.Request(_, Mcp.Initialize, _) => true
          case _ => false

        if isInit then
          val fresh = java.util.UUID.randomUUID().toString
          val wire = Wire(serving)
          sessions.put(fresh, wire)
          answer(wire, body, message, Seq((SessionHeader, fresh)))
        else id.flatMap(i => Option(sessions.get(i))) match
          case None if id.isEmpty && sessions.isEmpty =>
            // a client that never initialized, talking to a server
            // that has no sessions: give it one rather than a riddle
            val wire = Wire(serving)
            sessions.put("", wire)
            answer(wire, body, message, Nil)
          case None => pure(Response(404, Nil, Http.one(
            Rpc.encode(Rpc.Failed(Json.JNull, Rpc.InvalidRequest,
              "unknown session")).getBytes(java.nio.charset.StandardCharsets.UTF_8))))
          case Some(wire) => answer(wire, body, message, Nil)

    (route, handle)

  /** every push, to every session that has a stream open */
  private def fanOut(pushes: Channel[Rpc],
                     sessions: java.util.concurrent.ConcurrentHashMap[String, Wire])
  : Unit ! Async =
    async(pushes.receive()).flatMap {
      case None => pure(())
      case Some(m) =>
        import scala.jdk.CollectionConverters.*
        sessions.values().asScala.foreach(_.stream.send(m))
        fanOut(pushes, sessions)
    }

  /** a stream of messages as an SSE body */
  private def events(out: Channel[Rpc]): Source[Chunk[Byte]] =
    def go: Source[Chunk[Byte]] =
      effect[Writer % Chunk[Byte] + Async, Option[Rpc]](Async.Run(() => out.receive()))
        .flatMap {
          case None => pure(())
          case Some(m) =>
            val bytes = s"data: ${Rpc.encode(m)}\n\n"
              .getBytes(java.nio.charset.StandardCharsets.UTF_8)
            effect[Writer % Chunk[Byte] + Async, Unit](
              Writer(scala.collection.immutable.ArraySeq.unsafeWrapArray(bytes)))
              .flatMap(_ => go)
        }

    go

  private def header(r: Request, name: String): Option[String] =
    r.headers.collectFirst { case (k, v) if k.equalsIgnoreCase(name) => v }

  /** one posted message in, its one answer out (or 202: nothing owed) */
  private def answer(wire: Wire, body: String, message: Rpc,
                     extra: Seq[(String, String)]): Response ! Async =
    val owed = message match
      case _: Rpc.Request => true
      case _ => false
    async(wire.inbound.send(body)).flatMap { _ =>
      if !owed then pure(Response(202, extra, Http.one(Array.empty)))
      else async(wire.outbound.receive()).map {
        case Some(line) => Response(200,
          extra :+ ("content-type", "application/json"),
          Http.one(line.getBytes(java.nio.charset.StandardCharsets.UTF_8)))
        case None => Response(500, extra, Http.one(Array.empty))
      }
    }

  /**
   * One session's wire: two channels and okay-mcp's server running
   * over them. Nothing here knows the protocol — that is the whole
   * point of a Link being two functions.
   */
  private final class Wire(serving: McpServer.Serving)(using Scheduler, CanBlock) {
    val inbound: Channel[String] = Channel[String]()
    val outbound: Channel[String] = Channel[String]()

    /** what this session is told unasked — the GET stream reads it */
    val stream: Channel[Rpc] = Channel[Rpc]()

    private val link: okay.mcp.Link = new okay.mcp.Link:
      def send(line: String): Unit ! Async = async(outbound.send(line))
      def lines: Source[String] = Writer.of(inbound)

    Async.spawn(McpServer.run(link, serving)): Unit
  }
}
