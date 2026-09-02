package okay.jetty

import okay.*
import okay.given
import okay.http.{Body, Frame, Http, Method, Request, Response, Socket, Sockets}

import org.eclipse.jetty.client.{HttpClient as JettyClient, InputStreamResponseListener, Request as JRequest}
import org.eclipse.jetty.server.{Handler, Server, ServerConnector, Request as JsrRequest, Response as JsrResponse}
import org.eclipse.jetty.util.Callback
import org.eclipse.jetty.websocket.api.{Callback as WsCallback, Session}
import org.eclipse.jetty.websocket.server.{ServerUpgradeRequest, ServerUpgradeResponse, WebSocketUpgradeHandler}

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8
import scala.jdk.CollectionConverters.*

/**
 * Jetty behind okay-http's two seams — and one thing neither the JDK
 * nor `okay-http` can do.
 *
 * `specs/http.md` put serving WebSocket out of scope rather than
 * half-build it: the JDK has no server-side WebSocket API at all, and
 * `com.sun.net.httpserver` will not surrender its socket. That is the
 * gap this module exists to close, and it closes it with the session
 * type that already existed — a server session is the same
 * `Stage[Frame, Frame, Unit]` a client writes, so an echo written for
 * a test runs on either end unchanged.
 *
 * Everything owns threads, so everything is a `Resource`. A module
 * that hands back a bare client hands back a leak with instructions.
 */
object Jetty {

  // ---- the client seam

  /** REST over Jetty's client, streaming the body */
  def http(): Http ! Resource =
    Resource.acquire {
      val c = JettyClient()
      c.start()
      c
    }(_.stop()).map(of)

  /** an already-configured client, for a caller who needs its builder */
  def of(c: JettyClient): Http = new Http:
    def send(r: Request): Response ! Async =
      effect[Async, (Int, Seq[(String, String)], java.io.InputStream)](Async.Run { () =>
        val req: JRequest = c.newRequest(r.url).method(r.method.name)
        r.headers.foreach((k, v) => req.headers(h => { h.add(k, v); () }))
        r.body match
          case Body.Empty => ()
          case b => req.body(new org.eclipse.jetty.client.BytesRequestContent(b.bytes))
        val listener = InputStreamResponseListener()
        req.send(listener)
        // the HEAD only — the body stays on the wire until consumed,
        // which is the whole point of Response.body being a Source
        val res = listener.get(60, java.util.concurrent.TimeUnit.SECONDS)
        val hs = res.getHeaders.asScala.toSeq.map(f => (f.getName, f.getValue))
        (res.getStatus, hs, listener.getInputStream)
      }).map((status, hs, in) =>
        Response(status, hs, okay.http.Transports.ofInputStream(in)))

  // ---- the WebSocket client seam

  def sockets(): Sockets ! Resource =
    Resource.acquire {
      val c = org.eclipse.jetty.websocket.client.WebSocketClient()
      c.start()
      c
    }(_.stop()).map { c =>
      new Sockets:
        def connect(url: String, headers: Seq[(String, String)],
                    subprotocols: Seq[String]): Socket ! Async =
          Async.await[Socket] { k =>
            val q = new Channel[Frame](Int.MaxValue)
            val listener = adapter(q)
            val upgrade = org.eclipse.jetty.websocket.client.ClientUpgradeRequest()
            headers.foreach((h, v) => upgrade.setHeader(h, v))
            if subprotocols.nonEmpty then
              upgrade.setSubProtocols(subprotocols.asJava)
            c.connect(listener, java.net.URI.create(url), upgrade)
              .whenComplete { (s, e) =>
                if e != null then k(Left(e)) else k(Right(socket(s, q)))
              }
            () => ()
          }
    }

  // ---- the server, including the half okay-http could not serve

  /**
   * A server for both: REST routes and WebSocket sessions, dispatched
   * by the same `Request` the client speaks.
   *
   * A WebSocket route answers a `Stage[Frame, Frame, Unit]` — the
   * session — and it is the same type `Ws.over` runs on a client. That
   * is what makes an echo session portable between the two ends.
   */
  def serve(port: Int)(routes: PartialFunction[Request, Response ! Async])
           (ws: PartialFunction[Request, okay.Stage[Frame, Frame, Unit]] =
              PartialFunction.empty)
           (using CanBlock, Scheduler): Server ! Resource =
    Resource.acquire {
      val server = Server()
      val connector = ServerConnector(server)
      connector.setPort(port)
      server.addConnector(connector)

      val rest = new Handler.Abstract:
        def handle(req: JsrRequest, res: JsrResponse, cb: Callback): Boolean =
          val r = posted(requestOf(req), req)
          if !routes.isDefinedAt(r) then false
          else
            try
              val out = Async.run[Response, Pure](routes(r)).runWith
              res.setStatus(out.status)
              out.headers.foreach((k, v) => res.getHeaders.add(k, v))
              if streams(out) then stream(out, res, cb) else
                val bytes = Async.run[Chunk[Byte], Pure](Http.bytes(out)).runWith.toArray
                res.write(true, ByteBuffer.wrap(bytes), cb)
            catch
              case e: Throwable =>
                // damage as data, on the wire too — the same 500 the
                // built-in server gives
                val m = Option(e.getMessage).getOrElse(e.getClass.getName)
                res.setStatus(500)
                res.write(true, ByteBuffer.wrap(m.getBytes(UTF_8)), cb)
            true

      val upgrade = WebSocketUpgradeHandler.from(server,
        (container: org.eclipse.jetty.websocket.server.ServerWebSocketContainer) => {
          container.addMapping("/*",
            (req: ServerUpgradeRequest, _: ServerUpgradeResponse, _: Callback) => {
              val r = requestOf(req)
              if ws.isDefinedAt(r) then session(ws(r)) else null
            })
          ()
        })
      upgrade.setHandler(rest)
      server.setHandler(upgrade)
      server.start()
      server
    }(_.stop())

  /** the port a server bound to — useful when 0 asked for any free one */
  def port(s: Server): Int = s.getConnectors.head match
    case c: ServerConnector => c.getLocalPort
    case other => throw IllegalStateException(s"not a server connector: $other")

  // ---- the two directions of a session, over Jetty's own listener

  /**
   * The request WITH its body, for the REST side.
   *
   * `requestOf` deliberately does not read one: it also builds the
   * request a WebSocket upgrade is dispatched on, where there is no
   * body to read and reading would consume the upgrade. On the REST
   * side there is one, and a route that never sees it cannot serve a
   * POST at all — which is how this was found: an MCP route over
   * Jetty answered every message as though it were damaged, because
   * every body arrived empty.
   */
  private def posted(r: Request, req: org.eclipse.jetty.server.Request): Request =
    if r.method == Method.Get || r.method == Method.Head then r
    else
      val bb = org.eclipse.jetty.io.Content.Source.asByteBuffer(req)
      if !bb.hasRemaining then r
      else
        val bytes = new Array[Byte](bb.remaining)
        bb.get(bytes)
        r.copy(body = Body.Bytes(scala.collection.immutable.ArraySeq.unsafeWrapArray(bytes)))

  private def requestOf(req: org.eclipse.jetty.server.Request): Request =
    val hs = req.getHeaders.asScala.toSeq.map(f => (f.getName, f.getValue))
    val m = Method.values.find(_.name == req.getMethod).getOrElse(Method.Get)
    Request(m, org.eclipse.jetty.server.Request.getPathInContext(req), hs)

  /**
   * A Jetty session, told into a channel — the same shape the JDK and
   * the browser transports produce.
   *
   * `Listen` next door is Java, and its own comment says why: Jetty
   * picks callbacks by reflection and refuses a class declaring both
   * text forms, which is exactly what Scala's mixin forwarders produce.
   */
  /**
   * Does this response mean a STREAM?
   *
   * `Response.body` is a `Source[Chunk[Byte]]` and always was, so the
   * shape allowed streaming from the start; what buffering decided was
   * that nobody could use it. Draining first is right for REST — one
   * length, one write — and fatal for server-sent events, where the
   * whole point is that the caller sees an event before the source
   * ends. So the content type decides, because it is exactly the
   * place a caller says which one they meant.
   */
  private def streams(r: Response): Boolean =
    r.headers.exists((k, v) =>
      k.equalsIgnoreCase("content-type") && v.contains("text/event-stream"))

  /**
   * Write the body chunk by chunk, on a virtual thread: each pull of
   * the source may park (a channel with nothing in it yet), and the
   * last chunk is the one that closes the response. A source that
   * never ends is a stream that stays open, which is what a
   * subscription is.
   */
  private def stream(out: Response, res: JsrResponse, cb: Callback)
                    (using CanBlock): Unit =
    Thread.startVirtualThread: () =>
      try
        Async.run[Unit, Pure](write(out.body, res)).runWith
        res.write(true, ByteBuffer.allocate(0), cb)
      catch case _: Throwable => res.write(true, ByteBuffer.allocate(0), cb)
    ()

  private def write(body: okay.Source[Chunk[Byte]], res: JsrResponse): Unit ! Async =
    Writer.uncons[Chunk[Byte], Unit, Async](body).flatMap {
      case Left(_) => pure(())
      case Right((c, rest)) =>
        Async.await[Unit] { k =>
          res.write(false, ByteBuffer.wrap(c.toArray), new Callback {
            override def succeeded(): Unit = k(Right(()))
            override def failed(e: Throwable): Unit = k(Left(e))
          })
          () => ()
        }.flatMap(_ => write(rest, res))
    }

  private def adapter(q: Channel[Frame]): Listen =
    Listen(new Listen.Sink:
      def open(session: Session): Unit = ()
      def text(message: String): Unit = q.offer(Frame.Text(message)): Unit
      def binary(payload: Array[Byte]): Unit =
        q.offer(Frame.Binary(scala.collection.immutable.ArraySeq.unsafeWrapArray(payload))): Unit
      def closed(code: Int, reason: String): Unit =
        q.offer(Frame.Close(code, reason)): Unit
        q.close()
      def failed(cause: Throwable): Unit = q.fail(cause))

  private def socket(s: Session, q: Channel[Frame]): Socket = new Socket:
    private val closed = java.util.concurrent.atomic.AtomicBoolean(false)

    def send(f: Frame): Unit ! Async = Async.await[Unit] { k =>
      val cb = new WsCallback:
        override def succeed(): Unit = k(Right(()))
        override def fail(e: Throwable): Unit = k(Left(e))
      f match
        case Frame.Text(t) => s.sendText(t, cb)
        case Frame.Binary(b) => s.sendBinary(ByteBuffer.wrap(b.toArray), cb)
        case Frame.Ping(b) => s.sendPing(ByteBuffer.wrap(b.toArray), cb)
        case Frame.Pong(b) => s.sendPong(ByteBuffer.wrap(b.toArray), cb)
        case Frame.Close(c, r) =>
          if closed.compareAndSet(false, true) then s.close(c, r, cb)
          else cb.succeed()
      () => ()
    }

    def frames: Source[Frame] = Writer.of(q)

    def close(code: Int, reason: String): Unit ! Async =
      send(Frame.Close(code, reason))

  /**
   * A server-side session: run the caller's `Stage` over the socket, on
   * its own fiber, for as long as the socket lives.
   *
   * The session is the SAME `Stage[Frame, Frame, Unit]` a client runs.
   * That is the whole point of this module: `specs/http.md` could not
   * serve WebSocket, and closing that gap should not have introduced a
   * second session type.
   */
  private def session(stage: okay.Stage[Frame, Frame, Unit])
                     (using Scheduler): Listen =
    val q = new Channel[Frame](Int.MaxValue)
    Listen(new Listen.Sink:
      def open(s: Session): Unit =
        Async.spawn(okay.http.Ws.over(socket(s, q))(stage)): Unit
      def text(message: String): Unit = q.offer(Frame.Text(message)): Unit
      def binary(payload: Array[Byte]): Unit =
        q.offer(Frame.Binary(scala.collection.immutable.ArraySeq.unsafeWrapArray(payload))): Unit
      def closed(code: Int, reason: String): Unit =
        q.offer(Frame.Close(code, reason)): Unit
        q.close()
      def failed(cause: Throwable): Unit = q.fail(cause))
}
