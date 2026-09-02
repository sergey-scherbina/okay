package okay.http

import okay.*
import okay.given

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse, WebSocket}
import java.nio.ByteBuffer
import java.util.concurrent.CompletionStage
import scala.jdk.CollectionConverters.*

/**
 * The JVM transports: `java.net.http` for both halves.
 *
 * The seam itself is platform-free; this is one implementation of it
 * and the JS side has its own over `fetch` and the global `WebSocket`.
 * The split follows okay-llm exactly — a `Transports` object per
 * platform, named so because a companion cannot live in another file.
 */
object Transports {

  /** the default client: HTTP/2 where offered, redirects followed */
  def client(): HttpClient =
    HttpClient.newBuilder()
      .followRedirects(HttpClient.Redirect.NORMAL)
      .build()

  /**
   * REST over `java.net.http.HttpClient`, streaming the body.
   *
   * `ofInputStream` rather than `ofByteArray`: the head comes back as
   * soon as it arrives and the body is read only when someone consumes
   * the `Source`. A virtual thread parks on each read, which is the
   * same trade every interop module in this repository makes.
   */
  def http(c: HttpClient = client()): Http = new Http:
    def send(r: Request): Response ! Async =
      effect[Async, HttpResponse[java.io.InputStream]](Async.Run { () =>
        val b = HttpRequest.newBuilder(URI.create(r.url))
        r.headers.foreach((k, v) => b.header(k, v))
        val pub =
          if r.body == Body.Empty then HttpRequest.BodyPublishers.noBody()
          else HttpRequest.BodyPublishers.ofByteArray(r.body.bytes)
        b.method(r.method.name, pub)
        c.send(b.build(), HttpResponse.BodyHandlers.ofInputStream())
      }).map { res =>
        val hs = res.headers().map().asScala.toSeq
          .flatMap((k, vs) => vs.asScala.map(v => (k, v)))
        Response(res.statusCode(), hs, ofInputStream(res.body()),
          async { res.body().close() })
      }

  /**
   * A byte stream as a `Source`, one buffer at a time.
   *
   * The JDK documents that an unconsumed `ofInputStream` body can keep
   * an `HttpClient` from shutting down cleanly, so the stream is closed
   * when it ends — and a caller who abandons the source should close it
   * through `Resource`, which is what `Http.drain` below is for.
   */
  def ofInputStream(in: java.io.InputStream, size: Int = 8192): Source[Chunk[Byte]] =
    type F = Writer % Chunk[Byte] + Async
    def go: Source[Chunk[Byte]] =
      effect[F, Chunk[Byte] | Null](Async.Run { () =>
        val buf = new Array[Byte](size)
        val n = in.read(buf)
        if n < 0 then { in.close(); null }
        else scala.collection.immutable.ArraySeq.unsafeWrapArray(
          if n == size then buf else java.util.Arrays.copyOf(buf, n))
      }).flatMap {
        case null => pure(())
        case c =>
          effect[F, Unit](Writer(c)).flatMap(_ => go)
      }

    go

  /**
   * WebSocket over `java.net.http.WebSocket`.
   *
   * The whole of the interesting part is demand. The JDK's listener is
   * pull-based: a per-socket counter starts at zero, `request(n)` adds
   * to it, every receive call subtracts one, and at zero the socket
   * stops calling — real flow control, down to TCP. That is exactly
   * what a pull-based stream wants, and it is exactly what the JS side
   * cannot offer, which is why `request` appears nowhere in `Socket`
   * and why this transport spends its own demand: one `request(1)` per
   * frame handed on, so the wire is paced by the consumer.
   *
   * Fragmentation is joined here too — the JDK's `last` flag is the
   * only thing that says a long message is over, and a session should
   * never have to know that.
   */
  def sockets(c: HttpClient = client()): Sockets = new Sockets:
    def connect(url: String, headers: Seq[(String, String)],
                subprotocols: Seq[String]): Socket ! Async =
      Async.await[Socket] { k =>
        val q = new Channel[Frame](Int.MaxValue)
        val text = new StringBuilder
        val bin = scala.collection.mutable.ArrayBuilder.make[Byte]

        val listener = new WebSocket.Listener:
          override def onOpen(ws: WebSocket): Unit = ws.request(1)

          override def onText(ws: WebSocket, data: CharSequence,
                              last: Boolean): CompletionStage[?] =
            text.append(data)
            if last then { q.offer(Frame.Text(text.toString)): Unit; text.clear() }
            ws.request(1)
            null

          override def onBinary(ws: WebSocket, data: ByteBuffer,
                                last: Boolean): CompletionStage[?] =
            val a = new Array[Byte](data.remaining()); data.get(a)
            bin ++= a
            if last then
              q.offer(Frame.Binary(
                scala.collection.immutable.ArraySeq.unsafeWrapArray(bin.result()))): Unit
              bin.clear()
            ws.request(1)
            null

          override def onPing(ws: WebSocket, m: ByteBuffer): CompletionStage[?] =
            // the JDK sends the pong itself; the session is only told
            q.offer(Frame.Ping(bytesOf(m))): Unit
            ws.request(1)
            null

          override def onPong(ws: WebSocket, m: ByteBuffer): CompletionStage[?] =
            q.offer(Frame.Pong(bytesOf(m))): Unit
            ws.request(1)
            null

          override def onClose(ws: WebSocket, code: Int, reason: String)
          : CompletionStage[?] =
            q.offer(Frame.Close(code, reason)): Unit
            q.close()
            null

          override def onError(ws: WebSocket, e: Throwable): Unit = q.fail(e)

        val b = c.newWebSocketBuilder()
        headers.foreach((h, v) => b.header(h, v))
        if subprotocols.nonEmpty then
          b.subprotocols(subprotocols.head, subprotocols.tail*): Unit
        b.buildAsync(URI.create(url), listener).whenComplete { (ws, err) =>
          if err != null then k(Left(err)) else k(Right(of(ws, q)))
        }
        () => ()
      }

  private def bytesOf(b: ByteBuffer): Chunk[Byte] =
    val a = new Array[Byte](b.remaining()); b.get(a)
    scala.collection.immutable.ArraySeq.unsafeWrapArray(a)

  /** the socket, once the handshake is done */
  private def of(ws: WebSocket, q: Channel[Frame]): Socket = new Socket:
    // a session may close itself and the caller may close after it;
    // the second one is a no-op rather than an "Output closed"
    private val closed = java.util.concurrent.atomic.AtomicBoolean(false)

    def send(f: Frame): Unit ! Async = Async.await[Unit] { k =>
      val done: java.util.concurrent.CompletableFuture[WebSocket] = f match
        case Frame.Text(s) => ws.sendText(s, true)
        case Frame.Binary(b) => ws.sendBinary(ByteBuffer.wrap(b.toArray), true)
        case Frame.Ping(b) => ws.sendPing(ByteBuffer.wrap(b.toArray))
        case Frame.Pong(b) => ws.sendPong(ByteBuffer.wrap(b.toArray))
        case Frame.Close(c, r) =>
          if closed.compareAndSet(false, true) then ws.sendClose(c, r)
          else java.util.concurrent.CompletableFuture.completedFuture(ws)
      done.whenComplete((_, e) => if e != null then k(Left(e)) else k(Right(())))
      () => ()
    }

    /** the frames, as a source — the channel is the queue the listener
     * fills, and `Writer.of` tells it out again */
    def frames: Source[Frame] = Writer.of(q)

    /**
     * Half-duplex, as RFC 6455 has it: sending Close closes the write
     * half only, and frames already in flight still arrive until the
     * peer's Close ends the stream. That is the same drain-then-close
     * discipline `Channel` already has.
     */
    def close(code: Int, reason: String): Unit ! Async =
      send(Frame.Close(code, reason))
}
