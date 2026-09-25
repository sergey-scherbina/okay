package okay2.http

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse, WebSocket}
import java.nio.ByteBuffer
import java.util.concurrent.CompletionStage
import scala.collection.immutable.ArraySeq
import scala.jdk.CollectionConverters._
import okay2.{!, Writer, pure}
import okay2.async.Async
import okay2.stream.{Channel, Chunk, Source}

/** the JDK's HTTP client and WebSocket behind `Http` and `Sockets`
 * (okay-http's scala-jvm Transports.scala): a response body read as a
 * chunked source straight off the wire, never materialized */
object Transports {

  def client(): HttpClient = HttpClient.newBuilder().followRedirects(HttpClient.Redirect.NORMAL).build()

  def http(c: HttpClient = client()): Http = new Http {
    def send(r: Request): Response ! Async =
      Async {
        val b = HttpRequest.newBuilder(URI.create(r.url))
        r.headers.foreach { case (k, v) => b.header(k, v) }
        val pub =
          if (r.body == Body.Empty) HttpRequest.BodyPublishers.noBody()
          else HttpRequest.BodyPublishers.ofByteArray(r.body.bytes)
        b.method(r.method.name, pub)
        c.send(b.build(), HttpResponse.BodyHandlers.ofInputStream())
      }.map { res =>
        val hs = res.headers().map().asScala.toSeq.flatMap { case (k, vs) => vs.asScala.map(v => (k, v)) }
        Response(res.statusCode(), hs, ofInputStream(res.body()), Async(res.body().close()))
      }
  }

  /** a stream read `size` bytes at a time; closed at its end */
  def ofInputStream(in: java.io.InputStream, size: Int = 8192): Source[Chunk[Byte]] = {
    def go: Source[Chunk[Byte]] =
      Async[Option[Chunk[Byte]]] {
        val buf = new Array[Byte](size)
        val n = in.read(buf)
        if (n < 0) { in.close(); None }
        else Some(ArraySeq.unsafeWrapArray(if (n == size) buf else java.util.Arrays.copyOf(buf, n)))
      }.flatMap[Writer[Chunk[Byte]] with Async, Unit] {
        case None => pure(())
        case Some(c) => Writer.tell[Chunk[Byte]](c).flatMap(_ => go)
      }
    go
  }

  def sockets(c: HttpClient = client()): Sockets = new Sockets {
    def connect(url: String, headers: Seq[(String, String)], subprotocols: Seq[String]): Socket ! Async =
      Async.await[Socket] { k =>
        val q = Channel[Frame](Int.MaxValue)
        val text = new StringBuilder
        val bin = scala.collection.mutable.ArrayBuilder.make[Byte]

        val listener = new WebSocket.Listener {
          override def onOpen(ws: WebSocket): Unit = ws.request(1)

          override def onText(ws: WebSocket, data: CharSequence, last: Boolean): CompletionStage[_] = {
            text.append(data)
            if (last) { q.offer(Frame.Text(text.toString)): Unit; text.clear() }
            ws.request(1)
            null
          }

          override def onBinary(ws: WebSocket, data: ByteBuffer, last: Boolean): CompletionStage[_] = {
            val a = new Array[Byte](data.remaining()); data.get(a)
            bin ++= a
            if (last) {
              q.offer(Frame.Binary(ArraySeq.unsafeWrapArray(bin.result()))): Unit
              bin.clear()
            }
            ws.request(1)
            null
          }

          override def onPing(ws: WebSocket, m: ByteBuffer): CompletionStage[_] = {
            q.offer(Frame.Ping(bytesOf(m))): Unit
            ws.request(1)
            null
          }

          override def onPong(ws: WebSocket, m: ByteBuffer): CompletionStage[_] = {
            q.offer(Frame.Pong(bytesOf(m))): Unit
            ws.request(1)
            null
          }

          override def onClose(ws: WebSocket, code: Int, reason: String): CompletionStage[_] = {
            q.offer(Frame.Close(code, reason)): Unit
            q.close()
            null
          }

          override def onError(ws: WebSocket, e: Throwable): Unit = q.fail(e)
        }

        val b = c.newWebSocketBuilder()
        headers.foreach { case (h, v) => b.header(h, v) }
        if (subprotocols.nonEmpty) b.subprotocols(subprotocols.head, subprotocols.tail: _*): Unit
        b.buildAsync(URI.create(url), listener).whenComplete { (ws, err) =>
          if (err != null) k(Left(err)) else k(Right(of(ws, q)))
        }: Unit
        () => ()
      }
  }

  private def bytesOf(b: ByteBuffer): Chunk[Byte] = {
    val a = new Array[Byte](b.remaining()); b.get(a)
    ArraySeq.unsafeWrapArray(a)
  }

  private def of(ws: WebSocket, q: Channel[Frame]): Socket = new Socket {
    private val closed = new java.util.concurrent.atomic.AtomicBoolean(false)

    def send(f: Frame): Unit ! Async = Async.await[Unit] { k =>
      val done: java.util.concurrent.CompletableFuture[WebSocket] = f match {
        case Frame.Text(s) => ws.sendText(s, true)
        case Frame.Binary(b) => ws.sendBinary(ByteBuffer.wrap(b.toArray), true)
        case Frame.Ping(b) => ws.sendPing(ByteBuffer.wrap(b.toArray))
        case Frame.Pong(b) => ws.sendPong(ByteBuffer.wrap(b.toArray))
        case Frame.Close(code, r) =>
          if (closed.compareAndSet(false, true)) ws.sendClose(code, r)
          else java.util.concurrent.CompletableFuture.completedFuture(ws)
      }
      done.whenComplete((_, e) => if (e != null) k(Left(e)) else k(Right(()))): Unit
      () => ()
    }

    def frames: Source[Frame] = Channel.ChannelOps(q).drained

    def close(code: Int, reason: String): Unit ! Async = send(Frame.Close(code, reason))
  }
}
