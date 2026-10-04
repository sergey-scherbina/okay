package okay2.http

import scala.collection.immutable.ArraySeq
import scala.concurrent.ExecutionContext
import scala.scalajs.js
import scala.scalajs.js.typedarray.{ArrayBuffer, Uint8Array}
import scala.util.{Failure, Success}
import okay2.{!, Writer, pure}
import okay2.async.Async
import okay2.platform.Web
import okay2.stream.{Channel, Chunk, Source}

/**
 * The JS transports (okay-http's scala-js Transports.scala): the global
 * `fetch` and the global `WebSocket`, through the typed facades in
 * okay2-platform's `Web` — no scala-js-dom, no `js.Dynamic`.
 *
 * "JS" here means NODE, as in okay2-platform (`NodeNet`): the two APIs
 * are the web-standard ones Node provides as globals (`fetch` since 18,
 * `WebSocket` since 22), so a browser build works too, minus what a
 * browser forbids (custom headers off the forbidden list, any server).
 */
object Transports {

  /** a promise as a callback, on whatever turn of the loop settles it */
  private def settle[A](p: js.Promise[A])(k: Either[Throwable, A] => Unit): Unit =
    p.toFuture.onComplete {
      case Success(a) => k(Right(a))
      case Failure(e) => k(Left(e))
    }(ExecutionContext.parasitic)

  /**
   * REST over `fetch`, reading the body INCREMENTALLY: the response's
   * `body.getReader()` is pulled one chunk at a time, so a long response
   * is folded at constant memory here as on the JVM.
   */
  def fetch: Http = new Http {
    def send(r: Request): Response ! Async =
      Async.await[Response] { k =>
        val init = new Web.RequestInit {}
        init.method = r.method.name
        init.headers = js.Dictionary(r.headers: _*)
        r.body match {
          case Body.Empty => ()
          case Body.Text(s) => init.body = (s: js.Any)
          case Body.Bytes(b) => init.body = uint8(b)
        }
        settle(Web.Global.fetch(r.url, init)) {
          case Right(res) =>
            val headers = {
              val out = scala.collection.mutable.ListBuffer.empty[(String, String)]
              res.headers.forEach((v: String, key: String) => { out += ((key, v)); () })
              out.toList
            }
            k(Right(Response(res.status, headers, ofReader(res.body.getReader()))))
          case Left(e) => k(Left(e))
        }
        () => ()
      }
  }

  /** a ReadableStream reader as a Source, one `read()` per chunk */
  def ofReader(reader: Web.Reader): Source[Chunk[Byte]] = {
    def go: Source[Chunk[Byte]] =
      Async.await[Option[Chunk[Byte]]] { k =>
        settle(reader.read())(r => k(r.map(x => if (x.done) None else x.value.toOption.map(chunkOf))))
        () => ()
      }.flatMap[Writer[Chunk[Byte]] with Async, Unit] {
        case None => pure(())
        case Some(c) => Writer.tell[Chunk[Byte]](c).flatMap(_ => go)
      }
    go
  }

  private def chunkOf(a: Uint8Array): Chunk[Byte] = {
    val out = new Array[Byte](a.length)
    var i = 0
    while (i < a.length) { out(i) = a(i).toByte; i += 1 }
    ArraySeq.unsafeWrapArray(out)
  }

  private def uint8(b: Chunk[Byte]): Uint8Array = {
    val out = new Uint8Array(b.length)
    var i = 0
    while (i < b.length) { out(i) = (b(i) & 0xff).toShort; i += 1 }
    out
  }

  /**
   * WebSocket over the global `WebSocket`.
   *
   * This API is PUSH: messages arrive whether or not anyone is reading,
   * and there is no receive-side lever (only `bufferedAmount`, which is
   * about sending). So the frames land in a `Channel` of `capacity`, the
   * bound stated rather than hidden. The same socket-to-Channel
   * adaptation as the JVM transport; what differs is who brakes.
   */
  def sockets(capacity: Int = 1024): Sockets = new Sockets {
    def connect(url: String, headers: Seq[(String, String)], subprotocols: Seq[String]): Socket ! Async =
      Async.await[Socket] { k =>
        // the browser constructor takes no headers at all; Node's takes
        // an options object. `headers` are therefore not sent — the
        // Scala 3 transport's documented gap (specs/http.md, Out of scope)
        val ws =
          if (subprotocols.isEmpty) new Web.WebSocket(url)
          else new Web.WebSocket(url, js.Array(subprotocols: _*))
        ws.binaryType = "arraybuffer"

        val q = Channel[Frame](capacity)
        // one thread: a plain flag says whether `k` has been answered
        var opened = false

        ws.onmessage = (e: Web.MessageEvent) => {
          // a text frame is a string, a binary one an ArrayBuffer: a type
          // TEST, not a cast
          val f = e.data match {
            case s: String => Frame.Text(s)
            case buf: ArrayBuffer => Frame.Binary(chunkOf(new Uint8Array(buf)))
            case other => Frame.Text(other.toString)
          }
          q.sendAsync(f)(_ => ())
        }

        ws.onclose = (e: Web.CloseEvent) => {
          q.sendAsync(Frame.Close(e.code, e.reason))(_ => ())
          q.close()
        }

        ws.onerror = (_: js.Any) => {
          val e = new RuntimeException(s"websocket error: $url")
          q.fail(e)
          // an error before the open is a failed connect, not a hang
          if (!opened) { opened = true; k(Left(e)) }
        }

        ws.onopen = (_: js.Any) => if (!opened) { opened = true; k(Right(of(ws, q))) }

        () => ws.close()
      }
  }

  private def of(ws: Web.WebSocket, q: Channel[Frame]): Socket = new Socket {
    def send(f: Frame): Unit ! Async = Async {
      f match {
        case Frame.Text(s) => ws.send(s)
        case Frame.Binary(b) => ws.send(uint8(b))
        // the web-standard surface exposes neither ping nor pong, in a
        // browser or in Node: dropping them is honest, the alternative
        // is pretending they went
        case Frame.Ping(_) | Frame.Pong(_) => ()
        case Frame.Close(c, r) => ws.close(c, r)
      }
    }

    def frames: Source[Frame] = Channel.ChannelOps(q).drained

    def close(code: Int, reason: String): Unit ! Async = Async(ws.close(code, reason))
  }
}
