package okay.http

import okay.*
import okay.given

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import scala.scalajs.js.JSConverters.*

/**
 * The JS transports: the global `fetch` and the global `WebSocket`.
 *
 * No scala-js-dom (the dependency rule in specs/http.md), and since
 * typed-js-facades no raw `js.Dynamic` either: the globals are stated
 * once, in types, in okay.Web — the trade the spec said to revisit
 * out loud, revisited.
 *
 * "JS" in this repository means NODE — okay-cluster's JS side uses
 * `require("net")` and `process`. The two APIs used here are the
 * web-standard ones Node provides as globals (`fetch` since 18,
 * `WebSocket` since 22), so a browser build would work too, minus what
 * a browser forbids: custom headers off the forbidden list, and no
 * server at all.
 */
object Transports {

  /**
   * REST over `fetch`, reading the body INCREMENTALLY.
   *
   * `llm.TransportJs` reads the whole body and splits it, and its own
   * comment calls that a temporary simplification whose fix is "the
   * stated next step". This is that step: `response.body.getReader()`
   * is pulled one chunk at a time, so a long response is folded at
   * constant memory here as on the JVM.
   */
  def fetch: Http = new Http:
    def send(r: Request): Response ! Async =
      Async.await[Response] { k =>
        val init = new Web.RequestInit {}
        init.method = r.method.name
        init.headers = js.Dictionary(r.headers*)
        r.body match
          case Body.Empty => ()
          case Body.Text(s) => init.body = s
          case Body.Bytes(b) =>
            init.body = Uint8Array.from(b.map(x => (x & 0xff).toShort).toJSArray)

        Web.Global.fetch(r.url, init).`then`({ (res: Web.Response) =>
          val headers =
            try
              val out = scala.collection.mutable.ListBuffer.empty[(String, String)]
              res.headers.forEach((v, key) => { out += ((key, v)); () })
              out.toSeq
            catch case _: Throwable => Nil
          k(Right(Response(res.status, headers, ofReader(res.body.getReader()))))
          ()
        }: js.Function1[Web.Response, Unit])
          .`catch`({ (e: Any) =>
            k(Left(js.JavaScriptException(e))); ()
          }: js.Function1[Any, Unit | js.Thenable[Unit]])
        () => ()
      }

  /** a ReadableStream reader as a Source, one `read()` per chunk */
  def ofReader(reader: Web.Reader): Source[Chunk[Byte]] =
    type F = Writer % Chunk[Byte] + Async
    def go: Source[Chunk[Byte]] =
      effect[F, Chunk[Byte] | Null](Async.Await[Chunk[Byte] | Null] { k =>
        reader.read().`then`({ (r: Web.ReadResult) =>
          r.value.toOption match
            case Some(v) if !r.done => k(Right(chunkOf(v)))
            case _ => k(Right(null))
          ()
        }: js.Function1[Web.ReadResult, Unit])
        () => ()
      }).flatMap {
        case null => pure(())
        case c: Chunk[Byte] @unchecked =>
          effect[F, Unit](Writer(c)).flatMap(_ => go)
      }

    go

  private def chunkOf(a: Uint8Array): Chunk[Byte] =
    val out = new Array[Byte](a.length)
    var i = 0
    while i < a.length do { out(i) = a(i).toByte; i += 1 }
    scala.collection.immutable.ArraySeq.unsafeWrapArray(out)

  /**
   * WebSocket over the global `WebSocket`.
   *
   * The asymmetry the spec names, in code: this API is PUSH — messages
   * arrive whether or not anyone is reading, and there is no
   * receive-side lever of any kind (only `bufferedAmount`, which is
   * about sending). So the frames land in a bounded `Channel` and the
   * bound is stated rather than hidden: past it the channel fails,
   * which surfaces as a failed source instead of memory growing until
   * the process dies.
   *
   * This is the same socket-to-`Channel` adaptation `cluster.Remote`
   * uses on the JVM; what differs is only who applies the brakes.
   */
  def sockets(capacity: Int = 1024): Sockets = new Sockets:
    def connect(url: String, headers: Seq[(String, String)],
                subprotocols: Seq[String]): Socket ! Async =
      Async.await[Socket] { k =>
        // the browser constructor takes no headers at all; Node's
        // takes an options object. Passing them is best-effort and
        // documented as such — see specs/http.md, Out of scope.
        val ws =
          if subprotocols.isEmpty then new Web.WebSocket(url)
          else new Web.WebSocket(url, subprotocols.toJSArray)
        ws.binaryType = "arraybuffer"

        val q = new Channel[Frame](capacity)

        ws.onmessage = (e: Web.MessageEvent) =>
          // a text frame is a string, a binary one an ArrayBuffer: a
          // type TEST, not a cast
          val f = e.data match
            case s: String => Frame.Text(s)
            case buf: scala.scalajs.js.typedarray.ArrayBuffer => Frame.Binary(chunkOf(new Uint8Array(buf)))
            case other => Frame.Text(other.toString)
          q.sendAsync(f)(_ => ())

        ws.onclose = (e: Web.CloseEvent) =>
          q.sendAsync(Frame.Close(e.code, e.reason))(_ => ())
          q.close()

        ws.onerror = (_: js.Any) => q.fail(RuntimeException(s"websocket error: $url"))

        ws.onopen = (_: js.Any) => k(Right(of(ws, q)))

        () => ws.close()
      }

  private def of(ws: Web.WebSocket, q: Channel[Frame]): Socket = new Socket:
    def send(f: Frame): Unit ! Async = async {
      f match
        case Frame.Text(s) => ws.send(s)
        case Frame.Binary(b) =>
          ws.send(Uint8Array.from(b.map(x => (x & 0xff).toShort).toJSArray))
        // a browser exposes neither ping nor pong; Node's WebSocket
        // does not either through the web-standard surface. Dropping
        // them is honest — the alternative is pretending they went.
        case Frame.Ping(_) | Frame.Pong(_) => ()
        case Frame.Close(c, r) => ws.close(c, r)
      ()
    }

    def frames: Source[Frame] = Writer.of(q)

    def close(code: Int, reason: String): Unit ! Async =
      async { ws.close(code, reason); () }
}
