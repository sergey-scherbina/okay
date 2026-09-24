package okay.scala2

import okay.{!, %, +, Chunk}
import okay.Row.plus
import okay.codec.Schema
import okay.given
import okay.http.{Method, Request, Urls}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.CompletableFuture

/**
 * HTTP for Scala 2.13 (specs/scala2-facade.md, stage 7).
 *
 * Probed first: okay-http's `Request`, `Method`, `Body`, `Router`,
 * `Http` and `Server` are readable from scalac 2.13, and a 2.13 caller
 * builds and reads a `Request` directly. Two things are not: `Response`
 * (its body is a `Source`, whose row is a union, in its CONSTRUCTOR)
 * and `Route` (Scala 3 generic tuples). So this file supplies a
 * `Response`, routing as Scala 2 extractors, a server and a client —
 * each one a few lines over okay-http's own.
 */
final class Response private (val status: Int, val headers: Seq[(String, String)], private val body: ResponseBody) {

  /** the body, read in full */
  def bytes: Array[Byte] = body.bytes

  def text: String = new String(body.bytes, UTF_8)

  def header(name: String): Option[String] = {
    val n = name.toLowerCase
    headers.collectFirst { case (k, v) if k.toLowerCase == n => v }
  }

  def withHeader(name: String, value: String): Response = new Response(status, headers :+ (name -> value), body)

  def ok: Boolean = status >= 200 && status < 300

  private[scala2] def core: okay.http.Response = body.core(status, headers)
}

/** the body, out of `Response`'s constructor: a stream's type names the
 * union row, and scalac 2.13 refuses a class whose constructor does
 * (see `ProgBody` in Prog.scala) */
private[scala2] final class ResponseBody(val bytes: Array[Byte], val stream: Option[okay.Source[Chunk[Byte]]]) {
  def core(status: Int, headers: Seq[(String, String)]): okay.http.Response =
    okay.http.Response(status, headers, stream.getOrElse(okay.http.Http.one(bytes)))
}

object Response {

  private def of(status: Int, contentType: String, bytes: Array[Byte]): Response =
    new Response(status, Seq("content-type" -> contentType), new ResponseBody(bytes, None))

  def text(body: String, status: Int = 200): Response = of(status, "text/plain; charset=utf-8", body.getBytes(UTF_8))

  def html(body: String, status: Int = 200): Response = of(status, "text/html; charset=utf-8", body.getBytes(UTF_8))

  def bytes(body: Array[Byte], contentType: String, status: Int = 200): Response = of(status, contentType, body)

  /** `a` as JSON, through okay-codec's encoder */
  def json[A](a: A, status: Int = 200)(using s: Schema[A]): Response =
    of(status, "application/json", Json.write(a).getBytes(UTF_8))

  /** a status and nothing else */
  def status(code: Int): Response = new Response(code, Seq.empty, new ResponseBody(Array.emptyByteArray, None))

  val notFound: Response = text("not found", 404)

  /** a body sent as it is produced, one line per element: server-sent
   * events, a log, a model's tokens */
  def lines(src: Source[String], contentType: String = "text/plain; charset=utf-8", status: Int = 200): Response = {
    val chunks = src.map(l => (l + "\n").getBytes(UTF_8)).core
    val stream = okay.Writer.map[Array[Byte], Chunk[Byte], Unit, okay.Async](chunks)(b =>
      scala.collection.immutable.ArraySeq.unsafeWrapArray(b))
    new Response(status, Seq("content-type" -> contentType), new ResponseBody(Array.emptyByteArray, Some(stream)))
  }

  private[scala2] def read(r: okay.http.Response): Eff[Async, Response] =
    Async.lift(okay.http.Http.bytes(r).map(c => new Response(r.status, r.headers, new ResponseBody(c.toArray, None))))
}

/** what a request says, read the way `Route` reads it */
object Requests {

  /** the path's segments, percent-decoded; empty for a malformed escape */
  def path(r: Request): Vector[String] = Urls.segments(r.url).getOrElse(Vector.empty)

  /** the first value of a query parameter */
  def query(r: Request, name: String): Option[String] = queryAll(r, name).headOption

  def queryAll(r: Request, name: String): Vector[String] =
    Urls.params(r.url).flatMap(_.get(name)).getOrElse(Vector.empty)

  def text(r: Request): String = new String(r.body.bytes, UTF_8)

  /** the body decoded as JSON */
  def json[A](r: Request)(using s: Schema[A]): Either[String, A] = Json.read[A](text(r))
}

/**
 * Routing as Scala 2 pattern matching, over a `PartialFunction`:
 *
 * {{{
 * val routes = Routes {
 *   case GET(Path("users", id)) => Async(Response.text("user " + id))
 *   case r @ POST(Path("users")) => ...
 * }
 * }}}
 *
 * A request no case matches answers 404.
 */
object Routes {
  def apply(pf: PartialFunction[Request, Eff[Async, Response]]): Request => Eff[Async, Response] =
    r => pf.applyOrElse(r, (_: Request) => Eff.pure(Response.notFound))
}

/** the path, as segments: `case GET(Path("users", id))` */
object Path {
  def unapplySeq(r: Request): Option[Seq[String]] = Urls.segments(r.url)
}

sealed abstract class MethodMatch(m: Method) {
  def unapply(r: Request): Option[Request] = if (r.method == m) Some(r) else None
}
object GET extends MethodMatch(Method.Get)
object POST extends MethodMatch(Method.Post)
object PUT extends MethodMatch(Method.Put)
object PATCH extends MethodMatch(Method.Patch)
object DELETE extends MethodMatch(Method.Delete)

/** a server that is running; `close` stops it and waits until it has */
final class RunningServer private[scala2] (val port: Int, stop: () => Unit) {
  def close(): Unit = stop()
}

object Server {

  /**
   * Serve `handler` on `port` (0 for any free one) while `body` runs,
   * and stop when it ends, however it ends. `body` receives the port
   * actually bound. It is okay-http's `Server.serve` under okay's
   * `Resource`, so the stop is the same finaliser the Scala 3 API uses.
   */
  def use[A](port: Int)(handler: Request => Eff[Async, Response])(body: Int => Eff[Async, A]): Eff[Async, A] =
    Async.lift(okay.Resource.run[A, okay.Async](
      okay.http.Server.serve(port)(r => Async.core(handler(r)).map(_.core))
        .plus[okay.Async]
        .flatMap(s => Async.core(body(okay.http.Server.port(s))).plus[okay.Resource])))

  /**
   * Start serving and return at once, for a service whose server lives
   * as long as the process. The server runs on its own fiber inside
   * `use`, which waits for `close()`; a failure to bind is thrown here.
   */
  def start(port: Int)(handler: Request => Eff[Async, Response]): RunningServer = {
    val bound = new CompletableFuture[Int]
    val stopped = new CompletableFuture[Unit]
    val closing = new CompletableFuture[Unit]
    val awaitClose: Eff[Async, Unit] = Async.lift(okay.Async.await[Unit] { k =>
      closing.thenRun(() => k(Right(())))
      () => ()
    })
    val serving = use(port)(handler)(p => Async { bound.complete(p); () }.flatMap(_ => awaitClose))
    val runner = new Thread(() => {
      try Eff.runAsync(serving)
      catch { case e: Throwable => bound.completeExceptionally(e); () }
      finally { stopped.complete(()); () }
    }, "okay-scala2-server")
    runner.setDaemon(true)
    runner.start()
    val p = try bound.get() catch { case e: java.util.concurrent.ExecutionException => throw e.getCause }
    new RunningServer(p, () => { closing.complete(()); stopped.get(); () })
  }
}

/** an HTTP client over okay-http's JVM transport */
final class Client private (http: okay.http.Http) {

  /** send, and read the whole body */
  def send(r: Request): Eff[Async, Response] = Async.lift(http.send(r)).flatMap(Response.read)

  def get(url: String): Eff[Async, Response] = send(Request.get(url))

  def post(url: String, body: String, contentType: String = "text/plain; charset=utf-8"): Eff[Async, Response] =
    send(Request.post(url, okay.http.Body.Text(body), Seq("content-type" -> contentType)))

  def postJson[A](url: String, a: A)(using s: Schema[A]): Eff[Async, Response] =
    post(url, Json.write(a), "application/json")

  /** the body as lines, STREAMED: each line arrives as it is read */
  def lines(r: Request): Source[String] =
    Source.of(http.send(r).plus[okay.Writer % String].flatMap(resp => okay.http.Http.lines(resp)))
}

object Client {
  def apply(): Client = new Client(okay.http.Transports.http())
}
