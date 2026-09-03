package okay.http

import okay.*
import okay.given

import com.sun.net.httpserver.{HttpExchange, HttpServer as JdkServer}
import java.net.InetSocketAddress
import scala.jdk.CollectionConverters.*

/**
 * A REST server, JVM only — there is none in a browser and none in
 * Node's standard globals, so this deliberately does not appear in the
 * shared surface.
 *
 * `com.sun.net.httpserver` despite the package name: JEP 403 kept the
 * `jdk.httpserver` module exported precisely because this is a
 * supported API, and it adds no dependency, which is the rule this
 * repository holds to. Its limits are accepted rather than papered
 * over — HTTP/1.1 only, and no WebSocket upgrade, which is why serving
 * WebSocket is out of scope in specs/http.md instead of half-built.
 *
 * A route is `Request => Response ! Async`: the same two types the
 * client speaks, so a handler written for a test is a handler in
 * production, and a client and a server in one program share their
 * vocabulary rather than mirroring it.
 */
object Server {

  /** a running server; `Resource.run` stops it */
  def serve(port: Int)(route: Request => Response ! Async)
           (using CanBlock): JdkServer ! Resource =
    Resource.acquire {
      val s = JdkServer.create(InetSocketAddress(port), 0)
      s.setExecutor(java.util.concurrent.Executors.newVirtualThreadPerTaskExecutor())
      s.createContext("/", (x: HttpExchange) => handle(x, route))
      s.start()
      s
    }(_.stop(0))

  /** the port a server bound to — useful when 0 asked for any free one */
  def port(s: JdkServer): Int = s.getAddress.getPort

  /** the sender's HOST, without the port (http-peer-address): a port
   * changes per connection, and keying anything on one hands every
   * connection a fresh budget */
  private def hostOf(a: java.net.SocketAddress): Option[String] = a match
    case i: java.net.InetSocketAddress =>
      Option(i.getAddress).map(_.getHostAddress).orElse(Option(i.getHostString))
    case null => None
    case other => Option(other.toString).filter(_.nonEmpty)

  private def handle(x: HttpExchange, route: Request => Response ! Async)
                    (using CanBlock): Unit =
    try
      val method = Method.values.find(_.name == x.getRequestMethod).getOrElse(Method.Get)
      val headers = x.getRequestHeaders.asScala.toSeq
        .flatMap((k, vs) => vs.asScala.map(v => (k, v)))
      val body = x.getRequestBody.readAllBytes()
      val req = Request(method, x.getRequestURI.toString, headers,
        if body.isEmpty then Body.Empty
        else Body.Bytes(scala.collection.immutable.ArraySeq.unsafeWrapArray(body)),
        peer = hostOf(x.getRemoteAddress))

      // the route is a program; running it here parks a virtual thread,
      // which is what the executor above is for
      val res = Async.run[Response, Pure](route(req)).runWith
      val bytes = Async.run[Chunk[Byte], Pure](Http.bytes(res)).runWith.toArray

      res.headers.foreach((k, v) => x.getResponseHeaders.add(k, v))
      x.sendResponseHeaders(res.status, if bytes.isEmpty then -1 else bytes.length.toLong)
      if bytes.nonEmpty then x.getResponseBody.write(bytes)
    catch
      // a route that throws is a 500 with the message as the body —
      // damage as data, on the wire too
      case e: Throwable =>
        val m = Option(e.getMessage).getOrElse(e.getClass.getName).getBytes("UTF-8")
        x.sendResponseHeaders(500, m.length.toLong)
        x.getResponseBody.write(m)
    finally x.close()

  // ---- the smallest routing that is not a framework

  /** a response with a text body */
  def text(status: Int, s: String,
           headers: Seq[(String, String)] = Nil): Response ! Async =
    pure(Response(status, ("content-type", "text/plain; charset=utf-8") +: headers,
      Http.one(s.getBytes("UTF-8"))))

  /** a response carrying a value, encoded by its schema */
  def json[A](status: Int, a: A, headers: Seq[(String, String)] = Nil)
             (using okay.codec.Schema[A]): Response ! Async =
    pure(Response(status, ("content-type", "application/json") +: headers,
      Http.one(okay.codec.Json.write(a).getBytes("UTF-8"))))

  /** nothing found */
  def notFound: Response ! Async = text(404, "not found")

  /** the path of a request, without its query */
  def path(r: Request): String = r.url.takeWhile(_ != '?')
}
