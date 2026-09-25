package okay2.http

import com.sun.net.httpserver.{HttpExchange, HttpServer => JdkServer}
import java.net.InetSocketAddress
import scala.collection.immutable.ArraySeq
import scala.jdk.CollectionConverters._
import okay2.{!, Pure, Resource, Writer, pure}
import okay2.async.{Async, CanBlock}
import okay2.codec.{Json, Schema}
import okay2.platform.Schedulers
import okay2.stream.{Chunk, Source}

/**
 * The JDK's built-in server behind `Request => Response ! Async`
 * (okay-http's Server.scala): one exchange per virtual thread where the
 * JVM has them, the server a Resource — stopped when its scope ends. A
 * route that throws is a 500 carrying its message; a streaming response
 * (server-sent events) is written chunk by chunk, anything else buffered.
 */
object Server {

  def serve(port: Int)(route: Request => Response ! Async)(implicit cb: CanBlock): JdkServer ! Resource =
    Resource.acquire {
      val s = JdkServer.create(new InetSocketAddress(port), 0)
      s.setExecutor(
        if (Schedulers.hasVirtualThreads) java.util.concurrent.Executors.newVirtualThreadPerTaskExecutor()
        else java.util.concurrent.Executors.newCachedThreadPool())
      s.createContext("/", (x: HttpExchange) => handle(x, route))
      s.start()
      s
    }(_.stop(0))

  def port(s: JdkServer): Int = s.getAddress.getPort

  private def hostOf(a: java.net.SocketAddress): Option[String] = a match {
    case i: java.net.InetSocketAddress => Option(i.getAddress).map(_.getHostAddress).orElse(Option(i.getHostString))
    case null => None
    case other => Option(other.toString).filter(_.nonEmpty)
  }

  private def run[A](p: A ! Async)(implicit cb: CanBlock): A = okay2.!.run(Async.run[A, Pure](p))

  private def handle(x: HttpExchange, route: Request => Response ! Async)(implicit cb: CanBlock): Unit =
    try {
      val method = Method.values.find(_.name == x.getRequestMethod).getOrElse(Method.Get)
      val headers = x.getRequestHeaders.asScala.toSeq.flatMap { case (k, vs) => vs.asScala.map(v => (k, v)) }
      val body = x.getRequestBody.readAllBytes()
      val req = Request(method, x.getRequestURI.toString, headers,
        if (body.isEmpty) Body.Empty else Body.Bytes(ArraySeq.unsafeWrapArray(body)),
        peer = hostOf(x.getRemoteAddress))
      val res = run(route(req))
      res.headers.foreach { case (k, v) => x.getResponseHeaders.add(k, v) }
      if (Http.streams(res)) {
        x.sendResponseHeaders(res.status, 0)
        run(write(res.body, x.getResponseBody))
      } else {
        val bytes = run(Http.bytes(res)).toArray
        x.sendResponseHeaders(res.status, if (bytes.isEmpty) -1 else bytes.length.toLong)
        if (bytes.nonEmpty) x.getResponseBody.write(bytes)
      }
    } catch {
      case e: Throwable =>
        val m = Option(e.getMessage).getOrElse(e.getClass.getName).getBytes("UTF-8")
        x.sendResponseHeaders(500, m.length.toLong)
        x.getResponseBody.write(m)
    } finally x.close()

  private def write(body: Source[Chunk[Byte]], os: java.io.OutputStream): Unit ! Async =
    Writer.unconsIn[Chunk[Byte], Unit, Async](body).flatMap {
      case Left(_) => pure[Async, Unit](())
      case Right((c, rest)) => Async { os.write(c.toArray); os.flush() }.flatMap(_ => write(rest, os))
    }

  def text(status: Int, s: String, headers: Seq[(String, String)] = Nil): Response ! Async =
    pure[Async, Response](Response(status, ("content-type", "text/plain; charset=utf-8") +: headers, Http.one(s.getBytes("UTF-8"))))

  def json[A](status: Int, a: A, headers: Seq[(String, String)] = Nil)(implicit s: Schema[A]): Response ! Async =
    pure[Async, Response](Response(status, ("content-type", "application/json") +: headers, Http.one(Json.write(a).getBytes("UTF-8"))))

  def notFound: Response ! Async = text(404, "not found")

  /** the path, the query dropped */
  def path(r: Request): String = r.url.takeWhile(_ != '?')
}
