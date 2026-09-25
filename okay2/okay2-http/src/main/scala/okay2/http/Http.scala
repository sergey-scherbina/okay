package okay2.http

import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.immutable.ArraySeq
import okay2.{!, Writer, pure}
import okay2.async.Async
import okay2.codec.{Json, Schema}
import okay2.stream.{Chunk, Lines, Pipe, Source, Sse, Stage}

/**
 * HTTP as two plain types (okay-http's Http.scala): a `Request`, and a
 * `Response` whose body is a chunked `Source`, so a body STREAMS and a
 * status is data, never an exception. `Request => Response ! Async` is
 * what a route is and what a client sends, so one handler can be called
 * in-process or over a socket.
 */
sealed abstract class Method(val name: String)

object Method {
  case object Get extends Method("GET")
  case object Head extends Method("HEAD")
  case object Post extends Method("POST")
  case object Put extends Method("PUT")
  case object Patch extends Method("PATCH")
  case object Delete extends Method("DELETE")
  case object Options extends Method("OPTIONS")

  val values: Vector[Method] = Vector(Get, Head, Post, Put, Patch, Delete, Options)
}

sealed trait Body {
  def bytes: Array[Byte] = this match {
    case Body.Empty => Array.empty
    case Body.Text(s) => s.getBytes(UTF_8)
    case Body.Bytes(b) => b.toArray
  }
}

object Body {
  case object Empty extends Body
  final case class Text(s: String) extends Body
  final case class Bytes(b: Chunk[Byte]) extends Body
}

final case class Request(method: Method, url: String,
                         headers: Seq[(String, String)] = Nil,
                         body: Body = Body.Empty,
                         /** the remote host, where the server knows it */
                         peer: Option[String] = None)

object Request {
  def get(url: String, headers: Seq[(String, String)] = Nil): Request = Request(Method.Get, url, headers)

  def post(url: String, body: Body, headers: Seq[(String, String)] = Nil): Request =
    Request(Method.Post, url, headers, body)

  /** a POST of `a`, encoded by its schema */
  def json[A](url: String, a: A, headers: Seq[(String, String)] = Nil)(implicit s: Schema[A]): Request =
    post(url, Body.Text(Json.write(a)), ("content-type", "application/json") +: headers)
}

/** `release` lets the body go unread (a pooled connection back) */
final case class Response(status: Int,
                          headers: Seq[(String, String)],
                          body: Source[Chunk[Byte]],
                          release: Unit ! Async = pure[Async, Unit](())) {
  /** a header, case-insensitively */
  def header(name: String): Option[String] = {
    val n = name.toLowerCase
    headers.collectFirst { case (k, v) if k.toLowerCase == n => v }
  }

  def ok: Boolean = status >= 200 && status < 300
}

trait Http {
  def send(r: Request): Response ! Async
}

object Http {

  /** a body of one chunk */
  def one(bs: Array[Byte]): Source[Chunk[Byte]] =
    if (bs.isEmpty) pure(()) else Writer.tell[Chunk[Byte]](ArraySeq.unsafeWrapArray(bs))

  /** the whole body, as one chunk */
  def bytes(r: Response): Chunk[Byte] ! Async =
    Writer.loopWith[Chunk[Byte], scala.collection.mutable.ArrayBuilder[Byte], Unit, Chunk[Byte], Async](r.body)(
      scala.collection.mutable.ArrayBuilder.make[Byte])((b, c) => { b ++= c; b })((b, _) => ArraySeq.unsafeWrapArray(b.result()))

  /** let the body go unread */
  def discard(r: Response): Unit ! Async = r.release

  def text(r: Response): String ! Async = bytes(r).map(c => new String(c.toArray, UTF_8))

  /** the body as UTF-8 lines, framed on bytes */
  def lines(r: Response): Source[String] =
    Pipe.intoIn[Chunk[Byte], String, Async, Unit, Unit](r.body)(framing)

  /** the line framer every line protocol shares, as a Stage */
  def framing: Stage[Chunk[Byte], String, Unit] = Lines.stage

  /** whether a response is a stream the server must not buffer */
  def streams(r: Response): Boolean =
    r.headers.exists { case (k, v) => k.equalsIgnoreCase("content-type") && v.contains("text/event-stream") }

  def json[A](r: Response)(implicit s: Schema[A]): Either[String, A] ! Async =
    text(r).map(t => Json.read[A](t))

  /** server-sent events: the `data:` payload of each event */
  def sse(r: Response): Source[String] =
    Pipe.intoIn[String, String, Async, Unit, Unit](lines(r))(Sse.events)
}
