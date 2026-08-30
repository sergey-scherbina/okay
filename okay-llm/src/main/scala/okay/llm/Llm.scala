package okay.llm

import okay.{!, %, +, Async, Produce, Stage, Writer, async, effect, pure, through}
import okay.given
import okay.codec.{Json, Schema}

/**
 * Language models as streams (specs/llm.md): a completion IS a stream
 * of tokens, transport arrives line by line (SSE), events are parsed
 * by the TOTAL pipeline — a truncated payload is data, not a fault —
 * and everything upstream of the wire is a mock away from a test.
 */

/** the transport seam: post a body, stream back raw response lines —
 * a real HTTP client or a test mock plugs in here */
trait Transport:
  def post(url: String, headers: Map[String, String], body: String)
  : Unit ! (Writer % String + Async)

object Transport:
  /** java.net.http, streaming lines; the virtual thread parks on the wire */
  def http(client: java.net.http.HttpClient = java.net.http.HttpClient.newHttpClient())
  : Transport = new Transport:
    def post(url: String, headers: Map[String, String], body: String)
    : Unit ! (Writer % String + Async) =
      type F = Writer % String + Async
      effect[F, java.util.Iterator[String]](Async.Run { () =>
        val b = java.net.http.HttpRequest.newBuilder(java.net.URI.create(url))
          .POST(java.net.http.HttpRequest.BodyPublishers.ofString(body))
        headers.foreach((k, v) => b.header(k, v))
        client.send(b.build(),
          java.net.http.HttpResponse.BodyHandlers.ofLines()).body().iterator()
      }).flatMap { it =>
        def go(): Unit ! F =
          effect[F, Boolean](Async.Run(() => it.hasNext)).flatMap { has =>
            if !has then pure(())
            else effect[F, String](Async.Run(() => it.next()))
              .flatMap(line => effect[F, String](Writer(line)).flatMap(_ => go()))
          }
        go()
      }

object Sse {
  /** SSE framing as a Stage: lines await in, event payloads (the
   * joined data: fields) tell out; a partial trailing event flushes */
  def events: Stage[String, String, Unit] =
    def go(buf: List[String]): Stage[String, String, Unit] =
      Stage.await[String, String].flatMap {
        case Some(line) =>
          if line.isEmpty then
            if buf.isEmpty then go(Nil)
            else Stage.tell[String, String](buf.reverse.mkString("\n")).flatMap(_ => go(Nil))
          else if line.startsWith("data:") then go(line.drop(5).trim :: buf)
          else go(buf)   // comments, event:, id: — framing we do not need yet
        case None =>
          if buf.isEmpty then pure(())
          else Stage.tell[String, String](buf.reverse.mkString("\n")).map(_ => ())
      }

    go(Nil)
}

object Anthropic {

  final case class Message(role: String, content: String)
  final case class Request(model: String, max_tokens: Int,
                           messages: List[Message], stream: Boolean)
  given Schema[Message] = Schema.derived
  given Schema[Request] = Schema.derived

  // the streaming events we care about; everything else falls through
  final case class Delta(text: Option[String])
  final case class Event(`type`: String, delta: Option[Delta])
  given Schema[Delta] = Schema.derived
  given Schema[Event] = Schema.derived

  /** an event payload to its text token, if it carries one — TOTAL:
   * unknown or damaged events are simply not tokens */
  def token(payload: String): Option[String] =
    if payload == "[DONE]" then None
    else Json.read[Event](payload).toOption
      .filter(_.`type` == "content_block_delta")
      .flatMap(_.delta.flatMap(_.text))

  /**
   * The completion as a stream of text tokens: transport lines
   * through SSE framing, each event decoded by the total pipeline,
   * non-token events dropped. The whole thing is lazy — nothing is
   * sent until the stream is pulled.
   */
  def stream(transport: Transport, apiKey: String, request: Request,
             url: String = "https://api.anthropic.com/v1/messages")
  : Unit ! (Writer % String + Async) =
    val body = Json.write(request.copy(stream = true))
    val lines = transport.post(url, Map(
      "x-api-key" -> apiKey,
      "anthropic-version" -> "2023-06-01",
      "content-type" -> "application/json"), body)
    tokensOf(lines)

  /** the reusable tail: SSE lines to text tokens */
  def tokensOf(lines: Unit ! (Writer % String + Async))
  : Unit ! (Writer % String + Async) =
    def go(rest: Unit ! (Writer % String + Async), buf: List[String])
    : Unit ! (Writer % String + Async) =
      import okay.!.*
      type F = Writer % String + Async
      rest.resume match
        case Pure(_) => flushEvent(buf)
        case Effect(e) => okay.<|>[Async, Writer % String](e) match
          case Left(a) => Effect(a).flatMap(_ => flushEvent(buf))
          case Right(line) => emitFrom(line.asInstanceOf[String], buf)(b => flushEvent(b))
        case Bind(Effect(e), k) => okay.<|>[Async, Writer % String](e) match
          case Left(a) => Effect(a).flatMap(x => go(k(x), buf))
          case Right(line) =>
            emitFrom(line.asInstanceOf[String], buf)(b => go(k(line.asInstanceOf), b))

    def flushEvent(buf: List[String]): Unit ! (Writer % String + Async) =
      if buf.isEmpty then pure(())
      else tokenOf(buf.reverse.mkString("\n"))(pure(()))

    def emitFrom(line: String, buf: List[String])
                (next: List[String] => Unit ! (Writer % String + Async))
    : Unit ! (Writer % String + Async) =
      if line.isEmpty then
        if buf.isEmpty then next(Nil)
        else tokenOf(buf.reverse.mkString("\n"))(next(Nil))
      else if line.startsWith("data:") then next(line.drop(5).trim :: buf)
      else next(buf)

    def tokenOf(payload: String)(next: => Unit ! (Writer % String + Async))
    : Unit ! (Writer % String + Async) =
      token(payload) match
        case Some(t) => effect[Writer % String + Async, String](Writer(t)).flatMap(_ => next)
        case None => next

    go(lines, Nil)
}
