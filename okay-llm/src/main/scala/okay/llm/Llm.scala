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

  // ------------------------------------------------------------------
  // the Messages API, as an agent needs it (tools and all)
  //
  // The shape differs from the OpenAI one in every way that matters,
  // which is the point of having both: `system` is a TOP-LEVEL field
  // rather than a message, `content` is a list of typed BLOCKS rather
  // than a string, a tool's schema is `input_schema` rather than
  // `parameters`, a tool's arguments arrive as a JSON OBJECT rather
  // than a string of JSON, and tool results go back as a USER message
  // carrying tool_result blocks. None of that reached the `Model`
  // effect: the mapping lives here, in the handler's own module.
  //
  // The response is walked as a parsed `Json` rather than decoded by a
  // derived Schema, because its content blocks are heterogeneous and
  // carry arbitrary JSON in `input` — the same reason the REQUEST is
  // built as Json values. Both directions stay total.

  def textBlock(text: String): Json =
    Json.JObj(Vector("type" -> Json.JStr("text"), "text" -> Json.JStr(text)))

  def toolUseBlock(id: String, name: String, input: Json): Json =
    Json.JObj(Vector(
      "type" -> Json.JStr("tool_use"),
      "id" -> Json.JStr(id),
      "name" -> Json.JStr(name),
      "input" -> input))

  def toolResultBlock(id: String, content: String): Json =
    Json.JObj(Vector(
      "type" -> Json.JStr("tool_result"),
      "tool_use_id" -> Json.JStr(id),
      "content" -> Json.JStr(content)))

  def blocks(role: String, bs: Seq[Json]): Json =
    Json.JObj(Vector("role" -> Json.JStr(role), "content" -> Json.JArr(bs.toVector)))

  /** a tool declaration; note `input_schema`, not `parameters` */
  def toolDecl(name: String, description: String, schema: Json): Json =
    Json.JObj(Vector(
      "name" -> Json.JStr(name),
      "description" -> Json.JStr(description),
      "input_schema" -> schema))

  def messagesBody(model: String, system: Option[String], messages: Seq[Json],
                   tools: Seq[Json] = Nil, maxTokens: Int = 1024,
                   stream: Boolean = false): String =
    val base = Vector(
      "model" -> Json.JStr(model),
      "max_tokens" -> Json.JNum(maxTokens.toDouble),
      "messages" -> Json.JArr(messages.toVector),
      "stream" -> Json.JBool(stream))
    val withSystem = system.fold(base)(s => base :+ ("system" -> Json.JStr(s)))
    val full = if tools.isEmpty then withSystem
      else withSystem :+ ("tools" -> Json.JArr(tools.toVector))
    Json.print(Json.JObj(full))

  def messagesHeaders(apiKey: String): Map[String, String] = Map(
    "x-api-key" -> apiKey,
    "anthropic-version" -> "2023-06-01",
    "content-type" -> "application/json")

  val messagesUrl = "https://api.anthropic.com/v1/messages"

  /** what one reply carried: its text, and the tools it asked for */
  final case class Answer(text: String, calls: Seq[(String, String, Json)],
                          inputTokens: Option[Int], outputTokens: Option[Int])

  /** walk the parsed body — total, so a truncated one yields the
   * blocks that arrived */
  def answer(body: String): Answer =
    val fields = Json.parse(body) match
      case Json.JObj(fs) => fs.toMap
      case _ => Map.empty[String, Json]

    val bs = fields.get("content") match
      case Some(Json.JArr(vs)) => vs
      case _ => Vector.empty

    def field(o: Map[String, Json], k: String): Option[Json] = o.get(k)

    val texts = Vector.newBuilder[String]
    val calls = Vector.newBuilder[(String, String, Json)]
    for b <- bs do b match
      case Json.JObj(fs) =>
        val m = fs.toMap
        (field(m, "type"), field(m, "text"), field(m, "id"), field(m, "name")) match
          case (Some(Json.JStr("text")), Some(Json.JStr(t)), _, _) => texts += t
          case (Some(Json.JStr("tool_use")), _, Some(Json.JStr(id)), Some(Json.JStr(n))) =>
            calls += ((id, n, field(m, "input").getOrElse(Json.JObj(Vector.empty))))
          case _ => ()
      case _ => ()

    val usage = fields.get("usage") match
      case Some(Json.JObj(fs)) => fs.toMap
      case _ => Map.empty[String, Json]
    def count(k: String) = usage.get(k).collect { case Json.JNum(n) => n.toInt }

    Answer(texts.result().mkString, calls.result(),
      count("input_tokens"), count("output_tokens"))

  /** one completion, whole */
  def message(transport: Transport, apiKey: String, body: String,
              url: String = messagesUrl): Answer ! Async =
    Writer.run[String, Unit, Async](transport.post(url, messagesHeaders(apiKey), body))
      .map((lines, _) => answer(lines.mkString("\n")))

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
