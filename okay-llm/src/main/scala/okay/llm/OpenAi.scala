package okay.llm

import okay.{!, %, +, Async, Writer}
import okay.given
import okay.codec.{Json, Schema}

/**
 * The OpenAI-compatible protocol — which is the one most of the
 * market speaks: OpenAI itself, Groq, Together, OpenRouter, Fireworks,
 * and the local runtimes (Ollama, vLLM, llama.cpp) all serve it, so
 * one handler reaches a live model almost everywhere. The wire is the
 * only thing mocked in the tests; the protocol is the real one.
 *
 * Both directions use the tool that fits them. The REQUEST is built
 * as a `Json` value and printed — a tool's `parameters` is an
 * arbitrary JSON Schema, which a derived codec has no business
 * describing. The RESPONSE is decoded by derived Schemas through the
 * total pipeline, so a truncated or damaged body yields what it
 * carried instead of an exception (a half-arrived completion still
 * has its text).
 */
object OpenAi {

  // ---------------------------------------------------------------- responses

  final case class Fn(name: String, arguments: String)
  final case class Call(id: String, function: Fn)
  final case class Msg(role: String, content: Option[String],
                       tool_calls: Option[List[Call]])
  final case class Choice(message: Option[Msg], finish_reason: Option[String])

  /** what the provider says the exchange cost — the only authority
   * on tokens, and what a local counter is checked against */
  final case class Usage(prompt_tokens: Option[Int],
                         completion_tokens: Option[Int],
                         total_tokens: Option[Int])

  final case class Response(choices: List[Choice], usage: Option[Usage])

  given Schema[Fn] = Schema.derived
  given Schema[Call] = Schema.derived
  given Schema[Msg] = Schema.derived
  given Schema[Choice] = Schema.derived
  given Schema[Usage] = Schema.derived
  given Schema[Response] = Schema.derived

  /** the streaming shape: deltas rather than a whole message */
  final case class Delta(content: Option[String])
  final case class StreamChoice(delta: Option[Delta])
  final case class StreamChunk(choices: List[StreamChoice])

  given Schema[Delta] = Schema.derived
  given Schema[StreamChoice] = Schema.derived
  given Schema[StreamChunk] = Schema.derived

  /** one SSE payload to its text token, if it carries one — TOTAL:
   * `[DONE]`, an unknown shape or a cut-off payload is simply not a
   * token, at any prefix */
  def token(payload: String): Option[String] =
    if payload.trim == "[DONE]" then None
    else Json.read[StreamChunk](payload).toOption
      .flatMap(_.choices.headOption)
      .flatMap(_.delta)
      .flatMap(_.content)
      .filter(_.nonEmpty)

  // ---------------------------------------------------------------- requests

  /** a message as the wire wants it */
  def message(role: String, content: String,
              toolCallId: Option[String] = None,
              calls: Seq[(String, String, String)] = Nil): Json =
    val base = Vector("role" -> Json.JStr(role), "content" -> Json.JStr(content))
    val withId = toolCallId.fold(base)(id => base :+ ("tool_call_id" -> Json.JStr(id)))
    if calls.isEmpty then Json.JObj(withId)
    else Json.JObj(withId :+ ("tool_calls" -> Json.JArr(calls.map { (id, name, args) =>
      Json.JObj(Vector(
        "id" -> Json.JStr(id),
        "type" -> Json.JStr("function"),
        "function" -> Json.JObj(Vector(
          "name" -> Json.JStr(name),
          "arguments" -> Json.JStr(args)))))
    }.toVector)))

  /** a tool declaration; `parameters` is the JSON Schema our own
   * Schema algebra derived, passed through untouched */
  def tool(name: String, description: String, parameters: Json): Json =
    Json.JObj(Vector(
      "type" -> Json.JStr("function"),
      "function" -> Json.JObj(Vector(
        "name" -> Json.JStr(name),
        "description" -> Json.JStr(description),
        "parameters" -> parameters))))

  def request(model: String, messages: Seq[Json], tools: Seq[Json] = Nil,
              stream: Boolean = false, maxTokens: Option[Int] = None): String =
    val base = Vector(
      "model" -> Json.JStr(model),
      "messages" -> Json.JArr(messages.toVector),
      "stream" -> Json.JBool(stream))
    val withTools = if tools.isEmpty then base
      else base :+ ("tools" -> Json.JArr(tools.toVector))
    val full = maxTokens.fold(withTools)(n =>
      withTools :+ ("max_tokens" -> Json.JNum(n.toDouble)))
    Json.print(Json.JObj(full))

  def headers(apiKey: String): Map[String, String] = Map(
    "authorization" -> s"Bearer $apiKey",
    "content-type" -> "application/json")

  val chatUrl = "https://api.openai.com/v1/chat/completions"

  // ---------------------------------------------------------------- calls

  /**
   * One completion, whole. The agent loop needs the tool calls
   * complete before it can run them, so this is the non-streaming
   * door; `stream` below is the token-by-token one.
   */
  def complete(transport: Transport, apiKey: String, body: String,
               url: String = chatUrl): Response ! Async =
    Writer.run[String, Unit, Async](transport.post(url, headers(apiKey), body))
      .map { (lines, _) =>
        Json.read[Response](lines.mkString("\n"))
          .getOrElse(Response(Nil, None))   // total: a damaged body is no choices
      }

  /** the completion as a stream of text tokens (SSE) */
  def stream(transport: Transport, apiKey: String, body: String,
             url: String = chatUrl): Unit ! (Writer % String + Async) =
    tokensOf(transport.post(url, headers(apiKey), body))

  /** SSE lines to text tokens — the same walk the Anthropic client
   * uses, with this protocol's `token` */
  def tokensOf(lines: Unit ! (Writer % String + Async))
  : Unit ! (Writer % String + Async) =
    import okay.!.*
    type F = Writer % String + Async

    def go(rest: Unit ! F, buf: List[String]): Unit ! F = (rest.resume: @unchecked) match
      case Pure(_) => flush(buf)
      case Effect(e) => okay.<|>[Async, Writer % String](e) match
        case Left(a) => Effect(a).flatMap(_ => flush(buf))
        case Right(line) => absorb(line.asInstanceOf[String], buf)(b => flush(b))
      case Bind(Effect(e), k) => okay.<|>[Async, Writer % String](e) match
        case Left(a) => Effect(a).flatMap(x => go(k(x), buf))
        case Right(line) =>
          absorb(line.asInstanceOf[String], buf)(b => go(k(okay.answer), b))

    def flush(buf: List[String]): Unit ! F =
      if buf.isEmpty then okay.pure(())
      else emit(buf.reverse.mkString("\n"))(okay.pure(()))

    def absorb(line: String, buf: List[String])(next: List[String] => Unit ! F): Unit ! F =
      if line.isEmpty then
        if buf.isEmpty then next(Nil) else emit(buf.reverse.mkString("\n"))(next(Nil))
      else if line.startsWith("data:") then next(line.drop(5).trim :: buf)
      else next(buf)

    def emit(payload: String)(next: => Unit ! F): Unit ! F =
      token(payload) match
        case Some(t) => okay.effect[F, Unit](Writer(t)).flatMap(_ => next)
        case None => next

    go(lines, Nil)
}
