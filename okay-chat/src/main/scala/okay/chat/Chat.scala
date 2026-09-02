package okay.chat

import okay.*
import okay.given
import okay.http.{Body, Method, Request, Response}
import okay.llm.{Anthropic, Cut, OpenAi, Transport}
import okay.conf.{Secret, Secrets}
import okay.codec.Json
import okay.codec.Json.*
import java.nio.charset.StandardCharsets.UTF_8

/**
 * A streaming LLM chat component (specs/chat.md): the model seam,
 * Cut-guarded SSE framing, and the `/chat` route — extracted
 * 2026-09-02 from okay-demo/ChatDemo.scala (a pure move, no behavior
 * change; the demo's page/HTML stays where it is, market-flavored).
 */
object Chat {

  /** the model seam: history in, token stream out — scripted and
   * live both fit it, which is the whole doctrine */
  type Model = Seq[Anthropic.Message] => Unit ! (Writer % String + Async)

  /** an override for a special-cased turn (e.g. a consumer's own
   * command prefix): the FULL request (a consumer may need its
   * headers — a bearer token, say) plus the already-parsed messages,
   * answering an ALREADY-SSE-FRAMED source when it wants to
   * intercept, `None` to fall through to the plain model */
  type TurnOverride = (Request, Seq[Anthropic.Message]) => Option[Source[Chunk[Byte]]]

  /** offline: stream a deterministic reply, token by token, the
   * same shape the wire produces */
  def scripted: Model = messages =>
    val last = messages.lastOption.map(_.content).getOrElse("")
    val reply = s"You said: $last — and this reply is streamed token by token " +
      "by the scripted model (set ANTHROPIC_API_KEY for the real one)."
    def go(ts: List[String]): Unit ! (Writer % String + Async) = ts match
      case Nil => pure(())
      case t :: rest =>
        effect[Writer % String + Async, Unit](Writer(t + " ")).flatMap(_ => go(rest))
    go(reply.split(' ').toList)

  /** the config rides the Secrets capability as `env:NAME`
   * references (demo-ctx-wiring): the process edge installs
   * Secrets.env, a test installs Secrets.memory, and the model
   * DISPATCH below becomes testable without touching the process
   * environment */
  def secret(name: String)(using s: Secrets): Option[String] =
    s.get(Secret(s"env:$name")).toOption

  /** live: the provider's stream through okay-llm, over the AMBIENT
   * wire — a test wires a canned Transport and runs this very path
   * offline */
  def live(key: String)(using t: Transport): Model = messages =>
    Anthropic.stream(t, key, Anthropic.Request(
      model = "claude-sonnet-4-5", max_tokens = 1024,
      messages = messages.toList, stream = true))

  /** an OpenAI-compatible endpoint (a local model server fits): the
   * same seam, one more filling */
  def local(base: String)(using t: Transport): Model = messages =>
    val body = Json.print(JObj(Vector(
      "model" -> JStr("default"),
      "stream" -> JBool(true),
      "max_tokens" -> JNum(1024),
      "messages" -> JArr(messages.toVector.map(m => JObj(Vector(
        "role" -> JStr(m.role), "content" -> JStr(m.content))))))))
    OpenAi.stream(t, "local", body, s"$base/v1/chat/completions")

  /** which model serves — shown on a page and at startup */
  def modeName(using Secrets): String =
    if secret("ANTHROPIC_API_KEY").isDefined then "live (Anthropic)"
    else secret("OKAY_CHAT_BASE") match
      case Some(base) => s"local ($base)"
      case None => "scripted (no model — set OKAY_CHAT_BASE or ANTHROPIC_API_KEY)"

  def model(using Transport, Secrets): Model =
    secret("ANTHROPIC_API_KEY").map(live)
      .orElse(secret("OKAY_CHAT_BASE").map(local))
      .getOrElse(scripted)

  // ---- the SSE reply -------------------------------------------------

  /** one SSE frame — public: a consumer's OTHER streams (a live
   * feed, an inbox) reuse the exact same framing convention `reply`
   * uses, not just the `/chat` route */
  def sse(kind: String, data: String): Chunk[Byte] =
    scala.collection.immutable.ArraySeq.unsafeWrapArray(
      (if kind == "data" then s"data: $data\n\n"
       else s"event: $kind\ndata: $data\n\n").getBytes(UTF_8))

  def obj(fs: (String, Json)*): Json = JObj(fs.toVector)

  /** the guarded stream as SSE frames: tokens, then done — or cut */
  def reply(m: Model, budget: Int)(messages: Seq[Anthropic.Message])
  : Source[Chunk[Byte]] =
    val guarded: Either[Cut.Violation, Unit] ! (Writer % String + Async) =
      Cut.guard {
        Cut.checked(m(messages))((i, _) =>
          if i >= budget then Some(Cut.Violation("token-budget", i, s"> $budget tokens"))
          else None)
      }
    Writer.map(guarded)(t => sse("data", Json.print(JStr(t)))).flatMap {
      case Right(_) => effect[Writer % Chunk[Byte] + Async, Unit](
        Writer(sse("done", "")))
      case Left(v) => effect[Writer % Chunk[Byte] + Async, Unit](
        Writer(sse("cut", Json.print(obj("rule" -> JStr(v.rule), "at" -> JNum(v.at))))))
    }

  // ---- request parsing -------------------------------------------------

  def fieldOf(body: Body, name: String): String =
    Json.parse(new String(body.bytes, UTF_8)) match
      case JObj(fs) => fs.collectFirst { case (`name`, JStr(v)) => v }.getOrElse("")
      case _ => ""

  def messagesOf(body: Body): Seq[Anthropic.Message] =
    Json.parse(new String(body.bytes, UTF_8)) match
      case JObj(fs) => fs.collectFirst { case ("messages", JArr(ms)) => ms }
        .getOrElse(Vector.empty).flatMap {
          case JObj(m) =>
            for r <- m.collectFirst { case ("role", JStr(x)) => x }
                c <- m.collectFirst { case ("content", JStr(x)) => x }
            yield Anthropic.Message(r, c)
          case _ => None
        }
      case _ => Vector.empty

  /** the linked React/Scala.js bundle, if a link has been run;
   * absent, a consumer's server-rendered page serves */
  def appJs: Option[java.nio.file.Path] =
    sys.env.get("OKAY_CHAT_APP").map(java.nio.file.Path.of(_))
      .filter(java.nio.file.Files.exists(_))
      .orElse {
        val glob = java.nio.file.Path.of("okay-demo/web/.js/target")
        if !java.nio.file.Files.exists(glob) then None
        else
          import scala.jdk.CollectionConverters.*
          java.nio.file.Files.walk(glob).iterator().asScala
            .find(p => p.getFileName.toString == "main.js" &&
              p.toString.contains("fastopt"))
      }

  // ---- the route -------------------------------------------------------

  /** `POST /chat`: `turnOverride` gets first refusal at a turn (an
   * already-framed SSE source of its own); `None` falls through to
   * the plain guarded model stream */
  def chatRoute(m: Model, budget: Int, turnOverride: TurnOverride = (_, _) => None)
  : PartialFunction[Request, Response ! Async] =
    case r if r.method == Method.Post && r.url == "/chat" =>
      val messages = messagesOf(r.body)
      pure(Response(200, Seq("content-type" -> "text/event-stream"),
        turnOverride(r, messages).getOrElse(reply(m, budget)(messages))))
}
