package okay.demo

import okay.*
import okay.given
import okay.http.{Body, Http, Request, Response}
import okay.jetty.Jetty
import okay.llm.{Anthropic, Cut, OpenAi, Transports}
import okay.agent.{Agent, Compact, Handlers, Model as AgentModel, Provider, Tool, Turn, Context as AgentContext}
import okay.matching.{MatchStore, MemoryMatch, SqlMatch, Tools as MatchTools}
import okay.jdbc.JdbcSql
import okay.codec.Json
import okay.codec.Json.*
import java.nio.charset.StandardCharsets.UTF_8

/**
 * The chat with an LLM, as a web page (specs/demo-chat.md): one JVM
 * main, no frontend build. okay-jetty streams the reply body live,
 * okay-llm turns the provider's SSE into tokens, Cut guards the
 * stream with a visible budget, and the OFFLINE MODE IS THE DEMO —
 * a scripted model streams a deterministic reply when no key is
 * set, so the application always runs and the tests prove the same
 * path the key exercises.
 *
 *   ANTHROPIC_API_KEY=...  the real model
 *   OKAY_CHAT_PORT=8090    where to listen
 *   OKAY_CHAT_MAX=512      the token budget the Cut enforces
 */
object ChatDemo {

  /** the model seam: history in, token stream out — scripted and
   * live both fit it, which is the whole doctrine */
  type Model = Seq[Anthropic.Message] => Unit ! (Writer % String + Async)

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

  /** live: the provider's stream through okay-llm */
  def live(key: String): Model = messages =>
    Anthropic.stream(Transports.http(), key, Anthropic.Request(
      model = "claude-sonnet-4-5", max_tokens = 1024,
      messages = messages.toList, stream = true))

  /** an OpenAI-compatible endpoint (the local rozum model on :8089
   * fits): the same seam, one more filling */
  def local(base: String): Model = messages =>
    val body = Json.print(JObj(Vector(
      "model" -> JStr("default"),
      "stream" -> JBool(true),
      "max_tokens" -> JNum(1024),
      "messages" -> JArr(messages.toVector.map(m => JObj(Vector(
        "role" -> JStr(m.role), "content" -> JStr(m.content))))))))
    OpenAi.stream(Transports.http(), "local", body, s"$base/v1/chat/completions")

  def model: Model =
    sys.env.get("ANTHROPIC_API_KEY").map(live)
      .orElse(sys.env.get("OKAY_CHAT_BASE").map(local))
      .getOrElse(scripted)

  // ---- the matchmaking side (okay-match wired in) --------------------

  /** ONE marketplace for the whole server, DURABLE by default: a
   * sqlite file (OKAY_CHAT_DB; ":memory:" asks for the memory
   * engine) — the open-backend principle is a connection string */
  lazy val market: MatchStore =
    val db = sys.env.getOrElse("OKAY_CHAT_DB", "okay-chat.db")
    if db == ":memory:" then MemoryMatch()
    else SqlMatch(JdbcSql(java.sql.DriverManager.getConnection(s"jdbc:sqlite:$db")))
  private val turnNo = java.util.concurrent.atomic.AtomicLong(0)

  private val matchSystem =
    """You are a helpful chat assistant that ALSO runs a marketplace
      |over a structured database. Decide yourself when the tools
      |apply: when the user OFFERS a skill or service, or LOOKS FOR
      |one, work the marketplace; for anything else just answer —
      |no tools needed.
      |The marketplace flow: facts_register (email -> profile id)
      |first — ask for an email if none was given; registry_search
      |BEFORE registry_propose; facts_assert to record an offer or a
      |need (side "offer" or "need", value {"t":"text","s":...},
      |chat "web-demo", span = the user's words); find_candidates to
      |search offers, and report matches with their facts. Answer in
      |the user's language, briefly, and say what you stored or
      |found.""".stripMargin

  /** an agent turn over the match tools: the LLM structures the
   * chat into the store and searches it — the okay-match story */
  def agentTurn(text: String, history: Seq[Anthropic.Message],
                modelH: okay.Handler[AgentModel],
                store: MatchStore = market): String =
    given okay.Handler[AgentModel] = modelH
    given okay.Handler[Tool] = Handlers.tools(MatchTools.table(store))
    val ctx = Handlers.context(Compact.all)._2
    given okay.Handler[AgentContext] = ctx
    given r1: okay.Handler[AgentModel + Async] = okay.Handler.union[AgentModel, Async]
    given r2: okay.Handler[AgentContext + (AgentModel + Async)] =
      okay.Handler.union[AgentContext, AgentModel + Async]
    given r3: okay.Handler[Tool + (AgentContext + (AgentModel + Async))] =
      okay.Handler.union[Tool, AgentContext + (AgentModel + Async)]
    val prog =
      Agent.remember(Turn.System(matchSystem)).flatMap { _ =>
        def seed(ms: List[Anthropic.Message]): Unit ! okay.agent.Agent = ms match
          case Nil => pure(())
          case m :: rest =>
            Agent.remember(
              if m.role == "user" then Turn.User(m.content)
              else Turn.Assistant(m.content)).flatMap(_ => seed(rest))
        seed(history.toList).flatMap(_ =>
          Agent.converse(text, MatchTools.specs))
      }
    prog.runWith

  /** the deterministic offline "agent": the SAME tool table, driven
   * by two fixed phrasings — the tests' and the no-model mode's path */
  def scriptedAgent(text: String, store: MatchStore = market): String =
    import okay.codec.Json.*
    val t = MatchTools.table(store)
    def call(name: String, args: (String, Json)*): String =
      t(name)(okay.agent.ToolCall("d", name, JObj(args.toVector)))
    val email = "email ([^ ]+@[^ ]+)".r.findFirstMatchIn(text).map(_.group(1))
      .getOrElse("guest@demo")
    val off = turnNo.incrementAndGet()
    def profile: String =
      Json.parse(call("facts_register", "email" -> JStr(email))) match
        case JObj(fs) => fs.collectFirst { case ("profile", JStr(p)) => p }.get
        case _ => ""
    text match
      case s if s.contains("умею") || s.contains("offer:") =>
        val skill = s.replaceAll(".*(умею|offer:)\\s*", "").replaceAll("email [^ ]+", "").trim
        call("facts_assert", "profile" -> JStr(profile), "attr" -> JStr("skill"),
          "side" -> JStr("offer"), "chat" -> JStr("web-demo"),
          "offset" -> JNum(off.toDouble), "span" -> JStr(text),
          "value" -> JObj(Vector("t" -> JStr("text"), "s" -> JStr(skill))))
        s"""записал предложение: \"$skill\" (профиль $email)"""
      case s if s.contains("нужен") || s.contains("нужно") || s.contains("need:") =>
        val want = s.replaceAll(".*(нужен|нужно|need:)\\s*", "").replaceAll("email [^ ]+", "").trim
        Json.parse(call("find_candidates", "side" -> JStr("offer"),
          "text" -> JStr(want))) match
          case JArr(hits) if hits.nonEmpty =>
            val lines = hits.take(3).map {
              case JObj(fs) =>
                val facts = fs.collectFirst { case ("facts", JArr(vs)) => vs }.getOrElse(Vector.empty)
                val skills = facts.collect { case JObj(f)
                  if f.exists(_ == ("attr", JStr("skill"))) =>
                  f.collectFirst { case ("value", JObj(v)) =>
                    v.collectFirst { case ("s", JStr(x)) => x }.getOrElse("") }.getOrElse("") }
                s"- ${skills.mkString(", ")}"
              case _ => "- ?"
            }
            s"нашёл ${hits.length}: ${lines.mkString("; ")}"
          case _ => "пока никого не нашёл — но запрос я вижу"
      case _ =>
        """матч-режим: скажите \"умею <что>\" или \"нужен <кто>\" (и email <адрес>)"""

  /** which agent serves /match turns: real model when one is
   * configured, the deterministic table-driver otherwise */
  def matchTurn(text: String, history: Seq[Anthropic.Message],
                store: MatchStore = market): String =
    sys.env.get("ANTHROPIC_API_KEY").map { key =>
      agentTurn(text, history, Provider.anthropic(
        Transports.http(), key, "claude-sonnet-4-5"), store)
    }.orElse(sys.env.get("OKAY_CHAT_BASE").map { base =>
      agentTurn(text, history, Provider.openAi(
        Transports.http(), "local", "default", s"$base/v1/chat/completions"), store)
    }).getOrElse(scriptedAgent(text, store))

  // ---- the SSE reply -------------------------------------------------

  private def sse(kind: String, data: String): Chunk[Byte] =
    scala.collection.immutable.ArraySeq.unsafeWrapArray(
      (if kind == "data" then s"data: $data\n\n"
       else s"event: $kind\ndata: $data\n\n").getBytes(UTF_8))

  /** the guarded stream as SSE frames: tokens, then done — or cut */
  def reply(m: Model, budget: Int)(messages: Seq[Anthropic.Message])
  : Source[Chunk[Byte]] =
    val guarded: Either[Cut.Violation, Unit] ! (Writer % String + Async) =
      Cut.guarded[Unit] { p =>
        Cut.checked(p, m(messages))((i, _) =>
          if i >= budget then Some(Cut.Violation("token-budget", i, s"> $budget tokens"))
          else None)
      }
    Writer.map(guarded)(t => sse("data", Json.print(JStr(t)))).flatMap {
      case Right(_) => effect[Writer % Chunk[Byte] + Async, Unit](
        Writer(sse("done", "")))
      case Left(v) => effect[Writer % Chunk[Byte] + Async, Unit](
        Writer(sse("cut", Json.print(obj("rule" -> JStr(v.rule), "at" -> JNum(v.at))))))
    }

  private def obj(fs: (String, Json)*): Json = JObj(fs.toVector)

  // ---- the routes ----------------------------------------------------

  private def messagesOf(body: Body): Seq[Anthropic.Message] =
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

  /** the linked React app, if a link has been run (sbt
   * okayChatWebJS/fastLinkJS — the module lives in okay-demo/web);
   * absent, the vanilla page serves */
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

  def routes(m: Model, budget: Int,
             store: MatchStore = market): PartialFunction[Request, Response ! Async] =
    case r if r.method == okay.http.Method.Get && r.url == "/" =>
      val html = if appJs.isDefined then reactPage else page
      pure(Response(200, Seq("content-type" -> "text/html; charset=utf-8"),
        Http.one(html.getBytes(UTF_8))))
    case r if r.method == okay.http.Method.Get && r.url == "/app.js" && appJs.isDefined =>
      pure(Response(200, Seq("content-type" -> "text/javascript"),
        Http.one(java.nio.file.Files.readAllBytes(appJs.get))))
    case r if r.method == okay.http.Method.Post && r.url == "/chat" =>
      val messages = messagesOf(r.body)
      val last = messages.lastOption.map(_.content).getOrElse("")
      if last.startsWith("/match") then
        // the matchmaking turn: the agent works the okay-match tools,
        // the answer streams through the same SSE framing
        val answer = matchTurn(last.stripPrefix("/match").trim, messages.init, store)
        def stream(ts: List[String]): Unit ! (Writer % String + Async) = ts match
          case Nil => pure(())
          case t :: rest => effect[Writer % String + Async, Unit](
            Writer(t + " ")).flatMap(_ => stream(rest))
        pure(Response(200, Seq("content-type" -> "text/event-stream"),
          reply(_ => stream(answer.split(' ').toList), budget)(messages)))
      else
        pure(Response(200, Seq("content-type" -> "text/event-stream"),
          reply(m, budget)(messages)))

  def main(args: Array[String]): Unit =
    val port = sys.env.get("OKAY_CHAT_PORT").flatMap(_.toIntOption).getOrElse(8090)
    val budget = sys.env.get("OKAY_CHAT_MAX").flatMap(_.toIntOption).getOrElse(512)
    val mode =
      if sys.env.contains("ANTHROPIC_API_KEY") then "live"
      else if sys.env.contains("OKAY_CHAT_BASE") then s"local ${sys.env("OKAY_CHAT_BASE")}"
      else "scripted"
    Resource.run[Unit, Pure](
      Jetty.serve(port)(routes(model, budget))().map { s =>
        println(s"chat: http://127.0.0.1:${Jetty.port(s)}  (model: $mode)")
        Thread.sleep(Long.MaxValue)   // ctrl-c ends the process and the Resource
      }).runWith

  /** the React page: okay-ui's tree rendered by a real React (CDN
   * UMD globals), the logic cross-tested on the JVM — the frontend
   * the user asked for, still with no local build step beyond the
   * Scala.js link */
  val reactPage: String = """<!doctype html>
<meta charset="utf-8">
<title>okay chat</title>
<style>
  body { margin: 0; font: 15px/1.5 system-ui, sans-serif; background: #10141a; color: #e6e9ef; }
  #root > div { max-width: 640px; margin: 0 auto; padding: 1rem; display: flex; flex-direction: column; gap: .6rem; }
  input { padding: .6rem .8rem; border-radius: .7rem; border: 1px solid #2a3342; background: #171c25; color: inherit; width: 70%; }
  button { padding: .6rem 1rem; border-radius: .7rem; border: 0; background: #3563a8; color: white; cursor: pointer; }
</style>
<div id="root"></div>
<script crossorigin src="https://cdnjs.cloudflare.com/ajax/libs/react/18.3.1/umd/react.production.min.js"></script>
<script crossorigin src="https://cdnjs.cloudflare.com/ajax/libs/react-dom/18.3.1/umd/react-dom.production.min.js"></script>
<script src="/app.js"></script>"""

  // ---- the page (no build step: the demo is of the server stack) ----

  val page: String = """<!doctype html>
<meta charset="utf-8">
<title>okay chat</title>
<style>
  body { margin: 0; font: 15px/1.5 system-ui, sans-serif; background: #10141a; color: #e6e9ef; }
  main { max-width: 640px; margin: 0 auto; padding: 1rem; display: flex; flex-direction: column; height: 100vh; box-sizing: border-box; }
  h1 { font-size: 1rem; color: #7a869c; font-weight: 600; }
  #log { flex: 1; overflow-y: auto; display: flex; flex-direction: column; gap: .6rem; padding-bottom: 1rem; }
  .m { padding: .55rem .8rem; border-radius: .7rem; white-space: pre-wrap; max-width: 85%; }
  .user { background: #2b4a7a; align-self: flex-end; }
  .bot { background: #1d2430; align-self: flex-start; }
  .cut { color: #e0a050; font-size: .85em; }
  form { display: flex; gap: .5rem; }
  input { flex: 1; padding: .6rem .8rem; border-radius: .7rem; border: 1px solid #2a3342; background: #171c25; color: inherit; }
  button { padding: .6rem 1rem; border-radius: .7rem; border: 0; background: #3563a8; color: white; cursor: pointer; }
</style>
<main>
  <h1>okay chat — streamed by okay-jetty, guarded by Cut</h1>
  <div id="log"></div>
  <form id="f"><input id="i" autocomplete="off" placeholder="say something — or /match умею класть плитку email me@x"><button>send</button></form>
</main>
<script>
const log = document.getElementById('log'), f = document.getElementById('f'), i = document.getElementById('i');
const history = [];
function bubble(cls) { const d = document.createElement('div'); d.className = 'm ' + cls; log.appendChild(d); return d; }
f.onsubmit = async (ev) => {
  ev.preventDefault();
  const text = i.value.trim(); if (!text) return;
  i.value = '';
  bubble('user').textContent = text;
  history.push({role: 'user', content: text});
  const bot = bubble('bot');
  const res = await fetch('/chat', {method: 'POST', headers: {'content-type': 'application/json'},
    body: JSON.stringify({messages: history})});
  const reader = res.body.getReader(); const dec = new TextDecoder();
  let buf = '', answer = '';
  for (;;) {
    const {done, value} = await reader.read(); if (done) break;
    buf += dec.decode(value, {stream: true});
    let idx;
    while ((idx = buf.indexOf('\n\n')) >= 0) {
      const frame = buf.slice(0, idx); buf = buf.slice(idx + 2);
      const ev = frame.match(/^event: (.*)$/m)?.[1] ?? 'data';
      const data = frame.match(/^data: (.*)$/m)?.[1] ?? '';
      if (ev === 'data') { answer += JSON.parse(data); bot.textContent = answer; }
      else if (ev === 'cut') { const c = document.createElement('div'); c.className = 'cut';
        c.textContent = '✂ generation cut: ' + data; bot.appendChild(c); }
      log.scrollTop = log.scrollHeight;
    }
  }
  history.push({role: 'assistant', content: answer});
};
</script>"""
}
