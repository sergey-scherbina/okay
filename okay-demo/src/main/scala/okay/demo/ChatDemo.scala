package okay.demo

import okay.*
import okay.given
import okay.Condition
import okay.http.{Http, McpHttp, Request, Response}
import okay.mcp.{Mcp, Server as McpServer}
import okay.jetty.Jetty
import okay.llm.{Anthropic, Cut, Transport, Transports}
import okay.agent.{Agent, Compact, Handlers, Model as AgentModel, Provider, Tool, Turn, Context as AgentContext}
import okay.rag.{Embedding, Vectors}
import okay.ops.Ops
import okay.security.Secure
import okay.security.given
import okay.admin.Admin
import okay.chat.Chat
import okay.live.{Hub, Registry}
import okay.subscription.Subscription
import okay.subscription.Subscription.Period
import okay.persist.{FileStore, MemoryStore, Policy}
import okay.jdbc.JdbcSql
import okay.pg.{PgSql, PgTarget, PgTls}
import okay.sql.Placeholders
import okay.conf.Secrets
import okay.crypto.given
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

  // the chat mechanics are okay-chat now (extracted 2026-09-02,
  // specs/chat.md): Chat.Model/scripted/live/local/modeName/model/
  // reply/sse/obj/fieldOf/messagesOf/appJs, a bare String uuid-free
  // seam with no opinion about MatchStore/ChatLog held here or there.

  // ---- the board (the demo's own small domain) -----------------------
  //
  // What replaced the marketplace. The demo needs something to be
  // ABOUT so it can show the mechanism: an agent calling tools, a
  // projection rebuilt from a durable log, live notifications, an MCP
  // front door, a page that streams. See `Board` for why it is a task
  // list and deliberately not a two-sided market.

  /** a store at a path, or in memory for ":memory:" — the two-node
   * lane opens its own handle on the same log */
  def storeOf(path: String): okay.persist.Store = Board.store(path)

  /**
   * ONE handle on the log, opened once.
   *
   * Both the board and okay-ops read it, and opening it twice in one
   * process is what stopped two nodes booting on a shared directory —
   * found by the two-node test, which is the only place two processes
   * meet one log.
   */
  lazy val boardStore: okay.persist.Store =
    Board.store(sys.env.getOrElse("OKAY_CHAT_DB", "okay-board.log"))
  def opsStore: okay.persist.Store = boardStore

  /**
   * The production board, durable by default — and NOT what the
   * routes read.
   *
   * The board arrives as a context parameter instead, which is the
   * demo's own ctx-wiring claim made good: `main` wires this one, a
   * test wires a memory board, and the same `routes` value serves
   * both. A global the routes reached for directly would make every
   * test share one file and every run inherit the last one's tasks.
   */
  lazy val board: Board =
    val b = Board(Board.topicOf(boardStore))
    b.replay(): Unit
    b

  /** every open page, told when the board moves */
  private val feed = Hub[String]()
  def boardSub(): Channel[String] = feed.subscribe()
  def boardChanged(kind: String): Unit = feed.publish(kind)

  /** the per-person inbox an assignment rings (demo-chat-async) */
  private val inboxes = Registry[String, String]()
  def inbox(email: String): Channel[String] = inboxes(email)

  private val turnNo = java.util.concurrent.atomic.AtomicLong(0)

  /**
   * The tool table, with the notification wrapped around `assign`.
   *
   * The demo's async claim in one place: a tool call is what somebody
   * DID, and the person it happened TO is not in the room. Assigning a
   * task rings their inbox, and an open page hears it over SSE.
   */
  def boardTable(b: Board): Map[String, okay.agent.ToolCall => String] =
    val base = BoardTools.table(b)
    base.updated("board_assign", { c =>
      val out = base("board_assign")(c)
      Json.parse(out) match
        case JObj(fs) if fs.exists(_._1 == "id") =>
          for
            who <- fs.collectFirst { case ("assignee", JStr(v)) => v }
            what <- fs.collectFirst { case ("text", JStr(v)) => v }
          do inbox(who).offer(s"вам поручили: $what"): Unit
        case _ => ()
      out
    })

  /**
   * An agent turn over the board tools: the LLM turns a sentence into
   * tool calls, and the tools are the only way it can touch anything.
   *
   * The demo's central claim, and the reason it needs a domain at all.
   * The model never writes to the board; it calls `board_add` and the
   * board writes to its log. What it cannot do is invent a task that
   * is not there, because there is no path from a sentence to the
   * projection that does not go through a tool.
   */
  def agentTurn(b: Board, text: String, history: Seq[Anthropic.Message],
                modelH: okay.Handler[AgentModel], who: Option[String] = None): String =
    val system = boardSystem + who.fold("")(e =>
      s"\nThe speaker is a SIGNED-IN session as $e — use this as the owner, " +
      "even if the message names somebody else.")
    given okay.Handler[AgentModel] = modelH
    given okay.Handler[Tool] = Handlers.tools(boardTable(b))
    val ctx = Handlers.context(Compact.all)._2
    given okay.Handler[AgentContext] = ctx
    given r1: okay.Handler[AgentModel + Async] = okay.Handler.union[AgentModel, Async]
    given r2: okay.Handler[AgentContext + (AgentModel + Async)] =
      okay.Handler.union[AgentContext, AgentModel + Async]
    given r3: okay.Handler[Tool + (AgentContext + (AgentModel + Async))] =
      okay.Handler.union[Tool, AgentContext + (AgentModel + Async)]
    import okay.Direct.*
    def seed(ms: List[Anthropic.Message]): Unit ! okay.agent.Agent = ms match
      case Nil => pure(())
      case m :: rest =>
        Agent.remember(
          if m.role == "user" then Turn.User(m.content)
          else Turn.Assistant(m.content)).flatMap(_ => seed(rest))
    val prog = direct[[A] =>> A ! okay.agent.Agent] {
      Agent.remember(Turn.System(system)).reflect
      seed(history.toList).reflect
      Agent.converse(text, BoardTools.specs).reflect
    }
    prog.runWith

  private val boardSystem =
    """You keep a shared task board. Use the tools and nothing else: board_add to
      |put a task on it, board_list to read it, board_assign to give one to
      |somebody, board_done to finish one. Answer in the language the user wrote
      |in. Never claim a task exists unless a tool said so.""".stripMargin

  /**
   * The board WITHOUT a model, which is what runs by default.
   *
   * The offline mode is the demo: the same tool table, driven by a
   * handful of recognised commands instead of by an LLM, so the
   * application always runs and the tests exercise the same tools the
   * key exercises. It answers in the language it was addressed in,
   * because a demo that only speaks English is a demo of English.
   */
  def offlineTurn(b: Board, text: String, who: Option[String]): String =
    val owner = who.getOrElse("guest")
    val ru = text.exists(c => c >= 'а' && c <= 'я' || c == 'ё')
    def list: String =
      val ts = b.all
      if ts.isEmpty then (if ru then "доска пуста" else "the board is empty")
      else ts.map(t => s"${t.id}) ${t.text}" +
        t.assignee.fold("")(a => (if ru then s" — на $a" else s" — $a")) +
        (if t.done then (if ru then " ✓" else " (done)") else "")).mkString("\n")
    val add = "(?iU)^(?:добавь|добавить|add)\\s+(.+)$".r
    val assign = "(?iU)^(?:поручи|assign)\\s+(\\d+)\\s+(.+)$".r
    val done = "(?iU)^(?:готово|сделано|done)\\s+(\\d+)$".r
    text.trim match
      case add(what) => b.add(what, owner)
        .fold(if ru then "не получилось" else "could not add")(t =>
          (if ru then s"добавил ${t.id}) ${t.text}" else s"added ${t.id}) ${t.text}"))
      case assign(id, whom) => boardTable(b)("board_assign")(
        okay.agent.ToolCall("1", "board_assign", JObj(Vector(
          "id" -> JNum(id.toDouble), "who" -> JStr(whom.trim)))))
        match
          case out if out.contains("error") =>
            if ru then s"нет задачи $id" else s"no task $id"
          case _ => if ru then s"поручил $id: $whom" else s"assigned $id to $whom"
      case done(id) => b.complete(id.toLong)
        .fold(if ru then s"нет задачи $id" else s"no task $id")(t =>
          if ru then s"готово: ${t.text}" else s"done: ${t.text}")
      case _ => list

  /** the board as an MCP server — the same operations, another door */
  def mcpRoute(b: Board): Request => Response ! Async =
    McpHttp.route(McpServer.Serving(
      info = Mcp.Info("okay-demo-board", "0.1.0"),
      tools = BoardTools.specs,
      call = boardTable(b)))

  // ---- the streaming content cut (demo-streaming-cut) -----------------
  //
  // okay-chat's `reply`/`chatRoute` guard the token BUDGET; this is
  // the demo's own EXTRA rule, riding the same `policy` seam
  // (specs/llm-agentic.md, llm-streaming-cut) — a stand-in for
  // "off-policy content," proving the mechanism cuts on what a
  // stream SAYS, not only how much of it there is. Demonstrable
  // OFFLINE: `Chat.scripted` ECHOES the user's own message, so
  // typing a banned word is itself the trigger — no separate
  // demo-only model needed.

  private val bannedWords = Set("секрет", "пароль")

  def contentPolicy(i: Int, token: String): Option[Cut.Violation] =
    bannedWords.find(w => token.toLowerCase.contains(w))
      .map(_ => Cut.Violation("content-policy", i, token))

  // ---- the routes ----------------------------------------------------

  def routes(m: Chat.Model, budget: Int)(using Transport, Secrets, Board)
  : PartialFunction[Request, Response ! Async] =
    val board = summon[Board]
    // built ONCE per server, not per request — McpHttp keeps its
    // session table inside the route, and a session issued on one
    // request must still be found on the next
    val mcpR: Request => Response ! Async = mcpRoute(board)
    val core: PartialFunction[Request, Response ! Async] = {
    case r if r.method == okay.http.Method.Get && r.url == "/" =>
      val html = (if Chat.appJs.isDefined then reactPage else page)
        .replace("MODE", Chat.modeName)
      pure(Response(200, Seq("content-type" -> "text/html; charset=utf-8"),
        Http.one(html.getBytes(UTF_8))))

    case r if r.method == okay.http.Method.Get && r.url == "/board" =>
      // server-rendered at load (it works without JS), then re-rendered
      // from /board.json on every feed ping
      def rows = board.all.map(t =>
        s"<li>${t.id}) ${t.text}" +
          t.assignee.fold("")(a => s" <span style='color:#7a869c'>— $a</span>") +
          (if t.done then " ✓" else "") + "</li>").mkString
      pure(Response(200, Seq("content-type" -> "text/html; charset=utf-8"),
        Http.one(s"""<!doctype html><meta charset="utf-8"><title>board</title>
          |<style>body{font:15px system-ui;background:#10141a;color:#e6e9ef;padding:2rem}
          |h2{color:#7a869c} li{margin:.2rem 0}
          |button{background:#2a3342;color:#e6e9ef;border:0;padding:.5rem .9rem;border-radius:.6rem;cursor:pointer}</style>
          |<h2>the board</h2><ul id="tasks">$rows</ul>
          |<p><a style="color:#6b9fff" href="/">← to the chat</a> · live</p>
          |<button id="replay">rebuild from the log</button>
          |<span style="color:#7a869c;font-size:.85em"> — drop the projection and derive it again from the durable log (needs an admin token)</span>
          |<script>
          |async function render() {
          |  const d = await (await fetch('/board.json')).json();
          |  document.getElementById('tasks').innerHTML = d.tasks.map(t =>
          |    '<li>' + t.id + ') ' + t.text +
          |    (t.assignee ? " <span style='color:#7a869c'>— " + t.assignee + '</span>' : '') +
          |    (t.done ? ' ✓' : '') + '</li>').join('');
          |}
          |new EventSource('/events/board').addEventListener('board', render);
          |document.getElementById('replay').onclick = async () => {
          |  const t = prompt('admin token'); if (!t) return;
          |  await fetch('/admin/replay', {method:'POST', headers:{authorization:'Bearer ' + t}});
          |  render();
          |};
          |</script>""".stripMargin.getBytes(UTF_8))))

    case r if r.method == okay.http.Method.Get && r.url == "/board.json" =>
      pure(Response(200, Seq("content-type" -> "application/json"),
        Http.one(Json.print(JObj(Vector("tasks" -> JArr(board.all.map(t => JObj(Vector(
          "id" -> JNum(t.id.toDouble), "text" -> JStr(t.text), "owner" -> JStr(t.owner),
          "assignee" -> t.assignee.map(JStr(_)).getOrElse(JNull),
          "done" -> JBool(t.done)))))))).getBytes(UTF_8))))

    case r if r.url == "/mcp" => mcpR(r)

    case r if r.method == okay.http.Method.Get && r.url == "/app.js" && Chat.appJs.isDefined =>
      pure(Response(200, Seq("content-type" -> "text/javascript"),
        Http.one(java.nio.file.Files.readAllBytes(Chat.appJs.get))))

    case r if r.method == okay.http.Method.Get && r.url == "/events/board" =>
      // the board-wide feed — matched BEFORE the /events/<email>
      // prefix route: "board" must not parse as an email
      val src: Source[Chunk[Byte]] =
        effect[Writer % Chunk[Byte] + Async, Unit](Writer(Chat.sse("hello", "")))
          .flatMap(_ => Writer.map(Writer.of(boardSub()))(kind =>
            Chat.sse("board", Json.print(JStr(kind)))))
      pure(Response(200, Seq("content-type" -> "text/event-stream"), src))

    case r if r.method == okay.http.Method.Get && r.url.startsWith("/events/") =>
      // the email rides the PATH: requestOf keeps the path only, a
      // query string never reaches the route (found the hard way)
      val email = java.net.URLDecoder.decode(r.url.stripPrefix("/events/"), "UTF-8")
      // the inbox as a LIVE stream: jetty holds it open, and a task
      // assigned tomorrow becomes a frame then
      val src: Source[Chunk[Byte]] =
        effect[Writer % Chunk[Byte] + Async, Unit](Writer(Chat.sse("hello", "")))
          .flatMap(_ => Writer.map(Writer.of(inbox(email)))(note =>
            Chat.sse("note", Json.print(JStr(note)))))
      pure(Response(200, Seq("content-type" -> "text/event-stream"), src))

    case r if Ops.routes(opsStore).isDefinedAt(r) => Ops.routes(opsStore)(r)

    case r if r.method == okay.http.Method.Post && r.url == "/login" =>
      val email = Chat.fieldOf(r.body, "email")
      if email.isEmpty then
        pure(Response(400, Seq("content-type" -> "application/json"),
          Http.one(Json.print(JObj(Vector("error" -> JStr("email required")))).getBytes(UTF_8))))
      else
        val code = Login.start(email)
        println(s"login code for $email: $code (10 min)")
        // no email transport exists in this stack yet (specs/security.md):
        // the code rides the response so the demo is usable end to end;
        // real delivery replaces this ONE field with silence
        pure(Response(200, Seq("content-type" -> "application/json"),
          Http.one(Json.print(JObj(Vector("sent" -> JBool(true), "devCode" -> JStr(code)))).getBytes(UTF_8))))

    case r if r.method == okay.http.Method.Post && r.url == "/login/confirm" =>
      val email = Chat.fieldOf(r.body, "email")
      val code = Chat.fieldOf(r.body, "code")
      if Login.confirm(email, code) then
        val token = Login.issue(email)
        pure(Response(200, Seq("content-type" -> "application/json"),
          Http.one(Json.print(JObj(Vector("ok" -> JBool(true), "email" -> JStr(email), "token" -> JStr(token)))).getBytes(UTF_8))))
      else
        pure(Response(401, Seq("content-type" -> "application/json"),
          Http.one(Json.print(JObj(Vector("ok" -> JBool(false), "error" -> JStr("wrong or expired code")))).getBytes(UTF_8))))

    }
    // /chat itself is okay-chat (extracted 2026-09-02, specs/chat.md):
    // a "/board ..." turn rides the turnOverride seam rather than a
    // hardcoded prefix check inside the route
    val boardTurnOverride: Chat.TurnOverride = (r, messages) =>
      val last = messages.lastOption.map(_.content).getOrElse("")
      Option.when(last.startsWith("/board")) {
        // a verified session identifies the speaker over anything the
        // message text claims
        val who = Secure.bearerToken(r).flatMap(Login.verify(_))
        val answer = offlineTurn(board, last.stripPrefix("/board").trim, who)
        def stream(ts: List[String]): Unit ! (Writer % String + Async) = ts match
          case Nil => pure(())
          case t :: rest => effect[Writer % String + Async, Unit](
            Writer(t + " ")).flatMap(_ => stream(rest))
        Chat.reply(_ => stream(answer.split(' ').toList), budget)(messages)
      }
    // admin routes are okay-admin (extracted 2026-09-02, specs/admin.md):
    // /admin/replay is not reachable without an admin token
    core.orElse(Chat.chatRoute(m, budget, boardTurnOverride, contentPolicy))
      .orElse(Admin.routes(Admin.Issuer.verify)(
        () => board.replay(), () => boardChanged("replay")))

  /** the whole demo as ONE value awaiting its environment
   * (demo-ctx-wiring): `main` wires production, a test wires stubs —
   * the same value both times, and a missing capability is a compile
   * error, not a container exception */
  def handler(budget: Int)
  : (Transport, Secrets, Board) ?=> PartialFunction[Request, Response ! Async] =
    routes(Chat.model, budget)

  def main(args: Array[String]): Unit =
    val port = sys.env.get("OKAY_CHAT_PORT").flatMap(_.toIntOption).getOrElse(8090)
    val budget = sys.env.get("OKAY_CHAT_MAX").flatMap(_.toIntOption).getOrElse(512)
    val mode =
      if sys.env.contains("ANTHROPIC_API_KEY") then "live"
      else if sys.env.contains("OKAY_CHAT_BASE") then s"local ${sys.env("OKAY_CHAT_BASE")}"
      else "scripted"
    // OKAY_CHAT_NODE (demo-two-nodes): absent, everything below is
    // exactly as it always was — every existing test constructs
    // routes(...) directly and never sets this env var
    val node = sys.env.get("OKAY_CHAT_NODE")
    provide(Transports.http(), Secrets.env, board)(Resource.run[Unit, Pure](
      Jetty.serve(port)(node match
        case Some(n) =>
          val logDir = sys.env.getOrElse("OKAY_CHAT_LOG", "okay-chat.log")
          val tickMs = sys.env.get("OKAY_CHAT_TICK_MS").flatMap(_.toLongOption).getOrElse(500L)
          val leaseMs = sys.env.get("OKAY_CHAT_LEASE_MS").flatMap(_.toLongOption).getOrElse(5000L)
          val twoNode = TwoNode(java.nio.file.Path.of(logDir), n, tickMs, leaseMs)
          TwoNode.leaderGated(twoNode)(routes(Chat.model, budget))
        case None => handler(budget)
      )().map { s =>
        println(s"chat: http://127.0.0.1:${Jetty.port(s)}  (model: $mode)")
        node.foreach(n => println(s"two-node: $n — leader status at /whoami"))
        // no delivery channel yet (same limit Login.start states about
        // its one-time code) — the admin token rides the console
        println(s"admin token (okay-admin, /admin/replay): ${Admin.Issuer.issue()}")
        Thread.sleep(Long.MaxValue)   // ctrl-c ends the process and the Resource
      }).runWith)

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
<div style="max-width:640px;margin:0 auto;padding:.6rem 1rem;color:#7a869c;font-size:.85em">режим: MODE — маркетплейс: <a style="color:#6b9fff" href="/market">/market</a></div>
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
  <div id="status" style="color:#7a869c;font-size:.85em"></div>
  <div id="login" style="display:flex;gap:.4rem;align-items:center;font-size:.85em;color:#9fb0c8;margin:.3rem 0"></div>
  <div id="chips" style="display:flex;gap:.4rem;flex-wrap:wrap;margin:.4rem 0"></div>
  <div id="log"></div>
  <form id="f"><input id="i" autocomplete="off" placeholder="say something — or /match умею класть плитку email me@x"><button>send</button></form>
</main>
<script>
const log = document.getElementById('log'), f = document.getElementById('f'), i = document.getElementById('i');
const history = [];
let subscribed = false;
document.getElementById('status').textContent = 'режим: MODE — маркетплейс: /market';
const examples = [
  'умею класть плитку email tiler@x',
  'нужен сантехник email client@x',
  'спроси всех email client@x',
  'какая столица Франции?',
  'расскажи про секрет'];
const chips = document.getElementById('chips');
for (const ex of examples) {
  const b = document.createElement('button');
  b.type = 'button'; b.textContent = ex;
  b.style.cssText = 'font-size:.8em;background:#1d2430;border:1px solid #2a3342;color:#9fb0c8';
  b.onclick = () => { i.value = ex; i.focus(); };
  chips.appendChild(b);
}
function subscribe(email) {
  if (subscribed) return; subscribed = true;
  const es = new EventSource('/events/' + encodeURIComponent(email));
  es.addEventListener('match', ev => {
    const d = bubble('bot'); d.textContent = '🔔 ' + JSON.parse(ev.data);
    log.scrollTop = log.scrollHeight;
  });
}

// real login (demo-sessions): confirm-and-sign, not trust-the-field —
// the signed token then rides every /chat call as a Bearer header
const loginBox = document.getElementById('login');
function authHeaders() {
  const t = localStorage.getItem('okay_token');
  return t ? {authorization: 'Bearer ' + t} : {};
}
function renderLogin() {
  const email = localStorage.getItem('okay_email');
  loginBox.innerHTML = '';
  if (email) {
    loginBox.append('вошли как ' + email + ' ');
    const out = document.createElement('button');
    out.type = 'button'; out.textContent = 'выйти';
    out.onclick = () => { localStorage.removeItem('okay_token'); localStorage.removeItem('okay_email'); renderLogin(); };
    loginBox.appendChild(out);
    subscribe(email);
  } else {
    const ei = document.createElement('input'); ei.placeholder = 'email'; ei.style.cssText = 'width:11rem;padding:.3rem .5rem';
    const lb = document.createElement('button'); lb.type = 'button'; lb.textContent = 'войти';
    lb.onclick = async () => {
      if (!ei.value.trim()) return;
      await fetch('/login', {method: 'POST', headers: {'content-type': 'application/json'},
        body: JSON.stringify({email: ei.value.trim()})});
      const ci = document.createElement('input'); ci.placeholder = 'код из консоли сервера'; ci.style.cssText = 'width:11rem;padding:.3rem .5rem';
      const cb = document.createElement('button'); cb.type = 'button'; cb.textContent = 'подтвердить';
      cb.onclick = async () => {
        const res = await fetch('/login/confirm', {method: 'POST', headers: {'content-type': 'application/json'},
          body: JSON.stringify({email: ei.value.trim(), code: ci.value.trim()})});
        const d = await res.json();
        if (d.ok) { localStorage.setItem('okay_token', d.token); localStorage.setItem('okay_email', d.email); renderLogin(); }
        else { cb.textContent = 'не то — ещё раз'; }
      };
      loginBox.innerHTML = ''; loginBox.append('код для ' + ei.value.trim() + ': '); loginBox.appendChild(ci); loginBox.appendChild(cb);
    };
    loginBox.appendChild(ei); loginBox.appendChild(lb);
  }
}
renderLogin();
function bubble(cls) { const d = document.createElement('div'); d.className = 'm ' + cls; log.appendChild(d); return d; }
f.onsubmit = async (ev) => {
  ev.preventDefault();
  const text = i.value.trim(); if (!text) return;
  i.value = '';
  bubble('user').textContent = text;
  history.push({role: 'user', content: text});
  const em = text.match(/[\w.+-]+@[\w.-]+/); if (em) subscribe(em[0]);
  const bot = bubble('bot');
  const res = await fetch('/chat', {method: 'POST', headers: Object.assign({'content-type': 'application/json'}, authHeaders()),
    body: JSON.stringify({messages: history})});
  const reader = res.body.getReader(); const dec = new TextDecoder();
  let buf = '', answer = '', closed = false;
  for (;;) {
    const {done, value} = await reader.read(); if (done) break;
    buf += dec.decode(value, {stream: true});
    let idx;
    while ((idx = buf.indexOf('\n\n')) >= 0) {
      const frame = buf.slice(0, idx); buf = buf.slice(idx + 2);
      const ev = frame.match(/^event: (.*)$/m)?.[1] ?? 'data';
      const data = frame.match(/^data: (.*)$/m)?.[1] ?? '';
      if (ev === 'data') { answer += JSON.parse(data); bot.textContent = answer; }
      else if (ev === 'error') { const c = document.createElement('div'); c.className = 'cut';
        c.textContent = '⚠ ' + JSON.parse(data); bot.appendChild(c); closed = true; }
      else if (ev === 'cut') { const c = document.createElement('div'); c.className = 'cut';
        c.textContent = '✂ generation cut: ' + data; bot.appendChild(c); closed = true; }
      else if (ev === 'done') { closed = true; }
      log.scrollTop = log.scrollHeight;
    }
  }
  if (!closed) { const c = document.createElement('div'); c.className = 'cut';
    c.textContent = '⚠ поток оборвался — модель могла упасть'; bot.appendChild(c); }
  history.push({role: 'assistant', content: answer});
};
</script>"""
}
