package okay.demo

import okay.*
import okay.given
import okay.jetty.Jetty
import okay.mcp.{Client, Mcp, Session as McpSession}
import okay.agent.ToolCall
import okay.codec.Json
import okay.codec.Json.*
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.URI
import java.nio.charset.StandardCharsets.UTF_8

/**
 * specs/demo-chat.md, Behavior — over a REAL socket: the server the
 * demo runs is the server the test hits, scripted model, live jetty
 * streaming. (The live-key box is the TestLive pattern: skipped
 * here, exercised by running the main with ANTHROPIC_API_KEY.)
 */
class TestChatDemo extends munit.FunSuite {
  // live turns are model-speed-bound; a busy local model must not flake
  // live model calls under a loaded matrix outgrow munit's 30s —
  // the TestRepoAgent precedent; 180 covers a busy local model
  override val munitTimeout = scala.concurrent.duration.Duration(180, "s")

  /** a JUDGMENT assertion against a small live model is stochastic:
   * one retry of the whole turn cuts the flake quadratically, and a
   * consistent failure still fails (demo-live-judgment-flake) */
  def judged[A](attempt: => A)(ok: A => Boolean): A =
    val first = attempt
    if ok(first) then first else attempt

  /** a LIVE test needs the local model gateway; without it the test
   * is not a failure, it is not applicable — and a gateway that is
   * up at the probe and drops the connection mid-turn (a shared
   * gateway under load: "HTTP/1.1 header parser received no bytes")
   * is absent in the same sense. Anything from the wire — an
   * IOException anywhere in the cause chain — SKIPS the test, named;
   * a judgment failure still fails. Tagged `Live` (integration-test-
   * gate): a real model answering is real work, but its RESULT is
   * not something `sbt test` can hold a landing to — out of the
   * default gate, into `sbt integrationTest`. */
  def liveTest(name: String)(body: => Any): Unit = test(name.tag(new munit.Tag("Live"))) {
    try body
    catch case e: Throwable if okay.llm.Live.wireDropped(e) =>
      assume(false, s"the local model gateway went away mid-test (${okay.llm.Live.root(e).getMessage}) — skipped")
  }

  /** the offline environment (demo-ctx-wiring): a wire that PROVES
   * offline never reaches it, and secrets holding no model config */
  val deadWire: okay.llm.Transport = (url, _, _) =>
    throw new AssertionError(s"offline test touched the wire: $url")
  val noSecrets: okay.conf.Secrets = okay.conf.Secrets.memory(Map.empty)

  /** a board per server: a test that shared one file with every other
   * test would inherit their tasks and prove nothing */
  def memoryBoard: Board = Board(Board.topicOf(Board.store(":memory:")))

  def withServer[A](budget: Int, board: Board = memoryBoard)(f: Int => A): A =
    provide(deadWire, noSecrets, board)(Resource.run[A, Pure](
      Jetty.serve(0)(ChatDemo.routes(okay.chat.Chat.scripted, budget))()
        .map(s => f(Jetty.port(s)))).runWith)

  val client = HttpClient.newHttpClient()

  def post(port: Int, body: String): java.io.InputStream =
    client.send(
      HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:$port/chat"))
        .header("content-type", "application/json")
        .POST(HttpRequest.BodyPublishers.ofString(body)).build(),
      HttpResponse.BodyHandlers.ofInputStream()).body()

  test("the scripted reply streams token by token and ends with done") {
    withServer(budget = 512) { port =>
      val in = post(port, """{"messages":[{"role":"user","content":"hello okay"}]}""")
      // read INCREMENTALLY: frames must be available before the end
      val first = new Array[Byte](16)
      val n = in.read(first)
      assert(n > 0, "nothing streamed")
      assert(new String(first, 0, n, UTF_8).startsWith("data: "),
        "the first frame is a token event")
      val rest = new String(in.readAllBytes(), UTF_8)
      val whole = new String(first, 0, n, UTF_8) + rest
      // tokens are per-frame; the words arrive in separate data events
      assert(whole.contains("hello") && whole.contains("okay"),
        "the scripted reply echoes the message")
      assert(whole.trim.endsWith("event: done\ndata: ") ||
        whole.contains("event: done"), s"missing done: ${whole.takeRight(80)}")
      assert(!whole.contains("event: cut"))
    }
  }

  test("the page serves and carries the client script") {
    withServer(512) { port =>
      val res = client.send(
        HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:$port/")).GET().build(),
        HttpResponse.BodyHandlers.ofString())
      assertEquals(res.statusCode(), 200)
      assert(res.body().contains("okay chat"))
      // whichever face: the vanilla script inline, or the React shell
      assert(res.body().contains("fetch('/chat'") || res.body().contains("/app.js"))
    }
  }

  test("the React page serves when the linked app exists, with CDN React and /app.js") {
    assume(okay.chat.Chat.appJs.isDefined, "no linked app (sbt okayChatWebJS/fastLinkJS) — skipped")
    withServer(512) { port =>
      val res = client.send(
        HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:$port/")).GET().build(),
        HttpResponse.BodyHandlers.ofString())
      assert(res.body().contains("react.production.min.js"))
      assert(res.body().contains("/app.js"))
      val app = client.send(
        HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:$port/app.js")).GET().build(),
        HttpResponse.BodyHandlers.ofString())
      assertEquals(app.statusCode(), 200)
      assert(app.body().contains("okay chat"), "the linked app carries the chat view")
    }
  }

  liveTest("LIVE: the local model on :8089 streams through the same route") {
    val base = sys.env.getOrElse("OKAY_CHAT_BASE", "http://127.0.0.1:8089")
    val up = try {
      client.send(HttpRequest.newBuilder(URI.create(s"$base/v1/models")).GET().build(),
        HttpResponse.BodyHandlers.ofString()).statusCode() == 200
    } catch { case _: Throwable => false }
    assume(up, s"no local model at $base — skipped")
    provide(okay.llm.Transports.http(), noSecrets, memoryBoard)(Resource.run[Unit, Pure](
      Jetty.serve(0)(ChatDemo.routes(okay.chat.Chat.local(base), 512))()
        .map { s =>
          val port = Jetty.port(s)
          val whole = new String(post(port,
            """{"messages":[{"role":"user","content":"answer with one short word"}]}""").readAllBytes(), UTF_8)
          assert(whole.contains("data: "), s"no tokens: ${whole.take(200)}")
          assert(whole.contains("event: done"), s"no done: ${whole.takeRight(120)}")
        }).runWith)
  }
















  test("CTX WIRING: one handler value, two environments — LIVE parsing over a canned wire, scripted without a key") {
    // the canned wire: ANY post answers a fixed Anthropic SSE stream,
    // so the REAL Anthropic.stream parsing runs with no server anywhere
    val canned: okay.llm.Transport = (_, _, _) =>
      type F = Writer % String + Async
      Seq(
        """data: {"type":"content_block_delta","delta":{"text":"canned"}}""", "",
        """data: {"type":"content_block_delta","delta":{"text":" wire"}}""", "",
        "data: [DONE]", "")
        .foldLeft(pure(()): Unit ! F)((acc, l) =>
          acc.flatMap(_ => effect[F, Unit](Writer(l))))
    def run(wire: okay.llm.Transport, secrets: okay.conf.Secrets): String =
      provide(wire, secrets, memoryBoard)(
        Resource.run[String, Pure](
          Jetty.serve(0)(ChatDemo.handler(512))().map { s =>
            new String(post(Jetty.port(s),
              """{"messages":[{"role":"user","content":"hi"}]}""").readAllBytes(), UTF_8)
          }).runWith)
    // wired LIVE: memory-secrets hold the key, dispatch picks the live
    // branch, Anthropic.stream parses the canned SSE — offline
    val live = run(canned,
      okay.conf.Secrets.memory(Map("env:ANTHROPIC_API_KEY" -> "sk-canned")))
    assert(live.contains("canned") && live.contains("wire"), live.take(300))
    assert(live.contains("event: done"), live.takeRight(120))
    // the SAME value wired with no key: the scripted branch answers,
    // and the dead wire proves the dispatch never touched a transport
    // tokens are per-frame: the words arrive in separate data events
    val scripted = run(deadWire, noSecrets)
    assert(scripted.contains("You") && scripted.contains("said:"), scripted.take(300))
  }

  test("over budget the stream is cut, named, and no tokens follow") {
    withServer(budget = 3) { port =>
      val whole = new String(post(port,
        """{"messages":[{"role":"user","content":"anything"}]}""").readAllBytes(), UTF_8)
      val frames = whole.split("\n\n").toVector.filter(_.nonEmpty)
      val (tokens, after) = frames.span(!_.startsWith("event: cut"))
      assertEquals(tokens.length, 3, s"the budget is the cut point: $frames")
      assert(after.head.contains("token-budget"), "the rule is named")
      assertEquals(after.length, 1, "nothing follows the cut")
      assert(!whole.contains("event: done"))
    }
  }

  test("STREAMING CUT: a banned word in the echoed reply is cut, named content-policy, no tokens follow") {
    withServer(budget = 512) { port =>
      // scripted ECHOES the message — typing the banned word is the trigger
      val whole = new String(post(port,
        """{"messages":[{"role":"user","content":"расскажи про секрет"}]}""").readAllBytes(), UTF_8)
      val frames = whole.split("\n\n").toVector.filter(_.nonEmpty)
      val (tokens, after) = frames.span(!_.startsWith("event: cut"))
      assert(after.nonEmpty, s"expected a cut: $frames")
      assert(after.head.contains("content-policy"), "the rule is named")
      assertEquals(after.length, 1, "nothing follows the cut")
      assert(!whole.contains("event: done"))
      assert(!tokens.exists(_.contains("scripted")), s"no token past the violation must leak: $tokens")
    }
  }

  test("STREAMING CUT: a clean reply is unaffected — still ends with done, no content-policy") {
    withServer(budget = 512) { port =>
      val whole = new String(post(port,
        """{"messages":[{"role":"user","content":"hello okay"}]}""").readAllBytes(), UTF_8)
      assert(whole.contains("event: done"))
      assert(!whole.contains("content-policy"))
    }
  }

  // ---- the marketplace as an MCP server (demo-mcp-market) -------------

  def mcpClient(port: Int) =
    val link = okay.http.McpHttp.link(okay.http.Transports.http(), s"http://127.0.0.1:$port/mcp")
    Client.connect(link, Mcp.Info("test-client", "1")).runWith

  def mcpCall(session: McpSession, name: String, args: (String, Json)*): String =
    session.call(ToolCall("t", name, JObj(args.toVector))).runWith




  // ---- authoring scenarios (demo-scenario-editor) ----------------------





  // ---- the platform gate policy, live (demo-gate-ui) --------------------



  // PgTarget's own parsing behavior is proven in okay-pg's
  // TestPgTarget now (specs/sql.md) — moved 2026-09-02, it never had
  // a demo dependency. This test stays: it proves the DEMO'S OWN
  // wiring (marketOf -> SqlMatch over the live wire driver).



  private def postJson(port: Int, path: String, body: String, auth: Option[String] = None): (Int, String) =
    val b = HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:$port$path"))
      .header("content-type", "application/json")
    auth.foreach(t => b.header("authorization", s"Bearer $t"))
    val res = client.send(b.POST(HttpRequest.BodyPublishers.ofString(body)).build(),
      HttpResponse.BodyHandlers.ofString())
    (res.statusCode(), res.body())

  test("demo-sessions: confirm-and-sign — the login+confirm exchange mints a token, a wrong code is refused") {
    withServer(512) { port =>
      val (s1, b1) = postJson(port, "/login", """{"email":"ann@example.com"}""")
      assertEquals(s1, 200)
      val code = Json.parse(b1) match
        case JObj(fs) => fs.collectFirst { case ("devCode", JStr(c)) => c }.get
        case _ => fail(s"no devCode in $b1")
      val (wrongStatus, _) = postJson(port, "/login/confirm", s"""{"email":"ann@example.com","code":"000000"}""")
      assertEquals(wrongStatus, 401)
      val (s2, b2) = postJson(port, "/login/confirm", s"""{"email":"ann@example.com","code":"$code"}""")
      assertEquals(s2, 200)
      assert(Json.parse(b2).toString.contains("ann@example.com"))
    }
  }

  test("demo-sessions: a verified session is the identity of record — it overrides a DIFFERENT email typed in the message") {
    val board = memoryBoard
    withServer(512, board) { port =>
      val (_, b1) = postJson(port, "/login", """{"email":"real@example.com"}""")
      val code = Json.parse(b1) match
        case JObj(fs) => fs.collectFirst { case ("devCode", JStr(c)) => c }.get
        case _ => fail(s"no devCode in $b1")
      val (_, b2) = postJson(port, "/login/confirm", s"""{"email":"real@example.com","code":"$code"}""")
      val token = Json.parse(b2) match
        case JObj(fs) => fs.collectFirst { case ("token", JStr(t)) => t }.get
        case _ => fail(s"no token in $b2")
      client.send(
        HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:$port/chat"))
          .header("content-type", "application/json").header("authorization", s"Bearer $token")
          .POST(HttpRequest.BodyPublishers.ofString(
            """{"messages":[{"role":"user","content":"/board добавь покрасить дверь"}]}"""))
          .build(),
        HttpResponse.BodyHandlers.ofString())
      // the OWNER is the verified session, not anything the message
      // could have claimed about itself
      val mine = board.all.filter(_.text.contains("покрасить дверь"))
      assertEquals(mine.map(_.owner), Vector("real@example.com"))
    }
  }

  test("ops-monitoring: /healthz, /readyz, /stats, /metrics are wired into the demo's own routes") {
    withServer(512) { port =>
      val h = client.send(HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:$port/healthz")).GET().build(),
        HttpResponse.BodyHandlers.ofString())
      assertEquals(h.statusCode(), 200)
      assertEquals(h.body(), "live=true")
      // an empty store's /metrics is legitimately empty (no series
      // advertised for data that does not exist) — the wiring test's
      // job is that the ROUTE answers, not re-proving Prom.render
      // (okay-ops's own suite does that with real data)
      val m = client.send(HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:$port/metrics")).GET().build(),
        HttpResponse.BodyHandlers.ofString())
      assertEquals(m.statusCode(), 200)
      assert(m.headers().firstValue("content-type").orElse("").startsWith("text/plain; version=0.0.4"))
      // chatStore is a process-wide singleton other tests in this
      // suite touch too (/match traffic), so /stats' CONTENT is not
      // this test's to assert — only that the route answers, shaped
      val s2 = client.send(HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:$port/stats")).GET().build(),
        HttpResponse.BodyHandlers.ofString())
      assertEquals(s2.statusCode(), 200)
      assertEquals(s2.headers().firstValue("content-type").orElse(""), "application/json")
      assert(s2.body().contains("\"topics\""), s2.body())
    }
  }
}
