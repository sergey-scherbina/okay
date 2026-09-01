package okay.demo

import okay.*
import okay.given
import okay.jetty.Jetty
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

  def withServer[A](budget: Int,
                    store: okay.matching.MatchStore = okay.matching.MemoryMatch())
                   (f: Int => A): A =
    provide(store)(Resource.run[A, Pure](
      Jetty.serve(0)(ChatDemo.routes(ChatDemo.scripted, budget))()
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
    assume(ChatDemo.appJs.isDefined, "no linked app (sbt okayChatWebJS/fastLinkJS) — skipped")
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

  test("LIVE: the local model on :8089 streams through the same route") {
    val base = sys.env.getOrElse("OKAY_CHAT_BASE", "http://127.0.0.1:8089")
    val up = try {
      client.send(HttpRequest.newBuilder(URI.create(s"$base/v1/models")).GET().build(),
        HttpResponse.BodyHandlers.ofString()).statusCode() == 200
    } catch { case _: Throwable => false }
    assume(up, s"no local model at $base — skipped")
    provide(okay.matching.MemoryMatch(): okay.matching.MatchStore)(Resource.run[Unit, Pure](
      Jetty.serve(0)(ChatDemo.routes(ChatDemo.local(base), 512))()
        .map { s =>
          val port = Jetty.port(s)
          val whole = new String(post(port,
            """{"messages":[{"role":"user","content":"answer with one short word"}]}""").readAllBytes(), UTF_8)
          assert(whole.contains("data: "), s"no tokens: ${whole.take(200)}")
          assert(whole.contains("event: done"), s"no done: ${whole.takeRight(120)}")
        }).runWith)
  }

  test("MATCH offline: a provider chats in, a seeker finds them — through the real route") {
    withServer(512) { port =>
      def turn(text: String): String =
        new String(post(port,
          s"""{"messages":[{"role":"user","content":"$text"}]}""").readAllBytes(), UTF_8)
      val stored = turn("/match умею класть плитку email tiler@demo")
      assert(stored.contains("записал"), stored.take(200))
      val found = turn("/match нужен плиточник")
      assert(found.contains("нашёл") && found.contains("1:"), found.take(300))
      assert(found.contains("плитку"), "the offer's skill surfaces in the answer")
      // and the marketplace REMEMBERS across turns and sessions: the
      // second seeker sees the same provider
      assert(turn("/match нужен мастер по плитке").contains("нашёл"))
    }
  }

  test("LIVE MATCH: the local model drives the okay-match tools end to end") {
    val base = sys.env.getOrElse("OKAY_CHAT_BASE", "http://127.0.0.1:8089")
    val up = try {
      client.send(HttpRequest.newBuilder(URI.create(s"$base/v1/models")).GET().build(),
        HttpResponse.BodyHandlers.ofString()).statusCode() == 200
    } catch { case _: Throwable => false }
    assume(up, s"no local model at $base — skipped")
    val store = okay.matching.MemoryMatch()
    val before = store.candidates(
      okay.matching.Query(okay.matching.Side.Offer, text = "weld")).length
    val answer = provide(store)(ChatDemo.agentTurn(
      "I can weld metal gates, my email is welder@live-demo. Please store my offer.",
      Nil, okay.agent.Provider.openAi(
        okay.llm.Transports.http(), "local", "default", s"$base/v1/chat/completions")))
    assert(answer.nonEmpty)
    val after = store.candidates(
      okay.matching.Query(okay.matching.Side.Offer, text = "weld metal gates")).length
    assert(after > before || answer.toLowerCase.contains("email"),
      s"the agent neither stored the offer nor asked for what it lacked: $answer")
  }

  test("LIVE UNGATED: the model itself decides — an offer stores, small talk does not") {
    val base = sys.env.getOrElse("OKAY_CHAT_BASE", "http://127.0.0.1:8089")
    val up = try {
      client.send(HttpRequest.newBuilder(URI.create(s"$base/v1/models")).GET().build(),
        HttpResponse.BodyHandlers.ofString()).statusCode() == 200
    } catch { case _: Throwable => false }
    assume(up, s"no local model at $base — skipped")
    val store = okay.matching.MemoryMatch()
    def turn(text: String): String = provide(store)(ChatDemo.agentTurn(text, Nil,
      okay.agent.Provider.openAi(okay.llm.Transports.http(), "local", "default",
        s"$base/v1/chat/completions")))
    // an OFFER, no /match anywhere: the model should reach for the tools
    val a1 = turn("Я умею чинить велосипеды, почта bike@demo. Запиши моё предложение.")
    val stored = store.candidates(
      okay.matching.Query(okay.matching.Side.Offer, text = "велосипед")).nonEmpty
    assert(stored || a1.toLowerCase.contains("почт") || a1.toLowerCase.contains("email"),
      s"neither stored nor asked: $a1")
    // SMALL TALK: the marketplace must stay untouched
    val before = store.candidates(okay.matching.Query(okay.matching.Side.Offer, k = 100)).length
    turn("Какая столица Франции?")
    val after = store.candidates(okay.matching.Query(okay.matching.Side.Offer, k = 100)).length
    assertEquals(after, before, "small talk must not touch the marketplace")
  }

  test("LIVE SEEKER: 'найди мне кого-нибудь' finds the stored provider across two turns") {
    val base = sys.env.getOrElse("OKAY_CHAT_BASE", "http://127.0.0.1:8089")
    val up = try {
      client.send(HttpRequest.newBuilder(URI.create(s"$base/v1/models")).GET().build(),
        HttpResponse.BodyHandlers.ofString()).statusCode() == 200
    } catch { case _: Throwable => false }
    assume(up, s"no local model at $base — skipped")
    val store = okay.matching.MemoryMatch()
    val p = store.register("bike-master@demo")
    store.assert(p, "skill", okay.matching.Side.Offer,
      okay.matching.Value.VText("ремонт велосипедов, замена цепи и тормозов"),
      okay.matching.Provenance("seed", 1, "умею чинить велосипеды"), 1.0,
      okay.matching.Vis.Public)
    def modelH = okay.agent.Provider.openAi(okay.llm.Transports.http(),
      "local", "default", s"$base/v1/chat/completions")
    val q1 = "мне нужно починить велосипед, найди мне кого-нибудь"
    val a1 = provide(store)(ChatDemo.agentTurn(q1, Nil, modelH))
    // the intake may ask for the email first; the second turn supplies it
    val a2 = if a1.toLowerCase.contains("почт") || a1.toLowerCase.contains("email") then
      provide(store)(ChatDemo.agentTurn("моя почта seeker@demo",
        Vector(okay.llm.Anthropic.Message("user", q1),
               okay.llm.Anthropic.Message("assistant", a1)), modelH))
    else a1
    assert(a2.contains("велосипед"), s"the found provider's skill must surface: $a2")
  }

  test("REVERSE CHAIN: the need waits; the offer arriving later rings the seeker's inbox") {
    withServer(512) { port =>
      def turn(text: String): String =
        new String(post(port,
          s"""{"messages":[{"role":"user","content":"$text"}]}""").readAllBytes(), UTF_8)
      // window one, TODAY: the seeker asks; nobody fits yet
      val today = turn("/match нужен мастер починить велосипед email seeker@wait")
      assert(today.contains("запомнил"), today.take(300))
      // the seeker's page holds the inbox open
      val events = client.send(
        HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:$port/events/seeker@wait"))
          .GET().build(),
        HttpResponse.BodyHandlers.ofInputStream()).body()
      // window two, TOMORROW: the provider shows up
      turn("/match умею велосипед ремонт email master@late")
      // the chain fired: the waiting inbox receives the match, live —
      // read past the hello frame, under a watchdog
      val got = java.util.concurrent.CompletableFuture.supplyAsync { () =>
        val sb = new StringBuilder
        val buf = new Array[Byte](512)
        while !sb.toString.contains("event: match") do
          val n = events.read(buf)
          if n < 0 then throw new AssertionError(s"stream ended: $sb")
          sb.append(new String(buf, 0, n, UTF_8))
        sb.toString
      }.get(10, java.util.concurrent.TimeUnit.SECONDS)
      assert(got.contains("велосипед"), got)
      events.close()
    }
  }

  test("DEALS, jobs domain: choose, ask several, one declines, one accepts, the rest stand down") {
    withServer(512) { port =>
      def turn(text: String): String =
        new String(post(port,
          s"""{"messages":[{"role":"user","content":"$text"}]}""").readAllBytes(), UTF_8)
      def events(email: String) = client.send(
        HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:$port/events/$email"))
          .GET().build(), HttpResponse.BodyHandlers.ofInputStream()).body()
      def readUntil(in: java.io.InputStream, marker: String): String =
        java.util.concurrent.CompletableFuture.supplyAsync { () =>
          val sb = new StringBuilder; val buf = new Array[Byte](512)
          while !sb.toString.contains(marker) do
            val n = in.read(buf)
            if n < 0 then throw new AssertionError(s"stream ended: $sb")
            sb.append(new String(buf, 0, n, UTF_8))
          sb.toString
        }.get(10, java.util.concurrent.TimeUnit.SECONDS)

      // the THIRD domain: hiring — three developers offer their work
      turn("/match offer: разработчик scala, ищу проект email dev1@jobs")
      turn("/match offer: разработчик scala и котлин email dev2@jobs")
      turn("/match offer: разработчик джуниор scala email dev3@jobs")
      // the employer's need lists them NUMBERED
      val found = turn("/match need: нужен разработчик scala в команду email boss@jobs")
      assert(found.contains("1)") && found.contains("2)") && found.contains("3)"), found.take(400))
      assert(found.contains("спроси"), "the driver offers the choice")

      val boss = events("boss@jobs")
      val d1 = events("dev1@jobs"); val d2 = events("dev2@jobs"); val d3 = events("dev3@jobs")
      // the client CHOOSES — ask all three (someone will agree)
      val asked = turn("/match спроси всех email boss@jobs")
      assert(asked.contains("спросил") && asked.contains("3"), asked.take(300))
      val ask1 = readUntil(d1, "сделка")
      val ask2 = readUntil(d2, "сделка")
      readUntil(d3, "сделка")
      def dealNo(s: String): String = "сделка (\\d+)".r.findFirstMatchIn(s).get.group(1)

      // dev1 declines; the boss hears it
      turn(s"/match отказываюсь ${dealNo(ask1)} email dev1@jobs")
      readUntil(boss, "отказался")
      // dev2 accepts: the boss gets the CONTACT (the Matched unlock),
      // dev3 gets the stand-down (withdrawn), and cannot accept anymore
      turn(s"/match берусь ${dealNo(ask2)} email dev2@jobs")
      val won = readUntil(boss, "согласился")
      assert(won.contains("dev2@jobs"), s"the unlocked contact must surface: $won")
      readUntil(d3, "отбой")   // the unchosen-anymore hears the stand-down
      // (that a withdrawn ask cannot be accepted is the engine's
      // guarantee, proven in TestMatch — not re-proven over SSE)
      Seq(boss, d1, d2, d3).foreach(_.close())
    }
  }

  test("FLOWS in the demo: a transition's notifications reach the role inboxes, filled") {
    val store = okay.matching.MemoryMatch()
    given okay.matching.MatchStore = store
    val t = ChatDemo.chainedTable
    import okay.codec.Json
    def call(name: String, args: (String, Json)*): String =
      t(name)(okay.agent.ToolCall("t", name, Json.JObj(args.toVector)))
    val seeker = store.register("flow-seeker@x")
    val provider = store.register("flow-prov@x")
    store.assert(provider, "contact", okay.matching.Side.Offer,
      okay.matching.Value.VText("tg:@prov"),
      okay.matching.Provenance("c", 1, "..."), 1.0, okay.matching.Vis.Matched)
    val Right(id) = store.startFlow("deal",
      Map("seeker" -> seeker, "provider" -> provider), "полка"): @unchecked
    // the provider accepts THROUGH THE TOOL — the seeker's inbox rings
    val inb = ChatDemo.inbox("flow-seeker@x")
    call("flow_advance", "flow" -> Json.JNum(id.n.toDouble),
      "transition" -> Json.JStr("accept"), "by" -> Json.JStr(provider.uuid))
    val note = java.util.concurrent.CompletableFuture.supplyAsync { () =>
      var r: Option[String] = None
      while r.isEmpty do r = inb.receive()
      r.get
    }.get(5, java.util.concurrent.TimeUnit.SECONDS)
    assertEquals(note, "исполнитель согласился: полка")
    // and the unlock is queryable the generic way
    assertEquals(store.unlockedBy(seeker, provider).map(f =>
      okay.matching.Value.text(f.value)), Vector("tg:@prov"))
  }

  test("OFFLINE FLOWS: the driver plays ANY registered scenario by phrases alone") {
    val store = okay.matching.MemoryMatch()
    // register the three-role escrow scenario (configuration)
    val sale = okay.matching.ScenarioDef(
      name = "escrow-sale",
      roles = Vector("buyer", "seller", "escrow"),
      initial = "offered",
      states = Vector("offered", "under-contract", "funded", "closed"),
      terminal = Set("closed"),
      transitions = Vector(
        okay.matching.Transition("sign", "offered", "under-contract", by = "seller",
          notifies = Vector("buyer" -> "продавец подписал: {what}")),
        okay.matching.Transition("fund", "under-contract", "funded", by = "buyer"),
        okay.matching.Transition("release", "funded", "closed", by = "escrow",
          notifies = Vector("buyer" -> "сделка закрыта: {what}"))))
    assertEquals(store.defineScenario(sale), Vector.empty)
    withServer(512, store) { port =>
      def turn(text: String): String =
        new String(post(port,
          s"""{"messages":[{"role":"user","content":"$text"}]}""").readAllBytes(), UTF_8)
      val started = turn(
        "/match сценарий escrow-sale buyer=b@x seller=s@x escrow=e@x email b@x")
      assert(started.contains("начат"), started.take(300))
      val flowN = "флоу (\\d+)".r.findFirstMatchIn(started).map(_.group(1))
        .getOrElse("1")
      // the buyer holds the seller's transition — refused with the reason
      assert(turn(s"/match шаг $flowN sign email b@x").contains("отказ"), "role enforced")
      // each party fires its own step by phrase; the buyer's page rings
      val buyer = client.send(
        HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:$port/events/b@x")).GET().build(),
        HttpResponse.BodyHandlers.ofInputStream()).body()
      assert(turn(s"/match шаг $flowN sign email s@x").contains("under-contract"))
      assert(turn(s"/match шаг $flowN fund email b@x").contains("funded"))
      assert(turn(s"/match шаг $flowN release email e@x").contains("closed"))
      val heard = java.util.concurrent.CompletableFuture.supplyAsync { () =>
        val sb = new StringBuilder; val buf = new Array[Byte](512)
        while !sb.toString.contains("закрыта") do
          val n = buyer.read(buf)
          if n < 0 then throw new AssertionError(s"ended: $sb")
          sb.append(new String(buf, 0, n, UTF_8))
        sb.toString
      }.get(10, java.util.concurrent.TimeUnit.SECONDS)
      assert(heard.contains("подписал") || heard.contains("закрыта"))
      val hist = turn(s"/match флоу $flowN")
      assert(hist.contains("sign") && hist.contains("fund") && hist.contains("release"), hist.take(300))
      buyer.close()
    }
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
}
