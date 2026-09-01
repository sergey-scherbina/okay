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

  def withServer[A](budget: Int,
                    store: okay.matching.MatchStore = okay.matching.MemoryMatch())
                   (f: Int => A): A =
    Resource.run[A, Pure](
      Jetty.serve(0)(ChatDemo.routes(ChatDemo.scripted, budget, store))()
        .map(s => f(Jetty.port(s)))).runWith

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
    Resource.run[Unit, Pure](
      Jetty.serve(0)(ChatDemo.routes(ChatDemo.local(base), 512))()
        .map { s =>
          val port = Jetty.port(s)
          val whole = new String(post(port,
            """{"messages":[{"role":"user","content":"answer with one short word"}]}""").readAllBytes(), UTF_8)
          assert(whole.contains("data: "), s"no tokens: ${whole.take(200)}")
          assert(whole.contains("event: done"), s"no done: ${whole.takeRight(120)}")
        }).runWith
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
    val answer = ChatDemo.agentTurn(
      "I can weld metal gates, my email is welder@live-demo. Please store my offer.",
      Nil, okay.agent.Provider.openAi(
        okay.llm.Transports.http(), "local", "default", s"$base/v1/chat/completions"),
      store)
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
    def turn(text: String): String = ChatDemo.agentTurn(text, Nil,
      okay.agent.Provider.openAi(okay.llm.Transports.http(), "local", "default",
        s"$base/v1/chat/completions"), store)
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
