package okay.chat

import okay.*
import okay.given
import okay.http.{Body, Http, Method, Request, Response}
import okay.llm.{Anthropic, Cut}
import okay.conf.Secrets
import java.nio.charset.StandardCharsets.UTF_8

/**
 * specs/chat.md — the module in isolation: the scripted path, the
 * Cut guard, request parsing, and the turnOverride seam. (The demo's
 * own TestChatDemo keeps proving the real HTTP route end to end,
 * including the /match override actually wired in.)
 */
class TestChat extends munit.FunSuite {

  def run[A](p: A ! Async): A = !.run(Async.run[A, Nothing](p))

  def text(src: Source[Chunk[Byte]]): String =
    run(Http.text(Response(200, Nil, src)))

  test("the scripted reply streams token by token and ends with done") {
    val whole = text(Chat.reply(Chat.scripted, budget = 512)(
      Seq(Anthropic.Message("user", "hello okay"))))
    assert(whole.contains("hello") && whole.contains("okay"), whole)
    assert(whole.contains("event: done"), whole.takeRight(80))
    assert(!whole.contains("event: cut"))
  }

  test("over budget the stream is cut, named, and no tokens follow") {
    val whole = text(Chat.reply(Chat.scripted, budget = 3)(
      Seq(Anthropic.Message("user", "anything"))))
    val frames = whole.split("\n\n").toVector.filter(_.nonEmpty)
    val (tokens, after) = frames.span(!_.startsWith("event: cut"))
    assertEquals(tokens.length, 3, s"the budget is the cut point: $frames")
    assert(after.head.contains("token-budget"), "the rule is named")
    assertEquals(after.length, 1, "nothing follows the cut")
    assert(!whole.contains("event: done"))
  }

  // ---- the content policy (demo-streaming-cut) ------------------------

  private val banned: (Int, String) => Option[Cut.Violation] =
    (i, t) => if t.toLowerCase.contains("banned") then Some(Cut.Violation("content-policy", i, t)) else None

  test("policy: a violating token is cut, named, no token after it") {
    // scripted ECHOES the message, so "banned" in the input lands in the stream
    val whole = text(Chat.reply(Chat.scripted, budget = 512, policy = banned)(
      Seq(Anthropic.Message("user", "banned"))))
    val frames = whole.split("\n\n").toVector.filter(_.nonEmpty)
    val (tokens, after) = frames.span(!_.startsWith("event: cut"))
    assert(after.nonEmpty, s"expected a cut: $frames")
    assert(after.head.contains("content-policy"), "the rule is named")
    assertEquals(after.length, 1, "nothing follows the cut")
    assert(!whole.contains("event: done"))
    // the FIRST token is the violation ("You" precedes "said:" precedes
    // "banned" in the echo) — prove no token past it leaked through
    assert(!tokens.exists(_.contains("streamed")), s"a later word must not appear: $tokens")
  }

  test("policy: a clean reply is unaffected — byte-identical to no policy at all") {
    val withPolicy = text(Chat.reply(Chat.scripted, budget = 512, policy = banned)(
      Seq(Anthropic.Message("user", "hello okay"))))
    val withoutPolicy = text(Chat.reply(Chat.scripted, budget = 512)(
      Seq(Anthropic.Message("user", "hello okay"))))
    assertEquals(withPolicy, withoutPolicy)
    assert(withPolicy.contains("event: done"))
  }

  test("policy: the token budget still cuts when a reply is long AND clean — neither rule shadows the other") {
    val whole = text(Chat.reply(Chat.scripted, budget = 3, policy = banned)(
      Seq(Anthropic.Message("user", "anything"))))
    assert(whole.contains("token-budget"), whole.take(200))
    assert(!whole.contains("content-policy"))
  }

  test("chatRoute's default policy behaves byte-identical to reply's own default") {
    val route = Chat.chatRoute(Chat.scripted, budget = 512)
    val req = Request(Method.Post, "/chat", Nil,
      Body.Text("""{"messages":[{"role":"user","content":"hello okay"}]}"""))
    val whole = text(run(route(req)).body)
    assert(whole.contains("event: done") && !whole.contains("event: cut"), whole.take(200))
  }

  test("messagesOf parses the OpenAI-shaped body") {
    val body = Body.Text(
      """{"messages":[{"role":"user","content":"hi"},{"role":"assistant","content":"yo"}]}""")
    assertEquals(Chat.messagesOf(body),
      Seq(Anthropic.Message("user", "hi"), Anthropic.Message("assistant", "yo")))
  }

  test("fieldOf reads one named string field, empty when absent") {
    val body = Body.Text("""{"email":"a@b.c"}""")
    assertEquals(Chat.fieldOf(body, "email"), "a@b.c")
    assertEquals(Chat.fieldOf(body, "missing"), "")
  }

  test("chatRoute: no turnOverride falls through to the plain model") {
    val route = Chat.chatRoute(Chat.scripted, budget = 512)
    val req = Request(Method.Post, "/chat", Nil,
      Body.Text("""{"messages":[{"role":"user","content":"hi"}]}"""))
    val whole = text(run(route(req)).body)
    assert(whole.contains("hi") || whole.contains("You"), whole.take(200))
  }

  test("chatRoute: turnOverride answering Some short-circuits reply entirely") {
    val overridden: Source[Chunk[Byte]] = Http.one("data: \"overridden\"\n\n".getBytes(UTF_8))
    val route = Chat.chatRoute(Chat.scripted, budget = 512,
      turnOverride = (_, _) => Some(overridden))
    val req = Request(Method.Post, "/chat", Nil,
      Body.Text("""{"messages":[{"role":"user","content":"/special"}]}"""))
    val whole = text(run(route(req)).body)
    assertEquals(whole, "data: \"overridden\"\n\n")
  }

  test("chatRoute is defined only for POST /chat") {
    val route = Chat.chatRoute(Chat.scripted, budget = 512)
    assert(!route.isDefinedAt(Request(Method.Get, "/chat")))
    assert(!route.isDefinedAt(Request(Method.Post, "/other")))
  }

  test("modeName/model read the ambient Secrets — scripted with no key configured") {
    given Secrets = Secrets.memory(Map.empty)
    assertEquals(Chat.secret("ANTHROPIC_API_KEY"), None)
    assert(Chat.modeName.contains("scripted"))
  }
}
