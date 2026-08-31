package okay.llm

import okay.{!, %, +, Async, Writer, effect, pure}
import okay.given
import okay.codec.Json

/** The thin client over a mock transport: tokens stream, lazily. */
class TestLlm extends munit.FunSuite {

  def sse(events: List[String]): Transport = new Transport:
    def post(url: String, headers: Map[String, String], body: String)
    : Unit ! (Writer % String + Async) =
      type F = Writer % String + Async
      def go(ls: List[String]): Unit ! F = ls match
        case Nil => pure(())
        case l :: t => effect[F, Unit](Writer(l)).flatMap(_ => go(t))
      go(events)

  def deltaEvent(text: String): List[String] = List(
    s"""data: {"type":"content_block_delta","delta":{"text":"$text"}}""", "")

  val canned = sse(
    List("""data: {"type":"message_start"}""", "") ++
      deltaEvent("Hel") ++ deltaEvent("lo") ++ deltaEvent(" world") ++
      List("""data: {"type":"message_stop"}""", "", "data: [DONE]", ""))

  test("tokens stream out of the SSE transport, non-token events dropped") {
    val s = Anthropic.stream(canned, "key",
      Anthropic.Request("claude", 100, List(Anthropic.Message("user", "hi")), true))
    val tokens = collect(s)
    assertEquals(tokens.mkString, "Hello world")
    assertEquals(tokens, List("Hel", "lo", " world"))
  }

  test("a truncated stream yields the tokens it carried (totality end to end)") {
    val cut = sse(List("""data: {"type":"message_start"}""", "") ++
      deltaEvent("par") ++
      List("""data: {"type":"content_block_delta","delta":{"te"""))   // cut mid-json
    assertEquals(collect(Anthropic.stream(cut, "k",
      Anthropic.Request("m", 1, Nil, true))), List("par"))
  }

  test("retry-with-backoff recovers a flaky transport (P2 combinators as-is)") {
    var attempts = 0
    val flaky: Transport = new Transport:
      def post(url: String, headers: Map[String, String], body: String)
      : Unit ! (Writer % String + Async) =
        type F = Writer % String + Async
        effect[F, Unit](Async.Run { () =>
          attempts += 1
          if attempts == 1 then throw RuntimeException("wire down")
        }).flatMap(_ => canned.post(url, headers, body))
    val request = Anthropic.Request("claude", 100,
      List(Anthropic.Message("user", "hi")), true)
    val prog = okay.retry(okay.Retry.immediate(2))(
      okay.async(collect(Anthropic.stream(flaky, "k", request))))
    assertEquals(prog.runWith, List("Hel", "lo", " world"))
    assertEquals(attempts, 2)   // one failure, one clean pass
  }

  test("structured output rides the total parser: partial JSON decodes") {
    // a model, cut off mid-answer: the tree with holes still projects
    case class Answer(city: String, country: String)
    given okay.codec.Schema[Answer] = okay.codec.Schema.derived
    val partial = """{"city": "Kyiv", "country": "Ukra"""
    assertEquals(Json.read[Answer](partial), Right(Answer("Kyiv", "Ukra")))
  }

  /** drain the token stream (Writer % String + Async, no real waits here) */
  def collect(s: Unit ! (Writer % String + Async)): List[String] =
    import okay.!.*
    def go(rest: Unit ! (Writer % String + Async), acc: List[String]): List[String] =
      (rest.resume: @unchecked) match
        case Pure(_) => acc.reverse
        case Effect(e) => okay.<|>[Async, Writer % String](e) match
          case Left(a) => summon[okay.Handler[Async]].handle(a); acc.reverse
          case Right(w) => (w.asInstanceOf[String] :: acc).reverse
        case Bind(Effect(e), k) => okay.<|>[Async, Writer % String](e) match
          case Left(a) => go(k(summon[okay.Handler[Async]].handle(a)), acc)
          // a tell answers nothing — the continuation gets unit, not the line
          case Right(w) => go(k(okay.answer), w.asInstanceOf[String] :: acc)
    go(s, Nil)
}
