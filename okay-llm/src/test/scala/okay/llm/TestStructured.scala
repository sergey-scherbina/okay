package okay.llm

import okay.{!, %, +, Async, Writer, effect, pure}
import okay.given
import okay.codec.Schema

/**
 * Validated as it arrives, and cut when complete: the tokens after
 * the closing brace are never pulled — which, on a live model, means
 * never generated and never paid for.
 */
class TestStructured extends munit.FunSuite {

  case class Answer(city: String, country: String)
  given Schema[Answer] = Schema.derived

  /** a token stream that counts what was actually demanded */
  def stream(tokens: Seq[String], pulled: java.util.concurrent.atomic.AtomicInteger)
  : Unit ! (Writer % String + Async) =
    type F = Writer % String + Async
    def go(ts: List[String]): Unit ! F = ts match
      case Nil => pure(())
      case t :: rest =>
        // the async op stands for the wire: it runs only if pulled
        effect[F, Unit](Async.Run(() => { pulled.incrementAndGet(); () }))
          .flatMap(_ => effect[F, Unit](Writer(t)))
          .flatMap(_ => go(rest))
    go(tokens.toList)

  val answerTokens = Seq(
    "{\"city\"", ": ", "\"Kyiv\"", ", ", "\"country\"", ": ", "\"Ukraine\"", "}")

  val chatter = Seq("\n\nI hope", " that helps", "! Let me know", " if you need more.")

  test("the value is taken the moment it is complete, and the rest is not pulled") {
    val pulled = java.util.concurrent.atomic.AtomicInteger(0)
    val c = Structured.cut[Answer](stream(answerTokens ++ chatter, pulled))

    assertEquals(c.value, Some(Answer("Kyiv", "Ukraine")))
    assert(c.stopped, "the walk ran to the end instead of cutting")
    assertEquals(c.tokens, answerTokens.length)
    // the chatter after the closing brace was never demanded
    assertEquals(pulled.get, answerTokens.length,
      "tokens past the complete value were pulled — and would have been billed")
  }

  test("a prefix is not mistaken for a value") {
    val pulled = java.util.concurrent.atomic.AtomicInteger(0)
    // everything but the closing brace
    val c = Structured.cut[Answer](stream(answerTokens.dropRight(1), pulled))
    assertEquals(c.value, None)
    assert(!c.stopped)
    assertEquals(c.tokens, answerTokens.length - 1)
  }

  test("a complete value that does not FIT the schema is not accepted") {
    val pulled = java.util.concurrent.atomic.AtomicInteger(0)
    val wrong = Seq("{\"city\"", ": ", "\"Kyiv\"", "}")   // no country
    val c = Structured.cut[Answer](stream(wrong, pulled))
    assertEquals(c.value, None, "a well-formed but wrong-shaped value was accepted")
  }

  test("one token at a time, arriving character by character") {
    val pulled = java.util.concurrent.atomic.AtomicInteger(0)
    // .toList first: a bare String.map is hijacked by the package's
    // Comonad[Id] extension (an extension beats the StringOps
    // conversion) — the library's own documented footgun
    val chars = "{\"city\": \"Kyiv\", \"country\": \"Ukraine\"} and then some prose"
      .toList.map(_.toString)
    val c = Structured.cut[Answer](stream(chars, pulled))
    assertEquals(c.value, Some(Answer("Kyiv", "Ukraine")))
    assert(c.stopped)
    // it stopped exactly at the closing brace, not at the end
    assertEquals(c.text, "{\"city\": \"Kyiv\", \"country\": \"Ukraine\"}")
    assert(pulled.get < chars.length, s"pulled everything: ${pulled.get}/${chars.length}")
  }

  test("nested values complete only when the OUTER one does") {
    case class Inner(a: Int)
    case class Outer(inner: Inner, b: Int)
    given Schema[Inner] = Schema.derived
    given Schema[Outer] = Schema.derived

    val pulled = java.util.concurrent.atomic.AtomicInteger(0)
    val toks = Seq("{\"inner\"", ": {\"a\"", ": 1}", ", \"b\"", ": 2}", " trailing")
    val c = Structured.cut[Outer](stream(toks, pulled))
    assertEquals(c.value, Some(Outer(Inner(1), 2)))
    assertEquals(c.tokens, 5, "it stopped at the inner brace instead of the outer")
  }
}
