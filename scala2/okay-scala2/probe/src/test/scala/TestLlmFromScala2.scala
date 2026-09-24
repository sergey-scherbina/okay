package scala2probe

import java.util.concurrent.atomic.AtomicInteger
import okay.codec.Schema
import okay.scala2._

object LlmModel {
  final case class Point(x: Int, y: Int)
  object Point {
    implicit val schema: Schema[Point] = Schemas.product2("Point", "x", "y")(Point.apply)(p => (p.x, p.y))
  }
}

/** okay-llm from Scala 2.13 (specs/scala2-facade.md, stage 15.5) */
class TestLlmFromScala2 extends munit.FunSuite {
  import LlmModel._

  def openAiLine(token: String): String =
    "data: {\"choices\":[{\"delta\":{\"content\":\"" + token.replace("\"", "\\\"") + "\"}}]}"

  test("an Anthropic completion arrives as its text tokens, and the request says what was asked") {
    var asked = ""
    val server = Llm.transport { (url, headers, body) =>
      asked = url + " " + headers.getOrElse("x-api-key", "") + " " + body
      Source(
        "event: content_block_delta",
        "data: {\"type\":\"content_block_delta\",\"delta\":{\"text\":\"Hel\"}}",
        "",
        "event: content_block_delta",
        "data: {\"type\":\"content_block_delta\",\"delta\":{\"text\":\"lo\"}}",
        "")
    }
    val tokens = Llm.anthropic(server, "key-1", "claude-test", Seq("user" -> "Say hello"))
    assertEquals(tokens.runCollect.runWith, Vector("Hel", "lo"))
    assert(asked.startsWith("https://api.anthropic.com/v1/messages key-1 "), asked)
    assert(asked.contains("\"model\":\"claude-test\"") && asked.contains("Say hello"), asked)
  }

  test("an OpenAI-style completion streams the same way") {
    val server = Llm.transport((_, _, _) => Source(openAiLine("4"), "", openAiLine("2"), "", "data: [DONE]", ""))
    val answer = Llm.openAi(server, "key", "gpt-test", Seq("user" -> "6 * 7?")).runFold("")(_ + _)
    assertEquals(answer.runWith, "42")
  }

  test("a typed value is cut from the stream, and the rest is never read") {
    val pulled = new AtomicInteger
    val endless = Source.unfold(0) { i =>
      pulled.incrementAndGet()
      val token = i match {
        case 0 => "{\"x\":"
        case 1 => "1,\"y\":"
        case 2 => "2}"
        case _ => " and more"
      }
      Some((openAiLine(token), i + 1))
    }
    val server = Llm.transport((_, _, _) => endless.take(50).mapConcat(line => List(line, "")))
    val point = Llm.first[Point](Llm.openAi(server, "key", "gpt-test", Seq("user" -> "a point")))
    assertEquals(point.runWith, Some(Point(1, 2)))
    assert(pulled.get < 10, s"pulled ${pulled.get} tokens from an endless stream")
  }
}
