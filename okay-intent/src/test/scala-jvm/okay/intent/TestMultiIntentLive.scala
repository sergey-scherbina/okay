package okay.intent

import okay.given
import okay.codec.{Json, Schema}
import okay.llm.{OpenAi, Transports}

/**
 * The multi-intent claim, against the ONE tier that could ever make
 * it true (specs/intent-classify.md).
 *
 * `TestMultiIntent` measures what the shipped no-LLM path does with a
 * two-intent message: it answers with one label, because every tier
 * it holds returns a single best class. Only the model tier can
 * SEGMENT, and that has never been asked to.
 *
 * Following `TestClassifyLive`'s stance: what a model answers is not
 * ours to assert. The numbers are printed and recorded in the spec,
 * and the assertions cover only what is ours — that the harness ran
 * and that our own decoder read what came back.
 */
class TestMultiIntentLive extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(1800, "s")

  import IntentFixture.Support
  private given Schema[Support] = summon[Schema[Support]]
  private val sReading = Classify.reading[Support]

  private val url = sys.env.getOrElse("OKAY_LLM_URL", "http://127.0.0.1:8089/v1/chat/completions")
  private val model = sys.env.getOrElse("OKAY_LLM_MODEL",
    "claude-rozum-mlx-community-Qwen3-5-4B-MLX-4bit")
  private val key = sys.env.getOrElse("OKAY_LLM_KEY", "none")

  private lazy val reachable: Boolean =
    try
      val u = java.net.URI.create(url.replace("/chat/completions", "/models"))
      val c = u.toURL.openConnection()
      c.setConnectTimeout(1500); c.setReadTimeout(1500)
      c.getInputStream.close(); true
    catch case _: Throwable => false

  private def ask(prompt: String): String =
    val body = OpenAi.request(model, Seq(OpenAi.message("user", prompt)), maxTokens = Some(1200))
    OpenAi.complete(Transports.http(), key, body, url).runWith
      .choices.headOption.flatMap(_.message).flatMap(_.content).getOrElse("")

  /** the class each span carries, canonical, in the order the model
   * returned them — and the span TEXTS, which are the other half of a
   * segmentation and were never checked against the message */
  private def spansOf(reply: String): Option[(List[String], List[String])] =
    Json.decode(sReading)(Json.parseValue(reply)).toOption.map { r =>
      (r.spans.flatMap(_.alts.headOption.map(a => Classify.label(a.intent, depth = 1))),
        r.spans.map(_.text))
    }

  /** a segmentation segments THE MESSAGE: a span whose text is not in
   * it is not a stretch of anything, it is an invention */
  private def ofTheMessage(message: String, texts: List[String]): Boolean =
    val m = message.toLowerCase
    texts.forall(t => t.trim.nonEmpty && m.contains(t.trim.toLowerCase))

  test("does the model tier actually segment a two-intent message") {
    assume(reachable, s"no gateway at $url — the claim stays unmeasured")

    var decoded = 0
    var twoSpans = 0
    var setRight = 0
    var orderRight = 0
    var grounded = 0
    IntentFixture.twoIntents.foreach { (message, gold) =>
      val got = spansOf(ask(Classify.prompt[Support](message)))
      got match
        case None => println(s"[live] UNREADABLE  $message")
        case Some((found, texts)) =>
          decoded += 1
          if found.size == 2 then twoSpans += 1
          if found.toSet == gold.toSet then setRight += 1
          if found == gold then orderRight += 1
          if ofTheMessage(message, texts) then grounded += 1
          else println(f"[live] NOT OF THE MESSAGE (${texts.size}%d spans): $message%s")
          println(f"[live] ${gold.mkString("+")}%-24s -> ${found.mkString("+")}%-26s $message%s")
    }
    val n = IntentFixture.twoIntents.size
    println(f"[live] decoded $decoded%2d/$n%2d | two spans $twoSpans%2d | " +
      f"right SET $setRight%2d | right set AND order $orderRight%2d | " +
      f"every span found IN the message $grounded%2d")
    // ours to assert: the harness ran and our decoder read the shape
    assert(decoded > 0, "nothing decoded at all — that is our decoder, not the model")
  }
}
