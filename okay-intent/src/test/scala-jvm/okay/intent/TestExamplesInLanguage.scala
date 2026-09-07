package okay.intent

import okay.given
import okay.codec.Schema
import okay.llm.{OpenAi, Transports}

/**
 * The few-shot examples in the message's own language
 * (specs/intent-classify.md, intent-examples-in-language).
 *
 * The native-names lane moved ONE variable and kept the example
 * MESSAGES English throughout; translating the five examples is the
 * untried lever, and "examples OF A CLASS are the one lever that has
 * consistently paid here — unlike every prose addition, which has now
 * cost four times running". Two arms per language over the parallel
 * fixture, the same prompt, the same English case names, the same
 * decoder; the only difference is the language the five examples are
 * written in. The translations are the author's (the second-author
 * limitation applies to them as to the fixture), the `what` summaries
 * stay English because the model's own field is English in every
 * example it has seen.
 */
class TestExamplesInLanguage extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(3600, "s")

  import IntentFixture.Meeting
  private given sM: Schema[Meeting] = summon[Schema[Meeting]]
  private val mReading = Classify.reading[Meeting]

  private val url = sys.env.getOrElse("OKAY_LLM_URL", "http://127.0.0.1:8089/v1/chat/completions")
  private val model = sys.env.getOrElse("OKAY_LLM_MODEL", "claude-rozum-mlx-community-Qwen3-5-4B-MLX-4bit")
  private val key = sys.env.getOrElse("OKAY_LLM_KEY", "none")

  private lazy val reachable: Boolean =
    try
      val c = java.net.URI.create(url.replace("/chat/completions", "/models")).toURL.openConnection()
      c.setConnectTimeout(1500); c.setReadTimeout(1500); c.getInputStream.close(); true
    catch case _: Throwable => false

  private def ask(prompt: String): String =
    val body = OpenAi.request(model, Seq(OpenAi.message("user", prompt)), maxTokens = Some(1200))
    try OpenAi.complete(Transports.http(), key, body, url).runWith.choices.headOption.flatMap(_.message).flatMap(_.content).getOrElse("")
    catch case e: Throwable => s"<error ${e.getClass.getSimpleName}>"

  private def examples(lang: String): List[(String, Meeting)] = IntentFixture.meetingExamplesIn(lang)

  private val reasons = scala.collection.mutable.Buffer[String]()

  private def predict(reply: String): String =
    Classify.read[Meeting](reply)(using mReading) match
      case Right(r) =>
        r.spans.headOption.flatMap(_.alts.headOption)
          .map(a => IntentFixture.canonical.getOrElse(Classify.label(a.intent), Classify.label(a.intent)))
          .getOrElse { reasons += "empty"; "empty" }
      case Left(e) => reasons += s"undecodable ($e)"; "undecodable"

  private final case class Arm(macroF1: Double, undecodable: Int, other: Double)

  private def arm(name: String, data: List[(String, String)], classify: String => String): Arm =
    reasons.clear()
    val pairs = data.map((m, gold) => (gold, classify(m)))
    val undecodable = pairs.count(p => p._2 == "undecodable" || p._2 == "empty")
    val report = Eval.confusion.run(pairs.filterNot((_, p) => p == "undecodable" || p == "empty"))
    val other = report.perClass.get("Other").map(_.f1).getOrElse(0.0)
    println(f"  $name%-26s macro F1 ${report.macroF1}%.3f   Other F1 $other%.2f   undecodable $undecodable/${pairs.length}")
    Arm(report.macroF1, undecodable, other)

  test("live: the five examples in the message's language, against the same five in English, per language") {
    assume(reachable, s"no OpenAI-compatible endpoint at $url")
    println(s"\n=== examples in language: the parallel fixture, English case names throughout ===")
    val rows = for lang <- IntentFixture.languages if lang != "en" yield
      val data = IntentFixture.inLanguage(lang)
      println(s"\n  -- $lang (${data.length} messages) --")
      val en = arm(s"$lang, English examples", data, m => predict(ask(Classify.prompt[Meeting](m, IntentFixture.meetingExamples))))
      val own = arm(s"$lang, examples in $lang", data, m => predict(ask(Classify.prompt[Meeting](m, examples(lang)))))
      (lang, en, own)
    println("\n  lang   English examples   examples in language   delta")
    for (lang, en, own) <- rows do
      println(f"  $lang%-5s  ${en.macroF1}%.3f              ${own.macroF1}%.3f                  ${own.macroF1 - en.macroF1}%+.3f")
    val mean = rows.map((_, en, own) => own.macroF1 - en.macroF1).sum / rows.length
    println(f"  mean delta over ${rows.length} languages: $mean%+.3f; languages where it helped: ${rows.count((_, en, own) => own.macroF1 > en.macroF1)} of ${rows.length}")
    println(Conditions.line(Conditions("Qwen3.5-4B (chat)", "examples-in-language", 0, rows.map(_._1).length,
      corpus = "IntentFixture.inLanguage", extra = "taxonomy=Meeting names=English"), "conditions", ""))
    assert(rows.nonEmpty)
  }
}
