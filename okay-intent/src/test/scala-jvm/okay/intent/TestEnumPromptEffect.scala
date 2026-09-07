package okay.intent

import okay.given
import okay.codec.{Json, JsonSchema, Schema}
import okay.llm.{OpenAi, Transports}

/** Does `enum` in the prompt's rendered schema move the model? Two
 * runs each way over the whole fixture, so run-to-run spread is seen
 * beside the effect (codec-jsonschema-refinement-enum). Measured
 * 2026-09-07: the model is deterministic run to run, and the enum costs
 * 1.7 macro-F1 (0.909 -> 0.892; Request 0.93 -> 0.89) both times — so
 * `Classify.prompt` renders its schema WITHOUT vocabularies, and the
 * recorded journal stands. */
class TestEnumPromptEffect extends munit.FunSuite {
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(3600, "s")
  import IntentFixture.Meeting
  private given sM: Schema[Meeting] = summon[Schema[Meeting]]
  private val mReading = Classify.reading[Meeting]
  private val url = sys.env.getOrElse("OKAY_LLM_URL", "http://127.0.0.1:8089/v1/chat/completions")
  private val model = sys.env.getOrElse("OKAY_LLM_MODEL", "claude-rozum-mlx-community-Qwen3-5-4B-MLX-4bit")
  private def ask(prompt: String): String =
    OpenAi.complete(Transports.http(), "none", OpenAi.request(model, Seq(OpenAi.message("user", prompt)), maxTokens = Some(1200)), url)
      .runWith.choices.headOption.flatMap(_.message).flatMap(_.content).getOrElse("")
  private def stripEnum(j: Json): Json = j match
    case Json.JObj(fs) => Json.JObj(fs.filterNot(_._1 == "enum").map((k, v) => k -> stripEnum(v)))
    case Json.JArr(xs) => Json.JArr(xs.map(stripEnum))
    case other => other
  /** Classify.prompt's template with the schema rendered either way */
  private def prompt(message: String, withEnum: Boolean): String =
    val r = JsonSchema.of(mReading)
    val schema = if withEnum then r else stripEnum(r)
    val shown = IntentFixture.meetingExamples.map((m, i) => s"""  "$m" -> ${Json.write(i)}""")
      .mkString("Examples of the intent for a single-span message:\n", "\n", "\n\n")
    s"""Segment the message and classify the intent of each segment.
       |
       |Answer with ONE JSON object and nothing else, matching this schema:
       |${Json.print(schema)}
       |
       |The SHAPE of an answer, with placeholder values you must replace
       |(and as many spans and alts as the message needs):
       |${Classify.example(using mReading)}
       |
       |Rules:
       |- One span per intent. A message carrying two intents has two spans.
       |- Within a span, list alternatives in `alts` best first; give the
       |  reason in `why` BEFORE them.
       |- `conf` is one of: ${Conf.vocabulary}.
       |- If nothing in the taxonomy fits, say so through its own case
       |  rather than choosing the nearest positive class.
       |
       |$shown""".stripMargin + s"Message: $message"
  private def predict(reply: String): String =
    Classify.read[Meeting](reply)(using mReading).toOption.flatMap(_.spans.headOption).flatMap(_.alts.headOption)
      .map(a => IntentFixture.canonical.getOrElse(Classify.label(a.intent), Classify.label(a.intent))).getOrElse("undecodable")
  test("live: the prompt with and without enum in its schema, two runs each") {
    assertEquals(prompt("PROBE", withEnum = false), Classify.prompt[Meeting]("PROBE", IntentFixture.meetingExamples), "the template drifted from Classify.prompt")
    println("\n=== enum in the prompt's schema: two runs each way, 120 messages ===")
    for (withEnum, run) <- Seq((false, 1), (true, 1), (false, 2), (true, 2)) do
      val pairs = IntentFixture.labelled.map((m, g) => (g, predict(ask(prompt(m, withEnum)))))
      val r = Eval.confusion.run(pairs.filterNot(_._2 == "undecodable"))
      println(f"  ${if withEnum then "with enum   " else "without enum"} run $run   macro ${r.macroF1}%.3f   " +
        IntentFixture.classes.map(c => f"$c ${r.f1(c)}%.2f").mkString("  ") + f"   undecodable ${pairs.count(_._2 == "undecodable")}")
  }
}
