package okay.intent

import okay.given
import okay.codec.{Json, JsonSchema, Schema}
import okay.llm.{OpenAi, Transports}

/**
 * The answer's shape by contract instead of by persuasion
 * (specs/intent-classify.md, intent-structured-output).
 *
 * Every lane in this line bought its answer's SHAPE by persuasion — a
 * rendered example, written rules, a field order found by measuring
 * the residue. OpenAI-compatible gateways take `response_format` with
 * a JSON schema, and the schema this programme derives from `Schema[I]`
 * is exactly that document. Four arms over the same 120 messages, the
 * same taxonomy (`Meeting`, the recommended one), the same decoder:
 *
 *   shipped              `Classify.prompt` with examples — persuasion
 *                        only, the best configuration so far
 *   shipped + schema     the same prompt, plus `response_format`
 *   minimal + schema     one sentence and the message; the shape, the
 *                        rules and the examples all left to the schema
 *   minimal + schema + examples
 *                        the examples back, everything else left out
 *
 * What is measured: macro F1 over decoded replies, the undecodable and
 * empty counts (the decode rate was the dominant lever of every prior
 * lane), the prompt's length, and the reply's — so "the contract makes
 * the persuasion unnecessary" is a number with a cost beside it. A
 * gateway that refuses a schema this shape (sums as tagged cases,
 * nested lists) answers an error, which is counted, not hidden.
 */
class TestStructuredOutput extends munit.FunSuite {

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

  /** the contract: the reading's own JSON schema, as the gateway wants it */
  private val contract: Json = OpenAi.jsonSchema("reading", JsonSchema.of(mReading))

  private var promptChars = 0L; private var replyChars = 0L; private var calls = 0
  private var errors = 0
  /** per arm, whether anything decoded at all — an arm where nothing
   * does is a FINDING about the contract, printed and kept, not an
   * abort of the sweep; what is ours to assert is that the shipped
   * arm decoded */
  private val decodedArms = scala.collection.mutable.LinkedHashMap.empty[String, Boolean]

  private def ask(prompt: String, format: Option[Json]): String =
    promptChars += prompt.length; calls += 1
    val body = OpenAi.request(model, Seq(OpenAi.message("user", prompt)), maxTokens = Some(1200), responseFormat = format)
    val reply =
      try OpenAi.complete(Transports.http(), key, body, url).runWith.choices.headOption.flatMap(_.message).flatMap(_.content).getOrElse("")
      catch case e: Throwable => { errors += 1; s"<error ${e.getClass.getSimpleName}: ${String.valueOf(e.getMessage).take(80)}>" }
    replyChars += reply.length
    reply

  private def minimal(message: String): String =
    s"Segment the message and classify the intent of each segment; answer as JSON.\n\nMessage: $message"

  /** the one thing the derived schema cannot carry: `conf`'s vocabulary
   * is an enum in the decoder and a plain string in the JSON schema */
  private def minimalConf(message: String): String =
    s"Segment the message and classify the intent of each segment; answer as JSON. `conf` is one of: ${Conf.vocabulary}.\n\nMessage: $message"

  private def minimalWith(message: String, examples: List[(String, Meeting)]): String =
    val shown = examples.map((m, i) => s"""  "$m" -> ${Json.write(i)}""").mkString("Examples, one span each:\n", "\n", "\n\n")
    s"Segment the message and classify the intent of each segment; answer as JSON.\n\n$shown" + s"Message: $message"

  private val reasons = scala.collection.mutable.Buffer[String]()

  private def predict(reply: String): String =
    Classify.read[Meeting](reply)(using mReading) match
      case Right(r) =>
        r.spans.headOption.flatMap(_.alts.headOption)
          .map(a => IntentFixture.canonical.getOrElse(Classify.label(a.intent), Classify.label(a.intent)))
          .getOrElse { reasons += s"decoded but empty: ${reply.take(120)}"; "empty" }
      case Left(e) => reasons += s"undecodable ($e): ${reply.take(200)}"; "undecodable"

  private def arm(name: String, classify: String => String): Unit =
    reasons.clear(); promptChars = 0; replyChars = 0; calls = 0; errors = 0
    val data = IntentFixture.labelled
    val t0 = System.nanoTime()
    val pairs = data.map((m, gold) => (gold, classify(m)))
    val secs = (System.nanoTime() - t0) / 1e9
    val undecodable = pairs.count(_._2 == "undecodable"); val empty = pairs.count(_._2 == "empty")
    val report = Eval.confusion.run(pairs.filterNot((_, p) => p == "undecodable" || p == "empty"))
    println(f"\n[$name] macro F1 ${report.macroF1}%.3f (over decoded)   undecodable $undecodable/${pairs.length}   empty $empty/${pairs.length}   errors $errors   prompt ${promptChars / math.max(calls, 1)}%5d chars/msg   reply ${replyChars / math.max(calls, 1)}%4d chars/msg   ${secs / pairs.length}%.1f s/msg")
    reasons.groupBy(r => r.takeWhile(_ != ':')).view.mapValues(_.size).toList.sortBy(-_._2).take(4).foreach((s, n) => println(f"  $n%3d  $s"))
    reasons.take(2).foreach(r => println(s"  eg ${r.take(160)}"))
    for c <- IntentFixture.classes do report.perClass.get(c).foreach(s => println(f"  $c%-13s P=${s.precision}%.2f R=${s.recall}%.2f F1=${s.f1}%.2f"))
    println(Conditions.line(Conditions("Qwen3.5-4B (chat)", name, 0, pairs.length, extra = "taxonomy=Meeting"), "conditions", ""))
    decodedArms += (name -> (undecodable + empty + errors < pairs.length))

  test("live: the shape by contract — response_format against the shipped persuasion, and a minimal prompt under the contract") {
    assume(reachable, s"no OpenAI-compatible endpoint at $url")
    println(s"\n=== structured output: ${IntentFixture.labelled.length} messages, Meeting taxonomy ===")
    println(s"  the contract's schema: ${Json.print(JsonSchema.of(mReading)).length} chars")
    arm("shipped (persuasion only)", m => predict(ask(Classify.prompt[Meeting](m, IntentFixture.meetingExamples), None)))
    arm("shipped + schema", m => predict(ask(Classify.prompt[Meeting](m, IntentFixture.meetingExamples), Some(contract))))
    arm("minimal + schema", m => predict(ask(minimal(m), Some(contract))))
    arm("minimal + schema + conf line", m => predict(ask(minimalConf(m), Some(contract))))
    arm("minimal + schema + examples", m => predict(ask(minimalWith(m, IntentFixture.meetingExamples), Some(contract))))
    println(s"\n  arms that decoded anything: ${decodedArms.filter(_._2).keys.mkString(", ")}; that decoded nothing: ${decodedArms.filterNot(_._2).keys.mkString(", ")}")
    assert(decodedArms.getOrElse("shipped (persuasion only)", false), "the shipped arm decoded nothing — the harness, not the model")
  }
}
