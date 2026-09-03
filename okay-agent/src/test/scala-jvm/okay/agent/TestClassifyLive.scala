package okay.agent

import okay.given
import okay.codec.{Json, Schema}
import okay.llm.{OpenAi, Transports}

/**
 * The `Other`-collapse experiment, in the repository rather than in a
 * script beside it (specs/intent-classify.md).
 *
 * Four arms over the SAME fixture, so the comparison is arm-to-arm and
 * not against a number remembered from a different harness:
 *
 *   bare      a minimal prompt with no guidance — the baseline the
 *             collapse was first measured on, replicated HERE
 *   shipped   `Classify.prompt`, which already carries a
 *             none-of-the-above line that was never itself measured
 *   examples  shipped, plus examples (drawn from outside the fixture)
 *   gate      a binary in-domain question FIRST; only what survives it
 *             reaches the taxonomy
 *
 * Following `TestLive`'s stance: what a model answers is not ours to
 * assert, so the numbers are PRINTED and recorded in the spec, and the
 * assertions cover only what is ours — that the harness ran and that
 * our own decoder read what came back.
 */
class TestClassifyLive extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(1800, "s")

  import IntentFixture.Support
  private given Schema[Support] = summon[Schema[Support]]
  private val sReading = Classify.reading[Support]

  val url = sys.env.getOrElse("OKAY_LLM_URL", "http://127.0.0.1:8089/v1/chat/completions")
  val model = sys.env.getOrElse("OKAY_LLM_MODEL",
    "claude-rozum-mlx-community-Qwen3-5-4B-MLX-4bit")
  val key = sys.env.getOrElse("OKAY_LLM_KEY", "none")

  lazy val reachable: Boolean =
    try
      val u = java.net.URI.create(url.replace("/chat/completions", "/models"))
      val c = u.toURL.openConnection()
      c.setConnectTimeout(1500)
      c.setReadTimeout(1500)
      c.getInputStream.close()
      true
    catch case _: Throwable => false

  private def ask(prompt: String): String =
    val body = OpenAi.request(model, Seq(OpenAi.message("user", prompt)), maxTokens = Some(1200))
    OpenAi.complete(Transports.http(), key, body, url).runWith
      .choices.headOption.flatMap(_.message).flatMap(_.content).getOrElse("")

  /** the schema plus the written rules, but no rendered example — what
   * `Classify.prompt` WAS before the gate lane, kept as the BEFORE arm.
   * The schema-only arm that completed the six-arm sweep is gone with
   * the sweep: its result is recorded in the spec, and keeping dead
   * code to commemorate a measurement is not how a measurement is
   * kept. */
  private def rulesPrompt(message: String): String =
    s"""Segment the message and classify the intent of each segment.
       |
       |Answer with ONE JSON object and nothing else, matching this schema:
       |${Json.print(ToolSpec.jsonSchema(sReading))}
       |
       |Rules:
       |- One span per intent. A message carrying two intents has two spans.
       |- Within a span, list alternatives in `alts` best first; give the
       |  reason in `why` BEFORE them.
       |- `conf` is one of: ${Conf.vocabulary}.
       |- If nothing in the taxonomy fits, say so through its own case
       |  rather than choosing the nearest positive class.
       |
       |Message: $message""".stripMargin

  /** what the model said, as a class name — `unreadable` when our own
   * decoder could not take it, which is counted rather than hidden */
  /** decode failures and empty answers are DIFFERENT facts and the
   * first version of this harness conflated them under one
   * "unreadable" count — which is why two runs with a tripled token
   * cap reported byte-identical numbers and neither of them meant
   * what they appeared to */
  private val reasons = scala.collection.mutable.Buffer[String]()

  private def predict(reply: String): String =
    predictIn[Support](reply)(using summon[Schema[Support]], sReading)

  /** the same reading, over whatever taxonomy — so an arm can change
   * the TYPE and change nothing else. Case names are mapped back to
   * the canonical classes, because two taxonomies are only comparable
   * on one axis. */
  private def predictIn[I](reply: String)
                          (using si: Schema[I], sr: Schema[Reading[I]]): String =
    Classify.read[I](reply)(using sr) match
      case Right(r) =>
        r.spans.headOption.flatMap(_.alts.headOption)
          .map(a => IntentFixture.canonical.getOrElse(
            Classify.label(a.intent)(using si), Classify.label(a.intent)(using si)))
          .getOrElse:
            reasons += s"decoded but empty: ${reply.take(120)}"
            "empty"
      case Left(e) =>
        reasons += s"undecodable ($e): ${reply.take(120)}"
        "undecodable"

  private def arm(name: String, classify: String => String): Unit =
    armOver(name, IntentFixture.labelled, classify)

  private def armOver(name: String, data: List[(String, String)],
                      classify: String => String): Unit =
    reasons.clear()
    val pairs = data.map((m, gold) => (gold, classify(m)))
    val undecodable = pairs.count(_._2 == "undecodable")
    val empty = pairs.count(_._2 == "empty")
    // the sentinels are MINE, not classes: left in the matrix they
    // become a predicted-only class whose F1 is 0, and macro F1 then
    // moves with the decode rate instead of with the classification.
    // Two runs differing by ONE undecodable reply reported 0.916 and
    // 0.748 for the same per-class scores before this was seen.
    // `Eval`'s rule that an invented label is still a class is right
    // for a real label and wrong for a sentinel, so the sentinel is
    // excluded here and reported on its own line.
    val report = Eval.confusion.run(pairs.filterNot((_, p) => p == "undecodable" || p == "empty"))
    val other = report.perClass.get("Other")
    println(f"\n[$name] macro F1 ${report.macroF1}%.3f (over decoded replies)   undecodable $undecodable/${pairs.length}   empty $empty/${pairs.length}")
    reasons.take(2).foreach(r => println(s"  eg $r"))
    println(f"  Other  P=${other.map(_.precision).getOrElse(0.0)}%.2f " +
            f"R=${other.map(_.recall).getOrElse(0.0)}%.2f " +
            f"F1=${other.map(_.f1).getOrElse(0.0)}%.2f")
    for c <- IntentFixture.classes do
      report.perClass.get(c).foreach(s =>
        println(f"  $c%-13s P=${s.precision}%.2f R=${s.recall}%.2f F1=${s.f1}%.2f"))
    // ours to assert: the decoder read the shape we asked for at all
    assert(undecodable < pairs.length, s"[$name] nothing at all decoded — the harness, not the model")

  private def gated(fallback: String => String)(m: String): String =
    Classify.readInDomain(ask(Classify.inDomainPrompt[Support](m))) match
      case Right(v) if !v.inDomain => "Other"
      case _ => fallback(m)

  private def best(m: String): String =
    predict(ask(Classify.prompt[Support](m, IntentFixture.examples)))

  /**
   * The decisive pair only. The six-arm sweep that established WHY
   * (specs/intent-classify.md, Results) ran on 24 messages; at 120 it
   * would be an hour of calls to re-derive a conclusion already drawn,
   * so what runs here is the before and the after.
   */
  test("live: the taxonomy prompt, before and after, over the whole fixture") {
    assume(reachable, s"no OpenAI-compatible endpoint at $url")
    arm("rules (before)", m => predict(ask(rulesPrompt(m))))
    arm("examples+gate (after)", gated(best))
  }

  /**
   * Does a taxonomy carry its domain in its case NAMES?
   *
   * Three arms over the same 120 messages, differing in the type and
   * in nothing else. If the names alone rescue `Other`, the fix
   * belongs in the type and the gate is a workaround for a domain
   * nobody stated; if they do not, the gate is doing work no naming
   * can do — and either answer is worth more than another prompt.
   */
  test("live: does the taxonomy carry its domain in its case names") {
    assume(reachable, s"no OpenAI-compatible endpoint at $url")
    import IntentFixture.Meeting
    given sMeeting: Schema[Meeting] = summon[Schema[Meeting]]
    val mReading = Classify.reading[Meeting]

    def generic(m: String) =
      predict(ask(Classify.prompt[Support](m, IntentFixture.examples)))
    def domain(m: String) =
      predictIn[Meeting](ask(Classify.prompt[Meeting](m, IntentFixture.meetingExamples)))(
        using sMeeting, mReading)

    arm("generic names, no gate", generic)
    arm("domain names, no gate", domain)
  }

  /** whether naming the domain and gating for it COMPOSE, or whether
   * the second stops paying once the first is done. Its own test
   * because three arms over 120 messages is half an hour of calls and
   * a box that is shared. */
  test("live: domain names with the gate on top") {
    assume(reachable, s"no OpenAI-compatible endpoint at $url")
    import IntentFixture.Meeting
    given sMeeting: Schema[Meeting] = summon[Schema[Meeting]]
    val mReading = Classify.reading[Meeting]
    def domain(m: String) =
      predictIn[Meeting](ask(Classify.prompt[Meeting](m, IntentFixture.meetingExamples)))(
        using sMeeting, mReading)

    arm("domain names + gate", m =>
      Classify.readInDomain(ask(Classify.inDomainPrompt[Meeting](m)(using sMeeting))) match
        case Right(v) if !v.inDomain => "Other"
        case _ => domain(m))
  }

  /**
   * The same twelve meanings in six languages, one arm.
   *
   * Scattering foreign sentences through the main fixture proved
   * nothing — a miss could always be the sentence rather than the
   * language. Here a drop is attributable, because the only thing that
   * varies between the rows is the wording.
   */
  test("live: the same intents across languages") {
    assume(reachable, s"no OpenAI-compatible endpoint at $url")
    for lang <- IntentFixture.languages do
      armOver(s"lang $lang", IntentFixture.inLanguage(lang), gated(best))
  }
}
