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
        reasons += s"undecodable ($e): ${reply.take(200)}"
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
    // every failure, grouped by the decoder's own words: two examples
    // told us the rate and nothing about the shape, which is how 9%
    // sat unexamined for four lanes
    val byShape = reasons.groupBy(r => r.takeWhile(_ != ':')).view.mapValues(_.size).toList.sortBy(-_._2)
    byShape.foreach((shape, n) => println(f"  $n%3d  $shape"))
    reasons.take(3).foreach(r => println(s"  eg $r"))
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

  /**
   * The language table, re-measured on what is actually recommended.
   *
   * The first one was taken with generic names and the gate, and the
   * gate has since been demoted to a fallback — so those numbers
   * describe a mechanism nobody should now reach for first. Both arms
   * run here because the open question is whether the gate still pays
   * outside English: it held `Other` recall at 1.00 everywhere while
   * losing precision to 0.60 in Russian, which is the gate pushing
   * genuine meeting messages out of the domain.
   */
  test("live: languages under domain-bearing names, with and without the gate") {
    assume(reachable, s"no OpenAI-compatible endpoint at $url")
    import IntentFixture.Meeting
    given sM: Schema[Meeting] = summon[Schema[Meeting]]
    val mReading = Classify.reading[Meeting]

    def named(m: String) =
      predictIn[Meeting](ask(Classify.prompt[Meeting](m, IntentFixture.meetingExamples)))(
        using sM, mReading)
    def namedGated(m: String) =
      Classify.readInDomain(ask(Classify.inDomainPrompt[Meeting](m)(using sM))) match
        case Right(v) if !v.inDomain => "Other"
        case _ => named(m)

    for lang <- IntentFixture.languages do
      armOver(s"$lang names", IntentFixture.inLanguage(lang), named)
    for lang <- IntentFixture.languages do
      armOver(s"$lang names+gate", IntentFixture.inLanguage(lang), namedGated)
  }

  /**
   * Ablating the names.
   *
   * The previous lane's result rests on four identifiers, so this asks
   * how much of it is the domain they name. NO examples and NO gate in
   * any arm: examples would teach what the names are supposed to say on
   * their own, and a gate would add a second signal. The names are the
   * only thing that differs.
   */
  test("live: how much of the domain effect is the domain word") {
    assume(reachable, s"no OpenAI-compatible endpoint at $url")
    import IntentFixture.{Meeting, Shipping, Zarnic}
    given sM: Schema[Meeting] = summon[Schema[Meeting]]
    given sS: Schema[Shipping] = summon[Schema[Shipping]]
    given sZ: Schema[Zarnic] = summon[Schema[Zarnic]]

    def bare[I](m: String)(using si: Schema[I], sr: Schema[Reading[I]]): String =
      predictIn[I](ask(Classify.prompt[I](m)(using si)))(using si, sr)

    arm("generic names, no examples", m => bare[Support](m)(using summon[Schema[Support]], sReading))
    arm("true domain (Meeting)", m => bare[Meeting](m)(using sM, Classify.reading[Meeting]))
    arm("wrong domain (Shipping)", m => bare[Shipping](m)(using sS, Classify.reading[Shipping]))
    arm("nonsense qualifier (Zarnic)", m => bare[Zarnic](m)(using sZ, Classify.reading[Zarnic]))
  }

  /**
   * What is left undecodable on the best configuration, and why.
   *
   * The decode rate has been the dominant lever in every lane of this
   * line — 32% of replies unreadable on a bare prompt, 9% on the best
   * one — and the last 9% was never looked at, because the harness
   * printed two examples and dropped the rest.
   */
  test("live: what the remaining undecodable replies actually are") {
    assume(reachable, s"no OpenAI-compatible endpoint at $url")
    import IntentFixture.Meeting
    given sM: Schema[Meeting] = summon[Schema[Meeting]]
    val mReading = Classify.reading[Meeting]
    arm("best config, whole fixture", m =>
      predictIn[Meeting](ask(Classify.prompt[Meeting](m, IntentFixture.meetingExamples)))(
        using sM, mReading))
  }

  /**
   * A tie-break as EXAMPLES rather than as prose.
   *
   * Two arms over the same messages, differing by exactly two added
   * examples that carry the same two decisions the precedence lane
   * stated in words and lost 0.043 macro F1 doing.
   */
  test("live: does a tie-break carried as examples pay") {
    assume(reachable, s"no OpenAI-compatible endpoint at $url")
    import IntentFixture.Meeting
    given sM: Schema[Meeting] = summon[Schema[Meeting]]
    val mReading = Classify.reading[Meeting]

    def withExamples(ex: List[(String, Meeting)])(m: String) =
      predictIn[Meeting](ask(Classify.prompt[Meeting](m, ex)))(using sM, mReading)

    arm("examples as shipped", withExamples(IntentFixture.meetingExamples))
    arm("examples + two tie-breaks",
      withExamples(IntentFixture.meetingExamples ++ IntentFixture.tieBreakExamples))
  }

  /**
   * Candidate one for the language gap: case names in the message's
   * own language.
   *
   * Every measurement so far has read ENGLISH class names against
   * non-English messages. A domain-bearing name is what rescued
   * `Other`, and if the name has to be UNDERSTOOD for the domain to
   * land, a reader working in Russian has been handed the domain in a
   * second language.
   */
  test("live: case names in the message's own language") {
    assume(reachable, s"no OpenAI-compatible endpoint at $url")
    import IntentFixture.*
    given sM: Schema[Meeting] = summon[Schema[Meeting]]
    val mReading = Classify.reading[Meeting]

    def english(m: String) =
      predictIn[Meeting](ask(Classify.prompt[Meeting](m, meetingExamples)))(using sM, mReading)

    def native[I](ex: List[(String, I)])(using si: Schema[I])(m: String): String =
      predictIn[I](ask(Classify.prompt[I](m, ex)(using si)))(using si, Classify.reading[I])

    val inTongue: Map[String, String => String] = Map(
      "en" -> english,
      "fr" -> native(examplesFr),
      "de" -> native(examplesDe),
      "es" -> native(examplesEs),
      "ru" -> native(examplesRu),
      "ja" -> native(examplesJa))

    for lang <- languages do
      val data = inLanguage(lang)
      armOver(s"$lang english names", data, english)
      armOver(s"$lang native names", data, inTongue(lang))
  }

  /**
   * Candidate two: say the subject out loud in the reader's language
   * and leave the English names alone. Prepended, so the message stays
   * last — and built from `Classify.prompt` rather than beside it, so
   * the two arms cannot drift apart in anything but the sentence.
   */
  test("live: an explicit domain sentence, in the message's language") {
    assume(reachable, s"no OpenAI-compatible endpoint at $url")
    import IntentFixture.*
    given sM: Schema[Meeting] = summon[Schema[Meeting]]
    val mReading = Classify.reading[Meeting]

    def plain(m: String) =
      predictIn[Meeting](ask(Classify.prompt[Meeting](m, meetingExamples)))(using sM, mReading)
    def stated(lang: String)(m: String) =
      predictIn[Meeting](ask(
        domainSentence(lang) + "\n\n" + Classify.prompt[Meeting](m, meetingExamples)))(
        using sM, mReading)

    for lang <- languages do
      val data = inLanguage(lang)
      armOver(s"$lang plain", data, plain)
      armOver(s"$lang domain stated", data, stated(lang))
  }
}
