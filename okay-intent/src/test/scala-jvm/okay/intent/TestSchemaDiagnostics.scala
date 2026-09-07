package okay.intent

import okay.given
import okay.codec.Schema
import okay.llm.{OpenAi, Transports}

/**
 * Two DIAGNOSTICS from the operator's task-oriented-dialogue papers,
 * run against our own shipped taxonomy (tod-schema-diagnostics). Both
 * turn "our schemas are good" into a number, before anyone invests
 * more in writing them.
 *
 * (a) D3ST (Zhao, Cao, Xu et al.) randomizes slot names to arbitrary
 *     indices so a TRAINED model cannot lean on them. As a technique
 *     for a PROMPTED model it is exactly wrong — the priors around a
 *     word like "meeting" are what we are buying — but as a
 *     diagnostic it is the sharpest available: `Indexed` says C1..C4
 *     and `s1`, no word anywhere. Score it like `Meeting` and the
 *     names are decoration; collapse and the names are load-bearing,
 *     which means every schema in this repository is a prompt and
 *     should be reviewed as one.
 *
 * (b) SGD-X (Lee, Cheng, Zhang et al.) paraphrases schema
 *     descriptions and reports the SPREAD, because a system whose
 *     numbers move under a paraphrase was reading the wording rather
 *     than the meaning. Our taxonomy has no descriptions — the names
 *     are the description — so the paraphrase is of the names, at two
 *     distances: `MeetingNear` (the rename a colleague makes without
 *     thinking) and `MeetingFar` (still correct English, as far from
 *     the shipped wording as the meaning allows).
 *
 * Following TestClassifyLive's stance: what the model answers is not
 * ours to assert. The numbers are PRINTED and recorded in the spec;
 * the assertions cover only what is ours — that each arm ran and that
 * our decoder read the shape back.
 *
 * NO examples and NO gate in any arm, deliberately: an example would
 * teach what the names are supposed to say, and the gate would add a
 * second signal. The names are the only thing that differs.
 */
class TestSchemaDiagnostics extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(3600, "s")

  val url = sys.env.getOrElse("OKAY_LLM_URL", "http://127.0.0.1:8089/v1/chat/completions")
  val model = sys.env.getOrElse("OKAY_LLM_MODEL", "claude-rozum-mlx-community-Qwen3-5-4B-MLX-4bit")
  val key = sys.env.getOrElse("OKAY_LLM_KEY", "none")

  lazy val reachable: Boolean =
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

  private val reasons = scala.collection.mutable.ArrayBuffer.empty[String]

  private def predictIn[I](reply: String)(using si: Schema[I], sr: Schema[Reading[I]]): String =
    Classify.read[I](reply)(using sr) match
      case Right(r) =>
        r.spans.headOption.flatMap(_.alts.headOption)
          .map(a => IntentFixture.canonical.getOrElse(
            Classify.label(a.intent)(using si), Classify.label(a.intent)(using si)))
          .getOrElse { reasons += s"decoded but empty: ${reply.take(120)}"; "empty" }
      case Left(e) =>
        reasons += s"undecodable ($e): ${reply.take(160)}"
        "undecodable"

  /** one arm over the whole labelled fixture; the same scoring rule as
   * TestClassifyLive (the sentinels are mine, not classes, so they are
   * excluded from the matrix and reported on their own line) */
  private def arm[I](name: String)(using si: Schema[I], sr: Schema[Reading[I]]): (String, Double, Double, Int) =
    reasons.clear()
    val pairs = IntentFixture.labelled.map { (m, gold) =>
      (gold, predictIn[I](ask(Classify.prompt[I](m)(using si)))(using si, sr))
    }
    val undecodable = pairs.count(_._2 == "undecodable")
    val empty = pairs.count(_._2 == "empty")
    val scored = pairs.filterNot((_, p) => p == "undecodable" || p == "empty")
    val report = Eval.confusion.run(scored)
    val other = report.perClass.get("Other").map(_.f1).getOrElse(0.0)
    println(f"\n[$name] macro F1 ${report.macroF1}%.3f   Other F1 $other%.2f   " +
            f"undecodable $undecodable/${pairs.length}   empty $empty")
    for c <- IntentFixture.classes do
      report.perClass.get(c).foreach(s =>
        println(f"  $c%-13s P=${s.precision}%.2f R=${s.recall}%.2f F1=${s.f1}%.2f"))
    reasons.groupBy(r => r.takeWhile(_ != ':')).view.mapValues(_.size).toList.sortBy(-_._2)
      .foreach((shape, n) => println(f"  $n%3d  $shape"))
    assert(undecodable < pairs.length, s"[$name] nothing decoded at all — the harness, not the model")
    (name, report.macroF1, other, undecodable)

  private val table = scala.collection.mutable.ArrayBuffer.empty[(String, Double, Double, Int)]

  override def afterAll(): Unit =
    if table.nonEmpty then
      println("\n| arm | macro F1 | Other F1 | undecodable |")
      println("|---|---:|---:|---:|")
      table.foreach((n, f1, o, u) => println(f"| $n | $f1%.3f | $o%.2f | $u/${IntentFixture.labelled.length} |"))
      val base = table.find(_._1.startsWith("Meeting")).map(_._2).getOrElse(0.0)
      table.foreach((n, f1, _, _) => println(f"  delta vs Meeting: $n%-28s ${f1 - base}%+.3f"))

  test("D3ST: how much of the reading is the NAMES") {
    assume(reachable, s"no OpenAI-compatible endpoint at $url")
    import IntentFixture.{Meeting, Indexed}
    given sM: Schema[Meeting] = summon[Schema[Meeting]]
    given sI: Schema[Indexed] = summon[Schema[Indexed]]
    table += arm[Meeting]("Meeting (shipped names)")(using sM, Classify.reading[Meeting])
    table += arm[Indexed]("Indexed (C1..C4, no words)")(using sI, Classify.reading[Indexed])
  }

  test("SGD-X: is the reading stable under a paraphrase of the names") {
    assume(reachable, s"no OpenAI-compatible endpoint at $url")
    import IntentFixture.{MeetingNear, MeetingFar}
    given sN: Schema[MeetingNear] = summon[Schema[MeetingNear]]
    given sF: Schema[MeetingFar] = summon[Schema[MeetingFar]]
    table += arm[MeetingNear]("Near synonyms")(using sN, Classify.reading[MeetingNear])
    table += arm[MeetingFar]("Far synonyms")(using sF, Classify.reading[MeetingFar])
  }
}
