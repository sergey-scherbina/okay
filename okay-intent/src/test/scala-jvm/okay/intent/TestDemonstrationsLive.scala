package okay.intent

import okay.given
import okay.codec.Schema
import okay.llm.{OpenAi, Transports}

/**
 * "Show, Don't Tell" against our own taxonomy
 * (tod-demonstrations-from-the-log), sharpened by what
 * tod-schema-diagnostics measured a run earlier: with the words taken
 * out of the schema the model stops classifying (0.685 → 0.100 macro
 * F1, `C1` for every message). SDT's claim is that ONE annotated
 * example does the work descriptions are supposed to do. So the
 * decisive question here is not "do examples help" — this line
 * measured that long ago — but whether an example RECOVERS what the
 * names carry. If it does, a taxonomy's identifiers stop being the
 * prompt, and the rename hazard that lane found is a paper cut
 * instead of a trap.
 *
 * Four arms, one session, the whole labelled fixture (120 messages),
 * so the two baselines are re-measured beside the new arms rather
 * than remembered from another run:
 *
 *   Meeting          the shipped names, no demonstrations
 *   Meeting + SDT    the shipped names, one demonstration per class
 *   Indexed          C1..C4 and `s1`, no words anywhere
 *   Indexed + SDT    the same, with one demonstration per class
 *
 * The demonstrations are SELECTED by `Demonstrations.perClass` from a
 * recorded list, which is the mechanism okay-chat will point at its
 * ChatLog. They are drawn from messages OUTSIDE the scored fixture
 * (`IntentFixture.examples`, kept apart for exactly this reason) and
 * the selector is additionally given the scored set as `exclude`, so
 * no arm is ever shown its own answer key.
 *
 * TestClassifyLive's stance: the numbers are printed and recorded in
 * the spec; the assertions cover only what is ours.
 */
class TestDemonstrationsLive extends munit.FunSuite {

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

  private val table = scala.collection.mutable.ArrayBuffer.empty[(String, Double, Double, Int)]

  private def arm[I](name: String, demos: List[(String, I)])
                    (using si: Schema[I], sr: Schema[Reading[I]]): Unit =
    reasons.clear()
    val pairs = IntentFixture.labelled.map { (m, gold) =>
      (gold, predictIn[I](ask(Classify.prompt[I](m, demos)(using si)))(using si, sr))
    }
    val undecodable = pairs.count(_._2 == "undecodable")
    val empty = pairs.count(_._2 == "empty")
    val report = Eval.confusion.run(pairs.filterNot((_, p) => p == "undecodable" || p == "empty"))
    val other = report.perClass.get("Other").map(_.f1).getOrElse(0.0)
    println(f"\n[$name] macro F1 ${report.macroF1}%.3f   Other F1 $other%.2f   " +
            f"undecodable $undecodable/${pairs.length}   empty $empty   demos ${demos.length}")
    for c <- IntentFixture.classes do
      report.perClass.get(c).foreach(s =>
        println(f"  $c%-13s P=${s.precision}%.2f R=${s.recall}%.2f F1=${s.f1}%.2f"))
    reasons.groupBy(r => r.takeWhile(_ != ':')).view.mapValues(_.size).toList.sortBy(-_._2)
      .foreach((shape, n) => println(f"  $n%3d  $shape"))
    assert(undecodable < pairs.length, s"[$name] nothing decoded at all — the harness, not the model")
    table += ((name, report.macroF1, other, undecodable))

  override def afterAll(): Unit =
    if table.nonEmpty then
      println("\n| arm | macro F1 | Other F1 | undecodable |")
      println("|---|---:|---:|---:|")
      table.foreach((n, f1, o, u) => println(f"| $n | $f1%.3f | $o%.2f | $u/${IntentFixture.labelled.length} |"))

  /** the scored set: never a demonstration */
  private val scored = IntentFixture.labelled.map(_._1).toSet

  test("SDT: does one demonstration per class recover what the names carry") {
    assume(reachable, s"no OpenAI-compatible endpoint at $url")
    import IntentFixture.{Meeting, Indexed}
    given sM: Schema[Meeting] = summon[Schema[Meeting]]
    given sI: Schema[Indexed] = summon[Schema[Indexed]]
    val mReading = Classify.reading[Meeting]
    val iReading = Classify.reading[Indexed]

    // the same five messages, as each arm's own type; the selector
    // makes the choice, so the mechanism is the one okay-chat uses
    val meetingLog: List[(String, Meeting)] = List(
      "Are you free to meet on Wednesday afternoon?" -> Meeting.MeetingProposal("meet Wednesday"),
      "Please forward me the signed contract." -> Meeting.MeetingRequest("forward the contract"),
      "Note that payroll runs a day early this month." -> Meeting.MeetingNotification("payroll early"),
      "What is the capital of Portugal?" -> Meeting.NotAboutMeetings("general knowledge"),
      "My headphones arrived broken, I want a replacement." -> Meeting.NotAboutMeetings("a support issue"))
    val indexedLog: List[(String, Indexed)] = meetingLog.map { (m, i) =>
      val mapped = i match
        case Meeting.MeetingProposal(w) => Indexed.C1(w)
        case Meeting.MeetingRequest(w) => Indexed.C2(w)
        case Meeting.MeetingNotification(w) => Indexed.C3(w)
        case Meeting.NotAboutMeetings(w) => Indexed.C4(w)
      (m, mapped)
    }
    val mDemos = Demonstrations.perClass(meetingLog, exclude = scored)(using sM)
    val iDemos = Demonstrations.perClass(indexedLog, exclude = scored)(using sI)
    assertEquals(mDemos.length, 4, "one per class")
    assertEquals(iDemos.length, 4, "one per class")
    assert(mDemos.forall((m, _) => !scored.contains(m)), "no demonstration is a scored message")

    arm[Meeting]("Meeting, no demos", Nil)(using sM, mReading)
    arm[Meeting]("Meeting + SDT demos", mDemos)(using sM, mReading)
    arm[Indexed]("Indexed, no demos", Nil)(using sI, iReading)
    arm[Indexed]("Indexed + SDT demos", iDemos)(using sI, iReading)
  }
}
