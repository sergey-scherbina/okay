package okay.agent

import java.nio.file.{Files, Paths}

/**
 * Evaluation over a RECORDING instead of a model
 * (specs/intent-classify.md).
 *
 * Every measurement in this line has been a live run of ten to thirty
 * minutes, which is why several questions went four lanes without
 * being asked. A recorded journal turns the parts that do not involve
 * the model — decoding, the label mapping, the gate logic, the metrics
 * themselves — into a second-long check in the DEFAULT gate, and gives
 * `Eval.regressions` something real to guard. It has been executable
 * since the first lane and had never guarded anything.
 *
 * What it does NOT do, and must not pretend to: a changed PROMPT makes
 * the recording describe a question nobody is asking any more. That is
 * why each entry carries the prompt's fingerprint — the mismatch is
 * the signal to go and re-record, not a failure to route around.
 */
class TestEvalJournal extends munit.FunSuite {

  import IntentFixture.Meeting
  private given sMeeting: okay.codec.Schema[Meeting] = summon[okay.codec.Schema[Meeting]]
  private val mReading = Classify.reading[Meeting]

  private val store = Paths.get("okay-agent/src/test/resources/intent-journal")

  private def recording: Option[Rerun.Version] =
    if !Files.isDirectory(store) then None
    else new FileVersions(store).all.headOption

  /** the prompt's identity, stably: `String.hashCode` is specified by
   * the language, so a recording made on one JVM is readable on the
   * next. Not a security boundary — it only has to notice a change. */
  def fingerprint(prompt: String): String = f"${prompt.hashCode}%08x"

  /** the decode-and-label half, exactly as the live harness does it */
  private def predict(reply: String): String =
    Classify.read[Meeting](reply)(using mReading) match
      case Right(r) =>
        r.spans.headOption.flatMap(_.alts.headOption)
          .map(a => IntentFixture.canonical.getOrElse(
            Classify.label(a.intent), Classify.label(a.intent)))
          .getOrElse("empty")
      case Left(_) => "undecodable"

  private def goldOf(message: String): Option[String] =
    IntentFixture.labelled.collectFirst { case (m, g) if m == message => g }

  test("the recording still answers the question the code is asking") {
    assume(recording.isDefined, s"no recording at $store — run TestClassifyRecord")
    val v = recording.get
    val current = Classify.prompt[Meeting]("PROBE", IntentFixture.meetingExamples)
    val fp = fingerprint(current)
    val recorded = v.entries.headOption.map(_.fingerprint).getOrElse("")
    assertEquals(fp, recorded,
      "the prompt changed since the recording: re-record rather than trusting these numbers")
  }

  test("evaluation over the recording reproduces the report, with no model") {
    assume(recording.isDefined, s"no recording at $store — run TestClassifyRecord")
    val v = recording.get
    val pairs = v.entries.flatMap { e =>
      goldOf(e.key).map(g => (g, predict(e.answer.getOrElse(""))))
    }
    assert(pairs.length >= 100, s"the recording is too small to mean anything: ${pairs.length}")

    val report = Eval.confusion.run(pairs.filterNot((_, p) => p == "undecodable" || p == "empty"))
    val undecodable = pairs.count(_._2 == "undecodable")

    // the promotion rule, finally guarding something: these are the
    // numbers the live run measured, and a change to the decoder or
    // the label mapping that costs a class more than two points fails
    // here in a second instead of surviving to the next live run
    val baseline = Map(
      "Proposal" -> 0.95, "Request" -> 0.93, "Notification" -> 0.89, "Other" -> 0.86)
    val fell = baseline.toList.filter((c, f1) => f1 - report.f1(c) > 0.02).map(_._1).sorted
    assertEquals(fell, Nil,
      s"classes fell more than two points: ${report.perClass.view.mapValues(_.f1).toMap}")
    assertEquals(undecodable, 0, "a reply stopped decoding")
  }
}
