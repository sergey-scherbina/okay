package okay.intent

/**
 * The pattern tier, in the default gate with no model and no network
 * (specs/intent-classify.md).
 *
 * Same odd/even split as every other tier, so the row is comparable —
 * though patterns are hand-written rather than trained, so the split
 * matters less here: what it guards against is tuning the cues against
 * the messages they are scored on.
 */
class TestPatterns extends munit.FunSuite {

  private val (train, test) = IntentFixture.labelled.zipWithIndex
    .partition(_._2 % 2 == 1) match
      case (a, b) => (a.map(_._1), b.map(_._1))

  test("a cue fires on its frame, not on its subject") {
    // the mechanism BM25 could not see: same topic, different intent
    assertEquals(Patterns.classify(Patterns.meeting, "Shall we meet about the agenda?"),
      Some("Proposal"))
    assertEquals(Patterns.classify(Patterns.meeting, "Could you send me the agenda?"),
      Some("Request"))
  }

  test("position matters where it carries meaning") {
    assertEquals(Patterns.score(Patterns.meeting, "FYI the room changed").map(_.best),
      Some("Notification"))
    // the same words, not at the start, must not fire the start-anchored cue
    val late = Patterns.score(Patterns.meeting, "Could you tell them, fyi, that the room changed?")
    assert(late.forall(_.best != "Notification") || late.exists(_.fired.contains("could you")),
      s"a start-anchored cue fired mid-sentence: $late")
  }

  test("a message with no cue is declined, not guessed") {
    assertEquals(Patterns.classify(Patterns.meeting, "zzz qqq xxx"), None)
  }

  test("coverage and agreement, at full coverage and at a margin") {
    val t0 = System.nanoTime()
    val scored = test.map((m, gold) => (gold, Patterns.score(Patterns.meeting, m)))
    val micros = (System.nanoTime() - t0) / 1000 / math.max(test.length, 1)

    val fired = scored.count(_._2.isDefined)
    val rightAtZero = scored.count { case (g, v) => v.exists(_.best == g) }
    println(f"\n[patterns] ${test.length} messages, ${micros}us each, no network")
    println(f"  fires on ${fired * 100.0 / test.length}%.1f%% of messages")
    println(f"  accuracy over ALL messages (a miss counts as wrong): ${rightAtZero * 100.0 / test.length}%.1f%%")
    for floor <- Seq(0.0, 0.2, 0.4, 0.6) do
      val answered = scored.collect { case (g, Some(v)) if v.margin >= floor => (g, v.best) }
      val right = answered.count((g, b) => g == b)
      val acc = if answered.isEmpty then 0.0 else right * 100.0 / answered.length
      println(f"  margin >= $floor%.1f   coverage ${answered.length * 100.0 / test.length}%5.1f%%   agreement $acc%5.1f%%")

    assert(micros < 5000, s"a pattern pass taking ${micros}us is not a fast tier")
    assert(train.nonEmpty)
  }

  // --- a cue set is stated against a taxonomy (intent-cues-for-a-taxonomy)

  private val domain = Taxon.parsed(
    Seq("MeetingProposal", "MeetingRequest", "MeetingNotification", "NotAboutMeetings"))

  private val naming = Map(
    "Proposal" -> "MeetingProposal",
    "Request" -> "MeetingRequest",
    "Notification" -> "MeetingNotification",
    "Other" -> "NotAboutMeetings")

  test("a cue naming a class the taxonomy does not hold cannot be built") {
    val bad = Patterns.Cues.of(Patterns.canonical,
      Vector(Patterns.Cue("shall we", "Proposal"), Patterns.Cue("ping", "Escalation")))
    assert(bad.isLeft, bad)
    assert(bad.left.exists(_.contains("Escalation")), bad)
  }

  test("the shipped set can reach every class it declares") {
    assertEquals(Patterns.meeting.silent, Vector.empty)
  }

  test("a rename that forgets a class fails, and says which") {
    // the router's `case _ =>`, which sent every name it had not
    // thought of to one class and said nothing
    val partial = Patterns.meeting.renamed(domain, naming - "Notification")
    assert(partial.isLeft, partial)
    assert(partial.left.exists(_.contains("Notification")), partial)
  }

  test("a rename onto a class the destination does not hold fails") {
    val wrong = Patterns.meeting.renamed(domain,
      naming.updated("Other", "OutOfScope"))
    assert(wrong.isLeft, wrong)
    assert(wrong.left.exists(_.contains("OutOfScope")), wrong)
  }

  test("a renamed set decides the same messages, under the new names") {
    val renamed = Patterns.meeting.renamed(domain, naming).toOption.get
    assertEquals(renamed.taxon, domain)
    val agreed = test.forall { (m, _) =>
      (Patterns.score(Patterns.meeting, m), Patterns.score(renamed, m)) match
        case (None, None) => true
        case (Some(a), Some(b)) =>
          naming(a.best) == b.best && a.margin == b.margin &&
            a.runnerUp.map(naming) == b.runnerUp
        case _ => false
    }
    assert(agreed, "a rename changed a verdict")
  }
}

