package okay.intent

import okay.rag.embedding

/**
 * A tier fitted AGAINST a taxonomy, and an ensemble that cannot
 * silently disagree with itself (specs/intent-classify.md).
 *
 * The consumer's request 1 asked for one taxonomy both tiers read.
 * What landed was one taxonomy neither TRAINED tier read: every `fit`
 * inferred its classes from whatever labels its rows happened to
 * carry, and a caller checked agreement by hand or not at all.
 */
class TestTaxonWiring extends munit.FunSuite {

  private def vec(xs: Double*) = embedding(xs.map(_.toFloat).toArray)

  private val rows = Seq(
    vec(1.0, 0.0) -> "Proposal", vec(0.9, 0.1) -> "Proposal",
    vec(0.0, 1.0) -> "Request", vec(0.1, 0.9) -> "Request")

  private val declared = Taxon.parsed(Seq("Proposal", "Request", "Notification"))
  private val wrong = Taxon.parsed(Seq("MeetingProposal", "MeetingRequest"))

  test("a fit infers the taxonomy it was given, and says which classes it can reach") {
    val c = Centroid.train(rows)
    assertEquals(c.taxon.classes, Vector("Proposal", "Request"))
    assertEquals(c.silent, Vector.empty)
  }

  test("a DECLARED taxonomy is checked, and names what it never learned") {
    val c = Centroid.against(declared, rows).getOrElse(fail("it should fit"))
    assertEquals(c.taxon, declared)
    // the taxonomy holds a class the rows never taught, and the tier
    // says so instead of pretending it can answer with it
    assertEquals(c.silent, Vector("Notification"))
  }

  test("a label outside the declared taxonomy is an error at FIT time") {
    val bad = Centroid.against(wrong, rows)
    assert(bad.isLeft, bad)
    assert(bad.left.exists(_.contains("Proposal")), bad)
    // and the same for every other trained tier
    assert(Probe.against(wrong, rows).isLeft)
    assert(Nearest.against(wrong, rows).isLeft)
    assert(CharGrams.against(wrong, Seq("shall we meet" -> "Proposal")).isLeft)
  }

  test("the ensemble refuses cues and a probe that speak different names") {
    // THE LATENT BUG this lane exists for: `blend` adds a cue's weight
    // to a probe class by string equality, so a mismatch makes every
    // bonus zero and the ensemble silently becomes the probe alone.
    val domain = Taxon.parsed(Seq("MeetingProposal", "MeetingRequest",
      "MeetingNotification", "NotAboutMeetings"))
    val renamed = Patterns.meeting.renamed(domain, Map(
      "Proposal" -> "MeetingProposal", "Request" -> "MeetingRequest",
      "Notification" -> "MeetingNotification", "Other" -> "NotAboutMeetings"))
      .getOrElse(fail("the rename should hold"))

    // canonical rows against domain-bearing cues: every bonus would be
    // zero, and before this lane nothing would have noticed
    val boom = intercept[IllegalArgumentException](
      NoModel.fit(
        train = Seq(("shall we meet", vec(1.0, 0.0), "Proposal")),
        calibrate = Seq(("could you send it", vec(0.0, 1.0), "Request")),
        cues = Some(renamed)))
    assert(boom.getMessage.contains("Proposal"), boom.getMessage)
    assert(boom.getMessage.contains("silently"), boom.getMessage)
  }

  test("no cues at all is a configuration, and it is the default") {
    // it used to default to the MEETING cue set, whatever the corpus
    // was about, contributing nothing because the default weight grid
    // is a single zero — a default that is inert is a default that
    // lies about what it does
    val t = NoModel.fit(
      train = Seq(("aaa", vec(1.0, 0.0), "north"), ("bbb", vec(0.0, 1.0), "east")),
      calibrate = Seq(("aaa again", vec(0.9, 0.1), "north")))
    assertEquals(t.cues, None)
    assertEquals(t.probe.taxon.classes, Vector("east", "north"))
  }

  test("and it accepts them when they agree") {
    val t = NoModel.fit(
      train = Seq(("shall we meet", vec(1.0, 0.0), "Proposal"),
        ("could you send it", vec(0.0, 1.0), "Request")),
      calibrate = Seq(("shall we meet again", vec(0.9, 0.1), "Proposal")),
      cues = Some(Patterns.meeting))
    assertEquals(t.probe.taxon.classes, Vector("Proposal", "Request"))
    assert(t.cues.exists(_.taxon.has("Proposal")))
  }

  test("a round trip keeps what the model learned and forgets what was only declared") {
    // stated in Fitted's own comment: the taxonomy is fit-time
    // knowledge, the classes are model knowledge, and only the second
    // is on the wire
    val c = Centroid.against(declared, rows).getOrElse(fail("it should fit"))
    val back = Fitted.load(Fitted.save(c))
    assertEquals(back.byClass.keys.toVector.sorted, Vector("Proposal", "Request"))
    assertEquals(back.taxon.classes, Vector("Proposal", "Request"))
    assertEquals(back.silent, Vector.empty)
  }
}
