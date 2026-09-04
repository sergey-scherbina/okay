package okay.intent

import okay.frame.{Frame, Slot}

/**
 * The composed door, on every platform (specs/intent-classify.md).
 *
 * The point of `Router.offline` is that it needs nothing — no
 * gateway, no filesystem, no fitting — so the suite that proves it
 * runs where there is none of those.
 */
class TestRouterCross extends munit.FunSuite {

  private val offline = Router.Router.offline()

  test("a router with the shipped tiers answers with no network at all") {
    assertEquals(offline.tiers, Vector("cues", "grams"))
    offline.route("Could you send me the agenda?") match
      case Router.Action.Act(intent, frame) =>
        assertEquals(intent, "Request")
        assert(frame.complete, "a frame with no slots is actionable at once")
      case other => fail(s"expected an action, got $other")
  }

  test("with no tiers at all it escalates, and says so plainly") {
    val empty = Router.Router.of(Patterns.canonical).getOrElse(fail("did not build"))
    empty.route("anything") match
      case Router.Action.Escalate(Seq(), why) =>
        assert(why.contains("holds no tier"), why)
      case other => fail(s"expected an escalation, got $other")
  }

  test("a frame the class needs turns an answer into a question") {
    val what = Slot[String]("what", Map("en" -> "What would you like done?"),
      s => Option.when(s.trim.nonEmpty)(s.trim))
    val r = Router.Router.of(Patterns.canonical, cues = Some(Models.cues),
      frames = i => if i == "Request" then Frame.of(i, what) else Frame.of(i))
      .getOrElse(fail("did not build"))
    r.route("Could you tell them about it?") match
      case Router.Action.Ask(intent, slot, question, left) =>
        assertEquals(intent, "Request")
        assertEquals(slot, "what")
        assertEquals(question, "What would you like done?")
        assertEquals(left, 1)
      case other => fail(s"expected a question, got $other")
  }

  test("a tier that speaks other names cannot be wired in by accident") {
    // the silent disagreement this door exists to prevent: a model
    // fitted on one taxonomy, a router built against another
    val domain = Taxon.parsed(Seq("MeetingProposal", "MeetingRequest"))
    val wrong = Router.Router.of(domain, cues = Some(Models.cues))
    assert(wrong.isLeft, wrong)
    assert(wrong.left.exists(_.contains("Proposal")), wrong)

    val renamed = for
      c <- Patterns.meeting.renamed(domain,
        Map("Proposal" -> "MeetingProposal", "Request" -> "MeetingRequest",
          "Notification" -> "MeetingRequest", "Other" -> "MeetingRequest"))
      r <- Router.Router.of(domain, cues = Some(c))
    yield r
    assert(renamed.isRight, renamed)
  }

  test("a fitted model renames like a cue set does, or refuses") {
    val domain = Taxon.parsed(
      Seq("MeetingProposal", "MeetingRequest", "MeetingNotification", "NotAboutMeetings"))
    val full = Map("Proposal" -> "MeetingProposal", "Request" -> "MeetingRequest",
      "Notification" -> "MeetingNotification", "Other" -> "NotAboutMeetings")
    val renamed = CharGrams.renamed(Models.meeting, domain, full)
      .getOrElse(fail("the rename failed"))
    assertEquals(renamed.classes.sorted, domain.classes.sorted)
    // the same message, the same decision, under the new names
    val m = "Shall we meet on Tuesday?"
    assertEquals(CharGrams.score(renamed, m).map(_.best),
      CharGrams.score(Models.meeting, m).map(v => full(v.best)))
    // and a partial map is an error rather than a silent bucket
    assert(CharGrams.renamed(Models.meeting, domain, full - "Other").isLeft)
  }
}
