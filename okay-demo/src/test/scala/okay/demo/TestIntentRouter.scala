package okay.demo

import okay.intent.*

/**
 * The first caller okay-intent has ever had
 * (specs/intent-classify.md).
 *
 * These tests are not about accuracy — that is measured elsewhere and
 * on a fixture. They are about whether the pieces COMPOSE: whether a
 * message can travel from text to an action without the caller
 * reaching inside, re-implementing something, or holding a value the
 * types said it would have.
 *
 * No model, no network: the pattern tier needs neither, so the whole
 * path is exercisable in the default gate.
 */
class TestIntentRouter extends munit.FunSuite {

  private val today = Temporal.Date(2026, 9, 4)

  test("a message with a cue reaches an action without a model or a network") {
    IntentRouter.route("Could you send me the agenda?", today) match
      case IntentRouter.Action.Ask(intent, slot, question) =>
        assertEquals(intent, "MeetingRequest")
        assertEquals(slot, "what")
        assertEquals(question, "What would you like done?")
      case other => fail(s"expected a question, got $other")
  }

  test("a proposal is not actionable until it has a time, and the router says which") {
    IntentRouter.route("Shall we meet on Tuesday?", today) match
      case IntentRouter.Action.Ask(intent, slot, question) =>
        assertEquals(intent, "MeetingProposal")
        assertEquals(slot, "when")
        assert(question.contains("When"), question)
      case other => fail(s"expected the when question, got $other")
  }

  test("a class with nothing to fill is actionable at once") {
    IntentRouter.route("FYI the room has moved to B2", today) match
      case IntentRouter.Action.Act(intent, frame) =>
        assertEquals(intent, "MeetingNotification")
        assert(frame.complete())
      case other => fail(s"expected an action, got $other")
  }

  test("nothing recognised escalates to a person rather than guessing") {
    IntentRouter.route("zzz qqq xxx", today) match
      case IntentRouter.Action.Escalate(candidates, why) =>
        assertEquals(candidates, Seq.empty[String])
        assert(why.contains("no cue"), why)
      case other => fail(s"a router that guesses is the failure this prevents: $other")
  }

  test("the question comes in the reader's language, from the same call") {
    IntentRouter.route("Shall we meet on Tuesday?", today, lang = "ru") match
      case IntentRouter.Action.Ask(_, _, question) =>
        assertEquals(question, "Когда вам удобно встретиться?")
      case other => fail(s"$other")
  }

  test("the taxonomy is data: a service edits it without a compiler") {
    // the property the consumer asked for in request 1, exercised by a
    // caller rather than asserted in a unit test
    assert(IntentRouter.taxonomy.classes.forall(_.nonEmpty))
    assertEquals(IntentRouter.taxonomy.check(Seq("MeetingProposal")), Right(()))
    assert(IntentRouter.taxonomy.check(Seq("Refund")).isLeft)
  }

  test("a filled frame hands the caller a date, which is what acting on it needs") {
    // This test used to record a defect: the frame gave back the
    // string and the router parsed it a second time, with the same
    // reference day, because nothing in the type said to. Fixed in
    // intent-frame-typed-values, and the test now pins the property
    // rather than the workaround.
    val slot = Slots.when(today)
    val f = Frame.of("MeetingProposal", slot)
    val filled = f.answer("when", "en", "next thursday").toOption.get
    assert(filled.complete())
    assertEquals(filled.valueOf(slot).map(_.iso), Some("2026-09-10"))
    // and the text survives, for showing a person what they typed
    assertEquals(filled.filled("when"), "next thursday")
  }
}
