package okay.intent

import okay.frame.{Frame, Slot}

/**
 * The slot descriptor (specs/intent-classify.md) — request 5 of the
 * consumer seven, proposed for review.
 *
 * What is pinned here is the behaviour they asked for by name: a
 * parse failure is a RE-ASK and never a stored string, a question
 * exists per language, and `Temporal` fits the seam rather than
 * sitting beside it.
 */
class TestSlot extends munit.FunSuite {

  private val friday = Temporal.Date(2026, 9, 4)
  private val when = Slots.when(friday)

  test("a slot asks in the reader's language, and falls back rather than failing") {
    assertEquals(when.question("ru"), "Когда вам удобно встретиться?")
    assertEquals(when.question("ja"), "いつがご都合よろしいですか。")
    // a language nobody wrote a question for still gets one
    assertEquals(when.question("pl"), when.question("en"))
  }

  test("a parse failure is the question again, not an error message") {
    // the caller's next move is to ask, so that is what it is handed
    assertEquals(when.read("ru", "когда-нибудь"), Left("Когда вам удобно встретиться?"))
    assertEquals(when.read("en", "next thursday").map(_.iso), Right("2026-09-10"))
  }

  test("a frame knows what it still has to ask, and in what language") {
    val f = Frame.of("Proposal", when,
      Slots.text("who", Map("en" -> "Who should be there?"), required = false))
    assertEquals(f.missing, Vector("when" -> "When would you like to meet?"))
    assert(!f.complete)
    // an optional slot never appears in the questions
    assert(!f.missing.exists(_._1 == "who"))
  }

  test("an answer the slot cannot read leaves the frame UNCHANGED") {
    val f = Frame.of("Proposal", when)
    val bad = f.answer("when", "sometime soon")
    assertEquals(bad, Left("When would you like to meet?"))
    // and the property that failure is about: nothing was stored
    assertEquals(f.filled, Map.empty[String, String])
    val good = f.answer("when", "next thursday")
    assert(good.exists(_.complete), s"$good")
    assert(good.exists(_.filled.contains("when")))
  }

  test("a frame with every required slot answered is complete") {
    val f = Frame.of("Proposal", when, Slots.text("who", Map("en" -> "Who?")))
    val done = for
      a <- f.answer("when", "tomorrow at 2pm")
      b <- a.answer("who", "the design team")
    yield b
    assert(done.exists(_.complete), s"$done")
    assertEquals(done.map(_.missing), Right(Vector.empty))
  }

  test("an unknown slot is named, not ignored") {
    val f = Frame.of("Proposal", when)
    assertEquals(f.answer("where", "room B2"), Left("no slot named where"))
  }

  test("the descriptor holds no conversation state") {
    // the property the classifier's purity rests on: two frames built
    // the same way are equal, and answering returns a NEW frame
    val a = Frame.of("Proposal", when)
    val b = Frame.of("Proposal", when)
    assertEquals(a.filled, b.filled)
    val answered = a.answer("when", "tomorrow").toOption.get
    assertEquals(a.filled, Map.empty[String, String], "the original frame was mutated")
    assert(answered.filled.nonEmpty)
  }

  test("a filled frame hands back the VALUE, not the text again") {
    // the friction the first caller found: the router had proved
    // "next thursday" was an acceptable date and could only get the
    // string back, so it parsed it a second time with the same
    // reference day and nothing in the type said to
    val f = Frame.of("Proposal", when)
    val filled = f.answer("when", "next thursday").toOption.get
    assertEquals(filled.valueOf(when).map(_.iso), Some("2026-09-10"))
    // the text is still there for showing a person what they typed
    assertEquals(filled.filled("when"), "next thursday")
  }

  test("the value comes back at the slot's own type, and only for that slot") {
    // this is what makes the one cast in valueOf true: the answer
    // remembers WHICH slot parsed it, so a different slot with the
    // same name cannot collect it
    val other = Slots.text("when", Map("en" -> "When, as text?"))
    val filled = Frame.of("Proposal", when)
      .answer("when", "tomorrow").toOption.get
    assertEquals(filled.valueOf(when).map(_.iso), Some("2026-09-05"))
    assertEquals(filled.valueOf(other), None,
      "a slot that did not parse this answer must not be handed its value")
  }

  test("an unanswered slot has no value, and asking is not an error") {
    val f = Frame.of("Proposal", when)
    assertEquals(f.valueOf(when), None)
  }
}
